#include "erl_process_sched_ws.h"
#include "global.h"

/*This file contains the implementation of the work stealing strategies*/


/***************************
 ***************************
 * Default
 ***************************
 ***************************/
#ifdef ERTS_SMP
static ERTS_INLINE int try_steal_task(ErtsRunQueue *rq, int bring_home);
#endif
ERTS_INLINE int proc_sched_ws_default(ErtsRunQueue* rq) {
#ifdef ERTS_SMP
	return try_steal_task(rq, 0);
#else
	return 0;
#endif
}

/***************************
 ***************************
 * Disabled
 ***************************
 ***************************/

ERTS_INLINE int proc_sched_ws_disabled(ErtsRunQueue* rq) {
	return 0;
}

/***************************
 ***************************
 * NUMA Aware
 ***************************
 ***************************/

ERTS_INLINE int proc_sched_ws_numa_aware(ErtsRunQueue* rq) {
#ifdef ERTS_SMP
	return try_steal_task(rq, 1);
#else
	return 0;
#endif
};


/******************************************************
 ******************************************************/


#ifdef ERTS_SMP


static ERTS_INLINE Process* find_foreign_process_to_steal_from_victim(ErtsRunQueue *rq, ErtsRunQueue *vrq) {
	int my_node = rq->numa_node;
	ProcessLinkedList* cell = vrq->foreign_process_list_head[my_node]->next;
	while (cell) {
		Process *p = cell->p;
		if (!p->bound_runq && //not bound
			!(p->runq_flags & ERTS_PROC_RUNQ_FLG_RUNNING) && //not running
			(p->status_flags & ERTS_PROC_SFLG_INRUNQ)) //in the RQ
			return p;
		cell = cell->next;
	}
	return NULL;
}

static ERTS_INLINE Process* find_regular_process_to_steal_from_victim(ErtsRunQueue *rq, ErtsRunQueue *vrq) {
	
	/*
	 * Check for a runnable process to steal...
	 */

	Process* proc;
	switch (vrq->flags & ERTS_RUNQ_FLGS_PROCS_QMASK) {
	 	case MAX_BIT:
	 	case MAX_BIT|HIGH_BIT:
	 	case MAX_BIT|NORMAL_BIT:
	 	case MAX_BIT|LOW_BIT:
	 	case MAX_BIT|HIGH_BIT|NORMAL_BIT:
	 	case MAX_BIT|HIGH_BIT|LOW_BIT:
	 	case MAX_BIT|NORMAL_BIT|LOW_BIT:
	 	case MAX_BIT|HIGH_BIT|NORMAL_BIT|LOW_BIT:
		 	for (proc = vrq->procs.prio[PRIORITY_MAX].last; proc; proc = proc->prev) {
		 		if (!proc->bound_runq)
		 			break;
		 	}
		 	if (proc)
		 		break;
	 	case HIGH_BIT:
	 	case HIGH_BIT|NORMAL_BIT:
	 	case HIGH_BIT|LOW_BIT:
	 	case HIGH_BIT|NORMAL_BIT|LOW_BIT:
		 	for (proc = vrq->procs.prio[PRIORITY_HIGH].last;proc;proc = proc->prev) {
		 		if (!proc->bound_runq)
		 			break;
		 	}
		 	if (proc)
		 		break;
	 	case NORMAL_BIT:
	 	case LOW_BIT:
	 	case NORMAL_BIT|LOW_BIT:
		 	for (proc = vrq->procs.prio[PRIORITY_NORMAL].last;proc;proc = proc->prev) {
		 		if (!proc->bound_runq)
		 			break;
		 	}
		 	if (proc)
		 		break;
	 	case 0:
		 	proc = NULL;
		 	break;
	 	default:
		 	ASSERT(!"Invalid queue mask");
		 	proc = NULL;
	 		break;
	}
	return proc;
}


/* Bring home meanings
 * 0 - Any process will do
 * 1 - Try to bring home first, and then if not possible, any process
 * 2 - Only accept processes coming home
 */
static ERTS_INLINE Process* find_proc_to_steal_from_victim (ErtsRunQueue *rq, ErtsRunQueue *vrq, int bring_home) {
	Process* proc = NULL;
	
	if (bring_home) { //bring home: 1 or 2
		proc = find_foreign_process_to_steal_from_victim(rq, vrq);
//		if (proc) fprintf(stderr, "%d -> %d Work Stealing BRING HOME: %d Found: %p\n", vrq->ix, rq->ix, bring_home, proc); fflush(stderr);
	}

	if (proc == NULL && bring_home < 2) {
		proc = find_regular_process_to_steal_from_victim(rq, vrq);
//		if (proc) fprintf(stderr, "%d -> %d Work Stealing NOT bring home: %d Found: %p\n", vrq->ix, rq->ix, bring_home, proc); fflush(stderr);
	}
	return proc;
};


static ERTS_INLINE int try_steal_task_from_victim(ErtsRunQueue *rq, int *rq_lockedp, ErtsRunQueue *vrq, int bring_home) {
	Process *proc;
	int vrq_locked;

	if (*rq_lockedp)
		erts_smp_xrunq_lock(rq, vrq);
	else
		erts_smp_runq_lock(vrq);
	vrq_locked = 1;

	ERTS_SMP_LC_CHK_RUNQ_LOCK(rq, *rq_lockedp);
	ERTS_SMP_LC_CHK_RUNQ_LOCK(vrq, vrq_locked);

	if (rq->halt_in_progress)
		goto try_steal_port;

	proc = find_proc_to_steal_from_victim (rq, vrq, bring_home);

	if (proc) {
		ErtsProcLocks proc_locks = 0;
		int res;
		ErtsMigrateResult mres;
		mres = erts_proc_migrate(proc, &proc_locks,
			vrq, &vrq_locked,
			rq, rq_lockedp);
		if (proc_locks)
			erts_smp_proc_unlock(proc, proc_locks);
		res = !0;
		switch (mres) {
			case ERTS_MIGRATE_FAILED_RUNQ_SUSPENDED:
				res = 0;
			case ERTS_MIGRATE_SUCCESS:
	 			if (vrq_locked)
	 				erts_smp_runq_unlock(vrq);
//	 			fprintf(stderr, "%d -> %d Work Stealing SUCCESS Bring Home: %d Found: %p\n", vrq->ix, rq->ix, bring_home, proc); fflush(stderr);
	 			return res;
			default: /* Other failures */
				break;
		}
	}

	ERTS_SMP_LC_CHK_RUNQ_LOCK(rq, *rq_lockedp);
	ERTS_SMP_LC_CHK_RUNQ_LOCK(vrq, vrq_locked);

	if (!vrq_locked) {
		if (*rq_lockedp)
	 		erts_smp_xrunq_lock(rq, vrq);
	 	else
	 		erts_smp_runq_lock(vrq);
	 	vrq_locked = 1;
	 }

	 try_steal_port:

	 ERTS_SMP_LC_CHK_RUNQ_LOCK(rq, *rq_lockedp);
	 ERTS_SMP_LC_CHK_RUNQ_LOCK(vrq, vrq_locked);

	/*
	 * Check for a runnable port to steal...
	 */

	if (vrq->ports.info.len) {
		Port *prt = vrq->ports.end;
		int prt_locked = 0;
		int res;
		ErtsMigrateResult mres;
		mres = erts_port_migrate(prt, &prt_locked,
			vrq, &vrq_locked,
			rq, rq_lockedp);
		if (prt_locked)
			erts_smp_port_unlock(prt);
		res = !0;
		switch (mres) {
			case ERTS_MIGRATE_FAILED_RUNQ_SUSPENDED:
				res = 0;
			case ERTS_MIGRATE_SUCCESS:
				if (vrq_locked)
					erts_smp_runq_unlock(vrq);
				return res;
			default: /* Other failures */
				break;
		}
	}
	if (vrq_locked)
		erts_smp_runq_unlock(vrq);
	return 0;
}

static ERTS_INLINE int check_possible_steal_victim(ErtsRunQueue *rq, int *rq_lockedp, int vix, int bring_home) {
	ErtsRunQueue *vrq = ERTS_RUNQ_IX(vix);
	erts_aint32_t iflgs = erts_smp_atomic32_read_nob(&vrq->info_flags);
	if (iflgs & ERTS_RUNQ_IFLG_NONEMPTY)
		return try_steal_task_from_victim(rq, rq_lockedp, vrq, bring_home);
	else
		return 0;
}

static ERTS_INLINE int try_steal_task(ErtsRunQueue *rq, int numa_aware) {
	int res, rq_locked, vix, active_rqs, blnc_rqs, bring_home;

	/*
	 * We are not allowed to steal jobs to this run queue
	 * if it is suspended. Note that it might get suspended
	 * at any time when we don't have the lock on the run
	 * queue.
	 */
	if (rq->flags & ERTS_RUNQ_FLG_SUSPENDED)
		return 0;

	res = 0;
	rq_locked = 1;

	ERTS_SMP_LC_CHK_RUNQ_LOCK(rq, rq_locked);

	get_no_runqs(&active_rqs, &blnc_rqs);

	if (active_rqs > blnc_rqs)
		active_rqs = blnc_rqs;



	if (rq->ix < active_rqs) {

		/* First try to steal from an inactive run queue... */
		bring_home = (numa_aware) ? 1 : 0; // for inactive queues any process will do, but I'd rather bring processes home
		if (active_rqs < blnc_rqs) {
			int no = blnc_rqs - active_rqs;
			int stop_ix = vix = active_rqs + rq->ix % no;
			while (erts_smp_atomic32_read_acqb(&no_empty_run_queues) < blnc_rqs) {
				res = check_possible_steal_victim(rq, &rq_locked, vix, bring_home);
				if (res)
					goto done;
				vix++;
				if (vix >= blnc_rqs)
					vix = active_rqs;
				if (vix == stop_ix)
					break;
			}
		}

		/* ... then try to steal a job from another active queue... */
		//We first try to find someone to bring home, if not possible,
		//then we bring someone else
		bring_home = (numa_aware) ? 2 : 0;
		for (; bring_home >= 0; bring_home -= 2) {
			vix = rq->ix;
			while (erts_smp_atomic32_read_acqb(&no_empty_run_queues) < blnc_rqs) {
				vix++;
				if (vix >= active_rqs)
					vix = 0;
				if (vix == rq->ix)
					break;

				res = check_possible_steal_victim(rq, &rq_locked, vix, bring_home);
				if (res)
					goto done;
			}
		}

	}

	done:

	if (!rq_locked)
		erts_smp_runq_lock(rq);

	if (!res)
		res = rq->halt_in_progress ?
				!ERTS_EMPTY_RUNQ_PORTS(rq) : !ERTS_EMPTY_RUNQ(rq);

	return res;
}



#endif

