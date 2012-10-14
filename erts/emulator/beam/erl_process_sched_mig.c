#include "erl_process_sched_mig.h"

/*This file contains the implementation of the migration strategies*/

#ifdef ERTS_SMP
/* Run queue balancing */
typedef struct {
	Uint32 flags;
	struct {
		int max_len;
		int avail;
		int reds;
		int migration_limit;
		int emigrate_to;
		int immigrate_from;
	} prio[ERTS_NO_PRIO_LEVELS];
	int reds;
	int full_reds;
	int full_reds_history_sum;
	int full_reds_history_change;
	int oowc;
	int max_len;
} ErtsRunQueueBalance;
static ErtsRunQueueBalance *run_queue_info;

typedef struct {
	int qix;
	int len;
} ErtsRunQueueCompare;
static ErtsRunQueueCompare *run_queue_compare;

#endif

ERTS_INLINE void proc_sched_migrate_initialize(Uint no_runqs, Uint no_schedulers, Uint no_schedulers_online) {
#ifdef ERTS_SMP
	erts_aint32_t no_runqs_tmp;
	if (erts_no_run_queues != 1) {
		run_queue_info = erts_alloc(ERTS_ALC_T_RUNQ_BLNS,
				sizeof(ErtsRunQueueBalance) * no_runqs);
		run_queue_compare = erts_alloc(ERTS_ALC_T_RUNQ_BLNS,
				sizeof(ErtsRunQueueCompare) * no_runqs);
	}

    no_runqs_tmp = (erts_aint32_t) (no_schedulers & ERTS_NO_RUNQS_MASK);
    no_runqs_tmp |= (erts_aint32_t) ((no_schedulers_online & ERTS_NO_RUNQS_MASK) << ERTS_NO_USED_RUNQS_SHIFT);
	erts_smp_atomic32_init_nob(&balance_info.no_runqs, no_runqs_tmp);

	balance_info.last_active_runqs = no_schedulers;
	erts_smp_mtx_init(&balance_info.update_mtx, "migration_info_update");
	balance_info.forced_check_balance = 0;
	balance_info.halftime = 1;
	balance_info.full_reds_history_index = 0;
	erts_smp_atomic32_init_nob(&balance_info.checking_balance, 0);
	balance_info.prev_rise.active_runqs = 0;
	balance_info.prev_rise.max_len = 0;
	balance_info.prev_rise.reds = 0;
	balance_info.n = 0;
#endif
}


/***************************
 ***************************
 * Default
 ***************************
 ***************************/

//Forward declaration
#ifdef ERTS_SMP
static ERTS_INLINE Uint default_check_balance(ErtsRunQueue *rq);
#endif

Uint proc_sched_migrate_default_cb(ErtsRunQueue* rq) {
#ifdef ERTS_SMP
	return default_check_balance(rq);
#else
	return 0;
#endif
}

void proc_sched_migrate_default_immigrate(ErtsRunQueue* rq) {
#ifdef ERTS_SMP
	immigrate(rq);
#endif
}

/***************************
 ***************************
 * Disabled
 ***************************
 ***************************/

Uint proc_sched_migrate_disabled_cb(ErtsRunQueue* rq) {
	rq->check_balance_reds = INT_MAX;
	return 0;
}

void proc_sched_migrate_disabled_immigrate(ErtsRunQueue* ign) {
	//nothing to be done
}

/***************************************************************************************
 * Default implementation
 ***************************************************************************************/

#ifdef ERTS_SMP
static int rqc_len_cmp(const void *x, const void *y) {
	return ((ErtsRunQueueCompare *) x)->len - ((ErtsRunQueueCompare *) y)->len;
}

#define ERTS_PERCENT(X, Y) \
		((Y) == 0 \
				? ((X) == 0 ? 100 : INT_MAX) \
						: ((100*(X))/(Y)))

#define ERTS_UPDATE_FULL_REDS(QIX, LAST_REDS)				\
		do {									\
			run_queue_info[(QIX)].full_reds					\
			= run_queue_info[(QIX)].full_reds_history_sum;			\
			run_queue_info[(QIX)].full_reds += (LAST_REDS);			\
			run_queue_info[(QIX)].full_reds					\
			>>= ERTS_FULL_REDS_HISTORY_AVG_SHFT;				\
			run_queue_info[(QIX)].full_reds_history_sum				\
			-= run_queue_info[(QIX)].full_reds_history_change;		\
			run_queue_info[(QIX)].full_reds_history_sum += (LAST_REDS);		\
			run_queue_info[(QIX)].full_reds_history_change = (LAST_REDS);	\
		} while (0)

#define ERTS_DBG_CHK_FULL_REDS_HISTORY(RQ)				\
		do {									\
			int sum__ = 0;							\
			int rix__;								\
			for (rix__ = 0; rix__ < ERTS_FULL_REDS_HISTORY_SIZE; rix__++)	\
			sum__ += (RQ)->full_reds_history[rix__];			\
			ASSERT(sum__ == (RQ)->full_reds_history_sum);			\
		} while (0);

#define ERTS_BLNCE_SAVE_RISE(ACTIVE, MAX_LEN, REDS)	\
do {							\
    balance_info.prev_rise.active_runqs = (ACTIVE);	\
    balance_info.prev_rise.max_len = (MAX_LEN);		\
    balance_info.prev_rise.reds = (REDS);		\
} while (0)



static ERTS_INLINE int half_time_check(ErtsRunQueue *c_rq) {
	if (balance_info.halftime) {
		balance_info.halftime = 0;
		erts_smp_atomic32_set_nob(&balance_info.checking_balance, 0);
		ERTS_FOREACH_RUNQ(rq,
				{
						if (rq->waiting)
							rq->flags |= ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK;
						else
							rq->flags &= ~ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK;
						rq->check_balance_reds = ERTS_RUNQ_CALL_CHECK_BALANCE_REDS;
				});

		erts_smp_runq_lock(c_rq);
		return 1;
	}
	return 0;
}

static ERTS_INLINE void copy_run_queues_info(int blnc_no_rqs, int freds_hist_ix) {
	int qix, pix;
	for (qix = 0; qix < blnc_no_rqs; qix++) {
		ErtsRunQueue *rq = ERTS_RUNQ_IX(qix);
		erts_smp_runq_lock(rq);

		run_queue_info[qix].flags = rq->flags;
		for (pix = 0; pix < ERTS_NO_PROC_PRIO_LEVELS; pix++) {
			run_queue_info[qix].prio[pix].max_len =
					rq->procs.prio_info[pix].max_len;
			run_queue_info[qix].prio[pix].reds = rq->procs.prio_info[pix].reds;
		}
		run_queue_info[qix].prio[ERTS_PORT_PRIO_LEVEL].max_len =
				rq->ports.info.max_len;
		run_queue_info[qix].prio[ERTS_PORT_PRIO_LEVEL].reds =
				rq->ports.info.reds;

		run_queue_info[qix].full_reds_history_sum = rq->full_reds_history_sum;
		run_queue_info[qix].full_reds_history_change =
				rq->full_reds_history[freds_hist_ix];

		run_queue_info[qix].oowc = rq->out_of_work_count;
		run_queue_info[qix].max_len = rq->max_len;
		rq->check_balance_reds = INT_MAX;

		erts_smp_runq_unlock(rq);
	}
}

struct avail_calc {
	int full_scheds;
	int half_full_scheds;
	Sint64 full_scheds_reds;
	Sint64 scheds_reds;
	int oowc;
	int mmax_len;
};

static ERTS_INLINE void init_avail_calc(struct avail_calc *ac) {
	ac->full_scheds = 0;
	ac->half_full_scheds = 0;
	ac->full_scheds_reds = 0;
	ac->scheds_reds = 0;
	ac->oowc = 0;
	ac->mmax_len = 0;
}

static ERTS_INLINE void calculate_availability(int blnc_no_rqs, struct avail_calc *ac) {
	int qix, pix;
	for (qix = 0; qix < blnc_no_rqs; qix++) {
		int treds = 0;

		if (run_queue_info[qix].flags & ERTS_RUNQ_FLG_OUT_OF_WORK) {
			for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
				run_queue_info[qix].prio[pix].avail = 100;
				treds += run_queue_info[qix].prio[pix].reds;
			}
			if (!(run_queue_info[qix].flags	& ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK))
				ac->half_full_scheds++;
			ERTS_UPDATE_FULL_REDS(qix, ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED);
		} else {
			ASSERT(!(run_queue_info[qix].flags & ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK));
			for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++)
				treds += run_queue_info[qix].prio[pix].reds;
			if (treds == 0) {
				for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++)
					run_queue_info[qix].prio[pix].avail = 0;
			} else {
				Sint64 xreds = 0;
				Sint64 procreds = treds;
				procreds -=
						((Sint64) run_queue_info[qix].prio[ERTS_PORT_PRIO_LEVEL].reds);

				for (pix = 0; pix < ERTS_NO_PROC_PRIO_LEVELS; pix++) {
					Sint64 av;

					if (xreds == 0)
						av = 100;
					else if (procreds == xreds)
						av = 0;
					else {
						av = (100 * (procreds - xreds)) / procreds;
						if (av == 0)
							av = 1;
					}
					run_queue_info[qix].prio[pix].avail = (int) av;
					ASSERT(run_queue_info[qix].prio[pix].avail >= 0);
					if (pix < PRIORITY_NORMAL) /* ie., max or high */
							xreds += (Sint64) run_queue_info[qix].prio[pix].reds;
				}
				run_queue_info[qix].prio[ERTS_PORT_PRIO_LEVEL].avail = 100;
			}
			ERTS_UPDATE_FULL_REDS(qix, treds);
			ac->full_scheds_reds += run_queue_info[qix].full_reds;
			ac->full_scheds++;
			ac->half_full_scheds++;
		}
		run_queue_info[qix].reds = treds;
		ac->scheds_reds += treds;
		ac->oowc += run_queue_info[qix].oowc;
		if (ac->mmax_len < run_queue_info[qix].max_len)
			ac->mmax_len = run_queue_info[qix].max_len;
	}
}

static ERTS_INLINE int calculate_migration_paths_all_active(int blnc_no_rqs, struct avail_calc *ac) {
	int qix, pix;
	ErtsRunQueueBalance avg = { 0 };
	int active = blnc_no_rqs;

	for (qix = 0; qix < blnc_no_rqs; qix++) {

		if (ac->full_scheds_reds > 0) {
			/* Calculate availability compared to other schedulers */
			if (!(run_queue_info[qix].flags & ERTS_RUNQ_FLG_OUT_OF_WORK)) {
				Sint64 tmp = ((Sint64) run_queue_info[qix].full_reds
						* (Sint64) ac->full_scheds);
				for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
					Sint64 avail = run_queue_info[qix].prio[pix].avail;
					avail = (avail * tmp) / ac->full_scheds_reds;
					ASSERT(avail >= 0);
					run_queue_info[qix].prio[pix].avail = (int) avail;
				}
			}
		}

		/* Calculate average max length */
		for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
			run_queue_info[qix].prio[pix].emigrate_to = -1;
			run_queue_info[qix].prio[pix].immigrate_from = -1;
			avg.prio[pix].max_len += run_queue_info[qix].prio[pix].max_len;
			avg.prio[pix].avail += run_queue_info[qix].prio[pix].avail;
		}

	}

	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
		int max_len = avg.prio[pix].max_len;
		if (max_len != 0) {
			int avail = avg.prio[pix].avail;
			if (avail != 0) {
				max_len = (int) ((100 * ((Sint64) max_len) - 1)
						/ ((Sint64) avail)) + 1;
				avg.prio[pix].max_len = max_len;
				ASSERT(max_len >= 0);
			}
		}
	}

	/* Calculate migration limits for all priority queues in all
	 run queues */
	for (qix = 0; qix < blnc_no_rqs; qix++) {
		run_queue_info[qix].flags = 0; /* Reset for later use... */
		for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
			int limit;
			if (avg.prio[pix].max_len == 0
					|| run_queue_info[qix].prio[pix].avail == 0)
				limit = 0;
			else
				limit = (int) (((((Sint64) avg.prio[pix].max_len)
						* ((Sint64) run_queue_info[qix].prio[pix].avail)) - 1)
						/ 100 + 1);
			run_queue_info[qix].prio[pix].migration_limit = limit;
		}
	}

	/* Setup migration paths for all priorities */
	for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
		int low = 0, high = 0;
		for (qix = 0; qix < blnc_no_rqs; qix++) {
			int len_diff = run_queue_info[qix].prio[pix].max_len;
			len_diff -= run_queue_info[qix].prio[pix].migration_limit;
#ifdef DBG_PRINT
			if (pix == 2) erts_fprintf(stderr, "%d ", len_diff);
#endif
			run_queue_compare[qix].qix = qix;
			run_queue_compare[qix].len = len_diff;
			if (len_diff != 0) {
				if (len_diff < 0)
					low++;
				else
					high++;
			}
		}
#ifdef DBG_PRINT
		if (pix == 2) erts_fprintf(stderr, "\n");
#endif
		if (low && high) {
			int from_qix;
			int to_qix;
			int eof = 0;
			int eot = 0;
			int tix = 0;
			int fix = blnc_no_rqs - 1;
			qsort(run_queue_compare, blnc_no_rqs, sizeof(ErtsRunQueueCompare),
					rqc_len_cmp);

			while (1) {
				// no more overloaded queues
				if (run_queue_compare[fix].len <= 0)
					eof = 1;
				// no more underloaded queues
				if (run_queue_compare[tix].len >= 0)
					eot = 1;
				if (eof || eot)
					break;
				from_qix = run_queue_compare[fix].qix;
				to_qix = run_queue_compare[tix].qix;
				if (run_queue_info[from_qix].prio[pix].avail == 0) {
					ERTS_SET_RUNQ_FLG_EVACUATE( run_queue_info[from_qix].flags,
							pix);
					ERTS_SET_RUNQ_FLG_EVACUATE(run_queue_info[to_qix].flags,
							pix);
				}
				ERTS_SET_RUNQ_FLG_EMIGRATE(run_queue_info[from_qix].flags, pix);
				ERTS_SET_RUNQ_FLG_IMMIGRATE(run_queue_info[to_qix].flags, pix);
				run_queue_info[from_qix].prio[pix].emigrate_to = to_qix;
				run_queue_info[to_qix].prio[pix].immigrate_from = from_qix;
				tix++;
				fix--;

#ifdef DBG_PRINT
				if (pix == 2) erts_fprintf(stderr, "%d >--> %d\n", from_qix, to_qix);
#endif
			}

			if (!eot && eof) { //in this case there are more underloaded queues than overloaded
				if (fix < blnc_no_rqs - 1)
					fix++;

				if (run_queue_compare[fix].len > 0) {
					int fix2 = -1;
					while (tix < fix) {
						if (run_queue_compare[tix].len >= 0)
							break;
						if (fix2 < fix)
							fix2 = blnc_no_rqs - 1;
						from_qix = run_queue_compare[fix2].qix;
						to_qix = run_queue_compare[tix].qix;
						ASSERT(to_qix != from_qix);
						if (run_queue_info[from_qix].prio[pix].avail == 0)
							ERTS_SET_RUNQ_FLG_EVACUATE(
									run_queue_info[to_qix].flags, pix);
						ERTS_SET_RUNQ_FLG_IMMIGRATE(
								run_queue_info[to_qix].flags, pix);
						run_queue_info[to_qix].prio[pix].immigrate_from =
								from_qix;
						tix++;
						fix2--;
#ifdef DBG_PRINT
						if (pix == 2) erts_fprintf(stderr, "%d  --> %d\n", from_qix, to_qix);
#endif
					}
				}
			} else if (!eof && eot) {//in this case there are more overloaded queues than underloaded
				if (tix > 0)
					tix--;
				if (run_queue_compare[tix].len < 0) {
					int tix2 = 0;
					while (tix < fix) {
						if (run_queue_compare[fix].len <= 0)
							break;
						if (tix2 > tix)
							tix2 = 0;
						from_qix = run_queue_compare[fix].qix;
						to_qix = run_queue_compare[tix2].qix;
						ASSERT(to_qix != from_qix);
						if (run_queue_info[from_qix].prio[pix].avail == 0)
							ERTS_SET_RUNQ_FLG_EVACUATE(
									run_queue_info[from_qix].flags, pix);
						ERTS_SET_RUNQ_FLG_EMIGRATE(
								run_queue_info[from_qix].flags, pix);
						run_queue_info[from_qix].prio[pix].emigrate_to = to_qix;
						fix--;
						tix2++;
#ifdef DBG_PRINT
						if (pix == 2) erts_fprintf(stderr, "%d >--  %d\n", from_qix, to_qix);
#endif

					}
				}
			}
		}
	}

#ifdef DBG_PRINT
	erts_fprintf(stderr, "--------------------------------\n");
#endif
	return active;
}

static ERTS_INLINE int calculate_migration_paths_some_active(int current_active, int blnc_no_rqs, struct avail_calc *ac) {
	int qix, pix, active;
	int min = 1;
	if (min < ac->half_full_scheds)
		min = ac->half_full_scheds;
	if (ac->full_scheds) {
		active = (ac->scheds_reds - 1) / ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED + 1;
	} else {
		active = balance_info.last_active_runqs - 1;
	}

	if (balance_info.last_active_runqs < current_active) {
		ERTS_BLNCE_SAVE_RISE(current_active, ac->mmax_len, ac->scheds_reds);
		active = current_active;
	} else if (active < balance_info.prev_rise.active_runqs) {
		if (ERTS_PERCENT(ac->mmax_len,
				balance_info.prev_rise.max_len) >= 90
				&& ERTS_PERCENT(ac->scheds_reds,
						balance_info.prev_rise.reds) >= 90) {
			active = balance_info.prev_rise.active_runqs;
		}
	}

	if (active < min)
		active = min;
	else if (active > blnc_no_rqs)
		active = blnc_no_rqs;

	if (active == blnc_no_rqs) {
		active = calculate_migration_paths_all_active(blnc_no_rqs, ac);
	} else {
		for (qix = 0; qix < active; qix++) {
			run_queue_info[qix].flags = 0;
			for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
				run_queue_info[qix].prio[pix].emigrate_to = -1;
				run_queue_info[qix].prio[pix].immigrate_from = -1;
				run_queue_info[qix].prio[pix].migration_limit = 0;
			}
		}
		for (qix = active; qix < blnc_no_rqs; qix++) {
			run_queue_info[qix].flags = ERTS_RUNQ_FLG_INACTIVE;
			for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
				int tix = qix % active;
				ERTS_SET_RUNQ_FLG_EMIGRATE(run_queue_info[qix].flags, pix);
				run_queue_info[qix].prio[pix].emigrate_to = tix;
				run_queue_info[qix].prio[pix].immigrate_from = -1;
				run_queue_info[qix].prio[pix].migration_limit = 0;
			}
		}
	}
	return active;
}

static ERTS_INLINE void write_migration_paths(int blnc_no_rqs, int freds_hist_ix) {
	int pix, qix;

	for (qix = 0; qix < blnc_no_rqs; qix++) {
		int mqix;
		Uint32 flags;
		ErtsRunQueue *rq = ERTS_RUNQ_IX(qix);
		ErtsRunQueueInfo *rqi;
		flags = run_queue_info[qix].flags;
		erts_smp_runq_lock(rq);
		flags |= (rq->flags & ~ERTS_RUNQ_FLGS_MIGRATION_INFO);
		ASSERT(!(flags & ERTS_RUNQ_FLG_OUT_OF_WORK));
		if (rq->waiting)
			flags |= ERTS_RUNQ_FLG_OUT_OF_WORK;

		rq->full_reds_history_sum = run_queue_info[qix].full_reds_history_sum;
		rq->full_reds_history[freds_hist_ix] =
				run_queue_info[qix].full_reds_history_change;

		ERTS_DBG_CHK_FULL_REDS_HISTORY(rq);

		rq->out_of_work_count = 0;
		rq->flags = flags;
		rq->max_len = rq->len;
		for (pix = 0; pix < ERTS_NO_PRIO_LEVELS; pix++) {
			rqi = (pix == ERTS_PORT_PRIO_LEVEL ?
					&rq->ports.info : &rq->procs.prio_info[pix]);
			rqi->max_len = rqi->len;
			rqi->reds = 0;
			if (!(ERTS_CHK_RUNQ_FLG_EMIGRATE(flags, pix)
					| ERTS_CHK_RUNQ_FLG_IMMIGRATE(flags, pix))) {
				ASSERT(run_queue_info[qix].prio[pix].immigrate_from < 0); ASSERT(run_queue_info[qix].prio[pix].emigrate_to < 0);
#ifdef DEBUG
				rqi->migrate.limit.this = -1;
				rqi->migrate.limit.other = -1;
				ERTS_DBG_SET_INVALID_RUNQP(rqi->migrate.runq, 0x2);
#endif
			} else if (ERTS_CHK_RUNQ_FLG_EMIGRATE(flags, pix)) {
				ASSERT(!ERTS_CHK_RUNQ_FLG_IMMIGRATE(flags, pix)); ASSERT(run_queue_info[qix].prio[pix].immigrate_from < 0); ASSERT(run_queue_info[qix].prio[pix].emigrate_to >= 0);

				mqix = run_queue_info[qix].prio[pix].emigrate_to;
				rqi->migrate.limit.this =
						run_queue_info[qix].prio[pix].migration_limit;
				rqi->migrate.limit.other =
						run_queue_info[mqix].prio[pix].migration_limit;
				rqi->migrate.runq = ERTS_RUNQ_IX(mqix);
			} else {
				ASSERT(ERTS_CHK_RUNQ_FLG_IMMIGRATE(flags, pix)); ASSERT(run_queue_info[qix].prio[pix].emigrate_to < 0); ASSERT(run_queue_info[qix].prio[pix].immigrate_from >= 0);

				mqix = run_queue_info[qix].prio[pix].immigrate_from;
				rqi->migrate.limit.this =
						run_queue_info[qix].prio[pix].migration_limit;
				rqi->migrate.limit.other =
						run_queue_info[mqix].prio[pix].migration_limit;
				rqi->migrate.runq = ERTS_RUNQ_IX(mqix);
			}
		}

		rq->check_balance_reds = ERTS_RUNQ_CALL_CHECK_BALANCE_REDS;
		erts_smp_runq_unlock(rq);
	}
}

static ERTS_INLINE Uint default_check_balance(ErtsRunQueue *c_rq) {
	int forced, active, current_active, blnc_no_rqs, freds_hist_ix;
	Uint ret;
	struct avail_calc ac;

	//checks if some other scheduler is check-balancing
	if (erts_smp_atomic32_xchg_nob(&balance_info.checking_balance, 1)) {
		c_rq->check_balance_reds = INT_MAX;
		return 0;
	}

	//if the number of online schedulers == 1, nothing to be done
	get_no_runqs(NULL, &blnc_no_rqs);
	if (blnc_no_rqs == 1) {
		c_rq->check_balance_reds = INT_MAX;
		erts_smp_atomic32_set_nob(&balance_info.checking_balance, 0);
		return 0;
	}

	erts_smp_runq_unlock(c_rq);

	//Half-time check. Checks if schedulers are active and flags them
	if (half_time_check(c_rq)) return 0;

	/*
	 * check_balance() is never called in more threads
	 * than one at a time, i.e., we will normally never
	 * get any conflicts on the balance_info.update_mtx.
	 * However, when blocking multi scheduling (which performance
	 * critical applications do *not* do) migration information
	 * is manipulated. Such updates of the migration information
	 * might clash with balancing.
	 */
	erts_smp_mtx_lock(&balance_info.update_mtx);

	forced = balance_info.forced_check_balance;
	balance_info.forced_check_balance = 0;

	get_no_runqs(&current_active, &blnc_no_rqs);

	//again, if the number of online-schedulers == 1, nothing to be balanced
	if (blnc_no_rqs == 1) {
		erts_smp_mtx_unlock(&balance_info.update_mtx);
		erts_smp_runq_lock(c_rq);
		c_rq->check_balance_reds = INT_MAX;
		erts_smp_atomic32_set_nob(&balance_info.checking_balance, 0);
		return 0;
	}

	freds_hist_ix = balance_info.full_reds_history_index;
	balance_info.full_reds_history_index++;
	if (balance_info.full_reds_history_index >= ERTS_FULL_REDS_HISTORY_SIZE)
		balance_info.full_reds_history_index = 0;

	/* Read balance information for all run queues */
	copy_run_queues_info(blnc_no_rqs, freds_hist_ix);

	/* Calculate availability for each priority in each run queue */
	init_avail_calc(&ac);
	calculate_availability(blnc_no_rqs, &ac);

	/* if no compact_load, no need to calculate how many schedulers should be active*/
	if (!erts_sched_compact_load) {
		active = calculate_migration_paths_all_active(blnc_no_rqs, &ac);
	} else if (!forced && ac.half_full_scheds != blnc_no_rqs) {
		active = calculate_migration_paths_some_active(current_active, blnc_no_rqs, &ac);
	} else {
		if (balance_info.last_active_runqs < current_active)
			ERTS_BLNCE_SAVE_RISE(current_active, ac.mmax_len, ac.scheds_reds);
		active = calculate_migration_paths_all_active(blnc_no_rqs, &ac);
	}

	balance_info.last_active_runqs = active;
	set_no_active_runqs(active);

	balance_info.halftime = 1;
	erts_smp_atomic32_set_nob(&balance_info.checking_balance, 0);

	/* Write migration paths and reset balance statistics in all queues */
	write_migration_paths(blnc_no_rqs, freds_hist_ix);

	balance_info.n++;
	ret = balance_info.n;

	erts_smp_mtx_unlock(&balance_info.update_mtx);

	erts_smp_runq_lock(c_rq);

	return ret;
}
#endif
