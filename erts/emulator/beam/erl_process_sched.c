#include "erl_process_sched.h"

#include "erl_process_sched_ip.h"
#include "erl_process_sched_mig.h"
#include "erl_process_sched_ws.h"

#include "dtrace-wrapper.h"

//fw declarations
ERTS_INLINE static void proc_sched_ip_initialize(void);
ERTS_INLINE static void proc_sched_cb_initialize(void);
ERTS_INLINE static void proc_sched_ws_initialize(void);
ERTS_INLINE static void internal_proc_sched_set_initial_placement_strategy (proc_sched_ip_strategy strategy, int hub);
ERTS_INLINE static void internal_proc_sched_set_migration_strategy(proc_sched_migration_strategy strategy);
ERTS_INLINE static void internal_proc_sched_set_ws_strategy(proc_sched_ws_strategy strategy);
ERTS_INLINE void proc_sched_verify_tasks_to_run_after (Uint cbs);

/***********************************
 ***********************************
 * Initialization and Configuration
 ***********************************
 ***********************************/

ERTS_INLINE void proc_sched_initialize(Uint nQueues,  Uint no_schedulers, Uint no_schedulers_online) {
	proc_sched_ip_initialize();
	proc_sched_cb_initialize();
	proc_sched_ws_initialize();
	proc_sched_migrate_initialize(nQueues, no_schedulers, no_schedulers_online);
}

/***************************
 ***************************
 * Initial Placement Strategies
 ***************************
 ***************************/

static Uint SCHEDULED_IP_CHANGEMENT;
static byte SCHEDULED_IP_STRATEGY;
static int SCHEDULED_IP_CHANGEMENT_TYPE_HUB;

static byte PROC_SCHED_CURRENT_IP_STRATEGY_HUB;
static byte PROC_SCHED_CURRENT_IP_STRATEGY_REGULAR;
static ErtsRunQueue *(*PROC_SCHED_CURRENT_IP_STRATEGY_FUN[7])(Process*, Process*);

ERTS_INLINE static void proc_sched_ip_initialize(void) {

	SCHEDULED_IP_CHANGEMENT = INT_MAX;

	//DON'T FORGET TO CHANGE THE ARRAY SIZE!!! 
	PROC_SCHED_CURRENT_IP_STRATEGY_FUN[PROC_SCHED_IP_DEFAULT] = &proc_sched_ip_default; //0
#ifdef ERTS_SMP	
	PROC_SCHED_CURRENT_IP_STRATEGY_FUN[PROC_SCHED_IP_RANDOM] = &proc_sched_ip_random;  //1
	PROC_SCHED_CURRENT_IP_STRATEGY_FUN[PROC_SCHED_IP_CIRCULAR] = &proc_sched_ip_circular; //2
	PROC_SCHED_CURRENT_IP_STRATEGY_FUN[PROC_SCHED_IP_SIMPLE_RANDOM] = &proc_sched_ip_simple_random; //3
	PROC_SCHED_CURRENT_IP_STRATEGY_FUN[PROC_SCHED_IP_LOCAL_CIRCULAR] = &proc_sched_ip_local_circular; //4
	PROC_SCHED_CURRENT_IP_STRATEGY_FUN[PROC_SCHED_IP_SCATTER] = &proc_sched_ip_scatter; //5
	PROC_SCHED_CURRENT_IP_STRATEGY_FUN[PROC_SCHED_IP_COMPACT] = &proc_sched_ip_compact; //6
#endif
	internal_proc_sched_set_initial_placement_strategy(PROC_SCHED_IP_DEFAULT, 0);//regular
	internal_proc_sched_set_initial_placement_strategy(PROC_SCHED_IP_DEFAULT, 1);//hub
}


ERTS_INLINE void proc_sched_set_initial_placement_strategy (proc_sched_ip_strategy strategy, int hub) {
	if ((hub && strategy != PROC_SCHED_CURRENT_IP_STRATEGY_HUB) ||
		(!hub && strategy!= PROC_SCHED_CURRENT_IP_STRATEGY_REGULAR))
			internal_proc_sched_set_initial_placement_strategy(strategy, hub);
}

ERTS_INLINE static void internal_proc_sched_set_initial_placement_strategy (proc_sched_ip_strategy strategy, int hub) {
#ifdef USE_VM_PROBES
	//if (DTRACE_ENABLED(scheduler_ip_change))
	DTRACE1(scheduler_ip_strategy_change, strategy);
#endif
	if (hub) 
		PROC_SCHED_CURRENT_IP_STRATEGY_HUB = strategy & 0xFF;
	else
		PROC_SCHED_CURRENT_IP_STRATEGY_REGULAR = strategy & 0xFF;
}


ERTS_INLINE void proc_sched_set_initial_placement_strategy_after(proc_sched_ip_strategy str, int after_no_cb, int hub) {
	Uint n = 0;
#ifdef ERTS_SMP
	erts_smp_mtx_lock(&balance_info.update_mtx);
	n = balance_info.n;
#endif
	if (after_no_cb == 0) {
		SCHEDULED_IP_CHANGEMENT = INT_MAX;
	} else {
		SCHEDULED_IP_STRATEGY = str;
		SCHEDULED_IP_CHANGEMENT = n + after_no_cb;
		SCHEDULED_IP_CHANGEMENT_TYPE_HUB = hub;
	}
#ifdef ERTS_SMP
	erts_smp_mtx_unlock(&balance_info.update_mtx);
#endif
}


ERTS_INLINE int proc_sched_get_initial_placement_strategy(int hub) {
	if (hub)
		return PROC_SCHED_CURRENT_IP_STRATEGY_HUB;
	else
		return PROC_SCHED_CURRENT_IP_STRATEGY_REGULAR;
}


ERTS_INLINE ErtsRunQueue *proc_sched_initial_placement (Process* process, Process* parent) {
	if (process->hub)
		return PROC_SCHED_CURRENT_IP_STRATEGY_FUN[PROC_SCHED_CURRENT_IP_STRATEGY_HUB](process, parent);
	else
		return PROC_SCHED_CURRENT_IP_STRATEGY_FUN[PROC_SCHED_CURRENT_IP_STRATEGY_REGULAR](process, parent);
}


/***************************
 ***************************
 * Migration strategies
 ***************************
 ***************************/

static proc_sched_migration_strategy PROC_SCHED_CURRENT_MIGRATION_STRATEGY;
//check balance
static Uint (*PROC_SCHED_CURR_MIGR_STG_CB_FUN)(ErtsRunQueue *);
//immigration
static void (*PROC_SCHED_CURR_MIGR_STG_IMMIGRATION_FUN)(ErtsRunQueue *);


ERTS_INLINE static void proc_sched_cb_initialize(void) {
	internal_proc_sched_set_migration_strategy(PROC_SCHED_MIGRATION_DEFAULT);
}


ERTS_INLINE void proc_sched_set_migration_strategy(proc_sched_migration_strategy strategy) {
	if (PROC_SCHED_CURRENT_MIGRATION_STRATEGY == strategy) return;
	internal_proc_sched_set_migration_strategy(strategy);
}

ERTS_INLINE static void internal_proc_sched_set_migration_strategy(proc_sched_migration_strategy strategy) {
	switch (strategy) {
	case PROC_SCHED_MIGRATION_DEFAULT:
		PROC_SCHED_CURR_MIGR_STG_CB_FUN = &proc_sched_migrate_default_cb;
		PROC_SCHED_CURR_MIGR_STG_IMMIGRATION_FUN = &proc_sched_migrate_default_immigrate;
		break;
	case PROC_SCHED_MIGRATION_DISABLED:
		PROC_SCHED_CURR_MIGR_STG_CB_FUN = &proc_sched_migrate_disabled_cb;
		PROC_SCHED_CURR_MIGR_STG_IMMIGRATION_FUN = &proc_sched_migrate_disabled_immigrate;
		break;
	default:
		return;
	}
#ifdef USE_VM_PROBES
	DTRACE1(scheduler_cb_strategy_change, strategy);
#endif
	PROC_SCHED_CURRENT_MIGRATION_STRATEGY = strategy;
}


ERTS_INLINE int proc_sched_get_migration_strategy(void) {
	return PROC_SCHED_CURRENT_MIGRATION_STRATEGY;
}


ERTS_INLINE void proc_sched_check_balance (ErtsRunQueue *rq) {
	Uint cbs;
#ifdef USE_VM_PROBES
	//if (DTRACE_ENABLED(scheduler_check_balance))
	DTRACE1(scheduler_check_balance, rq->ix + 1);
#endif
	cbs = PROC_SCHED_CURR_MIGR_STG_CB_FUN(rq);
	proc_sched_verify_tasks_to_run_after(cbs);
}


ERTS_INLINE void proc_sched_immigrate (ErtsRunQueue *rq) {
	PROC_SCHED_CURR_MIGR_STG_IMMIGRATION_FUN(rq);
}


/***************************
 ***************************
 * Work stealing strategies
 ***************************
 ***************************/

//Work Stealing
static proc_sched_ws_strategy PROC_SCHED_CURRENT_WS_STRATEGY;
static int (*PROC_SCHED_CURR_WS_STG_FUN)(ErtsRunQueue *) = &proc_sched_ws_default;
int proc_sched_ws_strategy_numa_aware = 0;

ERTS_INLINE static void proc_sched_ws_initialize(void) {
	internal_proc_sched_set_ws_strategy(PROC_SCHED_WS_DEFAULT);
}


ERTS_INLINE void proc_sched_set_ws_strategy(proc_sched_ws_strategy strategy) {
	if (strategy == PROC_SCHED_CURRENT_WS_STRATEGY) return;
	internal_proc_sched_set_ws_strategy(strategy);
}

ERTS_INLINE static void internal_proc_sched_set_ws_strategy(proc_sched_ws_strategy strategy) {
	switch (strategy) {
	case PROC_SCHED_WS_DEFAULT:
		PROC_SCHED_CURR_WS_STG_FUN = &proc_sched_ws_default;
		proc_sched_ws_strategy_numa_aware = 0;
		break;
	case PROC_SCHED_WS_DISABLED:
		PROC_SCHED_CURR_WS_STG_FUN = &proc_sched_ws_disabled;
		proc_sched_ws_strategy_numa_aware = 0;
		break;
	case PROC_SCHED_WS_NUMA_AWARE:
		PROC_SCHED_CURR_WS_STG_FUN = &proc_sched_ws_numa_aware;
		proc_sched_ws_strategy_numa_aware = 1;
		break;
	default:
		return;
	}
#ifdef USE_VM_PROBES
	//if (DTRACE_ENABLED(scheduler_ws_strategy_change))
	DTRACE1(scheduler_ws_strategy_change, strategy);
#endif
	PROC_SCHED_CURRENT_WS_STRATEGY = strategy;
}


ERTS_INLINE int proc_sched_get_ws_strategy (void) {
	return PROC_SCHED_CURRENT_WS_STRATEGY;
}

ERTS_INLINE int proc_sched_work_stealing(ErtsRunQueue* rq) {
#ifdef USE_VM_PROBES
	//	if (DTRACE_ENABLED(scheduler_work_stealing))
	DTRACE1(scheduler_work_stealing, rq->ix + 1);
#endif
	return PROC_SCHED_CURR_WS_STG_FUN(rq);
}


/***************************
 ***************************
 * Misc
 ***************************
 ***************************/


ERTS_INLINE void proc_sched_verify_tasks_to_run_after (Uint cbs) {
#ifdef ERTS_SMP
	if (SCHEDULED_IP_CHANGEMENT < cbs) {
		erts_smp_mtx_lock(&balance_info.update_mtx);
		if (SCHEDULED_IP_CHANGEMENT < cbs) { //it might have changed
			proc_sched_set_initial_placement_strategy(SCHEDULED_IP_STRATEGY, SCHEDULED_IP_CHANGEMENT_TYPE_HUB);
			SCHEDULED_IP_CHANGEMENT = INT_MAX;
		}
		erts_smp_mtx_unlock(&balance_info.update_mtx);
	}
#endif
}


#ifdef ERTS_SMP

ERTS_INLINE void get_no_runqs(int *active, int *used) {
	erts_aint32_t no_runqs = erts_smp_atomic32_read_nob(&balance_info.no_runqs);
	if (active)
		*active = (int) (no_runqs & ERTS_NO_RUNQS_MASK);
	if (used)
		*used = (int) ((no_runqs >> ERTS_NO_USED_RUNQS_SHIFT) & ERTS_NO_RUNQS_MASK);
}

ERTS_INLINE void set_no_used_runqs(int used) {
	erts_aint32_t exp = erts_smp_atomic32_read_nob(&balance_info.no_runqs);
	while (1) {
		erts_aint32_t act, new;
		new = (used & ERTS_NO_RUNQS_MASK) << ERTS_NO_USED_RUNQS_SHIFT;
		new |= exp & ERTS_NO_RUNQS_MASK;
		act = erts_smp_atomic32_cmpxchg_nob(&balance_info.no_runqs, new, exp);
		if (act == exp)
			break;
		exp = act;
	}
}

ERTS_INLINE void set_no_active_runqs(int active) {
	erts_aint32_t exp = erts_smp_atomic32_read_nob(&balance_info.no_runqs);
	while (1) {
		erts_aint32_t act, new;
		new = exp & (ERTS_NO_RUNQS_MASK << ERTS_NO_USED_RUNQS_SHIFT);
		new |= active & ERTS_NO_RUNQS_MASK;
		act = erts_smp_atomic32_cmpxchg_nob(&balance_info.no_runqs, new, exp);
		if (act == exp)
			break;
		exp = act;
	}
}

ERTS_INLINE int try_inc_no_active_runqs(int active) {
	erts_aint32_t exp = erts_smp_atomic32_read_nob(&balance_info.no_runqs);
	if (((exp >> ERTS_NO_USED_RUNQS_SHIFT) & ERTS_NO_RUNQS_MASK) < active)
		return 0;
	if ((exp & ERTS_NO_RUNQS_MASK) + 1 == active) {
		erts_aint32_t new, act;
		new = exp & (ERTS_NO_RUNQS_MASK << ERTS_NO_USED_RUNQS_SHIFT);
		new |= active & ERTS_NO_RUNQS_MASK;
		act = erts_smp_atomic32_cmpxchg_nob(&balance_info.no_runqs, new, exp);
		if (act == exp)
			return 1;
	}
	return 0;
}

ERTS_INLINE void lock_balance_info(void) {
	erts_smp_mtx_lock(&balance_info.update_mtx);
}

ERTS_INLINE void unlock_balance_info(void) {
	erts_smp_mtx_unlock(&balance_info.update_mtx);
}

ERTS_INLINE void force_check_balance(void) {
	balance_info.forced_check_balance = 1;
}
#endif

ERTS_INLINE Uint erts_debug_nbalance(void) {
#ifdef ERTS_SMP
	Uint n;
	erts_smp_mtx_lock(&balance_info.update_mtx);
	n = balance_info.n;
	erts_smp_mtx_unlock(&balance_info.update_mtx);
	return n;
#else
	return 0;
#endif
}
