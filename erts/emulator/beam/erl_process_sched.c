#include "erl_process_sched.h"
#include "erl_process_sched_ip.h"
#include "erl_process_sched_mig.h"


/*
 * Initial Placement Strategies
 */

static proc_sched_ip_strategy PROC_SCHED_CURRENT_IP_STRATEGY = PROC_SCHED_IP_DEFAULT;
static ErtsRunQueue *(*PROC_SCHED_CURRENT_IP_STRATEGY_FUN)(Process*) = &proc_sched_ip_default;

void proc_sched_set_initial_placement_strategy (proc_sched_ip_strategy strategy) {
	switch (strategy) {
		case PROC_SCHED_IP_DEFAULT:
			PROC_SCHED_CURRENT_IP_STRATEGY_FUN = &proc_sched_ip_default;
			break;
		case PROC_SCHED_IP_RANDOM:
			PROC_SCHED_CURRENT_IP_STRATEGY_FUN = &proc_sched_ip_random;
			break;
		case PROC_SCHED_IP_CIRCULAR:
			PROC_SCHED_CURRENT_IP_STRATEGY_FUN = &proc_sched_ip_circular;
			break;
		default:
			return;
	}
	PROC_SCHED_CURRENT_IP_STRATEGY = strategy;
}

int proc_sched_get_initial_placement_strategy(void) {
	return PROC_SCHED_CURRENT_IP_STRATEGY;
}

ErtsRunQueue *proc_sched_initial_placement (Process* parent) {
	return PROC_SCHED_CURRENT_IP_STRATEGY_FUN(parent);
}


/*
 * Migration strategies
 */

static proc_sched_migration_strategy PROC_SCHED_CURRENT_MIGRATION_STRATEGY = PROC_SCHED_MIGRATION_DEFAULT;
//check balance
static void (*PROC_SCHED_CURR_MIGR_STG_CB_FUN)(ErtsRunQueue *) = &proc_sched_migrate_default_cb;
//immigration
static void (*PROC_SCHED_CURR_MIGR_STG_IMMIGRATION_FUN)(ErtsRunQueue *) = &proc_sched_migrate_default_immigrate;
//Work Stealing
static int (*PROC_SCHED_CURR_MIGR_STG_WS_FUN)(ErtsRunQueue *) = &proc_sched_migrate_default_ws;

void proc_sched_set_migration_strategy(proc_sched_migration_strategy strategy) {
	switch (strategy) {
		case PROC_SCHED_MIGRATION_DEFAULT:
			PROC_SCHED_CURR_MIGR_STG_CB_FUN = &proc_sched_migrate_default_cb;
			PROC_SCHED_CURR_MIGR_STG_IMMIGRATION_FUN = &proc_sched_migrate_default_immigrate;
			PROC_SCHED_CURR_MIGR_STG_WS_FUN = &proc_sched_migrate_default_ws;
			break;
		case PROC_SCHED_MIGRATION_DISABLED:
			PROC_SCHED_CURR_MIGR_STG_CB_FUN = &proc_sched_migrate_disabled_cb;
			PROC_SCHED_CURR_MIGR_STG_IMMIGRATION_FUN = &proc_sched_migrate_disabled_immigrate;
			PROC_SCHED_CURR_MIGR_STG_WS_FUN = &proc_sched_migrate_disabled_ws;
			break;
		default:
			return;
	}
	PROC_SCHED_CURRENT_MIGRATION_STRATEGY = strategy;
}

int proc_sched_get_migration_strategy(void) {
	return PROC_SCHED_CURRENT_MIGRATION_STRATEGY;
}

void proc_sched_check_balance (ErtsRunQueue *rq) {
	PROC_SCHED_CURR_MIGR_STG_CB_FUN(rq);
}

void proc_sched_immigrate (ErtsRunQueue *rq) {
	PROC_SCHED_CURR_MIGR_STG_IMMIGRATION_FUN(rq);
}

int proc_sched_work_stealing(ErtsRunQueue* rq) {
	return PROC_SCHED_CURR_MIGR_STG_WS_FUN(rq);
}

