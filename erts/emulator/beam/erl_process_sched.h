#ifndef __ERL_PROCESS_SCHED_H__
#define __ERL_PROCESS_SCHED_H__

#include "erl_process.h"

/*Internal data structure used during the scheduling of processes*/
typedef struct scheduling_data_struct {
	ErtsRunQueue *rq;
	ErtsRunPrioQueue *rpq;
	erts_aint_t dt;
	ErtsSchedulerData *esdp;
	int context_reds;
	int fcalls;
	int input_reductions;
	int actual_reds;
	int reds;
} scheduling_data;


/****************************************************
 ****************************************************
 * Initial placement
 ****************************************************
 ****************************************************/

/* Possible ip strategies */
typedef enum enum_proc_sched_ip_strategy {
	PROC_SCHED_IP_DEFAULT,
	PROC_SCHED_IP_RANDOM,
	PROC_SCHED_IP_CIRCULAR,
	PROC_SCHED_IP_PARENT
} proc_sched_ip_strategy;

void proc_sched_set_initial_placement_strategy (proc_sched_ip_strategy);
int proc_sched_initial_placement (Process*);


/****************************************************
 ****************************************************
 * Migration strategies
 ****************************************************
 ****************************************************/

/* Possible migration strategies */
typedef enum enum_proc_sched_migration_strategy {
	PROC_SCHED_MIGRATION_DEFAULT,
	PROC_SCHED_MIGRATION_DISABLED,
	PROC_SCHED_MIGRATION_RANDOM,
	PROC_SCHED_MIGRATION_CIRCULAR,
	PROC_SCHED_MIGRATION_PARENT
} proc_sched_migration_strategy;

extern static proc_sched_migration_strategy PROC_SCHED_CURRENT_MIGRATION_STRATEGY = PROC_SCHED_IP_DEFAULT;


void proc_sched_set_migration_strategy(proc_sched_migration_strategy);
void proc_sched_check_balance (scheduling_data*);
void proc_sched_immigrate (scheduling_data*);


#endif
