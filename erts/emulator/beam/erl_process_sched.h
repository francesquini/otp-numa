#ifndef __ERL_PROCESS_SCHED_H__
#define __ERL_PROCESS_SCHED_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_process.h"

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

void proc_sched_set_initial_placement_strategy (proc_sched_ip_strategy strategy);
int proc_sched_initial_placement (Process* proc);


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

void proc_sched_set_migration_strategy(proc_sched_migration_strategy);
void proc_sched_check_balance (scheduling_data*);
void proc_sched_immigrate (scheduling_data*);


#endif
