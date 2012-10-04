#ifndef __ERL_PROCESS_SCHED_H__
#define __ERL_PROCESS_SCHED_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_process.h"

/****************************************************
 ****************************************************
 * Initialization
 ****************************************************
 ****************************************************/
#ifdef ERTS_SMP
void proc_sched_initialize(Uint nQueues, balance_info_type* b_info);
#endif

/****************************************************
 ****************************************************
 * Initial placement
 ****************************************************
 ****************************************************/

/* Possible ip strategies */
typedef enum enum_proc_sched_ip_strategy {
	PROC_SCHED_IP_DEFAULT,
	PROC_SCHED_IP_RANDOM,
	PROC_SCHED_IP_CIRCULAR
} proc_sched_ip_strategy;

void proc_sched_set_initial_placement_strategy (proc_sched_ip_strategy strategy);
int proc_sched_get_initial_placement_strategy(void);
ErtsRunQueue* proc_sched_initial_placement (Process* proc);


/****************************************************
 ****************************************************
 * Migration strategies
 ****************************************************
 ****************************************************/

/* Possible migration strategies */
typedef enum enum_proc_sched_migration_strategy {
	PROC_SCHED_MIGRATION_DEFAULT,
	PROC_SCHED_MIGRATION_DISABLED
} proc_sched_migration_strategy;

void proc_sched_set_migration_strategy(proc_sched_migration_strategy);
int proc_sched_get_migration_strategy(void);

void proc_sched_check_balance (ErtsRunQueue*);
void proc_sched_immigrate (ErtsRunQueue*);



/****************************************************
 ****************************************************
 * Work stealing strategies
 ****************************************************
 ****************************************************/

/* Possible migration strategies */
typedef enum enum_proc_sched_ws_strategy {
	PROC_SCHED_WS_DEFAULT,
	PROC_SCHED_WS_DISABLED
} proc_sched_ws_strategy;

void proc_sched_set_ws_strategy(proc_sched_ws_strategy);
int proc_sched_get_ws_strategy (void);

int proc_sched_work_stealing(ErtsRunQueue*);

#endif
