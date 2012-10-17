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

void proc_sched_initialize(Uint nQueues,  Uint no_schedulers, Uint no_schedulers_online);


/****************************************************
 ****************************************************
 * Initial placement
 ****************************************************
 ****************************************************/

/* Possible ip strategies */
typedef enum enum_proc_sched_ip_strategy {
	PROC_SCHED_IP_DEFAULT = 0,
	PROC_SCHED_IP_RANDOM = 1,
	PROC_SCHED_IP_CIRCULAR = 2,
	PROC_SCHED_IP_SIMPLE_RANDOM = 3,
	PROC_SCHED_IP_LOCAL_CIRCULAR = 4
} proc_sched_ip_strategy;

void proc_sched_set_initial_placement_strategy (proc_sched_ip_strategy strategy);
void proc_sched_set_initial_placement_strategy_after(proc_sched_ip_strategy str, int after_no_cb);

int proc_sched_get_initial_placement_strategy(void);

ErtsRunQueue* proc_sched_initial_placement (Process* proc);


/****************************************************
 ****************************************************
 * Migration strategies
 ****************************************************
 ****************************************************/

/* Possible migration strategies */
typedef enum enum_proc_sched_migration_strategy {
	PROC_SCHED_MIGRATION_DEFAULT = 0,
	PROC_SCHED_MIGRATION_DISABLED = 1
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

/* Possible work stealing strategies */
typedef enum enum_proc_sched_ws_strategy {
	PROC_SCHED_WS_DEFAULT = 0,
	PROC_SCHED_WS_DISABLED = 1
} proc_sched_ws_strategy;

void proc_sched_set_ws_strategy(proc_sched_ws_strategy);
int proc_sched_get_ws_strategy (void);

int proc_sched_work_stealing(ErtsRunQueue*);

#endif

/****************************************************
 ****************************************************
 * Misc
 ****************************************************
 ****************************************************/

#ifdef ERTS_SMP
ERTS_INLINE void get_no_runqs(int *active, int *used);
ERTS_INLINE void set_no_active_runqs(int active);
ERTS_INLINE void set_no_used_runqs(int no_used);
ERTS_INLINE void lock_balance_info(void);
ERTS_INLINE void unlock_balance_info(void);
ERTS_INLINE void force_check_balance(void);

ERTS_INLINE int try_inc_no_active_runqs(int active);
#endif

ERTS_INLINE Uint erts_debug_nbalance(void);
