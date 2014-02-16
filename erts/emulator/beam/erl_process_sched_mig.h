#ifndef __ERL_PROCESS_SCHED_MIG_H__
#define __ERL_PROCESS_SCHED_MIG_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_process.h"

ERTS_INLINE void proc_sched_migrate_initialize(Uint nQueues, Uint no_schedulers, Uint no_schedulers_online);

#define ERTS_NO_USED_RUNQS_SHIFT 16
#define ERTS_NO_RUNQS_MASK 0xffff

#if ERTS_MAX_NO_OF_SCHEDULERS > ERTS_NO_RUNQS_MASK
#  error "Too large amount of schedulers allowed"
#endif

#if ERTS_MAX_PROCESSES >= (1 << 27)
#  error default_check_balance() assumes ERTS_MAX_PROCESS < (1 << 27)
#endif

//Default Strategy
Uint     proc_sched_migrate_default_cb(ErtsRunQueue* rq);
void     proc_sched_migrate_default_immigrate(ErtsRunQueue* rq);
Process *proc_sched_migrate_default_immigration_candidate(ErtsRunQueue *from_rq, int priority, ErtsRunQueue *to_rq);

//Disabled Strategy
Uint     proc_sched_migrate_disabled_cb(ErtsRunQueue* rq);
void     proc_sched_migrate_disabled_immigrate(ErtsRunQueue* rq);
Process *proc_sched_migrate_disabled_immigration_candidate(ErtsRunQueue *from_rq, int priority, ErtsRunQueue *to_rq);

//NUMA Strategy
Uint     proc_sched_migrate_numa_cb(ErtsRunQueue* rq);
void     proc_sched_migrate_numa_immigrate(ErtsRunQueue* rq);
Process *proc_sched_migrate_numa_immigration_candidate(ErtsRunQueue *from_rq, int priority, ErtsRunQueue *to_rq);

#ifdef ERTS_SMP
typedef struct struct_balance_info {
    erts_smp_mtx_t update_mtx;
    erts_smp_atomic32_t no_runqs;
    int last_active_runqs;
    int forced_check_balance;
    erts_smp_atomic32_t checking_balance;
    int halftime;
    int full_reds_history_index;
    struct {
	int active_runqs;
	int reds;
	int max_len;
    } prev_rise;
    Uint n;
} balance_info_type;
balance_info_type balance_info;
#endif


#endif
