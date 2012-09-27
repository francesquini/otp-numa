#ifndef __ERL_PROCESS_SCHED_MIG_H__
#define __ERL_PROCESS_SCHED_MIG_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_process.h"

void proc_sched_migrate_default_cb(ErtsRunQueue* rq);
void proc_sched_migrate_default_immigrate(ErtsRunQueue* rq);

void proc_sched_migrate_disabled_cb(ErtsRunQueue* rq);
void proc_sched_migrate_disabled_immigrate(ErtsRunQueue* rq);


#endif
