#ifndef __ERL_PROCESS_SCHED_WS_H__
#define __ERL_PROCESS_SCHED_WS_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_process.h"

int proc_sched_ws_default(ErtsRunQueue* rq);
int proc_sched_ws_disabled(ErtsRunQueue* rq);
int proc_sched_ws_numa_aware(ErtsRunQueue* rq);

#endif
