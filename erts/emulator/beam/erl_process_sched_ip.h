#ifndef __ERL_PROCESS_SCHED_IP_H__
#define __ERL_PROCESS_SCHED_IP_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_process.h"

ErtsRunQueue* proc_sched_ip_default(Process* process, Process* parent);

//random
ErtsRunQueue* proc_sched_ip_random(Process* process, Process* parent);
ErtsRunQueue* proc_sched_ip_simple_random(Process* process, Process* parent);

//round-robin
ErtsRunQueue* proc_sched_ip_circular(Process* process, Process* parent);
ErtsRunQueue* proc_sched_ip_local_circular(Process* process, Process* parent);

#endif
