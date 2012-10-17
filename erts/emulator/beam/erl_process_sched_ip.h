#ifndef __ERL_PROCESS_SCHED_IP_H__
#define __ERL_PROCESS_SCHED_IP_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_process.h"

ErtsRunQueue* proc_sched_ip_default(Process*);

//random
ErtsRunQueue* proc_sched_ip_random(Process*);
ErtsRunQueue* proc_sched_ip_simple_random(Process*);

//round-robin
ErtsRunQueue* proc_sched_ip_circular(Process*);
ErtsRunQueue* proc_sched_ip_local_circular(Process*);

#endif
