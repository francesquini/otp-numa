#ifndef __ERL_PROCESS_SCHED_IP_H__
#define __ERL_PROCESS_SCHED_IP_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_process.h"

unsigned int proc_sched_ip_default(Process*);
unsigned int proc_sched_ip_random(Process*);
unsigned int proc_sched_ip_circular(Process*);

#endif
