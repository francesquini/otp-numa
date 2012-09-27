#ifndef __ERL_PROCESS_SCHED_IP_H__
#define __ERL_PROCESS_SCHED_IP_H__

#include "erl_process_sched.h"

unsigned int proc_sched_ip_default(Process*);
unsigned int proc_sched_ip_random(Process*);
unsigned int proc_sched_ip_circular(Process*);

#endif
