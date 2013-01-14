#ifndef __ERL_PROCESS_MEM_H__
#define __ERL_PROCESS_MEM_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

extern char proc_mem_deffered;

/****************************************************
 ****************************************************
 * Initialization
 ****************************************************
 ****************************************************/

void proc_mem_initialize(char memory_allocation_policy, char deferred_allocation, char verbose);
void proc_mem_bind (int scheduler, int cpu);


int proc_mem_log(char *fmt, ...);

#endif
