#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#include "erl_process_sched_ip.h"


/*This file contains the implementation of the initial placement strategies*/


/*Common state variables*/

static __thread int proc_sched_ip_random_initialized = FALSE;
static __thread unsigned int proc_sched_ip_random_seed;

//Local strategy functions
void proc_sched_ip_random_initialize() {
	if (!proc_sched_ip_random_initialized) {
		pid_t pid = getpid();
		timespec ts;
		clock_gettime(CLOCK_MONOTONIC, &ts);
		seed = pid * ts.tv_nsec / ts.tv_sec;
		proc_sched_ip_random_initialized = TRUE;
	}
}

/***************************
 ***************************
 * Default
 ***************************
 ***************************/

unsigned int proc_sched_ip_default(Process* ign) {
	return 0;
}

/***************************
 ***************************
 * Random
 ***************************
 ***************************/

unsigned int proc_sched_ip_random(Process* ign) {
	proc_sched_ip_random_initialize();
	int rand = rand_r(&proc_sched_ip_random_seed);
	int scheduler = (rand % erts_no_schedulers) + 1;
	return scheduler;
}

/***************************
 ***************************
 * Circular
 ***************************
 ***************************/

volatile unsigned long long proc_sched_ip_circular_next = 1;

unsigned int proc_sched_ip_circular(Process* ign) {
	unsigned long long nextBig = __sync_fetch_and_add(&proc_sched_ip_circular_next, 1);
	unsigned int next = (nextBig % erts_no_schedulers) + 1;
	return next;
}

/***************************
 ***************************
 * Parent
 ***************************
 ***************************/



