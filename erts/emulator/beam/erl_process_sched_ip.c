#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <pthread.h>
#include <unistd.h>

#include "erl_process_sched_ip.h"

/*This file contains the implementation of the initial placement strategies*/

/*Common state variables*/

static __thread int proc_sched_ip_random_initialized = 0;
static __thread unsigned int proc_sched_ip_random_seed;

//Local strategy functions
static void proc_sched_ip_random_initialize(void) {
	if (!proc_sched_ip_random_initialized) {
		pid_t pid = getpid();
		struct timespec ts;
		printf("initializing random\n");
		clock_gettime(CLOCK_MONOTONIC, &ts);
		proc_sched_ip_random_seed = pid * ts.tv_nsec / ts.tv_sec;
		proc_sched_ip_random_initialized = 1;
	}
}

/***************************
 ***************************
 * Default
 ***************************
 ***************************/

ErtsRunQueue* proc_sched_ip_default(Process* parent) {
	return erts_get_runq_proc(parent);
}

/***************************
 ***************************
 * Random
 ***************************
 ***************************/

ErtsRunQueue* proc_sched_ip_random(Process* ign) {
	unsigned int rand, scheduler;
	proc_sched_ip_random_initialize();
	rand = rand_r(&proc_sched_ip_random_seed);
	scheduler = rand % erts_no_run_queues;
	return ERTS_RUNQ_IX(scheduler);
}

/***************************
 ***************************
 * Circular
 ***************************
 ***************************/

volatile unsigned long long proc_sched_ip_circular_next = 0;

ErtsRunQueue* proc_sched_ip_circular(Process* ign) {
	unsigned long long nextBig = __sync_fetch_and_add(&proc_sched_ip_circular_next, 1);
	unsigned long long next = nextBig % erts_no_run_queues;
	return ERTS_RUNQ_IX(next);
}
