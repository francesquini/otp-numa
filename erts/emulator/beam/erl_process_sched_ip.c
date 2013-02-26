#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <pthread.h>
#include <unistd.h>

#include "erl_process_sched_ip.h"
#include "erl_process_sched.h"
#include "erl_cpu_topology.h"

/*This file contains the implementation of the initial placement strategies*/


/***************************
 ***************************
 * Default
 ***************************
 ***************************/

ErtsRunQueue* proc_sched_ip_default(Process* process, Process* parent) {
	return erts_get_runq_proc(parent);
}

/***************************
 ***************************
 * Random - srand
 ***************************
 ***************************/

static __thread int proc_sched_ip_random_initialized = 0;
static __thread unsigned int proc_sched_ip_random_seed;

ERTS_INLINE static void proc_sched_ip_random_initialize(void) {
	if (!proc_sched_ip_random_initialized) {
		pid_t pid = getpid();
		struct timespec ts;
		clock_gettime(CLOCK_MONOTONIC, &ts);
		proc_sched_ip_random_seed = pid * ts.tv_nsec / ts.tv_sec;
		proc_sched_ip_random_initialized = 1;
	}
}


ErtsRunQueue* proc_sched_ip_random(Process* process, Process* parent) {
	unsigned int rand, scheduler;
	if (!proc_sched_hubs_only() || process->hub) {
		proc_sched_ip_random_initialize();
		rand = rand_r(&proc_sched_ip_random_seed);
		scheduler = rand % erts_no_run_queues;
		return ERTS_RUNQ_IX(scheduler);
	} else {
		return proc_sched_ip_default(process, parent);
	}
}


/***************************
 ***************************
 * Random - Simple RNG
 ***************************
 ***************************/

static __thread int simple_rng_initialized = 0;
static __thread unsigned int simple_rng_s_mw = 521288629;
static __thread unsigned int simple_rng_s_mz = 362436069;

ERTS_INLINE static void simple_rng_initialize(void) {
	if (!simple_rng_initialized) {
		unsigned int n1, n2;
		pid_t pid = getpid();
		struct timespec ts;
		clock_gettime(CLOCK_MONOTONIC, &ts);
		n1 = ((pid * 104623 + ts.tv_nsec * 96487 + ts.tv_sec * 75997) * 45587) % 4294967296;
		if (n1) simple_rng_s_mw = n1;
		n2 = ((pid * 48947 + ts.tv_nsec * 33181 + ts.tv_sec * 87523) * 101839) % 4294967296;
		if (n2) simple_rng_s_mz = n2;
		simple_rng_initialized = 1;
	}
}


/// SimpleRNG is a simple random number generator based on
/// George Marsaglia's MWC (multiply with carry) generator.
/// Although it is very simple, it passes Marsaglia's DIEHARD
/// series of random number generator tests.
/// Written by John D. Cook

ERTS_INLINE static unsigned int simple_rng_next(unsigned int mod) {
	// 0 <= u < 2^32
	unsigned int u;
	simple_rng_s_mz = 36969 * (simple_rng_s_mz & 65535) + (simple_rng_s_mz >> 16);
	simple_rng_s_mw = 18000 * (simple_rng_s_mw & 65535) + (simple_rng_s_mw >> 16);
	u = (simple_rng_s_mz << 16) + simple_rng_s_mw;
	return u % mod;
}


ErtsRunQueue* proc_sched_ip_simple_random(Process* process, Process* parent) {
	if (!proc_sched_hubs_only() || process->hub) {
		simple_rng_initialize();
		return ERTS_RUNQ_IX(simple_rng_next(erts_no_run_queues)) ;
	} else {
		return proc_sched_ip_default(process, parent);
	}
}

/***************************
 ***************************
 * Circular
 ***************************
 ***************************/

static unsigned long long proc_sched_ip_circular_next = 0;

ErtsRunQueue* proc_sched_ip_circular(Process* process, Process* parent) {
	if (!proc_sched_hubs_only() || process->hub) {
		unsigned long long nextBig = __sync_fetch_and_add(&proc_sched_ip_circular_next, 1);
		unsigned long long next = nextBig % erts_no_run_queues;
		return ERTS_RUNQ_IX(next) ;
	} else {
		return proc_sched_ip_default(process, parent);
	}
}

/***************************
 ***************************
 * Local Circular
 ***************************
 ***************************/

static __thread unsigned int local_circular_next = 0;

ErtsRunQueue* proc_sched_ip_local_circular(Process* process, Process* parent) {
	if (!proc_sched_hubs_only() || process->hub) {
		local_circular_next = (local_circular_next + 1) % erts_no_run_queues;
		return ERTS_RUNQ_IX(local_circular_next);
	} else {
		return proc_sched_ip_default(process, parent);
	}
}


/***************************
 ***************************
 * Scatter
 ***************************
 ***************************/


#ifdef ERTS_SMP

volatile static byte proc_sched_scatter_lock = 0;
static int proc_sched_ip_scatter_size = -1;
static int proc_sched_ip_scatter_next = -1;
static int *proc_sched_ip_scatter_list;
 

static ERTS_INLINE void proc_sched_ip_scatter_initialize(void) {
	if (proc_sched_ip_scatter_next == -1) {
		int i;
		erts_cpu_topology_t *cpudata;	    
		cpudata = get_cpu_data(&proc_sched_ip_scatter_size);		
		cpu_bind_order_sort(cpudata, proc_sched_ip_scatter_size, ERTS_CPU_BIND_SPREAD, 0);		
		proc_sched_ip_scatter_list = malloc(sizeof(int) * proc_sched_ip_scatter_size);	    
	    for (i = 0; i < proc_sched_ip_scatter_size; i++)
	    	proc_sched_ip_scatter_list[i] = cpudata[i].logical;	    
	    erts_free(ERTS_ALC_T_TMP, cpudata);	    
	    proc_sched_ip_scatter_next = 0;
	    proc_sched_scatter_lock = 0;
	}
}

#endif

ErtsRunQueue* proc_sched_ip_scatter(Process* process, Process* parent) {
#ifdef ERTS_SMP	
	if (!proc_sched_hubs_only() || process->hub) {
		int ret;		
		while (!__sync_bool_compare_and_swap (&proc_sched_scatter_lock, 0, 1));
		proc_sched_ip_scatter_initialize();
		ret = proc_sched_ip_scatter_list[proc_sched_ip_scatter_next];
		proc_sched_ip_scatter_next = (proc_sched_ip_scatter_next + 1) % proc_sched_ip_scatter_size;
		proc_sched_scatter_lock = 0;
		return ERTS_RUNQ_IX(ret);
	} else {
#endif
		return proc_sched_ip_default(process, parent);
#ifdef ERTS_SMP		
	}
#endif	
}


/***************************
 ***************************
 * Compact
 ***************************
 ***************************/

#ifdef ERTS_SMP

volatile static byte proc_sched_compact_lock = 0;
static int proc_sched_ip_compact_size = -1;
static int proc_sched_ip_compact_next = -1;
static int *proc_sched_ip_compact_list;

static ERTS_INLINE void proc_sched_ip_compact_initialize(void) {
	if (proc_sched_ip_compact_next == -1) {
		int i;
		erts_cpu_topology_t *cpudata;
		cpudata = get_cpu_data(&proc_sched_ip_compact_size);	    
		cpu_bind_order_sort(cpudata, proc_sched_ip_compact_size, ERTS_CPU_BIND_NO_SPREAD, 1);
		proc_sched_ip_compact_list = malloc(sizeof(int) * proc_sched_ip_compact_size);
	    for (i = 0; i < proc_sched_ip_compact_size; i++)
	    	proc_sched_ip_compact_list[i] = cpudata[i].logical;
	    erts_free(ERTS_ALC_T_TMP, cpudata);
	    proc_sched_ip_compact_next = 0;
	    proc_sched_compact_lock = 0;
	}
}

#endif


ErtsRunQueue* proc_sched_ip_compact(Process* process, Process* parent) {
#ifdef ERTS_SMP
	if (!proc_sched_hubs_only() || process->hub) {
		int ret;		
		while (!__sync_bool_compare_and_swap (&proc_sched_compact_lock, 0, 1));
		proc_sched_ip_compact_initialize();
		ret = proc_sched_ip_compact_list[proc_sched_ip_compact_next];
		proc_sched_ip_compact_next = (proc_sched_ip_compact_next + 1) % proc_sched_ip_compact_size;
		proc_sched_compact_lock = 0;
		return ERTS_RUNQ_IX(ret);
	} else {
#endif		
		return proc_sched_ip_default(process, parent);
#ifdef ERTS_SMP		
	}
#endif
}