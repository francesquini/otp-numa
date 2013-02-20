#include <stdio.h>
#include <stdarg.h>
#include <numa.h>
#include <numaif.h>
#include <sched.h>

#include "erl_process_mem.h"
#include "erl_cpu_topology.h"
#include "erl_mseg.h"

char proc_mem_mem_alloc_policy = 0;
char proc_mem_deffered = 0;
char proc_mem_verbose = 0;

void proc_mem_initialize(char memory_allocation_policy, char deferred_allocation, char verbose) {
	ErtsCpuBindOrder order;

	proc_mem_mem_alloc_policy = memory_allocation_policy;
	proc_mem_deffered = deferred_allocation;
	proc_mem_verbose = verbose;

	order = erts_bound_schedulers_order();
	if (order == ERTS_CPU_BIND_UNDEFINED || order == ERTS_CPU_BIND_NONE) {
		proc_mem_log("WARNING: Setting deferred allocation but schedulers are not bound\n");
	}

	proc_mem_log("Setting deferred memory allocation %d memory allocation policy %d\n",
			proc_mem_deffered, proc_mem_mem_alloc_policy);
}

static int mem_policy(void) {
	int mode;
	nodemask_t nodemask;
	int res = get_mempolicy(&mode, nodemask.n, NUMA_NUM_NODES + 1, NULL, 0);
	if (res) {
		switch (errno) {
		case EFAULT:
			proc_mem_log("EFAULT Error getting memory policy %d numa_avail %d\n", res, numa_available());
			exit(1);
		case EINVAL:
			proc_mem_log("EINVAL Error getting memory policy %d numa_avail %d\n", res, numa_available());
			exit(1);
		}
	}
	return mode;
}

void proc_mem_bind (int scheduler, int cpu) {
	if (proc_mem_mem_alloc_policy) {
		int node;
		unsigned long mask;

		node = numa_node_of_cpu(cpu);
		mask = 1 << node;

		if (set_mempolicy(MPOL_PREFERRED, &mask, sizeof(unsigned long))) {
			proc_mem_log("%d - Error setting NUMA memory allocation policy%d\n", cpu, errno);
			exit(1);
		}

		erts_mseg_clear_cache();

		proc_mem_log("Scheduler: %d Set to cpu: %d Running @: %d Node %u - Policy %d\n",
				scheduler, cpu, sched_getcpu(), node, mem_policy());

	}
}

int proc_mem_log(char *fmt, ...) {
	int r = 0;
	if (proc_mem_verbose) {
		va_list args;
		va_start(args, fmt);
		r = vfprintf(stderr, fmt, args);
		va_end(args);
		fflush(stderr);
	}
	return r;
}

unsigned int proc_mem_state(void) {
	unsigned int ret = 0;
	ret |= proc_mem_mem_alloc_policy 	? 1 : 0;
	ret |= proc_mem_deffered 			? 2 : 0;
	ret |= proc_mem_verbose 			? 4 : 0;
	return ret;
}

