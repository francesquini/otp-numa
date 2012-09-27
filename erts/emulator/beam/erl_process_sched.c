#include "erl_process_sched.h"
#include "erl_process_sched_ip.h"

static unsigned int (*PROC_SCHED_CURRENT_IP_STRATEGY)(Process*) = &proc_sched_ip_default;

void proc_sched_set_initial_placement_strategy (proc_sched_ip_strategy strategy) {
	switch (strategy) {
		case PROC_SCHED_IP_DEFAULT:
			PROC_SCHED_CURRENT_IP_STRATEGY = &proc_sched_ip_default;
			break;
		case PROC_SCHED_IP_RANDOM:
			PROC_SCHED_CURRENT_IP_STRATEGY = &proc_sched_ip_random;
			break;
		case PROC_SCHED_IP_CIRCULAR:
			PROC_SCHED_CURRENT_IP_STRATEGY = &proc_sched_ip_circular;
			break;
		case PROC_SCHED_IP_PARENT:
			break;
		default:
			break;
	}
}

int proc_sched_initial_placement (Process* parent) {
	return PROC_SCHED_CURRENT_IP_STRATEGY(parent);
}


void proc_sched_set_migration_strategy(proc_sched_migration_strategy strategy){
}
void proc_sched_check_balance (scheduling_data *sd) {
}
void proc_sched_immigrate (scheduling_data *sd) {
}


