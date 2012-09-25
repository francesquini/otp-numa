#ifndef __ERL_PROCESS_SCHED_H__
#define __ERL_PROCESS_SCHED_H__

#include "erl_process.h"

/*Internal data structure used during the scheduling of processes*/
typedef struct scheduling_data_struct {
	ErtsRunQueue *rq;
	ErtsRunPrioQueue *rpq;
	erts_aint_t dt;
	ErtsSchedulerData *esdp;
	int context_reds;
	int fcalls;
	int input_reductions;
	int actual_reds;
	int reds;
} scheduling_data;


int proc_sched_initial_placement (Process* parent);

void proc_sched_check_balance (scheduling_data *sd);

void proc_sched_immigrate (scheduling_data *sd);


#endif
