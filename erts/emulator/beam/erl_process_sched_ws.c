#include "erl_process_sched_ws.h"

/*This file contains the implementation of the work stealing strategies*/


/***************************
 ***************************
 * Default
 ***************************
 ***************************/

int proc_sched_ws_default(ErtsRunQueue* rq) {
#ifdef ERTS_SMP
	return try_steal_task(rq);
#else
	return 0;
#endif
}

/***************************
 ***************************
 * Disabled
 ***************************
 ***************************/

int proc_sched_ws_disabled(ErtsRunQueue* rq) {
	return 0;
}
