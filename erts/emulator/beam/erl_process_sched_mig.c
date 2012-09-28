#include "erl_process_sched_mig.h"

/*This file contains the implementation of the migration strategies*/

/*Common state variables*/

/***************************
 ***************************
 * Default
 ***************************
 ***************************/

void proc_sched_migrate_default_cb(ErtsRunQueue* rq) {
#ifdef ERTS_SMP
	check_balance(rq);
#endif
}

void proc_sched_migrate_default_immigrate(ErtsRunQueue* rq) {
#ifdef ERTS_SMP
	immigrate(rq);
#endif
}

/***************************
 ***************************
 * Disabled
 ***************************
 ***************************/

void proc_sched_migrate_disabled_cb(ErtsRunQueue* rq) {
	rq->check_balance_reds = INT_MAX;
}

void proc_sched_migrate_disabled_immigrate(ErtsRunQueue* ign) {
	//nothing to be done
}
