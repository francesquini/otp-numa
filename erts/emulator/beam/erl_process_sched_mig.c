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
#else
	printf("ERRO!!!!! DEVERIA SER SMP\n");
#endif
}

void proc_sched_migrate_default_immigrate(ErtsRunQueue* rq) {
#ifdef ERTS_SMP
	immigrate(rq);
#else
	printf("ERRO 2222!!!!! DEVERIA SER SMP\n");
#endif
}


int proc_sched_migrate_default_ws(ErtsRunQueue* rq) {
#ifdef ERTS_SMP
	return try_steal_task(rq);
#else
	printf("ERRO 3333!!!!! DEVERIA SER SMP\n");
	return 0;
#endif
}

/***************************
 ***************************
 * Disabled
 ***************************
 ***************************/

void proc_sched_migrate_disabled_cb(ErtsRunQueue* rq) {
	//nothing to be done
}

void proc_sched_migrate_disabled_immigrate(ErtsRunQueue* ign) {
	//nothing to be done
}

int proc_sched_migrate_disabled_ws(ErtsRunQueue* rq) {
	return 0;
}
