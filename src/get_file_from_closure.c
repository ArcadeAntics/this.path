/*
this.path : Get Executing Script's Path
Copyright (C) 2026   Iris Simmons
 */


#include "get_file_from_closure.h"


SEXP do_get_file_from_closure do_formals
{
    do_start_no_call_op("get_file_from_closure", 1);


    return get_file_from_closure(
        /* original */ Rf_asLogical(CAR(args)),
        /* for_msg  */ FALSE,
        /* where    */ ENCLOS(rho)
    );
}
