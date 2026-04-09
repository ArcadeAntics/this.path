/*
this.path : Get Executing Script's Path
Copyright (C) 2023-2026   Iris Simmons
 */


#include "thispathdefn.h"


static R_INLINE
SEXP get_file_from_closure(Rboolean original, Rboolean for_msg, SEXP where)
{
    /*
    This function chooses whether to return 'ofile' or 'file'. It examines the
    promises, determining which to return based on whether they're evaluated.
     */


    if (for_msg == NA_LOGICAL)
        Rf_error(_("invalid '%s' argument"), "for.msg");


    /* if 'for.msg = TRUE', we treat 'original = FALSE' as 'original = NA' */
    if (for_msg && !original) original = NA_LOGICAL;


    SEXP env;
    switch (TYPEOF(where)) {
    case SYMSXP:
    {
        SEXP fun = getFromMyNS(where);
        if (TYPEOF(fun) != CLOSXP)
            Rf_error(_("object '%s' of mode '%s' was not found"),
                EncodeChar(PRINTNAME(where)), "function");
        where = fun;
    }
    case CLOSXP:
        env = CLOENV(where);
        break;
    case ENVSXP:
        env = where;
        break;
    default:
        UNIMPLEMENTED_TYPE("get_file_from_closure", where);
        return R_NilValue;
    }


    if (original == NA_LOGICAL) {


#define get_and_check(var, sym)                                \
        binding_info_t var; my_findVarInFrame(env, (sym), &var);\
        if (var.value == my_UnboundValue)                      \
            Rf_error(_("object '%s' not found"), EncodeChar(PRINTNAME((sym))));\
        if (my_TYPEOF(var) != PROMSXP)                         \
            Rf_error("invalid '%s', must be a promise", EncodeChar(PRINTNAME((sym))))


        get_and_check(file, fileSymbol);
        /* if the promise has not already been forced, just get the original */
        if (my_PRVALUE(file) == my_UnboundValue)
            original = TRUE;
        else
            return my_PRVALUE(file);
    }
    if (original) {
#if defined(R_THIS_PATH_HAS_PRSEEN)
#define get_and_return(var, sym)                               \
        get_and_check(var, sym);                               \
        if (my_PRVALUE(var) == my_UnboundValue) {              \
            /* unlike a normal promise, we DO NOT want to */   \
            /* throw a warning if var is re-evaluated     */   \
            if (PRSEEN(var.value)) {                           \
                if (PRSEEN(var.value) == 1);                   \
                else SET_PRSEEN(var.value, 0);                 \
            }                                                  \
            return force(&var);                                \
        }                                                      \
        else                                                   \
            return force(&var)
#else
#define get_and_return(var, sym)                               \
        get_and_check(var, sym);                               \
        return force(&var)
#endif


        get_and_return(ofile, ofileSymbol);
    }
    else {
        get_and_return(file, fileSymbol);
    }


#undef get_and_return
#undef get_and_check
}
