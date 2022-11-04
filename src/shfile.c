#include <R.h>
#include <Rinternals.h>


#include "thispathdefn.h"





SEXP do_shfile(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int original = asLogical(CADR(args));
    int for_msg  = asLogical(CADDR(args));
    if (for_msg == NA_LOGICAL)
        error("invalid '%s' argument", "for.msg");


    /* if 'for.msg = TRUE', we treat 'original = FALSE' as 'original = NA' */
    if (for_msg && !original) original = NA_LOGICAL;


    if (original == NA_LOGICAL) {
#define get_and_check(var, sym)                                \
        SEXP var = findVarInFrame(ENCLOS(rho), sym);           \
        if (var == R_UnboundValue)                             \
            error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));\
        if (TYPEOF(var) != PROMSXP)                            \
            error("invalid '%s', must be a promise", EncodeChar(PRINTNAME(sym)))
        /* do not end with ; on purpose so can be used below */


        get_and_check(thispathfile, thispathfileSymbol);
        /* if the promise has not already been forced, just get the original */
        if (PRVALUE(thispathfile) == R_UnboundValue)
            original = TRUE;
        else
            return PRVALUE(thispathfile);
    }
    if (original) {
#define get_and_return(var, sym)                               \
        get_and_check(var, sym);                               \
        if (PRVALUE(var) == R_UnboundValue) {                  \
            /* unlike a normal promise, we DO NOT want to */   \
            /* raise a warning if var is re-evaluated     */   \
            if (PRSEEN(var)) {                                 \
                if (PRSEEN(var) == 1) {}                       \
                else SET_PRSEEN(var, 0);                       \
            }                                                  \
            return eval(var, R_EmptyEnv);                      \
        }                                                      \
        else                                                   \
            return PRVALUE(var)


        get_and_return(thispathofile, thispathofileSymbol);
    }
    else {
        get_and_return(thispathfile, thispathfileSymbol);
    }


#undef get_and_return
#undef get_and_check
}
