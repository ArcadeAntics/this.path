#include "thispathdefn.h"


static R_INLINE
SEXP get_file_from_closure(Rboolean original, Rboolean for_msg, SEXP where)
{
    /*
    This function chooses whether to return 'ofile' or 'file'. It examines the
    promises, determining which to return based on whether they're evaluated.
     */


    if (for_msg == NA_LOGICAL)
        error(_("invalid '%s' argument"), "for.msg");


    /* if 'for.msg = TRUE', we treat 'original = FALSE' as 'original = NA' */
    if (for_msg && !original) original = NA_LOGICAL;


    SEXP env;
    switch (TYPEOF(where)) {
    case SYMSXP:
    {
        SEXP fun = getFromMyNS(where);
        if (TYPEOF(fun) != CLOSXP)
            error(_("object '%s' of mode '%s' was not found"),
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
        SEXP var = findVarInFrame(env, (sym));                 \
        if (var == R_UnboundValue)                             \
            error(_("object '%s' not found"), EncodeChar(PRINTNAME((sym))));\
        if (TYPEOF(var) != PROMSXP)                            \
            error("invalid '%s', must be a promise", EncodeChar(PRINTNAME((sym))))


        get_and_check(file, fileSymbol);
        /* if the promise has not already been forced, just get the original */
        if (PRVALUE(file) == R_UnboundValue)
            original = TRUE;
        else
            return PRVALUE(file);
    }
    if (original) {
#define get_and_return(var, sym)                               \
        get_and_check(var, sym);                               \
        if (PRVALUE(var) == R_UnboundValue) {                  \
            /* unlike a normal promise, we DO NOT want to */   \
            /* throw a warning if var is re-evaluated     */   \
            if (PRSEEN(var)) {                                 \
                if (PRSEEN(var) == 1);                         \
                else SET_PRSEEN(var, 0);                       \
            }                                                  \
            PROTECT(var);                                      \
            var = eval(var, R_EmptyEnv);                       \
            UNPROTECT(1);                                      \
            return var;                                        \
        }                                                      \
        else                                                   \
            return PRVALUE(var)


        get_and_return(ofile, ofileSymbol);
    }
    else {
        get_and_return(file, fileSymbol);
    }


#undef get_and_return
#undef get_and_check
}
