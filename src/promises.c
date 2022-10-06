#include <R.h>
#include <Rinternals.h>


extern void (SET_PRSEEN)(SEXP x, int v);





SEXP do_isunevaluatedpromise(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* return TRUE if get(sym, env, inherits = FALSE) will force a promise,
     * return FALSE otherwise
     */


    SEXP sym = CADR(args);
    if (TYPEOF(sym) == SYMSXP) {}
    else if (isValidStringF(sym)) {
        if (XLENGTH(sym) > 1)
            errorcall(call, "first argument has length > 1");
        sym = installTrChar(STRING_ELT(sym, 0));
    }
    else errorcall(call, "invalid first argument");


    SEXP env = CADDR(args);
    if (TYPEOF(env) != ENVSXP)
        errorcall(call, "invalid second argument");


    SEXP value = findVarInFrame(env, sym);
    if (value == R_UnboundValue)
        errorcall(call, "object '%s' not found", CHAR(PRINTNAME(sym)));


    return ScalarLogical(TYPEOF(value) == PROMSXP &&
                         PRVALUE(value) == R_UnboundValue);
}


SEXP do_getpromisewithoutwarning(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nargs = length(args) - 1;
    SEXP sym, env;


#define get_sym                                                \
    sym = CADR(args);                                          \
    if (TYPEOF(sym) == SYMSXP) {}                              \
    else if (isValidStringF(sym)) {                            \
        if (XLENGTH(sym) > 1)                                  \
            errorcall(call, "first argument has length > 1");  \
        sym = installTrChar(STRING_ELT(sym, 0));               \
    }                                                          \
    else errorcall(call, "invalid first argument");


    if (nargs == 1) {
        get_sym
        env = ENCLOS(rho);
    }
    else if (nargs == 2) {
        get_sym
        env = CADDR(args);
        if (TYPEOF(env) != ENVSXP)
            errorcall(call, "invalid second argument");
    }
    else errorcall(call, "%d arguments passed to .External(%s) which requires 1 or 2", nargs, "C_getpromisewithoutwarning");


    SEXP value = findVarInFrame(env, sym);
    if (value == R_UnboundValue)
        errorcall(call, "object '%s' not found", CHAR(PRINTNAME(sym)));
    if (TYPEOF(value) != PROMSXP)
        errorcall(call, "'%s' is not a promise", CHAR(PRINTNAME(sym)));


    if (PRVALUE(value) == R_UnboundValue) {
        if (PRSEEN(value)) {
            if (PRSEEN(value) == 1) {}
            else SET_PRSEEN(value, 0);
        }
        eval(value, env);
    }
    return PRVALUE(value);
}
