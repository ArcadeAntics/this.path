#include <R.h>
#include <Rinternals.h>


extern void (SET_PRSEEN)(SEXP x, int v);





#define get_sym                                                \
    sym = CADR(args);                                          \
    if (TYPEOF(sym) == SYMSXP) {}                              \
    else if (isValidStringF(sym)) {                            \
        if (XLENGTH(sym) > 1)                                  \
            errorcall(call, "first argument has length > 1");  \
        sym = installTrChar(STRING_ELT(sym, 0));               \
    }                                                          \
    else errorcall(call, "invalid first argument");


#define handles_nargs(one_arg_env, name)                       \
    switch (length(args) - 1) {                                \
    case 1:                                                    \
        get_sym                                                \
        env = (one_arg_env);                                   \
        break;                                                 \
    case 2:                                                    \
        get_sym                                                \
        env = CADDR(args);                                     \
        if (TYPEOF(env) != ENVSXP)                             \
            errorcall(call, "invalid second argument");        \
        break;                                                 \
    default:                                                   \
        errorcall(call, "%d arguments passed to .External(%s) which requires 1 or 2", length(args) - 1, (name));\
    }





SEXP do_isunevaluatedpromise(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* return TRUE if get(sym, env, inherits = FALSE) will force a promise,
     * return FALSE otherwise
     */


    SEXP sym, env;
    handles_nargs(rho, "C_isunevaluatedpromise");


    SEXP value = findVarInFrame(env, sym);
    if (value == R_UnboundValue)
        errorcall(call, "object '%s' not found", CHAR(PRINTNAME(sym)));


    return ScalarLogical(TYPEOF(value) == PROMSXP &&
                         PRVALUE(value) == R_UnboundValue);
}


SEXP do_getpromisewithoutwarning(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* return the result of getting a promise, silencing a possible warning
     * about "restarting interrupted promise evaluation"
     *
     * if the requested variable is not a promise, throw an error
     */


    SEXP sym, env;
    handles_nargs(ENCLOS(rho), "C_getpromisewithoutwarning");


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
