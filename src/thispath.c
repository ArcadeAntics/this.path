#include <R.h>
#include <Rinternals.h>





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
//
//
//     if (TYPEOF(value) == PROMSXP)
//         Rprintf("\nPRSEEN(%s): %d\n\n", CHAR(PRINTNAME(sym)), PRSEEN(value));


    return ScalarLogical(TYPEOF(value) == PROMSXP &&
                         PRVALUE(value) == R_UnboundValue);
}
