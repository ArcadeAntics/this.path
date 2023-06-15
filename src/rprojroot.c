#include "thispathdefn.h"


SEXP do_resetproj do_formals
{
    do_start_no_call_op_rho("resetproj", 0);


    SEXP _proj = getInFrame(_projSymbol, mynamespace, FALSE);
    if (TYPEOF(_proj) != CLOSXP)
        error(_("object '%s' of mode '%s' was not found"), EncodeChar(PRINTNAME(_projSymbol)), "function");
    SEXP value = allocVector(STRSXP, 0);
    PROTECT(value);
    setAttrib(value, R_NamesSymbol, allocVector(STRSXP, 0));
    defineVar(xSymbol, value, CLOENV(_proj));
    INCREMENT_NAMED(value);
    UNPROTECT(1);  /* value */
    set_R_Visible(FALSE);
    return R_NilValue;
}
