#include "thispathdefn.h"


SEXP do_resetthisproj do_formals
{
    do_start_no_call_op_rho("resetthisproj", 0);


    SEXP _this_proj = getInFrame(_this_projSymbol, mynamespace, FALSE);
    if (TYPEOF(_this_proj) != CLOSXP)
        error(_("object '%s' of mode '%s' was not found"), EncodeChar(PRINTNAME(_this_projSymbol)), "function");
    SEXP value = allocVector(STRSXP, 0);
    PROTECT(value);
    setAttrib(value, R_NamesSymbol, allocVector(STRSXP, 0));
    defineVar(xSymbol, value, CLOENV(_this_proj));
    INCREMENT_NAMED(value);
    UNPROTECT(1);  /* value */
    set_R_Visible(FALSE);
    return R_NilValue;
}
