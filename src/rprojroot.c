#include "thispathdefn.h"


SEXP do_reset_proj do_formals
{
    do_start_no_call_op_rho("reset.proj", 0);


    SEXP _proj = getFromMyNS(_projSymbol);
    if (TYPEOF(_proj) != CLOSXP)
        error(_("object '%s' of mode '%s' was not found"),
            EncodeChar(PRINTNAME(_projSymbol)), "function");
    SEXP value = allocVector(STRSXP, 0);
    INCREMENT_NAMED_defineVar(xSymbol, value, CLOENV(_proj));
    setAttrib(value, R_NamesSymbol, allocVector(STRSXP, 0));
    set_R_Visible(FALSE);
    return R_NilValue;
}
