#include "thispathdefn.h"


SEXP do_reset_proj do_formals
{
    do_start_no_call_op_rho("reset_proj", 0);


    SEXP _proj = getFromMyNS(_projSymbol);
    Rf_protect(_proj);
    if (TYPEOF(_proj) != CLOSXP)
        Rf_error(_("object '%s' of mode '%s' was not found"),
            R_CHAR(PRINTNAME(_projSymbol)), "function");
    SEXP value = Rf_allocVector(STRSXP, 0);
    Rf_protect(value);
    INCREMENT_NAMED_defineVar(xSymbol, value, CLOENV(_proj));
    Rf_setAttrib(value, R_NamesSymbol, Rf_allocVector(STRSXP, 0));
    set_R_Visible(FALSE);
    Rf_unprotect(2);
    return R_NilValue;
}
