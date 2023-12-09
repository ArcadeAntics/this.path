#include "thispathdefn.h"


#define t_or_f(x) ( (x) ? (R_TrueValue) : (R_FalseValue) )


SEXP do_istrue do_formals
{
    do_start_no_call_op_rho("istrue", 1);


    return t_or_f(asLogical(CAR(args)) == TRUE);
}


SEXP do_isfalse do_formals
{
    do_start_no_call_op_rho("isfalse", 1);


    return t_or_f(asLogical(CAR(args)) == FALSE);
}


SEXP do_asInteger do_formals
{
    do_start_no_call_op_rho("asInteger", 1);


    return ScalarInteger(asInteger(CAR(args)));
}


SEXP do_asIntegerGE0 do_formals
{
    do_start_no_call_op_rho("asIntegerGE0", 1);


    int value = asInteger(CAR(args));
    if (value == NA_INTEGER || value < 0)
        error(_("invalid '%s' value"), "n");
    return ScalarInteger(value);
}


SEXP do_IS_SCALAR_STR do_formals
{
    do_start_no_call_op_rho("IS_SCALAR_STR", 1);
    SEXP x = CAR(args);
    return t_or_f(IS_SCALAR(x, STRSXP));
}


SEXP do_AS_SCALAR_STR do_formals
{
    do_start_no_call_op_rho("AS_SCALAR_STR", 1);
    return ScalarString(asChar(CAR(args)));
}


SEXP do_scalar_streql do_formals
{
    do_start_no_call_op_rho("scalar_streql", 2);
    SEXP e1 = CAR(args); args = CDR(args);
    if (!IS_SCALAR(e1, STRSXP))
        return R_FalseValue;
    SEXP e2 = CAR(args); args = CDR(args);
    // if (IS_SCALAR(e2, LGLSXP) && LOGICAL(e2)[0] == NA_LOGICAL)
    //     e2 = ScalarString(NA_STRING);
    if (!IS_SCALAR(e2, STRSXP))
        return R_FalseValue;
    e1 = STRING_ELT(e1, 0);
    e2 = STRING_ELT(e2, 0);
    if (e1 == e2)
        return R_TrueValue;
    if (e1 == NA_STRING || e2 == NA_STRING)
        return R_FalseValue;
    return t_or_f(streql(CHAR(e1), CHAR(e2)));
}


SEXP do_get_dyn do_formals
{
    do_start_no_op("get.dyn", 3);


    int nprotect = 0;


    SEXP sym = CAR(args); args = CDR(args);
    if (TYPEOF(sym) == SYMSXP);
    else if (isValidStringF(sym)) {
        if (XLENGTH(sym) > 1)
            errorcall(call, _("first argument has length > 1"));
        sym = installTrChar(STRING_ELT(sym, 0));
    }
    else errorcall(call, _("invalid first argument"));


    int minframe = asInteger(CAR(args)); args = CDR(args);
    if (minframe == NA_INTEGER || minframe < 0)
        errorcall(call, _("invalid '%s' argument"), "minframe");


    Rboolean inherits = asLogical(CAR(args)); args = CDR(args);
    if (inherits == NA_LOGICAL)
        errorcall(call, _("invalid '%s' argument"), "inherits");


    int N = asInteger(eval(expr_sys_nframe, rho));
    SEXP which = allocVector(INTSXP, 1);
    PROTECT(which); nprotect++;
    int *iwhich = INTEGER(which);
    SEXP getframe;
    {
        PROTECT_INDEX indx;
        PROTECT_WITH_INDEX(getframe = CONS(which, R_NilValue), &indx); nprotect++;
        REPROTECT(getframe = LCONS(getFromBase(sys_frameSymbol), getframe), indx);
    }


    SEXP frame, value;


    for (iwhich[0] = N - 1; iwhich[0] >= minframe; iwhich[0]--) {
        frame = eval(getframe, rho);
        value = (inherits ? findVar(sym, frame) : findVarInFrame(frame, sym));
        if (value != R_UnboundValue) {
            if (TYPEOF(value) == PROMSXP) {
                if (PRVALUE(value) == R_UnboundValue) {
                    PROTECT(value);
                    value = eval(value, R_EmptyEnv);
                    UNPROTECT(1);
                }
                else value = PRVALUE(value);
            }
            UNPROTECT(nprotect);
            return value;
        }
    }


    UNPROTECT(nprotect);
    return getInFrame(ifnotfoundSymbol, rho, FALSE);
}
