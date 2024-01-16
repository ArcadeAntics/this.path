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


SEXP do_asLogical do_formals
{
    do_start_no_call_op_rho("asLogical", 1);
    return ScalarLogical(asLogical(CAR(args)));
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
