#include "thispathdefn.h"


#define t_or_f(x) ( (x) ? (R_TrueValue) : (R_FalseValue) )


SEXP do_istrue do_formals
{
    do_start_no_call_op_rho("istrue", 1);


    return t_or_f(Rf_asLogical(CAR(args)) == TRUE);
}


SEXP do_isfalse do_formals
{
    do_start_no_call_op_rho("isfalse", 1);


    return t_or_f(Rf_asLogical(CAR(args)) == FALSE);
}


SEXP do_asLogical do_formals
{
    do_start_no_call_op_rho("asLogical", 1);
    return Rf_ScalarLogical(Rf_asLogical(CAR(args)));
}


SEXP do_asInteger do_formals
{
    do_start_no_call_op_rho("asInteger", 1);


    return Rf_ScalarInteger(Rf_asInteger(CAR(args)));
}


SEXP do_asIntegerGE0 do_formals
{
    do_start_no_call_op_rho("asIntegerGE0", 1);


    int value = Rf_asInteger(CAR(args));
    if (value == NA_INTEGER || value < 0)
        Rf_error(_("invalid '%s' value"), "n");
    return Rf_ScalarInteger(value);
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
    return Rf_ScalarString(Rf_asChar(CAR(args)));
}


SEXP do_scalar_streql do_formals
{
    do_start_no_call_op_rho("scalar_streql", 2);
    SEXP e1 = CAR(args); args = CDR(args);
    if (!IS_SCALAR(e1, STRSXP))
        return R_FalseValue;
    SEXP e2 = CAR(args); args = CDR(args);
    // if (IS_SCALAR(e2, LGLSXP) && LOGICAL(e2)[0] == NA_LOGICAL)
    //     e2 = Rf_ScalarString(NA_STRING);
    if (!IS_SCALAR(e2, STRSXP))
        return R_FalseValue;
    e1 = STRING_ELT(e1, 0);
    e2 = STRING_ELT(e2, 0);
    if (e1 == e2)
        return R_TrueValue;
    if (e1 == NA_STRING || e2 == NA_STRING)
        return R_FalseValue;
    return t_or_f(streql(R_CHAR(e1), R_CHAR(e2)));
}


SEXP do_tolower_ASCII do_formals
{
    do_start_no_call_op_rho("tolower_ASCII", 1);


    SEXP x = CAR(args);
    if (TYPEOF(x) != STRSXP)
        Rf_error(_("invalid '%s' argument"), "x");


    R_xlen_t n = XLENGTH(x);
    SEXP value = Rf_allocVector(STRSXP, n);
    Rf_protect(value);
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(x, i);
        const char *s = R_CHAR(cs);
        int nchar = (int) strlen(s);
        if (nchar == 0) continue;
        char _buf[nchar + 1];
        char *buf = _buf;
        for (int j = 0; j < nchar; j++) {
            if (s[j] >= 'A' && s[j] <= 'Z')
                *buf = s[j] + 32;
            else
                *buf = s[j];
            buf++;
        }
        *buf = '\0';
        SET_STRING_ELT(value, i, Rf_mkCharLenCE(_buf, nchar, Rf_getCharCE(cs)));
    }
    Rf_unprotect(1);
    return value;
}


SEXP do_toupper_ASCII do_formals
{
    do_start_no_call_op_rho("toupper_ASCII", 1);


    SEXP x = CAR(args);
    if (TYPEOF(x) != STRSXP)
        Rf_error(_("invalid '%s' argument"), "x");


    R_xlen_t n = XLENGTH(x);
    SEXP value = Rf_allocVector(STRSXP, n);
    Rf_protect(value);
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(x, i);
        const char *s = R_CHAR(cs);
        int nchar = (int) strlen(s);
        if (nchar == 0) continue;
        char _buf[nchar + 1];
        char *buf = _buf;
        for (int j = 0; j < nchar; j++) {
            if (s[j] >= 'a' && s[j] <= 'z')
                *buf = s[j] - 32;
            else
                *buf = s[j];
            buf++;
        }
        *buf = '\0';
        SET_STRING_ELT(value, i, Rf_mkCharLenCE(_buf, nchar, Rf_getCharCE(cs)));
    }
    Rf_unprotect(1);
    return value;
}
