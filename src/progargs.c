#include "thispathdefn.h"





SEXP do_asArgs do_formals
{
    /*
    do_asArgs                 package:this.path                  C Documentation

    Providing Arguments to a Script



    Description

    do_asArgs() coerces R objects into a character vector, primarily for use
    with command line applications. It accepts one optional argument, the
    number of arguments to skip in the ... list.

    This is used primarily for withArgs(), used to skip the first argument
    which is the expression to be evaluated.
     */


    do_start_no_op("asArgs", -1);


    int nprotect = 0;


    int n;


    switch (Rf_length(args)) {
    case 0:
        n = 0;
        break;
    case 1:
        n = Rf_asInteger(CAR(args));
        if (n == NA_INTEGER || n < 0)
            Rf_errorcall(call, _("argument must be coercible to non-negative integer"));
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), ".C_asArgs", "0 or 1"));
        return R_NilValue;
    }


    SEXP dots = Rf_findVarInFrame(rho, R_DotsSymbol);
    Rf_protect(dots); nprotect++;
    if (dots == R_UnboundValue)
        Rf_error("could not find the ... list; should never happen, please report!");


    int dots_length = (((TYPEOF(dots) == DOTSXP) ? Rf_length(dots) : 0) - n);


    if (dots_length <= 0) {
        Rf_unprotect(nprotect);
        return Rf_allocVector(STRSXP, 0);
    }


    if (n) dots = Rf_nthcdr(dots, n);


    SEXP x = Rf_allocVector(VECSXP, dots_length);
    Rf_protect(x); nprotect++;
    int i;
    SEXP d, xi;


    for (i = 0, d = dots; i < dots_length; i++, d = CDR(d)) {


        /* evaluate each argument of 'dots' */
        xi = CAR(d);
        xi = Rf_eval(xi, rho);
        SET_VECTOR_ELT(x, i, xi);
    }


    SEXP expr = Rf_lcons(_asArgsSymbol, Rf_cons(x, R_NilValue));
    Rf_protect(expr); nprotect++;
    SEXP value = Rf_eval(expr, mynamespace);
    Rf_unprotect(nprotect);
    return value;
}
