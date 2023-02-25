#include "thispathdefn.h"





SEXP do_asargs do_formals
{
    /*
    do_asargs {this.path}                                        C Documentation

    Providing Arguments to a Script



    Description

    do_asargs() coerces R objects into a character vector, primarily for use
    with command line applications. It accepts one optional argument, the
    number of arguments to skip in the ... list.

    This is used primarily for withArgs(), used to skip the first argument
    which is the expression to be evaluated.
     */


    do_start("asargs", -1);


    int nprotect = 0;


    int n;


    int nargs = length(args);
    if (nargs == 0) {
        n = 0;
    }
    else if (nargs == 1) {
        n = asInteger(CAR(args));
        if (n == NA_INTEGER || n < 0)
            errorcall(call, _("argument must be coercible to non-negative integer"));
    }
    else errorcall(call, wrong_nargs_to_External(nargs, "C_asargs", "0 or 1"));


    SEXP dots = findVarInFrame(rho, R_DotsSymbol);
    if (dots == R_UnboundValue)
        error("could not find the ... list; should never happen, please report!");


    int dots_length = (((TYPEOF(dots) == DOTSXP) ? length(dots) : 0) - n);


    if (dots_length <= 0) return allocVector(STRSXP, 0);


    if (n) dots = nthcdr(dots, n);


    SEXP x = allocVector(VECSXP, dots_length);
    PROTECT(x); nprotect++;
    int i;
    SEXP d, xi;


    for (i = 0, d = dots; i < dots_length; i++, d = CDR(d)) {


        /* evaluate each argument of 'dots' */
        xi = CAR(d);
        xi = eval(xi, rho);
        SET_VECTOR_ELT(x, i, xi);
    }


    SEXP expr = lang2(_asArgsSymbol, x);
    PROTECT(expr); nprotect++;
    SEXP value = eval(expr, rho);
    UNPROTECT(nprotect);
    return value;
}
