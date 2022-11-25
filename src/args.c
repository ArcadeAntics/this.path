#include <R.h>
#include <Rinternals.h>


#include "translations.h"





SEXP do_asargs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    int n;


    int nargs = length(args) - 1;
    if (nargs == 0) {
        n = 0;
    }
    else if (nargs == 1) {
        n = asInteger(CADR(args));
        if (n == NA_INTEGER || n < 0)
            errorcall(call, _("argument must be coercible to non-negative integer"));
    }
    else errorcall(call, "%d arguments passed to .External(%s) which requires 0 or 1", nargs, "C_asargs");


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


    SEXP expr = lang2(install(".asArgs"), x);
    PROTECT(expr); nprotect++;
    SEXP value = eval(expr, rho);
    UNPROTECT(nprotect);
    return value;
}
