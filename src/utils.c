#include "thispathdefn.h"


#if R_version_less_than(3, 5, 0)
SEXP do_dotslength do_formals
{
    do_start("dotslength", 0);


    static SEXP parent_frameSymbol = NULL;
    if (parent_frameSymbol == NULL) {
        parent_frameSymbol = install("parent.frame");
    }
    SEXP expr = lang1(parent_frameSymbol);
    PROTECT(expr);
    SEXP env = eval(expr, rho);
    UNPROTECT(1);
    SEXP vl = findVar(R_DotsSymbol, env);
    if (vl == R_UnboundValue)
        error(_("incorrect context: the current call has no '...' to look in"));
    return ScalarInteger((TYPEOF(vl) == DOTSXP ? length(vl) : 0));
}
#endif


#if R_version_less_than(3, 2, 0)
SEXP checkNSname(SEXP call, SEXP name)
{
    switch (TYPEOF(name)) {
    case SYMSXP:
        break;
    case STRSXP:
        if (LENGTH(name) >= 1) {
            name = installTrChar(STRING_ELT(name, 0));
            break;
        }
        /* else fall through */
    default:
        errorcall(call, _("bad namespace name"));
    }
    return name;
}


SEXP do_isRegisteredNamespace do_formals
{
    do_start("isRegisteredNamespace", 1);


    SEXP name = checkNSname(call, PROTECT(coerceVector(CAR(args), SYMSXP)));
    UNPROTECT(1);
    SEXP val = findVarInFrame(R_NamespaceRegistry, name);
    return ScalarLogical(val == R_UnboundValue ? FALSE : TRUE);
}
#endif
