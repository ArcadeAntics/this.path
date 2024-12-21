#include "thispathdefn.h"
#include "print.h"


Rboolean already_set_init_file = FALSE;


SEXP startup_file(Rboolean check_is_valid_init_file_expr, SEXP rho)
{
    SEXP promise = Rf_findVarInFrame(rho, exprSymbol);
    if (promise == R_UnboundValue)
        Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(exprSymbol)));
    if (promise == R_MissingArg) {
        // Rf_error(_("argument \"%s\" is missing, with no default"), R_CHAR(PRINTNAME(exprSymbol)));
        MissingArgError(exprSymbol, R_CurrentExpression, rho, "evalError");
        return R_NilValue;
    }
    if (TYPEOF(promise) != PROMSXP)
        Rf_error("invalid '%s', is not a promise", R_CHAR(PRINTNAME(exprSymbol)));


    SEXP code = ptr_PRCODE(promise);
    if (TYPEOF(code) != LANGSXP || CAR(code) != R_BraceSymbol)
        Rf_error("invalid '%s', expected a braced expression", R_CHAR(PRINTNAME(exprSymbol)));
    if (ptr_PRVALUE(promise) != R_UnboundValue)
        Rf_error("invalid '%s', must be an unevaluated call", R_CHAR(PRINTNAME(exprSymbol)));


    if (check_is_valid_init_file_expr) {
        if (already_set_init_file) return R_FalseValue;
        return Rf_ScalarLogical(ATTRIB(code) == R_NilValue &&
                                ptr_PRENV(promise) == R_GlobalEnv
#if defined(R_THIS_PATH_HAS_PRSEEN)
                                && PRSEEN(promise) == 0
#endif
                                );
    }


    int nprotect = 0;


    Rf_protect(promise); nprotect++;


    code = CDR(code);
    SEXP env = ptr_PRENV(promise);
    SEXP withVisible = getFromBase(withVisibleSymbol);
    Rf_protect(withVisible); nprotect++;


    SEXP expr, value;
    PROTECT_INDEX expr_indx, value_indx;
    R_ProtectWithIndex(expr = R_NilValue, &expr_indx); nprotect++;
    R_ProtectWithIndex(value = R_NilValue, &value_indx); nprotect++;


    for (; code != R_NilValue; code = CDR(code)) {
        R_Reprotect(expr = Rf_lcons(withVisible, Rf_cons(CAR(code), R_NilValue)), expr_indx);
        R_Reprotect(value = Rf_eval(expr, env), value_indx);
        if (Rf_asLogical(VECTOR_ELT(value, 1)))
            my_PrintValueEnv(VECTOR_ELT(value, 0), env);
    }


#if defined(R_THIS_PATH_HAS_PRSEEN)
        SET_PRSEEN (promise, 0);
#endif
    ptr_SET_PRVALUE(promise, value);
    ptr_SET_PRENV  (promise, R_NilValue);


    Rf_unprotect(nprotect);


    return R_NilValue;
}


SEXP do_with_startup_file do_formals
{
    do_start_no_call_op("with_startup_file", 0);
    return startup_file(FALSE, rho);
}


SEXP do_is_valid_init_file_expr do_formals
{
    do_start_no_call_op("is_valid_init_file_expr", 0);
    return startup_file(TRUE, rho);
}


SEXP do_set_init_file do_formals
{
    do_start_no_call_op_rho("set_init_file", 0);
    if (already_set_init_file) Rf_error("should be false");
    _in_init_file = TRUE;
    already_set_init_file = TRUE;
    return R_NilValue;
}


SEXP do_unset_init_file do_formals
{
    do_start_no_call_op_rho("unset_init_file", 0);
    if (!_in_init_file) Rf_error("should be true");
    _in_init_file = FALSE;
    return R_NilValue;
}
