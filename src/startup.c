/*
this.path : Get Executing Script's Path
Copyright (C) 2024-2026   Iris Simmons
 */


#include "thispathdefn.h"
#include "print.h"


Rboolean already_set_init_file = FALSE;


SEXP startup_file(Rboolean check_is_valid_init_file_expr, SEXP rho)
{
    int nprotect = 0;


    binding_info_t promise; my_findVarInFrame(rho, exprSymbol, &promise);
    Rf_protect(promise.value); nprotect++;
    if (promise.value == my_UnboundValue)
        Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(exprSymbol)));
    if (promise.value == R_MissingArg) {
        // Rf_error(_("argument \"%s\" is missing, with no default"), R_CHAR(PRINTNAME(exprSymbol)));
        MissingArgError(exprSymbol, R_CurrentExpression, rho, "evalError");
        Rf_unprotect(nprotect);
        return R_NilValue;
    }
    if (my_TYPEOF(promise) != PROMSXP)
        Rf_error("invalid '%s', is not a promise", R_CHAR(PRINTNAME(exprSymbol)));


    SEXP code = my_PREXPR(promise);
    if (TYPEOF(code) != LANGSXP || CAR(code) != R_BraceSymbol)
        Rf_error("invalid '%s', expected a braced expression", R_CHAR(PRINTNAME(exprSymbol)));
    if (my_PRVALUE(promise) != my_UnboundValue)
        Rf_error("invalid '%s', must be an unevaluated call", R_CHAR(PRINTNAME(exprSymbol)));


    if (check_is_valid_init_file_expr) {
        SEXP value;
        if (already_set_init_file) value = R_FalseValue;
        else value = Rf_ScalarLogical(NO_ATTRIB(code) &&
                                      my_PRENV(promise) == R_GlobalEnv
#if defined(R_THIS_PATH_HAS_PRSEEN)
                                      && PRSEEN(promise.value) == 0
#endif
                                      );
        Rf_unprotect(nprotect);
        return value;
    }


    code = CDR(code);
    SEXP env = my_PRENV(promise);
    SEXP withVisible = getFromBase(withVisibleSymbol);
    Rf_protect(withVisible); nprotect++;


    SEXP x;
    PROTECT_INDEX indx;
    R_ProtectWithIndex(x = R_NilValue, &indx); nprotect++;


    for (; code != R_NilValue; code = CDR(code)) {
        R_Reprotect(x = Rf_lcons(withVisible, Rf_cons(CAR(code), R_NilValue)), indx);
        R_Reprotect(x = Rf_eval(x, env), indx);
        if (Rf_asLogical(VECTOR_ELT(x, 1)))
            my_PrintValueEnv(VECTOR_ELT(x, 0), env);
    }


    set_R_Visible(FALSE);
    Rf_unprotect(nprotect);
    return x == R_NilValue ? R_NilValue : VECTOR_ELT(x, 0);
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
