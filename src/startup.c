#include "thispathdefn.h"
#include "print.h"


Rboolean already_set_init_file = FALSE;


SEXP startup_file(Rboolean check_is_valid_init_file_expr, SEXP rho)
{
    SEXP promise = Rf_findVarInFrame(rho, exprSymbol);
    if (promise == R_UnboundValue)
        Rf_error(_("object '%s' not found"), CHAR(PRINTNAME(exprSymbol)));
    if (promise == R_MissingArg)
        Rf_error(_("argument \"%s\" is missing, with no default"), CHAR(PRINTNAME(exprSymbol)));
    if (TYPEOF(promise) != PROMSXP)
        Rf_error("invalid '%s', is not a promise", CHAR(PRINTNAME(exprSymbol)));


    SEXP code = PRCODE(promise);
    if (TYPEOF(code) != LANGSXP || CAR(code) != R_BraceSymbol)
        Rf_error("invalid '%s', expected a braced expression", CHAR(PRINTNAME(exprSymbol)));
    if (PRVALUE(promise) != R_UnboundValue)
        Rf_error("invalid '%s', must be an unevaluated call", CHAR(PRINTNAME(exprSymbol)));


    if (check_is_valid_init_file_expr) {
        if (already_set_init_file) return R_FalseValue;
        return Rf_ScalarLogical(ATTRIB(code) == R_NilValue &&
                                PRENV(promise) == R_GlobalEnv &&
                                PRSEEN(promise) == 0);
    }


    int nprotect = 0;


    Rf_protect(promise); nprotect++;


    code = CDR(code);
    SEXP env = PRENV(promise);
    SEXP withVisible = getFromBase(withVisibleSymbol);
    Rf_protect(withVisible); nprotect++;


    SEXP expr, value;
    PROTECT_INDEX expr_indx, value_indx;
    R_ProtectWithIndex(expr = R_NilValue, &expr_indx); nprotect++;
    R_ProtectWithIndex(value = R_NilValue, &value_indx); nprotect++;


    extern SEXP on_exit_SET_PRSEEN_2(SEXP promises, SEXP rho);


    SEXP ptr = on_exit_SET_PRSEEN_2(R_NilValue, rho);
    Rf_protect(ptr); nprotect++;
    R_SetExternalPtrProtected(ptr, Rf_cons(promise, R_NilValue));
    if (PRSEEN(promise)) {
        if (PRSEEN(promise) == 1)
            Rf_error(_("promise already under evaluation: recursive default argument reference or earlier problems?"));
        else {
            SET_PRSEEN(promise, 1);
            Rf_warning(_("restarting interrupted promise evaluation"));
        }
    }
    else SET_PRSEEN(promise, 1);


    for (; code != R_NilValue; code = CDR(code)) {
        R_Reprotect(expr = Rf_lcons(withVisible, Rf_cons(CAR(code), R_NilValue)), expr_indx);
        R_Reprotect(value = Rf_eval(expr, env), value_indx);
        if (Rf_asLogical(VECTOR_ELT(value, 1)))
            my_PrintValueEnv(VECTOR_ELT(value, 0), env);
    }


    SET_PRSEEN (promise, 0);
    SET_PRVALUE(promise, value);
    SET_PRENV  (promise, R_NilValue);
    R_SetExternalPtrProtected(ptr, R_NilValue);


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
