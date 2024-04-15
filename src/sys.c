#include "thispathdefn.h"


SEXP sys_call(SEXP which, SEXP rho)
{
    SEXP expr;
    PROTECT_INDEX indx;
    R_ProtectWithIndex(expr = Rf_cons(which, R_NilValue), &indx);
    R_Reprotect(expr = Rf_lcons(getFromBase(sys_callSymbol), expr), indx);
    SEXP value = Rf_eval(expr, rho);
    Rf_unprotect(1);
    return value;
}


int sys_parent(int n, SEXP rho)
{
    SEXP expr;
    PROTECT_INDEX indx;
    R_ProtectWithIndex(expr = Rf_cons(Rf_ScalarInteger(n), R_NilValue), &indx);
    R_Reprotect(expr = Rf_lcons(getFromBase(sys_parentSymbol), expr), indx);
    int value = Rf_asInteger(Rf_eval(expr, rho));
    Rf_unprotect(1);
    return value;
}





typedef enum {
    CALLSTACK_WHICHES,
    CALLSTACK_SRCREF ,
    CALLSTACK_SRCFILE
} CALLSTACK_ACTION;


SEXP _callstack(int k, CALLSTACK_ACTION op, SEXP rho)
{
    SEXP Rparents = Rf_eval(expr_sys_parents, rho);
    Rf_protect(Rparents);
    int framedepth = LENGTH(Rparents);
    int *parents = INTEGER(Rparents);
    // make k negative; this speeds up call stack inspection
    if (k > 0) k -= framedepth;
    // this would normally be 0, but not in Jupyter
    int toplevel_framedepth = Rf_asInteger(Rf_eval(expr__toplevel_nframe, R_EmptyEnv));
    if (k <= toplevel_framedepth - framedepth) k = 0;
    // -1 because R is index 1 and C is index 0
    int indx = framedepth + k - 1;
    int parent = parents[indx];
    int *which = INTEGER(CADR(expr_sys_call_which));
    which[0] = k;
    int minimum_k = k;
    for (int previous_equal = 1, current_equal = 1; indx >= parent && indx >= toplevel_framedepth; indx--, which[0]--) {
        previous_equal = current_equal;
        current_equal = (parents[indx] == parent);
        if (current_equal)
            minimum_k = which[0];
        else if (previous_equal && (Rf_eval(expr_sys_function_which, rho) == eval_op))
            break;
    }
    if (op == CALLSTACK_WHICHES) {
        int len = 0;
        k += framedepth - 1;
        minimum_k += framedepth - 1;
        for (indx = minimum_k; indx <= k; indx++)
            len += (parents[indx] == parent);
        SEXP value = Rf_allocVector(INTSXP, len);
        int *ivalue = INTEGER(value);
        int ivalueindx = -1;
        for (indx = minimum_k; indx <= k; indx++)
            if (parents[indx] == parent)
                // +1 because R is index 1
                ivalue[++ivalueindx] = indx + 1;
        Rf_unprotect(1);
        return value;
    }
    which[0] = minimum_k;
    SEXP expr = Rf_eval(expr_sys_call_which, rho);
    Rf_protect(expr);
    SEXP srcref = Rf_getAttrib(expr, srcrefSymbol);
    if (srcref == R_NilValue);
    else if (op == CALLSTACK_SRCFILE) {
        Rf_protect(srcref);
        srcref = Rf_getAttrib(srcref, srcfileSymbol);
        Rf_unprotect(1);
    }
    else if (op == CALLSTACK_SRCREF) {
        Rf_protect(srcref);
        SEXP srcfile = Rf_getAttrib(srcref, srcfileSymbol);
        if (TYPEOF(srcfile) == ENVSXP) {
            Rf_protect(srcfile);
            indx = framedepth + k - 1;
            which[0] = k;
            for (int do_break = 0; which[0] > minimum_k && indx >= toplevel_framedepth; indx--, which[0]--) {
                if (parents[indx] == parent) {
                    SEXP current_expr = Rf_eval(expr_sys_call_which, rho);
                    Rf_protect(current_expr);
                    SEXP current_srcref = Rf_getAttrib(current_expr, srcrefSymbol);
                    if (current_srcref != R_NilValue) {
                        Rf_protect(current_srcref);
                        if (srcfile == Rf_getAttrib(current_srcref, srcfileSymbol)) {
                            srcref = current_srcref;
                            do_break = 1;
                        }
                        Rf_unprotect(1);
                    }
                    Rf_unprotect(1);
                }
                if (do_break) break;
            }
            Rf_unprotect(1);
        }
        Rf_unprotect(1);
    }
    Rf_unprotect(2);
    return srcref;
}


SEXP sys_srcref(int k, SEXP rho)
{
    return _callstack(k, CALLSTACK_SRCREF, rho);
}


SEXP sys_srcfile(int k, SEXP rho)
{
    return _callstack(k, CALLSTACK_SRCFILE, rho);
}


SEXP do_sys_srcref do_formals
{
    do_start_no_call_op("sys.srcref", 1);
    return sys_srcref(Rf_asInteger(CAR(args)), rho);
}


SEXP do_sys_whiches do_formals
{
    do_start_no_call_op("sys.whiches", 1);
    return _callstack(Rf_asInteger(CAR(args)), CALLSTACK_WHICHES, rho);
}
