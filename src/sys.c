#include "thispathdefn.h"


SEXP sys_call(SEXP which, SEXP rho)
{
    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = CONS(which, R_NilValue), &indx);
    REPROTECT(expr = LCONS(getFromBase(sys_callSymbol), expr), indx);
    SEXP value = eval(expr, rho);
    UNPROTECT(1);
    return value;
}


int sys_parent(int n, SEXP rho)
{
    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = CONS(ScalarInteger(n), R_NilValue), &indx);
    REPROTECT(expr = LCONS(getFromBase(sys_parentSymbol), expr), indx);
    int value = asInteger(eval(expr, rho));
    UNPROTECT(1);
    return value;
}





typedef enum {
    CALLSTACK_WHICHES,
    CALLSTACK_SRCREF ,
    CALLSTACK_SRCFILE
} CALLSTACK_ACTION;


SEXP _callstack(int k, CALLSTACK_ACTION op, SEXP rho)
{
    SEXP Rparents = eval(expr_sys_parents, rho);
    PROTECT(Rparents);
    int framedepth = LENGTH(Rparents);
    int *parents = INTEGER(Rparents);
    // make k negative; this speeds up call stack inspection
    if (k > 0) k -= framedepth;
    // this would normally be 0, but not in Jupyter
    int toplevel_framedepth = asInteger(eval(expr__toplevel_nframe, R_EmptyEnv));
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
        else if (previous_equal && (eval(expr_sys_function_which, rho) == eval_op))
            break;
    }
    if (op == CALLSTACK_WHICHES) {
        int len = 0;
        k += framedepth - 1;
        minimum_k += framedepth - 1;
        for (indx = minimum_k; indx <= k; indx++)
            len += (parents[indx] == parent);
        SEXP value = allocVector(INTSXP, len);
        int *ivalue = INTEGER(value);
        int ivalueindx = -1;
        for (indx = minimum_k; indx <= k; indx++)
            if (parents[indx] == parent)
                // +1 because R is index 1
                ivalue[++ivalueindx] = indx + 1;
        UNPROTECT(1);
        return value;
    }
    which[0] = minimum_k;
    SEXP expr = eval(expr_sys_call_which, rho);
    PROTECT(expr);
    SEXP srcref = getAttrib(expr, srcrefSymbol);
    if (srcref == R_NilValue);
    else if (op == CALLSTACK_SRCFILE) {
        PROTECT(srcref);
        srcref = getAttrib(srcref, srcfileSymbol);
        UNPROTECT(1);
    }
    else if (op == CALLSTACK_SRCREF) {
        PROTECT(srcref);
        SEXP srcfile = getAttrib(srcref, srcfileSymbol);
        if (TYPEOF(srcfile) == ENVSXP) {
            PROTECT(srcfile);
            indx = framedepth + k - 1;
            which[0] = k;
            for (int do_break = 0; which[0] > minimum_k && indx >= toplevel_framedepth; indx--, which[0]--) {
                if (parents[indx] == parent) {
                    SEXP current_expr = eval(expr_sys_call_which, rho);
                    PROTECT(current_expr);
                    SEXP current_srcref = getAttrib(current_expr, srcrefSymbol);
                    if (current_srcref != R_NilValue) {
                        PROTECT(current_srcref);
                        if (srcfile == getAttrib(current_srcref, srcfileSymbol)) {
                            srcref = current_srcref;
                            do_break = 1;
                        }
                        UNPROTECT(1);
                    }
                    UNPROTECT(1);
                }
                if (do_break) break;
            }
            UNPROTECT(1);
        }
        UNPROTECT(1);
    }
    UNPROTECT(2);
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
    return sys_srcref(asInteger(CAR(args)), rho);
}


SEXP do_sys_whiches do_formals
{
    do_start_no_call_op("sys.whiches", 1);
    return _callstack(asInteger(CAR(args)), CALLSTACK_WHICHES, rho);
}
