#include "thispathdefn.h"





Rboolean my_isMethodDispatchOn(void)
{
    return Rf_asLogical(Rf_eval(expr__isMethodsDispatchOn, R_EmptyEnv));
}


void my_PrintObjectS4(SEXP s, SEXP env)
{
    int nprotect = 0;


    SEXP methods = Rf_findVarInFrame(R_NamespaceRegistry, methodsSymbol);
    Rf_protect(methods); nprotect++;
    if (methods == R_UnboundValue)
        Rf_error("missing methods namespace: this should not happen");


    SEXP mask = R_NewEnv(env, FALSE, 0);
    Rf_protect(mask); nprotect++;


    SEXP show = getInFrame(showSymbol, methods, TRUE);
    if (show == R_UnboundValue)
        Rf_error("missing show() in methods namespace: this should not happen");


    Rf_defineVar(showSymbol, show, mask);
    Rf_defineVar(objectSymbol, makeEVPROMISE(s, s), mask);


    SEXP expr = Rf_lcons(showSymbol, Rf_cons(objectSymbol, R_NilValue));
    Rf_protect(expr); nprotect++;
    Rf_eval(expr, env);


    Rf_defineVar(showSymbol, R_NilValue, mask);
    Rf_defineVar(objectSymbol, R_NilValue, mask);


    Rf_unprotect(nprotect);
}


void my_PrintObjectS3(SEXP s, SEXP env)
{
    int nprotect = 0;


    SEXP mask = R_NewEnv(env, FALSE, 0);
    Rf_protect(mask); nprotect++;


    Rf_defineVar(printSymbol, findFunction(printSymbol, R_BaseNamespace), mask);
    Rf_defineVar(xSymbol, makeEVPROMISE(s, s), mask);


    SEXP expr = Rf_lcons(printSymbol, Rf_cons(xSymbol, R_NilValue));
    Rf_protect(expr); nprotect++;
    Rf_eval(expr, mask);


    Rf_defineVar(printSymbol, R_NilValue, mask);
    Rf_defineVar(xSymbol, R_NilValue, mask);


    Rf_unprotect(nprotect);
}


void my_PrintObject(SEXP s, SEXP env)
{
    if (my_isMethodDispatchOn() && IS_S4_OBJECT(s))
        my_PrintObjectS4(s, env);
    else
        my_PrintObjectS3(s, env);
}


void my_PrintValueRec(SEXP s, SEXP env)
{
    int nprotect = 0;


    SEXP mask = R_NewEnv(env, FALSE, 0);
    Rf_protect(mask); nprotect++;


    Rf_defineVar(print_defaultSymbol, findFunction(print_defaultSymbol, R_BaseNamespace), mask);
    Rf_defineVar(xSymbol, makeEVPROMISE(s, s), mask);


    SEXP expr = Rf_lcons(print_defaultSymbol, Rf_cons(xSymbol, R_NilValue));
    Rf_protect(expr); nprotect++;
    Rf_eval(expr, mask);


    Rf_defineVar(print_defaultSymbol, R_NilValue, mask);
    Rf_defineVar(xSymbol, R_NilValue, mask);


    Rf_unprotect(nprotect);
}


void my_PrintDispatch(SEXP s, SEXP env)
{
    if (Rf_isObject(s))
        my_PrintObject(s, env);
    else
        my_PrintValueRec(s, env);
}


void my_PrintValueEnv(SEXP s, SEXP env)
{
    Rf_protect(s);


    if (Rf_isFunction(s))
        my_PrintObject(s, env);
    else
        my_PrintDispatch(s, env);


    Rf_unprotect(1);
}


SEXP do_PrintValueEnv do_formals
{
    do_start_no_call_op_rho("PrintValueEnv", 2);
    my_PrintValueEnv(CAR(args), CADR(args));
    set_R_Visible(FALSE);
    return CAR(args);
}


SEXP do_print_ThisPathDocumentContext do_formals
{
    do_start_no_call_op("print.ThisPathDocumentContext", 2);


    /* print_ThisPathDocumentContext (for the most part) does not throw errors
     * for an invalid object. it will report that something is invalid in the
     * printed message, but will still make a sensible message regardless */


    SEXP x = CAR(args); args = CDR(args);
    if (TYPEOF(x) != ENVSXP)
        Rf_error(_("invalid '%s' value"), "x");
    Rboolean quote = Rf_asLogical(CAR(args)); args = CDR(args);
    if (quote == NA_LOGICAL)
        Rf_error(_("invalid '%s' value"), "quote");


    if (x == R_EmptyEnv) {
        Rprintf("<environment: R_EmptyEnv>\n");
        set_R_Visible(FALSE);
        return x;
    }


    SEXP expr;
    PROTECT_INDEX indx;
    R_ProtectWithIndex(expr = Rf_cons(R_NilValue, R_NilValue), &indx);
    SET_TAG(expr, R_QuoteSymbol);
    R_Reprotect(expr = Rf_lcons(encodeStringSymbol, Rf_cons(R_NilValue, expr)), indx);


    SEXP klass = Rf_getAttrib(x, R_ClassSymbol);
    int nklass = ((klass == R_NilValue) ? 0 : LENGTH(klass));
    if (nklass) {
        SETCADR(expr, klass);
        SETCADDR(expr, Rf_mkString("\""));
        SEXP tmp = Rf_eval(expr, R_BaseEnv);
        Rf_protect(tmp);
        Rprintf("<object of class ");
        for (int i = 0; i < nklass; i++)
            if (i) Rprintf(", %s", R_CHAR(STRING_ELT(tmp, i)));
            else   Rprintf("%s"  , R_CHAR(STRING_ELT(tmp, i)));
        Rprintf(" at %p>\n", (void *) x);
        Rf_unprotect(1);
    }
    else {
        Rprintf("<object of class \"%s\" at %p>\n",
                Rf_type2char(TYPEOF(x)), (void *) x);
    }


    if (!quote) SETCADDR(expr, R_BlankScalarString);


    SEXP sym;
#define print_invalid_unbound   Rprintf("%s: <invalid, R_UnboundValue>\n", R_CHAR(PRINTNAME(sym)))
#define print_null              Rprintf("%s: NULL\n", R_CHAR(PRINTNAME(sym)))
#define print_invalid_null      Rprintf("%s: <invalid, NULL>\n", R_CHAR(PRINTNAME(sym)))
#define print_type(var)         Rprintf("%s: <type = \"%s\", length = %d>\n"         , R_CHAR(PRINTNAME(sym)), Rf_type2char(TYPEOF((var))), Rf_length((var)))
#define print_invalid_type(var) Rprintf("%s: <invalid, type = \"%s\", length = %d>\n", R_CHAR(PRINTNAME(sym)), Rf_type2char(TYPEOF((var))), Rf_length((var)))
#define _print_encoded_str(fmt, str)                           \
    do {                                                       \
        SETCADR(expr, (str));                                  \
        SEXP tmp = Rf_eval(expr, R_BaseEnv);                   \
        Rf_protect(tmp);                                       \
        Rprintf((fmt), R_CHAR(PRINTNAME(sym)), R_CHAR(STRING_ELT(tmp, 0)));\
        Rf_unprotect(1);                                       \
    } while (0)
#define print_encoded_str(str) _print_encoded_str("%s: %s\n", (str))


    SEXP errcnd = Rf_findVarInFrame(x, sym = errcndSymbol);
    Rf_protect(errcnd);
    if (errcnd != R_UnboundValue) {
        if (errcnd == R_NilValue)
            print_invalid_null;
        else if (TYPEOF(errcnd) == VECSXP &&
                 LENGTH(errcnd) >= 2 &&
                 Rf_inherits(errcnd, "condition"))
        {
            Rprintf("%s: ", R_CHAR(PRINTNAME(sym)));
            my_PrintValueEnv(errcnd, rho);
        }
        else print_invalid_type(errcnd);


        SEXP for_msg = Rf_findVarInFrame(x, sym = for_msgSymbol);
        Rf_protect(for_msg);
        if (for_msg == R_UnboundValue)
            print_invalid_unbound;
        else if (for_msg == R_NilValue)
            print_invalid_null;
        else if (ptr_IS_SCALAR(for_msg, STRSXP))
            print_encoded_str(for_msg);
        else
            print_invalid_type(for_msg);
        Rf_unprotect(1);


        SEXP associated_with_file = Rf_findVarInFrame(x, sym = associated_with_fileSymbol);
        Rf_protect(associated_with_file);
        if (associated_with_file == R_UnboundValue);
        else if (associated_with_file == R_NilValue)
            print_null;
        else if (ptr_IS_SCALAR(associated_with_file, LGLSXP)) {
            Rboolean tmp = LOGICAL(associated_with_file)[0];
            Rprintf("%s: %s\n", R_CHAR(PRINTNAME(sym)),
                (tmp == NA_LOGICAL) ? "NA" : (tmp ? "TRUE" : "FALSE"));
        }
        else print_invalid_type(associated_with_file);
        Rf_unprotect(1);
    }
    else {
        SEXP ofile = Rf_findVarInFrame(x, sym = ofileSymbol);
        Rf_protect(ofile);
        if (ofile == R_UnboundValue)
            print_invalid_unbound;
        else if (ofile == R_NilValue)
            print_invalid_null;
        else if (ptr_IS_SCALAR(ofile, STRSXP))
            print_encoded_str(ofile);
        else
            print_invalid_type(ofile);
        Rf_unprotect(1);


        SEXP wd = Rf_findVarInFrame(x, sym = wdSymbol);
        Rf_protect(wd);
        if (wd == R_UnboundValue);
        else if (wd == R_NilValue)
            print_null;
        else if (ptr_IS_SCALAR(wd, STRSXP))
            print_encoded_str(wd);
        else
            print_invalid_type(wd);
        Rf_unprotect(1);


        SEXP file = Rf_findVarInFrame(x, sym = fileSymbol);
        Rf_protect(file);
        if (file == R_UnboundValue)
            print_invalid_unbound;
        else if (file == R_NilValue)
            print_invalid_null;
        else if (TYPEOF(file) == PROMSXP) {
            SEXP val = PRVALUE(file);
            if (val == R_UnboundValue) {
                Rprintf("%s: ", R_CHAR(PRINTNAME(sym)));
                my_PrintValueEnv(R_PromiseExpr(file), rho);
            }
            else if (val == R_NilValue) {
                print_null;
            }
            else if (ptr_IS_SCALAR(val, STRSXP)) {
                print_encoded_str(val);
            }
            else print_invalid_type(val);
        }
        else print_invalid_type(file);
        Rf_unprotect(1);


        SEXP lines = Rf_findVarInFrame(x, sym = linesSymbol);
        Rf_protect(lines);
        if (lines == R_UnboundValue);
        else if (lines == R_NilValue)
            print_invalid_null;
        else if (TYPEOF(lines) == PROMSXP) {
            SEXP val = PRVALUE(lines);
            if (val == R_UnboundValue) {
                Rprintf("%s: ", R_CHAR(PRINTNAME(sym)));
                my_PrintValueEnv(R_PromiseExpr(lines), rho);
            }
            else if (val == R_NilValue) {
                print_invalid_null;
            }
            else if (TYPEOF(val) == STRSXP) {
                print_type(val);
            }
            else print_invalid_type(val);
        }
        else if (TYPEOF(lines) == STRSXP)
            print_type(lines);
        else print_invalid_type(lines);
        Rf_unprotect(1);
    }
    Rf_unprotect(1);  /* errcnd */


    SEXP source = Rf_findVarInFrame(x, sym = sourceSymbol);
    Rf_protect(source);
    if (source == R_UnboundValue)
        print_invalid_unbound;
    else if (source == R_NilValue)
        print_invalid_null;
    else if (TYPEOF(source) == CHARSXP)
        _print_encoded_str("%s: <CHARSXP: %s>\n", Rf_ScalarString(source));
    else print_invalid_type(source);
    Rf_unprotect(1);


    SEXP setsyspathwashere = Rf_findVarInFrame(x, sym = setsyspathwashereSymbol);
    Rf_protect(setsyspathwashere);
    if (setsyspathwashere != R_UnboundValue) {
        if (setsyspathwashere == R_NilValue)
            print_null;
        else if (ptr_IS_SCALAR(setsyspathwashere, LGLSXP)) {
            Rboolean tmp = LOGICAL(setsyspathwashere)[0];
            Rprintf("%s: %s\n", R_CHAR(PRINTNAME(sym)),
                (tmp == NA_LOGICAL) ? "NA" : (tmp ? "TRUE" : "FALSE"));
        }
        else print_invalid_type(setsyspathwashere);
    }


    SEXP n = Rf_findVarInFrame(x, sym = nSymbol);
    Rf_protect(n);
    if (n == R_UnboundValue) {
        if (setsyspathwashere != R_UnboundValue)
            print_invalid_unbound;
    }
    else if (n == R_NilValue)
        print_invalid_null;
    else if (ptr_IS_SCALAR(n, INTSXP))
        Rprintf("%s: %d\n", R_CHAR(PRINTNAME(sym)), INTEGER(n)[0]);
    else print_invalid_type(n);


    Rf_unprotect(2);


    set_R_Visible(FALSE);
    Rf_unprotect(1);
    return x;
}
