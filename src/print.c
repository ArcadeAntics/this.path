#include "thispathdefn.h"





Rboolean my_isMethodDispatchOn(void)
{
    return asLogical(eval(expr__isMethodsDispatchOn, R_EmptyEnv));
}


void my_PrintObjectS4(SEXP s, SEXP env)
{
    int nprotect = 0;


    SEXP methods = findVarInFrame(R_NamespaceRegistry, methodsSymbol);
    if (methods == R_UnboundValue)
        error("missing methods namespace: this should not happen");


    SEXP mask = R_NewEnv(env, FALSE, 0);
    PROTECT(mask); nprotect++;


    SEXP show = getInFrame(showSymbol, methods, TRUE);
    if (show == R_UnboundValue)
        error("missing show() in methods namespace: this should not happen");


    defineVar(showSymbol, show, mask);
    defineVar(objectSymbol, makeEVPROMISE(s, s), mask);


    SEXP expr = LCONS(showSymbol, CONS(objectSymbol, R_NilValue));
    PROTECT(expr); nprotect++;
    eval(expr, env);


    defineVar(showSymbol, R_NilValue, mask);
    defineVar(objectSymbol, R_NilValue, mask);


    UNPROTECT(nprotect);
}


void my_PrintObjectS3(SEXP s, SEXP env)
{
    int nprotect = 0;


    SEXP mask = R_NewEnv(env, FALSE, 0);
    PROTECT(mask); nprotect++;


    defineVar(printSymbol, findFunction(printSymbol, R_BaseNamespace), mask);
    defineVar(xSymbol, makeEVPROMISE(s, s), mask);


    SEXP expr = LCONS(printSymbol, CONS(xSymbol, R_NilValue));
    PROTECT(expr); nprotect++;
    eval(expr, mask);


    defineVar(printSymbol, R_NilValue, mask);
    defineVar(xSymbol, R_NilValue, mask);


    UNPROTECT(nprotect);
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
    PROTECT(mask); nprotect++;


    defineVar(print_defaultSymbol, findFunction(print_defaultSymbol, R_BaseNamespace), mask);
    defineVar(xSymbol, makeEVPROMISE(s, s), mask);


    SEXP expr = LCONS(print_defaultSymbol, CONS(xSymbol, R_NilValue));
    PROTECT(expr); nprotect++;
    eval(expr, mask);


    defineVar(print_defaultSymbol, R_NilValue, mask);
    defineVar(xSymbol, R_NilValue, mask);


    UNPROTECT(nprotect);
}


void my_PrintDispatch(SEXP s, SEXP env)
{
    if (isObject(s))
        my_PrintObject(s, env);
    else
        my_PrintValueRec(s, env);
}


void my_PrintValueEnv(SEXP s, SEXP env)
{
    PROTECT(s);


    if (isFunction(s))
        my_PrintObject(s, env);
    else
        my_PrintDispatch(s, env);


    UNPROTECT(1);
}


SEXP do_PrintValueEnv do_formals
{
    do_start_no_call_op_rho("PrintValueEnv", 2);
    my_PrintValueEnv(CAR(args), CADR(args));
    set_R_Visible(FALSE);
    return CAR(args);
}


SEXP do_printThisPathDocumentContext do_formals
{
    do_start_no_call_op("printThisPathDocumentContext", 2);


    /* printThisPathDocumentContext (for the most part) does not throw errors
     * for an invalid object. it will report that something is invalid in the
     * printed message, but will still make a sensible message regardless */


    SEXP x = CAR(args); args = CDR(args);
    if (TYPEOF(x) != ENVSXP)
        error(_("invalid '%s' value"), "x");
    Rboolean quote = asLogical(CAR(args)); args = CDR(args);
    if (quote == NA_LOGICAL)
        error(_("invalid '%s' value"), "quote");


    if (x == R_EmptyEnv) {
        Rprintf("<environment: R_EmptyEnv>\n");
        set_R_Visible(FALSE);
        return x;
    }


    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = CONS(R_NilValue, R_NilValue), &indx);
    SET_TAG(expr, R_QuoteSymbol);
    REPROTECT(expr = LCONS(encodeStringSymbol, CONS(R_NilValue, expr)), indx);


    SEXP klass = getAttrib(x, R_ClassSymbol);
    int nklass = LENGTH(klass);
    if (nklass) {
        SETCADR(expr, klass);
        SETCADDR(expr, mkString("\""));
        SEXP tmp = eval(expr, R_BaseEnv);
        PROTECT(tmp);
        Rprintf("<object of class ");
        for (int i = 0; i < nklass; i++)
            if (i) Rprintf(", %s", CHAR(STRING_ELT(tmp, i)));
            else   Rprintf("%s"  , CHAR(STRING_ELT(tmp, i)));
        Rprintf(" at %p>\n", (void *) x);
        UNPROTECT(1);
    }
    else {
        Rprintf("<object of class \"%s\" at %p>\n",
                type2char(TYPEOF(x)), (void *) x);
    }


    if (!quote) SETCADDR(expr, R_BlankScalarString);


    SEXP sym;
#define print_invalid_unbound   Rprintf("%s: <invalid, R_UnboundValue>\n", CHAR(PRINTNAME(sym)))
#define print_null              Rprintf("%s: NULL\n", CHAR(PRINTNAME(sym)))
#define print_invalid_null      Rprintf("%s: <invalid, NULL>\n", CHAR(PRINTNAME(sym)))
#define print_type(var)         Rprintf("%s: <type = \"%s\", length = %d>\n"         , CHAR(PRINTNAME(sym)), type2char(TYPEOF((var))), length((var)))
#define print_invalid_type(var) Rprintf("%s: <invalid, type = \"%s\", length = %d>\n", CHAR(PRINTNAME(sym)), type2char(TYPEOF((var))), length((var)))
#define _print_encoded_str(fmt, str)                           \
    do {                                                       \
        SETCADR(expr, (str));                                  \
        SEXP tmp = eval(expr, R_BaseEnv);                      \
        PROTECT(tmp);                                          \
        Rprintf((fmt), CHAR(PRINTNAME(sym)), CHAR(STRING_ELT(tmp, 0)));\
        UNPROTECT(1);                                          \
    } while (0)
#define print_encoded_str(str) _print_encoded_str("%s: %s\n", (str))


    SEXP errcnd = findVarInFrame(x, sym = errcndSymbol);
    if (errcnd != R_UnboundValue) {
        if (errcnd == R_NilValue)
            print_invalid_null;
        else if (TYPEOF(errcnd) == VECSXP &&
                 LENGTH(errcnd) >= 2 &&
                 inherits(errcnd, "condition"))
        {
            Rprintf("%s: ", CHAR(PRINTNAME(sym)));
            my_PrintValueEnv(errcnd, rho);
        }
        else print_invalid_type(errcnd);


        SEXP for_msg = findVarInFrame(x, sym = for_msgSymbol);
        if (for_msg == R_UnboundValue)
            print_invalid_unbound;
        else if (for_msg == R_NilValue)
            print_invalid_null;
        else if (IS_SCALAR(for_msg, STRSXP))
            print_encoded_str(for_msg);
        else
            print_invalid_type(for_msg);


        SEXP associated_with_file = findVarInFrame(x, sym = associated_with_fileSymbol);
        if (associated_with_file == R_UnboundValue);
        else if (associated_with_file == R_NilValue)
            print_null;
        else if (IS_SCALAR(associated_with_file, LGLSXP)) {
            Rboolean tmp = LOGICAL(associated_with_file)[0];
            Rprintf("%s: %s\n", CHAR(PRINTNAME(sym)),
                (tmp == NA_LOGICAL) ? "NA" : (tmp ? "TRUE" : "FALSE"));
        }
        else print_invalid_type(associated_with_file);
    }
    else {
        SEXP ofile = findVarInFrame(x, sym = ofileSymbol);
        if (ofile == R_UnboundValue)
            print_invalid_unbound;
        else if (ofile == R_NilValue)
            print_invalid_null;
        else if (IS_SCALAR(ofile, STRSXP))
            print_encoded_str(ofile);
        else
            print_invalid_type(ofile);


        SEXP wd = findVarInFrame(x, sym = wdSymbol);
        if (wd == R_UnboundValue);
        else if (wd == R_NilValue)
            print_null;
        else if (IS_SCALAR(wd, STRSXP))
            print_encoded_str(wd);
        else
            print_invalid_type(wd);


        SEXP file = findVarInFrame(x, sym = fileSymbol);
        if (file == R_UnboundValue)
            print_invalid_unbound;
        else if (file == R_NilValue)
            print_invalid_null;
        else if (TYPEOF(file) == PROMSXP) {
            SEXP val = PRVALUE(file);
            if (val == R_UnboundValue) {
                Rprintf("%s: ", CHAR(PRINTNAME(sym)));
                my_PrintValueEnv(PREXPR(file), rho);
            }
            else if (val == R_NilValue) {
                print_null;
            }
            else if (IS_SCALAR(val, STRSXP)) {
                print_encoded_str(val);
            }
            else print_invalid_type(val);
        }
        else print_invalid_type(file);


        SEXP lines = findVarInFrame(x, sym = linesSymbol);
        if (lines == R_UnboundValue);
        else if (lines == R_NilValue)
            print_invalid_null;
        else if (TYPEOF(lines) == PROMSXP) {
            SEXP val = PRVALUE(lines);
            if (val == R_UnboundValue) {
                Rprintf("%s: ", CHAR(PRINTNAME(sym)));
                my_PrintValueEnv(PREXPR(lines), rho);
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
    }


    SEXP source = findVarInFrame(x, sym = sourceSymbol);
    if (source == R_UnboundValue)
        print_invalid_unbound;
    else if (source == R_NilValue)
        print_invalid_null;
    else if (TYPEOF(source) == CHARSXP)
        _print_encoded_str("%s: <CHARSXP: %s>\n", ScalarString(source));
    else print_invalid_type(source);


    SEXP setsyspathwashere = findVarInFrame(x, sym = setsyspathwashereSymbol);
    if (setsyspathwashere != R_UnboundValue) {
        if (setsyspathwashere == R_NilValue)
            print_null;
        else if (IS_SCALAR(setsyspathwashere, LGLSXP)) {
            Rboolean tmp = LOGICAL(setsyspathwashere)[0];
            Rprintf("%s: %s\n", CHAR(PRINTNAME(sym)),
                (tmp == NA_LOGICAL) ? "NA" : (tmp ? "TRUE" : "FALSE"));
        }
        else print_invalid_type(setsyspathwashere);
    }


    SEXP n = findVarInFrame(x, sym = nSymbol);
    if (n == R_UnboundValue) {
        if (setsyspathwashere != R_UnboundValue)
            print_invalid_unbound;
    }
    else if (n == R_NilValue)
        print_invalid_null;
    else if (IS_SCALAR(n, INTSXP))
        Rprintf("%s: %d\n", CHAR(PRINTNAME(sym)), INTEGER(n)[0]);
    else print_invalid_type(n);


    set_R_Visible(FALSE);
    UNPROTECT(1);
    return x;
}
