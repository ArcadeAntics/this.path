/*
this.path : Get Executing Script's Path
Copyright (C) 2023-2026   Iris Simmons
 */


#include "thispathdefn.h"





Rboolean my_isMethodDispatchOn(void)
{
    return Rf_asLogical(Rf_eval(expr__isMethodsDispatchOn, R_EmptyEnv));
}


void my_PrintObjectS4(SEXP s, SEXP env)
{
    int nprotect = 0;


    SEXP methods = my_getRegisteredNamespace("methods", methodsSymbol);
    Rf_protect(methods); nprotect++;
    if (methods == R_NilValue)
        Rf_error("missing methods namespace: this should not happen");


    SEXP mask = R_NewEnv(env, FALSE, 0);
    Rf_protect(mask); nprotect++;


    SEXP show = my_getVarInFrame(methods, showSymbol, TRUE);
    if (show == my_UnboundValue)
        Rf_error("missing show() in methods namespace: this should not happen");


    Rf_defineVar(showSymbol, show, mask);
    R_MakeForcedBinding(objectSymbol, s, s, mask);


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
    R_MakeForcedBinding(xSymbol, s, s, mask);


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
    R_MakeForcedBinding(xSymbol, s, s, mask);


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
#define print_invalid_C_NULL    Rprintf("%s: <invalid, C NULL> \n", R_CHAR(PRINTNAME(sym)))
#define print_invalid_unbound   Rprintf("%s: <invalid, unbound>\n", R_CHAR(PRINTNAME(sym)))
#define print_null              Rprintf("%s: NULL\n", R_CHAR(PRINTNAME(sym)))
#define print_invalid_null      Rprintf("%s: <invalid, NULL>\n", R_CHAR(PRINTNAME(sym)))
#define _print_type(type, len)         Rprintf("%s: <type = \"%s\", length = %lld>\n"         , R_CHAR(PRINTNAME(sym)), Rf_type2char((type)), (long long int) (len))
#define _print_invalid_type(type, len) Rprintf("%s: <invalid, type = \"%s\", length = %lld>\n", R_CHAR(PRINTNAME(sym)), Rf_type2char((type)), (long long int) (len))
#define print_type(var)              _print_type        (my_TYPEOF((var)), Rf_xlength((var).value))
#define print_type_SEXP(var)         _print_type        (   TYPEOF((var)), Rf_xlength((var)      ))
#define print_invalid_type(var)      _print_invalid_type(my_TYPEOF((var)), Rf_xlength((var).value))
#define print_invalid_type_SEXP(var) _print_invalid_type(   TYPEOF((var)), Rf_xlength((var)      ))
#define _print_encoded_str(fmt, str)                           \
    do {                                                       \
        SETCADR(expr, (str));                                  \
        SEXP tmp = Rf_eval(expr, R_BaseEnv);                   \
        Rf_protect(tmp);                                       \
        Rprintf((fmt), R_CHAR(PRINTNAME(sym)), R_CHAR(STRING_ELT(tmp, 0)));\
        Rf_unprotect(1);                                       \
    } while (0)
#define print_encoded_str(str) _print_encoded_str("%s: %s\n", (str))


    binding_info_t errcnd; my_findVarInFrame(x, sym = errcndSymbol, &errcnd);
    Rf_protect(errcnd.value);
    if (!errcnd.value)
        print_invalid_C_NULL;
    else if (errcnd.value != my_UnboundValue) {
        if (errcnd.value == R_NilValue)
            print_invalid_null;
        else if (my_TYPEOF(errcnd) == VECSXP &&
                 LENGTH(errcnd.value) >= 2 &&
                 Rf_inherits(errcnd.value, "condition"))
        {
            Rprintf("%s: ", R_CHAR(PRINTNAME(sym)));
            my_PrintValueEnv(errcnd.value, rho);
        }
        else print_invalid_type(errcnd);


        binding_info_t for_msg; my_findVarInFrame(x, sym = for_msgSymbol, &for_msg);
        Rf_protect(for_msg.value);
        if (!for_msg.value)
            print_invalid_C_NULL;
        else if (for_msg.value == my_UnboundValue)
            print_invalid_unbound;
        else if (for_msg.value == R_NilValue)
            print_invalid_null;
        else if (IS_SCALAR(for_msg.value, STRSXP))
            print_encoded_str(for_msg.value);
        else
            print_invalid_type(for_msg);
        Rf_unprotect(1);


        binding_info_t associated_with_file; my_findVarInFrame(x, sym = associated_with_fileSymbol, &associated_with_file);
        Rf_protect(associated_with_file.value);
        if (!associated_with_file.value)
            print_invalid_C_NULL;
        else if (associated_with_file.value == my_UnboundValue);
        else if (associated_with_file.value == R_NilValue)
            print_null;
        else if (IS_SCALAR(associated_with_file.value, LGLSXP)) {
            Rboolean tmp = LOGICAL(associated_with_file.value)[0];
            Rprintf("%s: %s\n", R_CHAR(PRINTNAME(sym)),
                (tmp == NA_LOGICAL) ? "NA" : (tmp ? "TRUE" : "FALSE"));
        }
        else print_invalid_type(associated_with_file);
        Rf_unprotect(1);
    }
    else {
        binding_info_t ofile; my_findVarInFrame(x, sym = ofileSymbol, &ofile);
        Rf_protect(ofile.value);
        if (!ofile.value)
            print_invalid_C_NULL;
        else if (ofile.value == my_UnboundValue)
            print_invalid_unbound;
        else if (ofile.value == R_NilValue)
            print_invalid_null;
        else if (IS_SCALAR(ofile.value, STRSXP))
            print_encoded_str(ofile.value);
        else
            print_invalid_type(ofile);
        Rf_unprotect(1);


        binding_info_t wd; my_findVarInFrame(x, sym = wdSymbol, &wd);
        Rf_protect(wd.value);
        if (!wd.value)
            print_invalid_C_NULL;
        else if (wd.value == my_UnboundValue);
        else if (wd.value == R_NilValue)
            print_null;
        else if (IS_SCALAR(wd.value, STRSXP))
            print_encoded_str(wd.value);
        else
            print_invalid_type(wd);
        Rf_unprotect(1);


        binding_info_t file; my_findVarInFrame(x, sym = fileSymbol, &file);
        Rf_protect(file.value);
        if (!file.value)
            print_invalid_C_NULL;
        else if (file.value == my_UnboundValue)
            print_invalid_unbound;
        else if (file.value == R_NilValue)
            print_invalid_null;
        else if (my_TYPEOF(file) == PROMSXP) {
            SEXP val = my_PRVALUE(file);
            if (val == my_UnboundValue) {
                Rprintf("%s: ", R_CHAR(PRINTNAME(sym)));
                my_PrintValueEnv(my_PREXPR(file), rho);
            }
            else if (val == R_NilValue) {
                print_null;
            }
            else if (IS_SCALAR(val, STRSXP)) {
                print_encoded_str(val);
            }
            else print_invalid_type_SEXP(val);
        }
        else print_invalid_type(file);
        Rf_unprotect(1);


        binding_info_t lines; my_findVarInFrame(x, sym = linesSymbol, &lines);
        Rf_protect(lines.value);
        if (!lines.value)
            print_invalid_C_NULL;
        else if (lines.value == my_UnboundValue);
        else if (lines.value == R_NilValue)
            print_invalid_null;
        else if (my_TYPEOF(lines) == PROMSXP) {
            SEXP val = my_PRVALUE(lines);
            if (val == my_UnboundValue) {
                Rprintf("%s: ", R_CHAR(PRINTNAME(sym)));
                my_PrintValueEnv(my_PREXPR(lines), rho);
            }
            else if (val == R_NilValue) {
                print_invalid_null;
            }
            else if (TYPEOF(val) == STRSXP) {
                print_type_SEXP(val);
            }
            else print_invalid_type_SEXP(val);
        }
        else if (my_TYPEOF(lines) == STRSXP)
            print_type(lines);
        else print_invalid_type(lines);
        Rf_unprotect(1);
    }
    Rf_unprotect(1);  /* errcnd */


    binding_info_t source; my_findVarInFrame(x, sym = sourceSymbol, &source);
    Rf_protect(source.value);
    if (!source.value)
        print_invalid_C_NULL;
    else if (source.value == my_UnboundValue)
        print_invalid_unbound;
    else if (source.value == R_NilValue)
        print_invalid_null;
    else if (my_TYPEOF(source) == CHARSXP)
        _print_encoded_str("%s: <CHARSXP: %s>\n", Rf_ScalarString(source.value));
    else print_invalid_type(source);
    Rf_unprotect(1);


    binding_info_t setsyspathwashere; my_findVarInFrame(x, sym = setsyspathwashereSymbol, &setsyspathwashere);
    Rf_protect(setsyspathwashere.value);
    if (!setsyspathwashere.value)
        print_invalid_C_NULL;
    else if (setsyspathwashere.value != my_UnboundValue) {
        if (setsyspathwashere.value == R_NilValue)
            print_null;
        else if (IS_SCALAR(setsyspathwashere.value, LGLSXP)) {
            Rboolean tmp = LOGICAL(setsyspathwashere.value)[0];
            Rprintf("%s: %s\n", R_CHAR(PRINTNAME(sym)),
                (tmp == NA_LOGICAL) ? "NA" : (tmp ? "TRUE" : "FALSE"));
        }
        else print_invalid_type(setsyspathwashere);
    }


    binding_info_t n; my_findVarInFrame(x, sym = nSymbol, &n);
    Rf_protect(n.value);
    if (!n.value)
        print_invalid_C_NULL;
    else if (n.value == my_UnboundValue) {
        if (setsyspathwashere.value != my_UnboundValue)
            print_invalid_unbound;
    }
    else if (n.value == R_NilValue)
        print_invalid_null;
    else if (IS_SCALAR(n.value, INTSXP))
        Rprintf("%s: %d\n", R_CHAR(PRINTNAME(sym)), INTEGER(n.value)[0]);
    else print_invalid_type(n);


    Rf_unprotect(2);


    set_R_Visible(FALSE);
    Rf_unprotect(1);
    return x;
}
