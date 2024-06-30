#include "thispathdefn.h"


SEXP errorCondition(const char *msg, SEXP call, int numFields, SEXP Class)
{
    SEXP value = Rf_allocVector(VECSXP, 2 + numFields);
    Rf_protect(value);
    SEXP names = Rf_allocVector(STRSXP, 2 + numFields);
    Rf_setAttrib(value, R_NamesSymbol, names);


    SET_STRING_ELT(names, 0, Rf_mkChar("message"));
    SET_VECTOR_ELT(value, 0, Rf_mkString(msg));
    SET_STRING_ELT(names, 1, Rf_mkChar("call"));
    SET_VECTOR_ELT(value, 1, call);


    Rf_setAttrib(value, R_ClassSymbol, Class);


    Rf_unprotect(1);
    return value;
}


SEXP errorCondition_strings(const char *msg, SEXP call, int numFields, const char **Class)
{
    /* count the number of strings in 'Class' */
    int nClass = 0;
    if (Class) { while (Class[nClass]) ++nClass; }


    SEXP klass = Rf_allocVector(STRSXP, nClass + 2);
    Rf_protect(klass);
    for (int i = 0; i < nClass; i++) {
    SET_STRING_ELT(klass, i         , Rf_mkChar(Class[i]));
    }
    SET_STRING_ELT(klass, nClass    , Rf_mkChar("error"));
    SET_STRING_ELT(klass, nClass + 1, Rf_mkChar("condition"));


    SEXP value = errorCondition(msg, call, numFields, klass);


    Rf_unprotect(1);
    return value;
}


SEXP errorCondition_string(const char *msg, SEXP call, int numFields, const char *Class)
{
    SEXP klass = Rf_allocVector(STRSXP, 3);
    Rf_protect(klass);
    SET_STRING_ELT(klass, 0, Rf_mkChar(Class));
    SET_STRING_ELT(klass, 1, Rf_mkChar("error"));
    SET_STRING_ELT(klass, 2, Rf_mkChar("condition"));


    SEXP value = errorCondition(msg, call, numFields, klass);


    Rf_unprotect(1);
    return value;
}


SEXP simpleError(const char *msg, SEXP call)
{
    return errorCondition_string(msg, call, 0, "simpleError");
}


SEXP ThisPathInAQUAError(SEXP call)
{
    const char *msg = "R is running from AQUA for which 'this.path' is currently unimplemented\n"
                      " consider using RStudio, VSCode, or Emacs until such a time when this is implemented";
    return errorCondition(msg, call, 0, ThisPathInAQUAErrorClass);
}


SEXP ThisPathInZipFileError(SEXP call, SEXP description)
{
    const char *msg = "'this.path' cannot be used within a zip file";
    SEXP cond = errorCondition(msg, call, 1, ThisPathInZipFileErrorClass);
    Rf_protect(cond);
    SEXP names = Rf_getAttrib(cond, R_NamesSymbol);
    Rf_protect(names);
    SET_STRING_ELT(names, 2, Rf_mkChar("description"));
    SET_VECTOR_ELT(cond , 2, Rf_ScalarString(description));
    Rf_unprotect(2);
    return cond;
}


SEXP ThisPathNotExistsError(const char *msg, SEXP call)
{
    return errorCondition(msg, call, 0, ThisPathNotExistsErrorClass);
}


SEXP ThisPathNotFoundError(const char *msg, SEXP call)
{
    return errorCondition(msg, call, 0, ThisPathNotFoundErrorClass);
}


SEXP ThisPathNotImplementedError(const char *msg, SEXP call)
{
    return errorCondition(msg, call, 0, ThisPathNotImplementedErrorClass);
}


#define funbody(connection_class_as_CHARSXP, summConn)         \
    const char *klass = EncodeChar((connection_class_as_CHARSXP));\
    const char *format = "'this.path' not implemented when source()-ing a connection of class '%s'";\
    const int n = strlen(format) + strlen(klass) + 1;          \
    char msg[n];                                               \
    snprintf(msg, n, format, klass);                           \
    SEXP cond = errorCondition(msg, call, 1, ThisPathUnrecognizedConnectionClassErrorClass);\
    Rf_protect(cond);                                          \
    SEXP names = Rf_getAttrib(cond, R_NamesSymbol);            \
    Rf_protect(names);                                         \
    SET_STRING_ELT(names, 2, Rf_mkChar("summary"));            \
    SET_VECTOR_ELT(cond , 2, (summConn));                      \
    Rf_unprotect(2);                                           \
    return cond
SEXP ThisPathUnrecognizedConnectionClassError(SEXP call, SEXP summary)
{
    funbody(STRING_ELT(VECTOR_ELT(summary, 1), 0), summary);
}
#if defined(R_CONNECTIONS_VERSION_1)
SEXP ThisPathUnrecognizedConnectionClassError_Rcon_V1(SEXP call, Rconnection Rcon)
{
    funbody(Rf_mkChar(Rcon->class), summary_connection_Rcon_V1(Rcon));
}
#endif
#undef funbody


SEXP ThisPathUnrecognizedMannerError(SEXP call)
{
    const char *msg = "R is running in an unrecognized manner";
    return errorCondition(msg, call, 0, ThisPathUnrecognizedMannerErrorClass);
}


void stop(SEXP cond)
{
    SEXP expr = Rf_lcons(stopSymbol, Rf_cons(cond, R_NilValue));
    Rf_protect(expr);
    Rf_eval(expr, R_BaseEnv);
    Rf_unprotect(1);
    return;
}





SEXP do_ThisPathInAQUAError do_formals
{
    do_start_no_call_op_rho("ThisPathInAQUAError", 1);
    return ThisPathInAQUAError(Rf_lazy_duplicate(CAR(args)));
}


SEXP do_ThisPathInZipFileError do_formals
{
    do_start_no_op_rho("ThisPathInZipFileError", 2);
    SEXP call2 = Rf_lazy_duplicate(CAR(args)); args = CDR(args);
    if (!IS_SCALAR(CAR(args), STRSXP) ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        Rf_errorcall(call, _("invalid first argument"));
    }
    SEXP description = STRING_ELT(CAR(args), 0);
    return ThisPathInZipFileError(call2, description);
}


SEXP do_ThisPathNotExistsError do_formals
{
    do_start_no_op_rho("ThisPathNotExistsError", 2);
    if (!IS_SCALAR(CAR(args), STRSXP) ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        Rf_errorcall(call, _("invalid first argument"));
    }
    const char *msg = Rf_translateChar(STRING_ELT(CAR(args), 0));
    return ThisPathNotExistsError(msg, Rf_lazy_duplicate(CADR(args)));
}


SEXP do_ThisPathNotFoundError do_formals
{
    do_start_no_op_rho("ThisPathNotFoundError", 2);
    if (!IS_SCALAR(CAR(args), STRSXP) ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        Rf_errorcall(call, _("invalid first argument"));
    }
    const char *msg = Rf_translateChar(STRING_ELT(CAR(args), 0));
    return ThisPathNotFoundError(msg, Rf_lazy_duplicate(CADR(args)));
}


SEXP do_ThisPathNotImplementedError do_formals
{
    do_start_no_op_rho("ThisPathNotImplementedError", 2);
    if (!IS_SCALAR(CAR(args), STRSXP) ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        Rf_errorcall(call, _("invalid first argument"));
    }
    const char *msg = Rf_translateChar(STRING_ELT(CAR(args), 0));
    return ThisPathNotImplementedError(msg, Rf_lazy_duplicate(CADR(args)));
}


SEXP do_ThisPathUnrecognizedConnectionClassError do_formals
{
    do_start_no_call_op_rho("ThisPathUnrecognizedConnectionClassError", 2);
#if defined(R_CONNECTIONS_VERSION_1)
    if (ptr_R_GetConnection)
        return ThisPathUnrecognizedConnectionClassError_Rcon_V1(Rf_lazy_duplicate(CAR(args)), ptr_R_GetConnection(CADR(args)));
#endif
    SEXP summary = summary_connection(CADR(args));
    Rf_protect(summary);
    SEXP value = ThisPathUnrecognizedConnectionClassError(Rf_lazy_duplicate(CAR(args)), summary);
    Rf_unprotect(1);
    return value;
}


SEXP do_ThisPathUnrecognizedMannerError do_formals
{
    do_start_no_call_op_rho("ThisPathUnrecognizedMannerError", 1);
    return ThisPathUnrecognizedMannerError(Rf_lazy_duplicate(CAR(args)));
}





SEXP do_last_condition do_formals
{
    do_start_no_op("last_condition", -1);
    switch (Rf_length(args)) {
#if R_version_at_least(3,0,0)
    case 0:
        return CAR(last_condition);
    case 1:
        set_R_Visible(FALSE);
        return SETCAR(last_condition, CAR(args));
#else
    case 0:
        set_this_path_value(CAR(last_condition));
        return R_NilValue;
    case 1:
        set_this_path_value(SETCAR(last_condition, CAR(args)));
        set_this_path_visible(FALSE);
        return R_NilValue;
#endif
    default:
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), ".C_last_condition", "0 or 1"));
        return R_NilValue;
    }
}


typedef enum {TRYCATCHOP_2, TRYCATCHOP_3} TRYCATCHOP;


SEXP tryCatch(TRYCATCHOP op, SEXP rho)
{
    int nprotect = 0;


    SEXP finally = Rf_findVarInFrame(rho, finallySymbol);
    if (finally == R_UnboundValue)
        Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(finallySymbol)));
    if (finally != R_MissingArg) {
        SEXP on_exit = Rf_lcons(on_exitSymbol, Rf_cons(finallySymbol, R_NilValue));
        Rf_protect(on_exit);
        Rf_eval(on_exit, rho);
        Rf_unprotect(1);
    }


    SEXP dots = Rf_findVarInFrame(rho, R_DotsSymbol);
    Rf_protect(dots); nprotect++;
    if (dots == R_UnboundValue)
        Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(R_DotsSymbol)));
    int dots_length = (TYPEOF(dots) == DOTSXP ? Rf_length(dots) : 0);


    SEXP else_ = Rf_findVarInFrame(rho, else_Symbol);
    Rf_protect(else_); nprotect++;
    if (else_ == R_UnboundValue)
        Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(else_Symbol)));
    if (else_ != R_MissingArg && dots_length <= 0)
        Rf_error("'tryCatch' with 'else.' but no condition handlers makes no sense");


    SEXP expr;
    PROTECT_INDEX indx;
    R_ProtectWithIndex(expr = R_NilValue, &indx); nprotect++;


    if (dots_length) {
        switch (op) {
        case TRYCATCHOP_2:
        {
            SEXP formals = Rf_cons(R_MissingArg, R_NilValue);
            Rf_protect(formals);
            SET_TAG(formals, cSymbol);

            SEXP body = Rf_lcons(invisibleSymbol, R_NilValue);
            Rf_protect(body);

            MARK_NOT_MUTABLE_defineVar(funSymbol, R_mkClosure(formals, body, rho), rho);
            Rf_unprotect(2);

            SEXP expr0 = funSymbol;
            for (int i = dots_length - 1; i >= 0; i--) {
                SEXP dot = Rf_nthcdr(dots, i);
                if (CAR(dot) != R_MissingArg) {
                    char buf[15];
                    snprintf(buf, 15, "..%d", i + 1);
                    expr0 = Rf_install(buf);
                }
                R_Reprotect(expr = Rf_cons(expr0, expr), indx);
                SET_TAG(expr, TAG(dot));
                if (TAG(dot) == R_NilValue)
                    Rf_error("condition handlers must be specified with a condition class");
            }
            break;
        }
        case TRYCATCHOP_3:
        {
            SEXP fun0, formals, assign_last_condition;

            formals = Rf_cons(R_MissingArg, R_NilValue);
            Rf_protect(formals);
            SET_TAG(formals, cSymbol);

            SEXP body = Rf_lcons(
                R_BraceSymbol,
                Rf_cons(
                    R_NilValue,
                    Rf_cons(
                        Rf_lcons(invisibleSymbol, R_NilValue),
                        R_NilValue
                    )
                )
            );
            Rf_protect(body);

            assign_last_condition = SETCADR(
                body,
                Rf_lcons(
                    _last_conditionSymbol,
                    Rf_cons(cSymbol, R_NilValue)
                )
            );

            fun0 = R_mkClosure(formals, body, rho);
            MARK_NOT_MUTABLE(fun0);
            Rf_protect(fun0);

            SEXP funs = Rf_allocVector(VECSXP, dots_length);
            Rf_protect(funs);
            MARK_NOT_MUTABLE_defineVar(funsSymbol, funs, rho);
            for (int i = dots_length - 1; i >= 0; i--) {
                SEXP dot = Rf_nthcdr(dots, i);
                if (CAR(dot) == R_MissingArg)
                    SET_VECTOR_ELT(funs, i, fun0);
                else {
                    char buf[15];
                    snprintf(buf, 15, "..%d", i + 1);
                    fun0 = R_mkClosure(
                        formals,
                        Rf_lcons(
                            R_BraceSymbol,
                            Rf_cons(
                                assign_last_condition,
                                Rf_cons(Rf_install(buf), R_NilValue)
                            )
                        ),
                        rho
                    );
                    SET_VECTOR_ELT(funs, i, fun0);
                }
                R_Reprotect(
                    expr = Rf_cons(
                        Rf_lcons(
                            R_Bracket2Symbol,
                            Rf_cons(
                                funsSymbol,
                                Rf_cons(Rf_ScalarInteger(i + 1), R_NilValue)
                            )
                        ),
                        expr
                    ),
                    indx
                );
                SET_TAG(expr, TAG(dot));
                if (TAG(dot) == R_NilValue)
                    Rf_error("condition handlers must be specified with a condition class");
            }
            Rf_unprotect(4);
            break;
        }
        default:
            Rf_error(_("invalid '%s' value"), "op");
            Rf_unprotect(nprotect);
            return R_NilValue;
        }
    }


    R_Reprotect(expr = Rf_cons(exprSymbol, expr), indx);
    SET_TAG(expr, exprSymbol);
    R_Reprotect(expr = Rf_lcons(tryCatchSymbol, expr), indx);


    SEXP value;
    if (else_ == R_MissingArg) {
#if R_version_at_least(3,0,0)
        value = Rf_eval(expr, rho);
#else
        R_Reprotect(expr = Rf_lcons(withVisibleSymbol, Rf_cons(expr, R_NilValue)), indx);
        value = Rf_eval(expr, rho);
        Rf_protect(value);
        set_this_path_value(VECTOR_ELT(value, 0));
        set_this_path_visible(Rf_asLogical(VECTOR_ELT(value, 1)));
        Rf_unprotect(1);
        value = R_NilValue;
#endif
    }
    else {
        Rf_defineVar(do_elseSymbol, R_FalseValue, rho);
        SETCADR(
            expr,
            Rf_lcons(
                R_BraceSymbol,
                Rf_cons(
                    exprSymbol,
                    Rf_cons(
                        Rf_lcons(
                            AssignSymbol,
                            Rf_cons(
                                do_elseSymbol,
                                Rf_cons(R_TrueValue, R_NilValue)
                            )
                        ),
                        R_NilValue
                    )
                )
            )
        );
#if R_version_at_least(3,0,0)
        value = Rf_eval(expr, rho);
#else
        R_Reprotect(expr = Rf_lcons(withVisibleSymbol, Rf_cons(expr, R_NilValue)), indx);
        value = Rf_eval(expr, rho);
#endif
        Rf_protect(value); nprotect++;
        SEXP do_else = Rf_findVarInFrame(rho, do_elseSymbol);
        if (do_else == R_UnboundValue)
            Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(do_elseSymbol)));
        if (!IS_SCALAR(do_else, LGLSXP))
            Rf_error(_("invalid '%s' value"), R_CHAR(PRINTNAME(do_elseSymbol)));
        switch (LOGICAL(do_else)[0]) {
        case TRUE:
#if R_version_at_least(3,0,0)
            value = Rf_eval(else_Symbol, rho);
#else
            expr = Rf_lcons(withVisibleSymbol, Rf_cons(else_Symbol, R_NilValue));
            Rf_protect(expr);
            value = Rf_eval(expr, rho);
            Rf_unprotect(1);
            Rf_protect(value); nprotect++;
#endif
            break;
        case FALSE:
            break;
        default:
            Rf_error(_("invalid '%s' value"), R_CHAR(PRINTNAME(do_elseSymbol)));
        }
#if R_version_less_than(3,0,0)
        set_this_path_value(VECTOR_ELT(value, 0));
        set_this_path_visible(Rf_asLogical(VECTOR_ELT(value, 1)));
        value = R_NilValue;
#endif
    }


    Rf_unprotect(nprotect);
    return value;
}


SEXP do_tryCatch2 do_formals
{
    do_start_no_call_op("tryCatch2", 0);
    return tryCatch(TRYCATCHOP_2, rho);
}


SEXP do_tryCatch3 do_formals
{
    do_start_no_call_op("tryCatch3", 0);
    return tryCatch(TRYCATCHOP_3, rho);
}
