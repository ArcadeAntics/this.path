#include "thispathdefn.h"


SEXP errorCondition(const char *msg, SEXP call, int numFields, SEXP Class)
{
    SEXP value = allocVector(VECSXP, 2 + numFields);
    PROTECT(value);
    SEXP names = allocVector(STRSXP, 2 + numFields);
    setAttrib(value, R_NamesSymbol, names);


    SET_STRING_ELT(names, 0, mkChar("message"));
    SET_VECTOR_ELT(value, 0, mkString(msg));
    SET_STRING_ELT(names, 1, mkChar("call"));
    SET_VECTOR_ELT(value, 1, call);


    setAttrib(value, R_ClassSymbol, Class);


    UNPROTECT(1);
    return value;
}


SEXP errorCondition_strings(const char *msg, SEXP call, int numFields, const char **Class)
{
    /* count the number of strings in 'Class' */
    int nClass = 0;
    if (Class) { while (Class[nClass]) ++nClass; }


    SEXP klass = allocVector(STRSXP, nClass + 2);
    PROTECT(klass);
    for (int i = 0; i < nClass; i++) {
    SET_STRING_ELT(klass, i         , mkChar(Class[i]));
    }
    SET_STRING_ELT(klass, nClass    , mkChar("error"));
    SET_STRING_ELT(klass, nClass + 1, mkChar("condition"));


    SEXP value = errorCondition(msg, call, numFields, klass);


    UNPROTECT(1);
    return value;
}


SEXP errorCondition_string(const char *msg, SEXP call, int numFields, const char *Class)
{
    SEXP klass = allocVector(STRSXP, 3);
    PROTECT(klass);
    SET_STRING_ELT(klass, 0, mkChar(Class));
    SET_STRING_ELT(klass, 1, mkChar("error"));
    SET_STRING_ELT(klass, 2, mkChar("condition"));


    SEXP value = errorCondition(msg, call, numFields, klass);


    UNPROTECT(1);
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
    PROTECT(cond);
    SEXP names = getAttrib(cond, R_NamesSymbol);
    PROTECT(names);
    SET_STRING_ELT(names, 2, mkChar("description"));
    SET_VECTOR_ELT(cond , 2, ScalarString(description));
    UNPROTECT(2);
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
    PROTECT(cond);                                             \
    SEXP names = getAttrib(cond, R_NamesSymbol);               \
    PROTECT(names);                                            \
    SET_STRING_ELT(names, 2, mkChar("summary"));               \
    SET_VECTOR_ELT(cond , 2, (summConn));                      \
    UNPROTECT(2);                                              \
    return cond
SEXP ThisPathUnrecognizedConnectionClassError(SEXP call, SEXP summary)
{
    funbody(STRING_ELT(VECTOR_ELT(summary, 1), 0), summary);
}
#if defined(R_CONNECTIONS_VERSION_1)
SEXP ThisPathUnrecognizedConnectionClassError_Rcon_V1(SEXP call, Rconnection Rcon)
{
    funbody(mkChar(Rcon->class), summary_connection_Rcon_V1(Rcon));
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
    SEXP expr = LCONS(stopSymbol, CONS(cond, R_NilValue));
    PROTECT(expr);
    eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return;
}





SEXP do_ThisPathInAQUAError do_formals
{
    do_start_no_call_op_rho("ThisPathInAQUAError", 1);
    return ThisPathInAQUAError(lazy_duplicate(CAR(args)));
}


SEXP do_ThisPathInZipFileError do_formals
{
    do_start_no_op_rho("ThisPathInZipFileError", 2);
    SEXP call2 = lazy_duplicate(CAR(args)); args = CDR(args);
    if (!IS_SCALAR(CAR(args), STRSXP) ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        errorcall(call, _("invalid first argument"));
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
        errorcall(call, _("invalid first argument"));
    }
    const char *msg = translateChar(STRING_ELT(CAR(args), 0));
    return ThisPathNotExistsError(msg, lazy_duplicate(CADR(args)));
}


SEXP do_ThisPathNotFoundError do_formals
{
    do_start_no_op_rho("ThisPathNotFoundError", 2);
    if (!IS_SCALAR(CAR(args), STRSXP) ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        errorcall(call, _("invalid first argument"));
    }
    const char *msg = translateChar(STRING_ELT(CAR(args), 0));
    return ThisPathNotFoundError(msg, lazy_duplicate(CADR(args)));
}


SEXP do_ThisPathNotImplementedError do_formals
{
    do_start_no_op_rho("ThisPathNotImplementedError", 2);
    if (!IS_SCALAR(CAR(args), STRSXP) ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        errorcall(call, _("invalid first argument"));
    }
    const char *msg = translateChar(STRING_ELT(CAR(args), 0));
    return ThisPathNotImplementedError(msg, lazy_duplicate(CADR(args)));
}


SEXP do_ThisPathUnrecognizedConnectionClassError do_formals
{
    do_start_no_call_op_rho("ThisPathUnrecognizedConnectionClassError", 2);
#if defined(R_CONNECTIONS_VERSION_1)
    if (ptr_R_GetConnection)
        return ThisPathUnrecognizedConnectionClassError_Rcon_V1(lazy_duplicate(CAR(args)), ptr_R_GetConnection(CADR(args)));
#endif
    return ThisPathUnrecognizedConnectionClassError(lazy_duplicate(CAR(args)), summary_connection(CADR(args)));
}


SEXP do_ThisPathUnrecognizedMannerError do_formals
{
    do_start_no_call_op_rho("ThisPathUnrecognizedMannerError", 1);
    return ThisPathUnrecognizedMannerError(lazy_duplicate(CAR(args)));
}





SEXP do_last_condition do_formals
{
    do_start_no_op("last_condition", -1);
    switch (length(args)) {
#if R_version_at_least(3, 0, 0)
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
        errorcall(call, wrong_nargs_to_External(length(args), ".C_last_condition", "0 or 1"));
        return R_NilValue;
    }
}


typedef enum {TRYCATCHOP_2, TRYCATCHOP_3} TRYCATCHOP;


SEXP tryCatch(TRYCATCHOP op, SEXP rho)
{
    int nprotect = 0;


    SEXP finally = findVarInFrame(rho, finallySymbol);
    if (finally == R_UnboundValue)
        error(_("object '%s' not found"), CHAR(PRINTNAME(finallySymbol)));
    if (finally != R_MissingArg) {
        SEXP on_exit = LCONS(on_exitSymbol, CONS(finallySymbol, R_NilValue));
        PROTECT(on_exit);
        eval(on_exit, rho);
        UNPROTECT(1);
    }


    SEXP dots = findVarInFrame(rho, R_DotsSymbol);
    if (dots == R_UnboundValue)
        error(_("object '%s' not found"), CHAR(PRINTNAME(R_DotsSymbol)));
    int dots_length = (TYPEOF(dots) == DOTSXP ? length(dots) : 0);


    SEXP else_ = findVarInFrame(rho, else_Symbol);
    if (else_ == R_UnboundValue)
        error(_("object '%s' not found"), CHAR(PRINTNAME(else_Symbol)));
    if (else_ != R_MissingArg && dots_length <= 0)
        error("'tryCatch' with 'else.' but no condition handlers makes no sense");


    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = R_NilValue, &indx); nprotect++;


    if (dots_length) {
        switch (op) {
        case TRYCATCHOP_2:
        {
            SEXP fun = allocSExp(CLOSXP), formals;
            MARK_NOT_MUTABLE_defineVar(funSymbol, fun, rho);
            SET_FORMALS(fun, formals = CONS(R_MissingArg, R_NilValue));
            SET_TAG(formals, cSymbol);
            SET_BODY(fun, LCONS(invisibleSymbol, R_NilValue));
            SET_CLOENV(fun, rho);
            SEXP expr0 = funSymbol;
            for (int i = dots_length - 1; i >= 0; i--) {
                SEXP dot = nthcdr(dots, i);
                if (CAR(dot) != R_MissingArg) {
                    char buf[15];
                    snprintf(buf, 15, "..%d", i + 1);
                    expr0 = install(buf);
                }
                REPROTECT(expr = CONS(expr0, expr), indx);
                SET_TAG(expr, TAG(dot));
                if (TAG(dot) == R_NilValue)
                    error("condition handlers must be specified with a condition class");
            }
            break;
        }
        case TRYCATCHOP_3:
        {
            SEXP fun0 = allocSExp(CLOSXP), formals, body, assign_last_condition;
            MARK_NOT_MUTABLE(fun0);
            PROTECT(fun0); nprotect++;
            SET_FORMALS(fun0, formals = CONS(R_MissingArg, R_NilValue));
            SET_TAG(formals, cSymbol);
            SET_BODY(fun0, body = LCONS(R_BraceSymbol,
                                        CONS(R_NilValue,
                                             CONS(LCONS(invisibleSymbol,
                                                        R_NilValue),
                                                  R_NilValue))));
            SETCADR(body, assign_last_condition = LCONS(_last_conditionSymbol,
                                                        CONS(cSymbol,
                                                             R_NilValue)));
            SET_CLOENV(fun0, rho);
            SEXP funs = allocVector(VECSXP, dots_length);
            MARK_NOT_MUTABLE_defineVar(funsSymbol, funs, rho);
            for (int i = dots_length - 1; i >= 0; i--) {
                SEXP dot = nthcdr(dots, i);
                if (CAR(dot) == R_MissingArg)
                    SET_VECTOR_ELT(funs, i, fun0);
                else {
                    SET_VECTOR_ELT(funs, i, fun0 = allocSExp(CLOSXP));
                    SET_FORMALS(fun0, formals);
                    char buf[15];
                    snprintf(buf, 15, "..%d", i + 1);
                    SET_BODY(fun0, LCONS(R_BraceSymbol,
                                         CONS(assign_last_condition,
                                              CONS(install(buf),
                                                   R_NilValue))));
                    SET_CLOENV(fun0, rho);
                }
                REPROTECT(expr = CONS(LCONS(R_Bracket2Symbol,
                                            CONS(funsSymbol,
                                                 CONS(ScalarInteger(i + 1),
                                                      R_NilValue))),
                                      expr), indx);
                SET_TAG(expr, TAG(dot));
                if (TAG(dot) == R_NilValue)
                    error("condition handlers must be specified with a condition class");
            }
            break;
        }
        default:
            error(_("invalid '%s' value"), "op");
            UNPROTECT(nprotect);
            return R_NilValue;
        }
    }


    REPROTECT(expr = CONS(exprSymbol, expr), indx);
    SET_TAG(expr, exprSymbol);
    REPROTECT(expr = LCONS(tryCatchSymbol, expr), indx);


    SEXP value;
    if (else_ == R_MissingArg) {
#if R_version_at_least(3, 0, 0)
        value = eval(expr, rho);
#else
        REPROTECT(expr = LCONS(withVisibleSymbol, CONS(expr, R_NilValue)), indx);
        value = eval(expr, rho);
        PROTECT(value);
        set_this_path_value(VECTOR_ELT(value, 0));
        set_this_path_visible(asLogical(VECTOR_ELT(value, 1)));
        UNPROTECT(1);
        value = R_NilValue;
#endif
    }
    else {
        defineVar(do_elseSymbol, R_FalseValue, rho);
        SETCADR(expr, LCONS(R_BraceSymbol,
                            CONS(exprSymbol,
                                 CONS(LCONS(AssignSymbol,
                                            CONS(do_elseSymbol,
                                                 CONS(R_TrueValue,
                                                      R_NilValue))),
                                      R_NilValue))));
#if R_version_at_least(3, 0, 0)
        value = eval(expr, rho);
#else
        REPROTECT(expr = LCONS(withVisibleSymbol, CONS(expr, R_NilValue)), indx);
        value = eval(expr, rho);
#endif
        PROTECT(value); nprotect++;
        SEXP do_else = findVarInFrame(rho, do_elseSymbol);
        if (do_else == R_UnboundValue)
            error(_("object '%s' not found"), CHAR(PRINTNAME(do_elseSymbol)));
        if (!IS_SCALAR(do_else, LGLSXP))
            error(_("invalid '%s' value"), CHAR(PRINTNAME(do_elseSymbol)));
        switch (LOGICAL(do_else)[0]) {
        case TRUE:
#if R_version_at_least(3, 0, 0)
            value = eval(else_Symbol, rho);
#else
            expr = LCONS(withVisibleSymbol, CONS(else_Symbol, R_NilValue));
            PROTECT(expr);
            value = eval(expr, rho);
            UNPROTECT(1);
            PROTECT(value); nprotect++;
#endif
            break;
        case FALSE:
            break;
        default:
            error(_("invalid '%s' value"), CHAR(PRINTNAME(do_elseSymbol)));
        }
#if R_version_less_than(3, 0, 0)
        set_this_path_value(VECTOR_ELT(value, 0));
        set_this_path_visible(asLogical(VECTOR_ELT(value, 1)));
        value = R_NilValue;
#endif
    }


    UNPROTECT(nprotect);
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
