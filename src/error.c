#include "thispathdefn.h"


#include <wchar.h>


LibExtern Rboolean mbcslocale;


char *mbcsTruncateToValid(char *s)
{
    if (!mbcslocale || *s == '\0')
        return s;

    mbstate_t mb_st;
    size_t slen = strlen(s);
    size_t goodlen = 0;

    mbsinit(&mb_st);

    if (my_utf8locale) {
        goodlen = slen - 1;
        /*
         * check that the first two binary digits are 10
        while (goodlen && ((s[goodlen] & b'11000000') == b'10000000')) */
        while (goodlen && ((s[goodlen] & '\xC0') == '\x80'))
            --goodlen;
    }
    while (goodlen < slen) {
        size_t res = mbrtowc(NULL, s + goodlen, slen - goodlen, &mb_st);
        if (res == (size_t) -1 || res == (size_t) -2) {
            for (; goodlen < slen; goodlen++)
                s[goodlen] = '\0';
            return s;
        }
        goodlen += res;
    }
    return s;
}


int vsnprintf_mbcs(char *buf, size_t size, const char *format, va_list ap)
{
    int value = vsnprintf(buf, size, format, ap);
    if (size) {
        if (value < 0) buf[0] = '\0';
        else buf[size - 1] = '\0';
        if (value >= size)
            mbcsTruncateToValid(buf);
    }
    return value;
}


int snprintf_mbcs(char *buf, size_t size, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    int value = vsnprintf_mbcs(buf, size, format, ap);
    va_end(ap);
    return value;
}


#define BUFSIZE 8192
static char emsg_buf[BUFSIZE];


SEXP vmake_error_condition(SEXP call, SEXP rho,
                           SEXP Class,
                           int nextra, const char *format, va_list ap)
{
    if (call == R_CurrentExpression)
        call = getCurrentCall(rho);
    Rf_protect(call);


    SEXP value = Rf_allocVector(VECSXP, 2 + nextra);
    Rf_protect(value);
    SEXP names = Rf_allocVector(STRSXP, 2 + nextra);
    Rf_setAttrib(value, R_NamesSymbol, names);


    SET_STRING_ELT(names, 0, Rf_mkChar("message"));
    vsnprintf_mbcs(emsg_buf, BUFSIZE, format, ap);
    SET_VECTOR_ELT(value, 0, Rf_mkString(emsg_buf));
    SET_STRING_ELT(names, 1, Rf_mkChar("call"));
    SET_VECTOR_ELT(value, 1, call);


    Rf_setAttrib(value, R_ClassSymbol, Class);


    Rf_unprotect(2);
    return value;
}


SEXP make_error_condition(SEXP call, SEXP rho,
                          SEXP Class,
                          int nextra, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    SEXP value = vmake_error_condition(call, rho, Class,
                                       nextra, format, ap);
    va_end(ap);
    return value;
}


SEXP vmake_error_condition_strings(SEXP call, SEXP rho,
                                   const char **Class,
                                   int nextra, const char *format, va_list ap)
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


    SEXP value = vmake_error_condition(call, rho, klass, nextra, format, ap);


    Rf_unprotect(1);
    return value;
}


SEXP make_error_condition_strings(SEXP call, SEXP rho,
                                  const char **Class,
                                  int nextra, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    SEXP value = vmake_error_condition_strings(call, rho, Class, nextra, format, ap);
    va_end(ap);
    return value;
}


SEXP vmake_error_condition_string(SEXP call, SEXP rho,
                                  const char *Class,
                                  int nextra, const char *format, va_list ap)
{
    SEXP klass = Rf_allocVector(STRSXP, 3);
    Rf_protect(klass);
    SET_STRING_ELT(klass, 0, Rf_mkChar(Class));
    SET_STRING_ELT(klass, 1, Rf_mkChar("error"));
    SET_STRING_ELT(klass, 2, Rf_mkChar("condition"));


    SEXP value = vmake_error_condition(call, rho, klass, nextra, format, ap);


    Rf_unprotect(1);
    return value;
}


SEXP make_error_condition_string(SEXP call, SEXP rho,
                                 const char *Class,
                                 int nextra, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    SEXP value = vmake_error_condition_string(call, rho, Class, nextra, format, ap);
    va_end(ap);
    return value;
}


SEXP simpleError(SEXP call, SEXP rho, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    SEXP value = vmake_error_condition_string(call, rho, "simpleError", 0, format, ap);
    va_end(ap);
    return value;
}


SEXP ThisPathInAQUAError(SEXP call, SEXP rho)
{
    return make_error_condition(
        call, rho, ThisPathInAQUAErrorClass, 0,
        "R is running from AQUA for which 'this.path' is currently unimplemented\n"
        " consider using RStudio, Positron, VSCode, or Emacs until such a time when this is implemented"
    );
}


SEXP ThisPathInZipFileError(SEXP call, SEXP rho, SEXP description)
{
    SEXP cond = make_error_condition(
        call, rho, ThisPathInZipFileErrorClass, 1,
        "'this.path' cannot be used within a zip file"
    );
    Rf_protect(cond);
    SEXP names = Rf_getAttrib(cond, R_NamesSymbol);
    Rf_protect(names);
    SET_STRING_ELT(names, 2, Rf_mkChar("description"));
    SET_VECTOR_ELT(cond , 2, Rf_ScalarString(description));
    Rf_unprotect(2);
    return cond;
}


SEXP ThisPathNotExistsError(SEXP call, SEXP rho, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    SEXP value = vmake_error_condition(call, rho, ThisPathNotExistsErrorClass, 0, format, ap);
    va_end(ap);
    return value;
}


SEXP ThisPathNotFoundError(SEXP call, SEXP rho, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    SEXP value = vmake_error_condition(call, rho, ThisPathNotFoundErrorClass, 0, format, ap);
    va_end(ap);
    return value;
}


SEXP ThisPathNotImplementedError(SEXP call, SEXP rho, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    SEXP value = vmake_error_condition(call, rho, ThisPathNotImplementedErrorClass, 0, format, ap);
    va_end(ap);
    return value;
}


#define funbody(connection_class_as_CHARSXP, summConn)         \
    SEXP cond = make_error_condition(                          \
        call, rho, ThisPathUnrecognizedConnectionClassErrorClass, 1,\
        "'this.path' not implemented when source()-ing a connection of class '%s'",\
        EncodeChar((connection_class_as_CHARSXP))              \
    );                                                         \
    Rf_protect(cond); nprotect++;                              \
    SEXP names = Rf_getAttrib(cond, R_NamesSymbol);            \
    Rf_protect(names); nprotect++;                             \
    SET_STRING_ELT(names, 2, Rf_mkChar("summary"));            \
    SET_VECTOR_ELT(cond , 2, (summConn));                      \
    Rf_unprotect(nprotect);                                    \
    return cond
SEXP ThisPathUnrecognizedConnectionClassError(SEXP call, SEXP rho, SEXP summary)
{
    int nprotect = 0;
    funbody(STRING_ELT(VECTOR_ELT(summary, 1), 0), summary);
}
#if defined(R_CONNECTIONS_VERSION_1)
SEXP ThisPathUnrecognizedConnectionClassError_Rcon_V1(SEXP call, SEXP rho, Rconnection Rcon)
{
    int nprotect = 0;
    SEXP charsxp = Rf_mkChar(Rcon->class);
    Rf_protect(charsxp); nprotect++;
    SEXP summary = summary_connection_Rcon_V1(Rcon);
    Rf_protect(summary); nprotect++;
    funbody(charsxp, summary);
}
#endif
#undef funbody


SEXP ThisPathUnrecognizedMannerError(SEXP call, SEXP rho)
{
    return make_error_condition(
        call, rho, ThisPathUnrecognizedMannerErrorClass, 0,
        "R is running in an unrecognized manner"
    );
}


void stop(SEXP cond)
{
    SEXP expr = Rf_lcons(stopSymbol, Rf_cons(cond, R_NilValue));
    Rf_protect(expr);
    Rf_eval(expr, R_BaseEnv);
    Rf_unprotect(1);
    return;
}


#if R_version_less_than(4,5,0)
void MissingArgError_c(const char *arg, SEXP call, SEXP rho, const char *subclass)
{
    if (call == R_CurrentExpression) {
        if (*arg)
            Rf_error(_("argument \"%s\" is missing, with no default"), arg);
        else
            Rf_error(_("argument is missing, with no default"));
    }
    else {
        SEXP cond;
        if (*arg) {
            cond = simpleError(call, rho,
                               _("argument \"%s\" is missing, with no default"), arg);
        }
        else
            cond = simpleError(call, rho,
                               _("argument is missing, with no default"));
        Rf_protect(cond);
        stop(cond);
        Rf_unprotect(1);
    }
}
#else
void MissingArgError_c(const char *arg, SEXP call, SEXP rho, const char *subclass)
{
    Rf_protect(call);
    const char *Class[3];
    if (subclass == NULL) {
        Class[0] = "missingArgError";
        Class[1] = NULL;
    }
    else {
        Class[0] = subclass;
        Class[1] = "missingArgError";
        Class[2] = NULL;
    }
    SEXP cond;
    if (*arg)
        cond = make_error_condition_strings(
            call, rho, Class, 0,
            _("argument \"%s\" is missing, with no default"), arg
        );
    else
        cond = make_error_condition_strings(
            call, rho, Class, 0,
            _("argument is missing, with no default")
        );
    Rf_protect(cond);
    stop(cond);
    Rf_unprotect(2);
}
#endif


void MissingArgError(SEXP symbol, SEXP call, SEXP rho, const char *subclass)
{
    MissingArgError_c(R_CHAR(PRINTNAME(symbol)), call, rho, subclass);
}





SEXP do_ThisPathInAQUAError do_formals
{
    do_start_no_call_op("ThisPathInAQUAError", 1);
    return ThisPathInAQUAError(Rf_lazy_duplicate(CAR(args)), rho);
}


SEXP do_ThisPathInZipFileError do_formals
{
    do_start_no_op("ThisPathInZipFileError", 2);
    SEXP call2 = Rf_lazy_duplicate(CAR(args)); args = CDR(args);
    if (!IS_SCALAR(CAR(args), STRSXP) ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        Rf_errorcall(call, _("invalid first argument"));
    }
    SEXP description = STRING_ELT(CAR(args), 0);
    return ThisPathInZipFileError(call2, rho, description);
}


SEXP do_ThisPathNotExistsError do_formals
{
    do_start_no_op("ThisPathNotExistsError", 2);
    if (!IS_SCALAR(CAR(args), STRSXP) ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        Rf_errorcall(call, _("invalid first argument"));
    }
    const char *msg = Rf_translateChar(STRING_ELT(CAR(args), 0));
    return ThisPathNotExistsError(
        Rf_lazy_duplicate(CADR(args)), rho,
        "%s", msg
    );
}


SEXP do_ThisPathNotFoundError do_formals
{
    do_start_no_op("ThisPathNotFoundError", 2);
    if (!IS_SCALAR(CAR(args), STRSXP) ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        Rf_errorcall(call, _("invalid first argument"));
    }
    const char *msg = Rf_translateChar(STRING_ELT(CAR(args), 0));
    return ThisPathNotFoundError(
        Rf_lazy_duplicate(CADR(args)), rho,
        "%s", msg
    );
}


SEXP do_ThisPathNotImplementedError do_formals
{
    do_start_no_op("ThisPathNotImplementedError", 2);
    if (!IS_SCALAR(CAR(args), STRSXP) ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        Rf_errorcall(call, _("invalid first argument"));
    }
    const char *msg = Rf_translateChar(STRING_ELT(CAR(args), 0));
    return ThisPathNotImplementedError(
        Rf_lazy_duplicate(CADR(args)), rho,
        "%s", msg
    );
}


SEXP do_ThisPathUnrecognizedConnectionClassError do_formals
{
    do_start_no_call_op("ThisPathUnrecognizedConnectionClassError", 2);
#if defined(R_CONNECTIONS_VERSION_1)
    if (ptr_R_GetConnection)
        return ThisPathUnrecognizedConnectionClassError_Rcon_V1(
            Rf_lazy_duplicate(CAR(args)), rho,
            ptr_R_GetConnection(CADR(args))
        );
#endif
    SEXP summary = summary_connection(CADR(args));
    Rf_protect(summary);
    SEXP value = ThisPathUnrecognizedConnectionClassError(
        Rf_lazy_duplicate(CAR(args)), rho,
        summary
    );
    Rf_unprotect(1);
    return value;
}


SEXP do_ThisPathUnrecognizedMannerError do_formals
{
    do_start_no_call_op("ThisPathUnrecognizedMannerError", 1);
    return ThisPathUnrecognizedMannerError(Rf_lazy_duplicate(CAR(args)), rho);
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
