#include "thispathdefn.h"


#if R_version_less_than(3, 0, 0)
#define XLENGTH LENGTH
#define xlength length
#define R_xlen_t R_len_t
#endif


R_xlen_t dispatch_xlength(SEXP x, SEXP rho)
{
    static SEXP R_LengthSymbol = NULL;
    /*
     * if the object has a class attribute, call length(x)
     * we must call it in the user's environment in case they defined any
     * length methods in said environment
     */
    if (isObject(x)) {
        if (R_LengthSymbol == NULL)
            R_LengthSymbol = install("length");
        SEXP expr = allocList(2), expr2;
        PROTECT(expr);
        SET_TYPEOF(expr, LANGSXP);
        SETCAR(expr, findVarInFrame(R_BaseEnv, R_LengthSymbol));
        switch (TYPEOF(x)) {
            case BCODESXP:
            case SYMSXP:
            case PROMSXP:
            case LANGSXP:
            case DOTSXP:
                SETCADR(expr, expr2 = allocList(2));
                SET_TYPEOF(expr2, LANGSXP);
                SETCAR(expr2, findVarInFrame(R_BaseEnv, R_QuoteSymbol));
                SETCADR(expr2, x);
                break;
            default:
                SETCADR(expr, x);
        }
        expr = PROTECT(eval(expr, rho));
        R_xlen_t value = (R_xlen_t)
            (TYPEOF(expr) == REALSXP ? REAL(expr)[0] : asInteger(expr));
        UNPROTECT(2);
        return value;
    }
    else return xlength(x);  /* otherwise, return the internal length */
}


SEXP dispatch_subset2(SEXP x, R_xlen_t i, SEXP rho)
{
    if (isObject(x)) {
        SEXP expr = allocList(3), expr2;
        PROTECT(expr);
        SET_TYPEOF(expr, LANGSXP);
        SETCAR(expr, findVarInFrame(R_BaseEnv, R_Bracket2Symbol));
        switch (TYPEOF(x)) {
            case BCODESXP:
            case SYMSXP:
            case PROMSXP:
            case LANGSXP:
            case DOTSXP:
                SETCADR(expr, expr2 = allocList(2));
                SET_TYPEOF(expr2, LANGSXP);
                SETCAR(expr2, findVarInFrame(R_BaseEnv, R_QuoteSymbol));
                SETCADR(expr2, x);
                break;
            default:
                SETCADR(expr, x);
        }
        SETCADDR(expr, ScalarReal(((double) i) + 1));
        expr = eval(expr, rho);
        UNPROTECT(1);
        return expr;
    }
    else return VECTOR_ELT(x, i);
}


R_xlen_t getElementLength(SEXP x, R_xlen_t i, SEXP rho)
{
    SEXP x0 = dispatch_subset2(x, i, rho);
    PROTECT(x0);
    R_xlen_t value = dispatch_xlength(x0, rho);
    UNPROTECT(1);
    return value;
}


SEXP lengths_real(SEXP x, R_xlen_t len, SEXP rho)
{
    SEXP value;
    R_xlen_t i;
    double *rvalue;


    value = allocVector(REALSXP, len);
    PROTECT(value);
    for (i = 0, rvalue = REAL(value); i < len; i++, rvalue++) {
        *rvalue = (double) getElementLength(x, i, rho);
    }
    UNPROTECT(1);
    return value;
}


#if R_version_less_than(3, 3, 0)


SEXP do_strrep do_formals
{
    do_start("strrep", 2);


    SEXP x = CAR(args); args = CDR(args);
    SEXP times = CAR(args);


    R_xlen_t x_length     = XLENGTH(x    ),
             times_length = XLENGTH(times);
    if (x_length == 0 || times_length == 0)
        return allocVector(STRSXP, 0);


    R_xlen_t len = (x_length > times_length) ? x_length : times_length;


    SEXP value = allocVector(STRSXP, len);
    PROTECT(value);


    R_xlen_t x_indx     = 0,
             times_indx = 0;
    for (R_xlen_t i = 0; i < len; i++) {
        SEXP x0 = STRING_ELT(x, x_indx);
        int times0 = INTEGER(times)[times_indx];
        if (x0 == NA_STRING || times0 == NA_INTEGER) {
            SET_STRING_ELT(value, i, NA_STRING);
        } else {
            if (times0 < 0)
                error(_("invalid '%s' value"), "times");
            const char *str = CHAR(x0);
            int nchar = (int) strlen(str);


            double nchar_out = ((double) nchar) * times0;
            if (nchar_out > INT_MAX)
                error("R character strings are limited to 2^31-1 bytes");


            char _buf[nchar * times0 + 1];
            char *buf = _buf;
            const char *cbuf = _buf;
            for (int j = 0; j < times0; j++) {
                strcpy(buf, str);
                buf += nchar;
            }
            buf[0] = '\0';
            SET_STRING_ELT(value, i, mkCharCE(cbuf, getCharCE(x0)));
        }
        if (++x_indx     == x_length    ) x_indx     = 0;
        if (++times_indx == times_length) times_indx = 0;
    }


    if (x_length == len) {
        SEXP names = getAttrib(x, R_NamesSymbol);
        if (names != R_NilValue)
            setAttrib(value, R_NamesSymbol, names);
    }


    UNPROTECT(1);
    return value;
}


#define do_startsWith_body(startsWith)                         \
    SEXP x = CAR(args); args = CDR(args);                      \
    SEXP xxxfix = CAR(args);                                   \
    if (!isString(x) || !isString(xxxfix))                     \
        error(_("non-character object(s)"));                   \
                                                               \
                                                               \
    R_xlen_t x_length      = XLENGTH(x     ),                  \
             xxxfix_length = XLENGTH(xxxfix);                  \
    if (x_length == 0 || xxxfix_length == 0)                   \
        return allocVector(LGLSXP, 0);                         \
                                                               \
                                                               \
    R_xlen_t len = (x_length > xxxfix_length) ? x_length : xxxfix_length;\
                                                               \
                                                               \
    SEXP value = allocVector(LGLSXP, len);                     \
    PROTECT(value);                                            \
    int *lvalue = LOGICAL(value);                              \
                                                               \
                                                               \
    if (xxxfix_length == 1) {                                  \
        SEXP xxxfix0 = STRING_ELT(xxxfix, 0);                  \
        if (xxxfix0 == NA_STRING) {                            \
            for (R_xlen_t i = 0; i < len; i++)                 \
                lvalue[i] = NA_LOGICAL;                        \
        } else {                                               \
            const char *xxxfix0_str = translateCharUTF8(xxxfix0);\
            int xxxfix0_nchar = (int) strlen(xxxfix0_str);     \
            for (R_xlen_t i = 0; i < len; i++) {               \
                SEXP x0 = STRING_ELT(x, i);                    \
                if (x0 == NA_STRING) {                         \
                    lvalue[i] = NA_LOGICAL;                    \
                } else {                                       \
                    const char *x0_str = translateCharUTF8(x0);\
                    if ((startsWith)) {                        \
                        lvalue[i] = strncmp(x0_str, xxxfix0_str, xxxfix0_nchar) == 0;\
                    } else {                                   \
                        int shift = ((int) strlen(x0_str)) - xxxfix0_nchar;\
                        if (shift < 0)                         \
                            lvalue[i] = FALSE;                 \
                        else                                   \
                            lvalue[i] = memcmp(x0_str + shift, xxxfix0_str, xxxfix0_nchar) == 0;\
                    }                                          \
                }                                              \
            }                                                  \
        }                                                      \
    } else {                                                   \
        const char **x_str      = (const char **) R_alloc(x_length     , sizeof(char *));\
        const char **xxxfix_str = (const char **) R_alloc(xxxfix_length, sizeof(char *));\
        int *x_nchar      = (int *) R_alloc(x_length     , sizeof(int));\
        int *xxxfix_nchar = (int *) R_alloc(xxxfix_length, sizeof(int));\
        for (R_xlen_t i = 0; i < x_length; i++) {              \
            SEXP x0 = STRING_ELT(x, i);                        \
            if (x0 == NA_STRING)                               \
                x_nchar[i] = -1;                               \
            else {                                             \
                x_str[i] = translateCharUTF8(x0);              \
                x_nchar[i] = (int) strlen(x_str[i]);           \
            }                                                  \
        }                                                      \
        for (R_xlen_t i = 0; i < xxxfix_length; i++) {         \
            SEXP x0 = STRING_ELT(x, i);                        \
            if (x0 == NA_STRING)                               \
                xxxfix_nchar[i] = -1;                          \
            else {                                             \
                xxxfix_str[i] = translateCharUTF8(x0);         \
                xxxfix_nchar[i] = (int) strlen(xxxfix_str[i]); \
            }                                                  \
        }                                                      \
        R_xlen_t x_indx      = 0,                              \
                 xxxfix_indx = 0;                              \
        if ((startsWith)) {                                    \
            for (R_xlen_t i = 0; i < len; i++) {               \
                if (x_nchar[x_indx] < 0 || xxxfix_nchar[xxxfix_indx] < 0)\
                    lvalue[i] = NA_LOGICAL;                    \
                else if (x_nchar[x_indx] < xxxfix_nchar[xxxfix_indx])\
                    lvalue[i] = FALSE;                         \
                else                                           \
                    lvalue[i] = memcmp(x_str[x_indx], xxxfix_str[xxxfix_indx], xxxfix_nchar[xxxfix_indx]) == 0;\
                if (++x_indx      == x_length     ) x_indx      = 0;\
                if (++xxxfix_indx == xxxfix_length) xxxfix_indx = 0;\
            }                                                  \
        } else {                                               \
            for (R_xlen_t i = 0; i < len; i++) {               \
                if (x_nchar[x_indx] < 0 || xxxfix_nchar[xxxfix_indx] < 0)\
                    lvalue[i] = NA_LOGICAL;                    \
                else {                                         \
                    int shift = x_nchar[x_indx] - xxxfix_nchar[xxxfix_indx];\
                    if (shift < 0)                             \
                        lvalue[i] = FALSE;                     \
                    else                                       \
                        lvalue[i] = memcmp(x_str[x_indx] + shift, xxxfix_str[xxxfix_indx], xxxfix_nchar[xxxfix_indx]) == 0;\
                }                                              \
                if (++x_indx      == x_length     ) x_indx      = 0;\
                if (++xxxfix_indx == xxxfix_length) xxxfix_indx = 0;\
            }                                                  \
        }                                                      \
    }                                                          \
                                                               \
                                                               \
    UNPROTECT(1);                                              \
    return value


SEXP do_startsWith do_formals
{
    do_start("startsWith", 2);


    do_startsWith_body(TRUE);
}


SEXP do_endsWith do_formals
{
    do_start("endsWith", 2);


    do_startsWith_body(FALSE);
}


#endif


#if R_version_less_than(3, 2, 0)


SEXP do_direxists do_formals
{
    do_start("direxists", 1);


    SEXP fn = CAR(args);
    if (!isString(fn))
        error(_("invalid filename argument"));
    int n = LENGTH(fn);


    static SEXP file_infoSymbol = NULL;
    if (file_infoSymbol == NULL)
        file_infoSymbol = install("file.info");


    SEXP expr = allocList(2);
    PROTECT(expr);
    SET_TYPEOF(expr, LANGSXP);
    SETCAR(expr, file_infoSymbol);
    SETCADR(expr, fn);
    SEXP value = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    PROTECT(value);


    if (TYPEOF(value) != VECSXP)
        error(_("invalid '%s' value"), "file.info(paths)");
    if (LENGTH(value) < 6)
        error(_("invalid '%s' value"), "file.info(paths)");
    value = VECTOR_ELT(value, 1);
    if (TYPEOF(value) != LGLSXP)
        error(_("invalid '%s' value"), "file.info(paths)$isdir");
    if (LENGTH(value) != n)
        error(_("invalid '%s' value"), "file.info(paths)$isdir");


    int *lvalue = LOGICAL(value);
    for (int i = 0; i < n; i++) {
        if (lvalue[i] == NA_LOGICAL) {
            lvalue[i] = FALSE;
        }
    }


    UNPROTECT(1);
    return value;
}


SEXP do_lengths do_formals
{
    do_start("lengths", 2);


    SEXP x = CAR(args), value;
    R_xlen_t len, i;
    int *ivalue;
    int useNames = asLogical(CADR(args));
    if (useNames == NA_LOGICAL)
        error(_("invalid '%s' value"), "use.names");


    Rboolean isList = isVectorList(x) || isS4(x);
    if (!isList) switch (TYPEOF(x)) {
        case NILSXP:
        case CHARSXP:
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
        case RAWSXP:
            break;
        default:
            error(_("'%s' must be a list or atomic vector"), "x");
    }


    len = dispatch_xlength(x, rho);
    value = allocVector(INTSXP, len);
    PROTECT(value);


    if (isList) {
        for (i = 0, ivalue = INTEGER(value); i < len; i++, ivalue++) {
            R_xlen_t x0_len = getElementLength(x, i, rho);
#ifdef LONG_VECTOR_SUPPORT
            if (x0_len > INT_MAX) {
                value = lengths_real(x, len, rho);
                UNPROTECT(1);
                PROTECT(value);
                break;
            }
#endif
            *ivalue = (int) x0_len;
        }
    } else {
        for (i = 0, ivalue = INTEGER(value); i < len; i++, ivalue++)
            *ivalue = 1;
    }


    SEXP dim = getAttrib(x, R_DimSymbol);
    if (!isNull(dim)) setAttrib(value, R_DimSymbol, dim);
    if (useNames) {
        SEXP names = getAttrib(x, R_NamesSymbol);
        if (!isNull(names)) setAttrib(value, R_NamesSymbol, names);
        SEXP dimnames = getAttrib(x, R_DimNamesSymbol);
        if (!isNull(dimnames)) setAttrib(value, R_DimNamesSymbol, dimnames);
    }


    UNPROTECT(1);
    return value;
}


#endif


#if R_version_less_than(3, 1, 0)


Rboolean anyNA(SEXP x, Rboolean recursive, SEXP rho)
{
    static SEXP is_naSymbol = NULL,
                anySymbol   = NULL;


    SEXPTYPE type = TYPEOF(x);
    Rboolean isList = (type == VECSXP || type == LISTSXP);


    if (OBJECT(x) || (isList && !recursive)) {
        if (is_naSymbol == NULL) {
            is_naSymbol = install("is.na");
            anySymbol   = install("any");
        }
        SEXP expr = allocList(2), expr2, expr3;
        PROTECT(expr);
        SET_TYPEOF(expr, LANGSXP);
        SETCAR(expr, anySymbol);
        SETCADR(expr, expr2 = allocList(2));
        SET_TYPEOF(expr2, LANGSXP);
        SETCAR(expr2, is_naSymbol);
        switch (type) {
            case BCODESXP:
            case SYMSXP:
            case PROMSXP:
            case LANGSXP:
            case DOTSXP:
                SETCADR(expr2, expr3 = allocList(2));
                SET_TYPEOF(expr3, LANGSXP);
                SETCAR(expr3, R_QuoteSymbol);
                SETCADR(expr3, x);
                break;
            default:
                SETCADR(expr2, x);
        }
        SEXP res = PROTECT(eval(expr, rho));
        Rboolean value = (asLogical(res) == TRUE);
        UNPROTECT(2);
        return value;
    }


    R_xlen_t i, n = xlength(x);
    switch (type) {
    case REALSXP:
    {
        double *rx = REAL(x);
        for (i = 0; i < n; i++) {
            if (ISNAN(rx[i]))
                return TRUE;
        }
        break;
    }
    case INTSXP:
    {
        int *ix = INTEGER(x);
        for (i = 0; i < n; i++) {
            if (ix[i] == NA_INTEGER)
                return TRUE;
        }
        break;
    }
    case LGLSXP:
    {
        int *lx = LOGICAL(x);
        for (i = 0; i < n; i++) {
            if (lx[i] == NA_LOGICAL)
                return TRUE;
        }
        break;
    }
    case CPLXSXP:
    {
        Rcomplex *cx = COMPLEX(x);
        for (i = 0; i < n; i++) {
            if (ISNAN(cx[i].r) || ISNAN(cx[i].i))
                return TRUE;
        }
        break;
    }
    case STRSXP:
    {
        for (i = 0; i < n; i++) {
            if (STRING_ELT(x, i) == NA_STRING)
                return TRUE;
        }
        break;
    }
    case RAWSXP:
        return FALSE;
    case NILSXP:
        return FALSE;
    case VECSXP:
    {
        for (i = 0; i < n; i++) {
            if (anyNA(VECTOR_ELT(x, i), recursive, rho))
                return TRUE;
        }
        break;
    }
    case LISTSXP:
    {
        for (i = 0; i < n; i++, x = CDR(x)) {
            if (anyNA(CAR(x), recursive, rho))
                return TRUE;
        }
        break;
    }
    default:
        error("anyNA() applied to non-(list or vector) of type '%s'", type2char(type));
    }
    return FALSE;
}


SEXP do_anyNA do_formals
{
    do_start("anyNA", -1);


    SEXP x;
    Rboolean recursive = FALSE;

    switch (length(args)) {
    case 2:
        recursive = asLogical(CADR(args));
    case 1:
        x = CAR(args);
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), "C_anyNA", "1 or 2"));
    }


    return ScalarLogical(anyNA(x, recursive, rho));
}


#endif
