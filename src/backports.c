#include "thispathdefn.h"


R_xlen_t dispatch_xlength(SEXP x, SEXP rho)
{
    /*
     * if the object has a class attribute, call length(x)
     * we must call it in the user's environment in case they defined any
     * length methods in said environment
     */
    if (Rf_isObject(x)) {
        SEXP expr;
        PROTECT_INDEX indx;
        R_ProtectWithIndex(expr = Rf_cons(x, R_NilValue), &indx);
        if (needQuote(x)) {
            R_Reprotect(expr = Rf_lcons(getFromBase(R_QuoteSymbol), expr), indx);
            R_Reprotect(expr = Rf_cons(expr, R_NilValue), indx);
        }
        R_Reprotect(expr = Rf_lcons(getFromBase(R_LengthSymbol), expr), indx);
        SEXP res = Rf_protect(Rf_eval(expr, rho));
        R_xlen_t value = (R_xlen_t)
            ((TYPEOF(res) == REALSXP) ? REAL(res)[0] : Rf_asInteger(res));
        Rf_unprotect(2);
        return value;
    }
    else return Rf_xlength(x);  /* otherwise, return the internal length */
}


SEXP dispatch_subset2(SEXP x, R_xlen_t i, SEXP rho)
{
    if (Rf_isObject(x)) {
        SEXP expr = Rf_allocList(3);
        Rf_protect(expr);
        SET_TYPEOF(expr, LANGSXP);
        SETCAR(expr, getFromBase(R_Bracket2Symbol));
        if (needQuote(x)) {
            SEXP expr2 = SETCADR(expr, Rf_allocList(2));
            SET_TYPEOF(expr2, LANGSXP);
            SETCAR (expr2, getFromBase(R_QuoteSymbol));
            SETCADR(expr2, x);
        }
        else SETCADR(expr, x);
        SETCADDR(expr, Rf_ScalarReal(((double) i) + 1));
        expr = Rf_eval(expr, rho);
        Rf_unprotect(1);
        return expr;
    }
    else return VECTOR_ELT(x, i);
}


R_xlen_t dispatch_subset2_xlength(SEXP x, R_xlen_t i, SEXP rho)
{
    SEXP x0 = dispatch_subset2(x, i, rho);
    Rf_protect(x0);
    R_xlen_t value = dispatch_xlength(x0, rho);
    Rf_unprotect(1);
    return value;
}


#if R_version_less_than(3,1,0)


SEXP Rf_lazy_duplicate(SEXP s)
{
    switch (TYPEOF(s)) {
    case NILSXP:
    case SYMSXP:
    case ENVSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case EXTPTRSXP:
    case BCODESXP:
    case WEAKREFSXP:
    case CHARSXP:
    case PROMSXP:
        break;
    case CLOSXP:
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    case EXPRSXP:
    case VECSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case RAWSXP:
    case STRSXP:
    case S4SXP:
        ENSURE_NAMEDMAX(s);
        break;
    default:
        UNIMPLEMENTED_TYPE("lazy_duplicate", s);
    }
    return s;
}


SEXP Rf_shallow_duplicate(SEXP s)
{
    return Rf_duplicate(s);
}


int IS_SCALAR(SEXP x, int type)
{
    return TYPEOF(x) == type && Rf_xlength(x) == 1;
}


Rboolean anyNA_default(SEXP x, Rboolean recursive, SEXP rho);


Rboolean anyNA(SEXP x, Rboolean recursive, SEXP rho)
{
    if (OBJECT(x)) {
        SEXP expr;
        PROTECT_INDEX indx;
        if (needQuote(x)) {
            R_ProtectWithIndex(expr = Rf_cons(x, R_NilValue), &indx);
            R_Reprotect(expr = Rf_lcons(getFromBase(R_QuoteSymbol), expr), indx);
            R_Reprotect(expr = Rf_cons(expr, Rf_cons((recursive == TRUE) ? R_TrueValue : R_FalseValue, R_NilValue)), indx);
        } else {
            R_ProtectWithIndex(expr = Rf_cons(x, Rf_cons((recursive == TRUE) ? R_TrueValue : R_FalseValue, R_NilValue)), &indx);
        }
        R_Reprotect(expr = Rf_lcons(getFromMyNS(_anyNA_dispatchSymbol), expr), indx);
        SEXP env = Rf_eval(expr_parent_frame, rho);
        Rf_protect(env);
        SEXP res = Rf_protect(Rf_eval(expr, env));
        Rboolean value = (Rf_asLogical(res) == TRUE);
        Rf_unprotect(3);
        return value;
    }
    else return anyNA_default(x, recursive, rho);
}


Rboolean anyNA_default(SEXP x, Rboolean recursive, SEXP rho)
{
    SEXPTYPE type = TYPEOF(x);
    if (OBJECT(x) || (!recursive && (type == VECSXP || type == LISTSXP))) {
        SEXP expr;
        PROTECT_INDEX indx;
        R_ProtectWithIndex(expr = Rf_cons(x, R_NilValue), &indx);
        if (needQuote(x)) {
            R_Reprotect(expr = Rf_lcons(getFromBase(R_QuoteSymbol), expr), indx);
            R_Reprotect(expr = Rf_cons(expr, R_NilValue), indx);
        }
        R_Reprotect(expr = Rf_lcons(getFromBase(is_naSymbol), expr), indx);
        R_Reprotect(expr = Rf_cons(expr, R_NilValue), indx);
        R_Reprotect(expr = Rf_lcons(getFromBase(anySymbol), expr), indx);
        SEXP env = Rf_eval(expr_parent_frame, rho);
        Rf_protect(env);
        SEXP res = Rf_protect(Rf_eval(expr, env));
        Rboolean value = (Rf_asLogical(res) == TRUE);
        Rf_unprotect(3);
        return value;
    }


    R_xlen_t i, n = Rf_xlength(x);
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
        Rf_error("anyNA() applied to non-(list or vector) of type '%s'", Rf_type2char(type));
    }
    return FALSE;
}


SEXP do_anyNA do_formals
{
    do_start_no_call_op("anyNA", 2);
    return Rf_ScalarLogical(anyNA(CAR(args), Rf_asLogical(CADR(args)), rho));
}


SEXP do_anyNA_data_frame do_formals
{
    do_start_no_call_op("anyNA.data.frame", 2);


    SEXP x = CAR(args);
    Rboolean recursive = Rf_asLogical(CADR(args));
    for (R_xlen_t i = 0, n = Rf_xlength(x); i < n; i++) {
        if (anyNA(VECTOR_ELT(x, i), recursive, rho))
            return R_TrueValue;
    }
    return R_FalseValue;
}


SEXP do_anyNA_numeric_version do_formals
{
    do_start_no_call_op("anyNA.numeric_version", 1);


    SEXP x = CAR(args);
    for (R_xlen_t i = 0, n = Rf_xlength(x); i < n; i++) {
        if (dispatch_xlength(VECTOR_ELT(x, i), rho) <= 0)
            return R_TrueValue;
    }
    return R_FalseValue;
}


SEXP do_anyNA_default do_formals
{
    do_start_no_call_op("anyNA.default", 2);
    return Rf_ScalarLogical(anyNA_default(CAR(args), Rf_asLogical(CADR(args)), rho));
}


#endif


#if R_version_less_than(3,2,0)


SEXP R_lsInternal3(SEXP env, Rboolean all, Rboolean sorted)
{
    SEXP expr;
    PROTECT_INDEX indx;
    R_ProtectWithIndex(expr = Rf_cons(sorted ? R_TrueValue : R_FalseValue, R_NilValue), &indx);
    SET_TAG(expr, sortedSymbol);
    R_Reprotect(expr = Rf_cons(all ? R_TrueValue : R_FalseValue, expr), indx);
    SET_TAG(expr, all_namesSymbol);
    R_Reprotect(expr = Rf_cons(env, expr), indx);
    SET_TAG(expr, envirSymbol);
    R_Reprotect(expr = Rf_lcons(getFromBase(lsSymbol), expr), indx);


    SEXP value = Rf_eval(expr, R_EmptyEnv);
    Rf_unprotect(1);
    return value;
}


SEXP Rf_topenv(SEXP target, SEXP envir)
{
    SEXP expr = Rf_lcons(topenvSymbol, Rf_cons(envir, Rf_cons(target, R_NilValue)));
    Rf_protect(expr);
    SEXP value = Rf_eval(expr, R_BaseEnv);
    Rf_unprotect(1);
    return value;
}


SEXP do_dir_exists do_formals
{
    do_start_no_call_op_rho("dir.exists", 1);


    SEXP fn = CAR(args);
    if (!Rf_isString(fn))
        Rf_error(_("invalid filename argument"));
    int n = LENGTH(fn);


    SEXP expr = Rf_lcons(file_infoSymbol, Rf_cons(fn, R_NilValue));
    Rf_protect(expr);
    SEXP value = Rf_eval(expr, R_BaseEnv);
    Rf_unprotect(1);  /* expr */
    Rf_protect(value);


    if (TYPEOF(value) != VECSXP)
        Rf_error(_("invalid '%s' value"), "file.info(paths)");
    if (LENGTH(value) < 6)
        Rf_error(_("invalid '%s' value"), "file.info(paths)");
    value = VECTOR_ELT(value, 1);
    if (TYPEOF(value) != LGLSXP)
        Rf_error(_("invalid '%s' value"), "file.info(paths)$isdir");
    if (LENGTH(value) != n)
        Rf_error(_("invalid '%s' value"), "file.info(paths)$isdir");


    int *lvalue = LOGICAL(value);
    for (int i = 0; i < n; i++) {
        if (lvalue[i] == NA_LOGICAL) {
            lvalue[i] = FALSE;
        }
    }


    Rf_unprotect(1);
    return value;
}


static R_INLINE
SEXP lengths_default(SEXP args, SEXP rho)
{
    SEXP x = CAR(args), value;
    R_xlen_t len, i;
    int *ivalue;
    int useNames = Rf_asLogical(CADR(args));
    if (useNames == NA_LOGICAL)
        Rf_error(_("invalid '%s' value"), "use.names");


    Rboolean isList = Rf_isVectorList(x) || Rf_isS4(x);
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
            Rf_error(_("'%s' must be a list or atomic vector"), "x");
    }


    len = dispatch_xlength(x, rho);
    value = Rf_allocVector(INTSXP, len);
    Rf_protect(value);


    if (isList) {
        for (i = 0, ivalue = INTEGER(value); i < len; i++, ivalue++) {
            R_xlen_t x0_len = dispatch_subset2_xlength(x, i, rho);
#if defined(LONG_VECTOR_SUPPORT)
            if (x0_len > INT_MAX) {
                SEXP oldvalue = value;
                R_xlen_t oldi = i;
                value = Rf_allocVector(REALSXP, len);
                Rf_protect(value);
                double *rvalue;
                /* copy old values to new vector */
                for (i = 0, ivalue = INTEGER(oldvalue), rvalue = REAL(value); i < oldi; i++, ivalue++, rvalue++)
                    *rvalue = (double) *ivalue;
                Rf_unprotect(2);  /* oldvalue and value */
                Rf_protect(value);
                /* place current value in new vector */
                *rvalue = (double) x0_len;
                i++, rvalue++;
                for (; i < len; i++, rvalue++)
                    *rvalue = (double) dispatch_subset2_xlength(x, i, rho);
                break;
            }
#endif
            *ivalue = (int) x0_len;
        }
    } else {
        for (i = 0, ivalue = INTEGER(value); i < len; i++, ivalue++)
            *ivalue = 1;
    }


    SEXP dim = Rf_getAttrib(x, R_DimSymbol);
    if (!Rf_isNull(dim)) Rf_setAttrib(value, R_DimSymbol, dim);
    if (useNames) {
        SEXP names = Rf_getAttrib(x, R_NamesSymbol);
        if (!Rf_isNull(names)) Rf_setAttrib(value, R_NamesSymbol, names);
        SEXP dimnames = Rf_getAttrib(x, R_DimNamesSymbol);
        if (!Rf_isNull(dimnames)) Rf_setAttrib(value, R_DimNamesSymbol, dimnames);
    }


    Rf_unprotect(1);
    return value;
}


SEXP do_lengths do_formals
{
    do_start_no_call_op("lengths", 2);


    SEXP x = CAR(args);
    int useNames = Rf_asLogical(CADR(args));
    if (useNames == NA_LOGICAL)
        Rf_error(_("invalid '%s' value"), "use.names");


    if (OBJECT(x)) return Rf_eval(expr_UseMethod_lengths, rho);
    else return lengths_default(args, rho);
}


SEXP do_lengths_default do_formals
{
    do_start_no_call_op("lengths.default", 2);
    return lengths_default(args, rho);
}


SEXP checkNSname(SEXP call, SEXP name)
{
    switch (TYPEOF(name)) {
    case SYMSXP:
        break;
    case STRSXP:
        if (LENGTH(name) >= 1) {
            name = Rf_installTrChar(STRING_ELT(name, 0));
            break;
        }
        /* else fall through */
    default:
        Rf_errorcall(call, _("bad namespace name"));
    }
    return name;
}


SEXP do_isRegisteredNamespace do_formals
{
    do_start_no_op_rho("isRegisteredNamespace", 1);


    SEXP name = checkNSname(call, Rf_protect(Rf_coerceVector(CAR(args), SYMSXP)));
    Rf_unprotect(1);
    SEXP val = Rf_findVarInFrame(R_NamespaceRegistry, name);
    return val == R_UnboundValue ? R_FalseValue : R_TrueValue;
}


#endif


#if R_version_less_than(3,3,0)


SEXP do_strrep do_formals
{
    do_start_no_call_op_rho("strrep", 2);


    SEXP x = CAR(args); args = CDR(args);
    SEXP times = CAR(args);


    R_xlen_t x_length     = XLENGTH(x    ),
             times_length = XLENGTH(times);
    if (x_length == 0 || times_length == 0)
        return Rf_allocVector(STRSXP, 0);


    R_xlen_t len = (x_length > times_length) ? x_length : times_length;


    SEXP value = Rf_allocVector(STRSXP, len);
    Rf_protect(value);


    R_xlen_t x_indx     = 0,
             times_indx = 0;
    for (R_xlen_t i = 0; i < len; i++) {
        SEXP x0 = STRING_ELT(x, x_indx);
        int times0 = INTEGER(times)[times_indx];
        if (x0 == NA_STRING || times0 == NA_INTEGER) {
            SET_STRING_ELT(value, i, NA_STRING);
        } else {
            if (times0 < 0)
                Rf_error(_("invalid '%s' value"), "times");
            const char *str = R_CHAR(x0);
            int nchar = (int) strlen(str);


            double nchar_out = ((double) nchar) * times0;
            if (nchar_out > INT_MAX)
                Rf_error("R character strings are limited to 2^31-1 bytes");


            char _buf[nchar * times0 + 1];
            char *buf = _buf;
            const char *cbuf = _buf;
            for (int j = 0; j < times0; j++) {
                strcpy(buf, str);
                buf += nchar;
            }
            buf[0] = '\0';
            SET_STRING_ELT(value, i, Rf_mkCharCE(cbuf, Rf_getCharCE(x0)));
        }
        if (++x_indx     == x_length    ) x_indx     = 0;
        if (++times_indx == times_length) times_indx = 0;
    }


    if (x_length == len) {
        SEXP names = Rf_getAttrib(x, R_NamesSymbol);
        if (names != R_NilValue)
            Rf_setAttrib(value, R_NamesSymbol, names);
    }


    Rf_unprotect(1);
    return value;
}


static R_INLINE
SEXP startsWith(SEXP args, int op)
{
    SEXP x = CAR(args); args = CDR(args);
    SEXP xxxfix = CAR(args);
    if (!Rf_isString(x) || !Rf_isString(xxxfix))
        Rf_error(_("non-character object(s)"));


    R_xlen_t x_length      = XLENGTH(x     ),
             xxxfix_length = XLENGTH(xxxfix);
    if (x_length == 0 || xxxfix_length == 0)
        return Rf_allocVector(LGLSXP, 0);


    R_xlen_t len = (x_length > xxxfix_length) ? x_length : xxxfix_length;


    SEXP value = Rf_allocVector(LGLSXP, len);
    Rf_protect(value);
    int *lvalue = LOGICAL(value);


    if (xxxfix_length == 1) {
        SEXP xxxfix0 = STRING_ELT(xxxfix, 0);
        if (xxxfix0 == NA_STRING) {
            for (R_xlen_t i = 0; i < len; i++)
                lvalue[i] = NA_LOGICAL;
        } else {
            const char *xxxfix0_str = Rf_translateCharUTF8(xxxfix0);
            int xxxfix0_nchar = (int) strlen(xxxfix0_str);
            for (R_xlen_t i = 0; i < len; i++) {
                SEXP x0 = STRING_ELT(x, i);
                if (x0 == NA_STRING) {
                    lvalue[i] = NA_LOGICAL;
                } else {
                    const char *x0_str = Rf_translateCharUTF8(x0);
                    if (op) {
                        lvalue[i] = strncmp(x0_str, xxxfix0_str, xxxfix0_nchar) == 0;
                    } else {
                        int shift = ((int) strlen(x0_str)) - xxxfix0_nchar;
                        if (shift < 0)
                            lvalue[i] = FALSE;
                        else
                            lvalue[i] = memcmp(x0_str + shift, xxxfix0_str, xxxfix0_nchar) == 0;
                    }
                }
            }
        }
    } else {
        const char **x_str      = (const char **) R_alloc(x_length     , sizeof(char *));
        const char **xxxfix_str = (const char **) R_alloc(xxxfix_length, sizeof(char *));
        int *x_nchar      = (int *) R_alloc(x_length     , sizeof(int));
        int *xxxfix_nchar = (int *) R_alloc(xxxfix_length, sizeof(int));
        for (R_xlen_t i = 0; i < x_length; i++) {
            SEXP x0 = STRING_ELT(x, i);
            if (x0 == NA_STRING)
                x_nchar[i] = -1;
            else {
                x_str[i] = Rf_translateCharUTF8(x0);
                x_nchar[i] = (int) strlen(x_str[i]);
            }
        }
        for (R_xlen_t i = 0; i < xxxfix_length; i++) {
            SEXP x0 = STRING_ELT(x, i);
            if (x0 == NA_STRING)
                xxxfix_nchar[i] = -1;
            else {
                xxxfix_str[i] = Rf_translateCharUTF8(x0);
                xxxfix_nchar[i] = (int) strlen(xxxfix_str[i]);
            }
        }
        R_xlen_t x_indx      = 0,
                 xxxfix_indx = 0;
        if (op) {
            for (R_xlen_t i = 0; i < len; i++) {
                if (x_nchar[x_indx] < 0 || xxxfix_nchar[xxxfix_indx] < 0)
                    lvalue[i] = NA_LOGICAL;
                else if (x_nchar[x_indx] < xxxfix_nchar[xxxfix_indx])
                    lvalue[i] = FALSE;
                else
                    lvalue[i] = memcmp(x_str[x_indx], xxxfix_str[xxxfix_indx], xxxfix_nchar[xxxfix_indx]) == 0;
                if (++x_indx      == x_length     ) x_indx      = 0;
                if (++xxxfix_indx == xxxfix_length) xxxfix_indx = 0;
            }
        } else {
            for (R_xlen_t i = 0; i < len; i++) {
                if (x_nchar[x_indx] < 0 || xxxfix_nchar[xxxfix_indx] < 0)
                    lvalue[i] = NA_LOGICAL;
                else {
                    int shift = x_nchar[x_indx] - xxxfix_nchar[xxxfix_indx];
                    if (shift < 0)
                        lvalue[i] = FALSE;
                    else
                        lvalue[i] = memcmp(x_str[x_indx] + shift, xxxfix_str[xxxfix_indx], xxxfix_nchar[xxxfix_indx]) == 0;
                }
                if (++x_indx      == x_length     ) x_indx      = 0;
                if (++xxxfix_indx == xxxfix_length) xxxfix_indx = 0;
            }
        }
    }


    Rf_unprotect(1);
    return value;
}


SEXP do_startsWith do_formals
{
    do_start_no_call_op_rho("startsWith", 2);
    return startsWith(args, TRUE);
}


SEXP do_endsWith do_formals
{
    do_start_no_call_op_rho("endsWith", 2);
    return startsWith(args, FALSE);
}


#endif


#if R_version_less_than(3,5,0)


#define length_DOTS(_v_) (TYPEOF(_v_) == DOTSXP ? Rf_length(_v_) : 0)


SEXP do_dotslength do_formals
{
    do_start_no_call_op("...length", 0);


    SEXP env = Rf_eval(expr_parent_frame, rho);
    SEXP vl = Rf_findVar(R_DotsSymbol, env);
    if (vl == R_UnboundValue)
        Rf_error(_("incorrect context: the current call has no '...' to look in"));
    return Rf_ScalarInteger(length_DOTS(vl));
}


SEXP ddfind(int i, SEXP rho)
{
    if (i <= 0)
        Rf_error(_("indexing '...' with non-positive index %d"), i);
    SEXP vl = Rf_findVar(R_DotsSymbol, rho);
    if (vl != R_UnboundValue) {
        if (length_DOTS(vl) >= i) {
            vl = Rf_nthcdr(vl, i - 1);
            return CAR(vl);
        }
        else
            Rf_error(_((i == 1) ? "the ... list contains fewer than %d element" :
                                  "the ... list contains fewer than %d elements"),
                     i);
    }
    else Rf_error(_("..%d used in an incorrect context, no ... to look in"), i);
    return R_NilValue;
}


#undef length_DOTS


#endif


#if R_version_less_than(3,6,0)


SEXP R_shallow_duplicate_attr(SEXP x) { return Rf_shallow_duplicate(x); }


SEXP Rf_installTrChar(SEXP x)
{
    return Rf_install(Rf_translateChar(x));
}


#endif


#if R_version_less_than(4,0,0)


void R_removeVarFromFrame(SEXP name, SEXP env)
{
    if (TYPEOF(env) == NILSXP)
        Rf_error(_("use of NULL environment is defunct"));

    if (!Rf_isEnvironment(env))
        Rf_error(_("argument to '%s' is not an environment"), "R_removeVarFromFrame");

    if (TYPEOF(name) != SYMSXP)
        Rf_error(_("not a symbol"));

    SEXP expr;
    PROTECT_INDEX indx;
    R_ProtectWithIndex(expr = Rf_cons(R_FalseValue, R_NilValue), &indx);
    SET_TAG(expr, inheritsSymbol);
    R_Reprotect(expr = Rf_cons(env, expr), indx);
    SET_TAG(expr, envirSymbol);
    R_Reprotect(expr = Rf_lcons(removeSymbol, Rf_cons(name, expr)), indx);
    Rf_eval(expr, R_BaseEnv);
    Rf_unprotect(1);
}


#endif


#if R_version_less_than(4,1,0)


// ...elt(n) was added in R 3.5.0
// but did not propagate visibility until R 4.1.0
SEXP do_dotselt do_formals
{
    do_start_no_op("...elt", 1);


    SEXP env = Rf_eval(expr_parent_frame, rho);
    SEXP si = CAR(args);
    if (!Rf_isNumeric(si) || XLENGTH(si) != 1)
        Rf_errorcall(call, _("indexing '...' with an invalid index"));
    int i = Rf_asInteger(si);
#if R_version_at_least(3,0,0)
    return Rf_eval(ddfind(i, env), env);
#else
    SEXP expr;
    PROTECT_INDEX indx;
    char buf[15];
    snprintf(buf, 15, "..%d", i);
    R_ProtectWithIndex(expr = Rf_cons(Rf_install(buf), R_NilValue), &indx);
    R_Reprotect(expr = Rf_lcons(getFromBase(withVisibleSymbol), expr), indx);
    SEXP value = Rf_eval(expr, env);
    Rf_protect(value);
    set_this_path_value(VECTOR_ELT(value, 0));
    set_this_path_visible(Rf_asLogical(VECTOR_ELT(value, 1)));
    Rf_unprotect(2);
    return R_NilValue;
#endif
}


SEXP R_NewEnv(SEXP enclos, int hash, int size)
{
    SEXP expr = Rf_lcons(
        new_envSymbol,
        Rf_cons(
            /* hash */ Rf_ScalarLogical(hash),
            Rf_cons(
                /* parent */ enclos,
                Rf_cons(
                    /* size */ Rf_ScalarInteger(size),
                    R_NilValue
                )
            )
        )
    );
    Rf_protect(expr);
    SEXP value = Rf_eval(expr, R_BaseEnv);
    Rf_unprotect(1);
    return value;
}


int IS_ASCII(SEXP x)
{
    for (const char *s = R_CHAR(x); *s; s++) {
        if (*s > 0x7f) {
            return FALSE;
        }
    }
    return TRUE;
}


#endif


#if R_version_less_than(4,2,0)


Rboolean R_existsVarInFrame(SEXP rho, SEXP symbol)
{
    SEXP expr;
    PROTECT_INDEX indx;
    /* exists(symbol, envir = rho, inherits = FALSE) */
    R_ProtectWithIndex(expr = Rf_cons(R_FalseValue, R_NilValue), &indx);
    SET_TAG(expr, inheritsSymbol);
    R_Reprotect(expr = Rf_cons(rho, expr), indx);
    SET_TAG(expr, envirSymbol);
    R_Reprotect(expr = Rf_cons(Rf_ScalarString(PRINTNAME(symbol)), expr), indx);
    R_Reprotect(expr = Rf_lcons(getFromBase(existsSymbol), expr), indx);
    SEXP value = Rf_protect(Rf_eval(expr, R_EmptyEnv));
    if (!ptr_IS_SCALAR(value, LGLSXP))
        Rf_error(_("invalid '%s' value"), "exists()");
    Rboolean lvalue = LOGICAL(value)[0];
    Rf_unprotect(2);
    return lvalue;
}


#endif
