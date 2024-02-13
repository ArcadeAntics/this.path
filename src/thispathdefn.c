#include "thispathdefn.h"


#if R_version_at_least(3, 0, 0)
R_xlen_t asXLength(SEXP x)
{
    const R_xlen_t na = -999;

    if (isVectorAtomic(x) && XLENGTH(x) >= 1) {
        switch (TYPEOF(x)) {
        case LGLSXP:
        case INTSXP:
        {
            int x0 = INTEGER(x)[0];
            if (x0 == NA_INTEGER)
                return na;
            else
                return (R_xlen_t) x0;
        }
        case REALSXP:
        {
            double x0 = REAL(x)[0];
            if (!R_FINITE(x0) || x0 > R_XLEN_T_MAX || x0 < 0)
                return na;
            else
                return (R_xlen_t) x0;
        }
        case CPLXSXP:
        case STRSXP:
            break;
        default:
            UNIMPLEMENTED_TYPE("asXLength", x);
        }
    } else if (TYPEOF(x) != CHARSXP)
        return na;

    double d = asReal(x);
    if (!R_FINITE(d) || d > R_XLEN_T_MAX || d < 0)
        return na;
    else
        return (R_xlen_t) d;
}
#else
R_xlen_t asXLength(SEXP x)
{
    return asInteger(x);
}
#endif


int ddVal(SEXP symbol)
{
    const char *buf;
    char *endp;
    int rval;

    buf = CHAR(PRINTNAME(symbol));
    if (!strncmp(buf, "..", 2) && strlen(buf) > 2) {
        buf += 2;
        rval = (int) strtol(buf, &endp, 10);
        return ((*endp != '\0') ? 0 : rval);
    }
    return 0;
}


SEXP ddfindVar(SEXP symbol, SEXP rho)
{
    int i = ddVal(symbol);
    return ddfind(i, rho);
}


Rboolean needQuote(SEXP x)
{
    switch (TYPEOF(x)) {
    case BCODESXP:
    case SYMSXP:
    case PROMSXP:
    case LANGSXP:
    case DOTSXP:
        return TRUE;
    }
    return FALSE;
}


void UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t)
{
    error(_("unimplemented type '%s' in '%s'\n"), type2char(t), s);
}


void UNIMPLEMENTED_TYPE(const char *s, SEXP x)
{
    UNIMPLEMENTED_TYPEt(s, TYPEOF(x));
}


const char *EncodeChar(SEXP x)
{
    /* accepts a CHARSXP and escapes the special / / non-printing characters */
    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = CONS(R_FalseValue, R_NilValue), &indx);
    SET_TAG(expr, na_encodeSymbol);
    REPROTECT(expr = LCONS(encodeStringSymbol, CONS(ScalarString(x), expr)), indx);
    SEXP value = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return CHAR(STRING_ELT(value, 0));
}


SEXP getInFrame(SEXP sym, SEXP env, int unbound_ok)
{
    SEXP value = findVarInFrame(env, sym);
    if (!unbound_ok && value == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    if (TYPEOF(value) == PROMSXP) {
        if (PRVALUE(value) == R_UnboundValue) {
            PROTECT(value);
            value = eval(value, R_EmptyEnv);
            UNPROTECT(1);
        }
        else value = PRVALUE(value);
    }
    return value;
}


SEXP getInList(SEXP sym, SEXP list, int NULL_ok)
{
    const char *what = translateChar(PRINTNAME(sym));
    SEXP names = PROTECT(getAttrib(list, R_NamesSymbol));
    for (R_xlen_t i = 0, n = xlength(names); i < n; i++) {
        if (!strcmp(translateChar(STRING_ELT(names, i)), what)) {
            UNPROTECT(1);
            return VECTOR_ELT(list, i);
        }
    }
    if (!NULL_ok)
        error("element '%s' not found", EncodeChar(PRINTNAME(sym)));
    UNPROTECT(1);
    return NULL;
}


void INCREMENT_NAMED_defineVar(SEXP symbol, SEXP value, SEXP rho)
{
    INCREMENT_NAMED(value);
    defineVar(symbol, value, rho);
}


void MARK_NOT_MUTABLE_defineVar(SEXP symbol, SEXP value, SEXP rho)
{
    MARK_NOT_MUTABLE(value);
    defineVar(symbol, value, rho);
}


SEXP findFunction3(SEXP symbol, SEXP rho, SEXP call)
{
    SEXP vl;
    for (; rho != R_EmptyEnv; rho = ENCLOS(rho)) {
        vl = findVarInFrame(rho, symbol);
        if (vl != R_UnboundValue) {
            if (TYPEOF(vl) == PROMSXP) {
                if (PRVALUE(vl) == R_UnboundValue)
                    vl = eval(vl, R_EmptyEnv);
                else
                    vl = PRVALUE(vl);
            }
            if (TYPEOF(vl) == CLOSXP ||
                TYPEOF(vl) == BUILTINSXP ||
                TYPEOF(vl) == SPECIALSXP)
            {
                return vl;
            }
            if (vl == R_MissingArg)
                errorcall(call,
                    _("argument \"%s\" is missing, with no default"),
                    EncodeChar(PRINTNAME(symbol)));
        }
    }
    errorcall(call,
        _("could not find function \"%s\""),
        EncodeChar(PRINTNAME(symbol)));
    return R_UnboundValue;
}


SEXP findFunction(SEXP symbol, SEXP rho)
{
    return findFunction3(symbol, rho, R_CurrentExpression);
}


#if defined(R_CONNECTIONS_VERSION_1)
SEXP summaryconnection(Rconnection Rcon)
{
    SEXP value, names;
    value = allocVector(VECSXP, 7);
    PROTECT(value);
    names = allocVector(STRSXP, 7);
    setAttrib(value, R_NamesSymbol, names);
    SET_STRING_ELT(names, 0, mkChar("description"));
    SET_VECTOR_ELT(value, 0, ScalarString(
        mkCharCE(Rcon->description, (Rcon->enc == CE_UTF8) ? CE_UTF8 : CE_NATIVE)
    ));
    SET_STRING_ELT(names, 1, mkChar("class"));
    SET_VECTOR_ELT(value, 1, mkString(Rcon->class));
    SET_STRING_ELT(names, 2, mkChar("mode"));
    SET_VECTOR_ELT(value, 2, mkString(Rcon->mode));
    SET_STRING_ELT(names, 3, mkChar("text"));
    SET_VECTOR_ELT(value, 3, mkString(Rcon->text ? "text" : "binary"));
    SET_STRING_ELT(names, 4, mkChar("opened"));
    SET_VECTOR_ELT(value, 4, mkString(Rcon->isopen ? "opened" : "closed"));
    SET_STRING_ELT(names, 5, mkChar("can read"));
    SET_VECTOR_ELT(value, 5, mkString(Rcon->canread ? "yes" : "no"));
    SET_STRING_ELT(names, 6, mkChar("can write"));
    SET_VECTOR_ELT(value, 6, mkString(Rcon->canwrite ? "yes" : "no"));
    UNPROTECT(1);
    return value;
}
#else
SEXP summaryconnection(SEXP sConn)
{
    if (!inherits(sConn, "connection")) error(_("invalid connection"));
    SEXP expr = LCONS(summary_connectionSymbol, CONS(sConn, R_NilValue));
    PROTECT(expr);
    SEXP value = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return value;
}
#endif


SEXP DocumentContext(void)
{
    SEXP documentcontext = R_NewEnv(mynamespace, TRUE, 10);
    PROTECT(documentcontext);
    setAttrib(documentcontext, R_ClassSymbol, DocumentContextClass);
    UNPROTECT(1);
    return documentcontext;
}


/* we used to do:
 * document.context$ofile <- NULL
 * to indicate that a document context does not refer to a file, now we do:
 * document.context <- emptyenv()
 * this is much easier to test for, to read, and to debug
 */


#define _assign(ofile, documentcontext)                        \
    INCREMENT_NAMED_defineVar(ofileSymbol, (ofile), (documentcontext));\
    SEXP e = makePROMISE(R_NilValue, (documentcontext));       \
    defineVar(fileSymbol, e, (documentcontext))


#define _assign_default(srcfile_original, owd, trans, documentcontext, na)\
    if (srcfile_original) {                                    \
        SET_PRCODE(e, LCONS(_normalizePath_srcfilealiasSymbol, \
                            CONS(srcfile_original,             \
                                 CONS(trans, R_NilValue))));   \
    }                                                          \
    else if (owd) {                                            \
        INCREMENT_NAMED_defineVar(wdSymbol, owd, documentcontext);\
        SEXP sym;                                              \
        switch (na) {                                          \
        case NA_DEFAULT: sym = _normalizePath_againstSymbol        ; break;\
        case NA_NOT_DIR: sym = _normalizePath_not_dir_againstSymbol; break;\
        case NA_FIX_DIR: sym = _normalizePath_fix_dir_againstSymbol; break;\
        default:                                               \
            errorcall(R_NilValue, _("invalid '%s' value"), "na");\
            sym = R_NilValue;                                  \
        }                                                      \
        SET_PRCODE(e, LCONS(sym, CONS(wdSymbol, CONS(trans, R_NilValue))));\
    }                                                          \
    else {                                                     \
        SEXP sym;                                              \
        switch (na) {                                          \
        case NA_DEFAULT: sym = _normalizePathSymbol        ; break;\
        case NA_NOT_DIR: sym = _normalizePath_not_dirSymbol; break;\
        case NA_FIX_DIR: sym = _normalizePath_fix_dirSymbol; break;\
        default:                                               \
            errorcall(R_NilValue, _("invalid '%s' value"), "na");\
            sym = R_NilValue;                                  \
        }                                                      \
        SET_PRCODE(e, LCONS(sym, CONS(trans, R_NilValue)));    \
    }


void assign_default(SEXP srcfile_original, SEXP owd, SEXP ofile, SEXP file, SEXP documentcontext, NORMALIZE_ACTION na)
{
    _assign(ofile, documentcontext);


    const char *url;
    cetype_t ienc = CE_NATIVE;
// https://github.com/wch/r-source/blob/trunk/src/main/connections.c#L5531
#ifdef _WIN32
    if (!IS_ASCII(file)) {
        ienc = CE_UTF8;
        url = translateCharUTF8(file);
    } else {
        ienc = getCharCE(file);
        url = CHAR(file);
    }
#else
    url = translateChar(file);
#endif


    _assign_default(srcfile_original, owd, ScalarString(mkCharCE(url, ienc)), documentcontext, na);
    return;
}


void assign_file_uri(SEXP srcfile_original, SEXP owd, SEXP ofile, SEXP file, SEXP documentcontext, NORMALIZE_ACTION na)
{
    _assign(ofile, documentcontext);


    /* translate the string, then extract the string after file://
     * or file:/// on windows where path is file:///d:/path/to/file */
    const char *url;
    cetype_t ienc = CE_NATIVE;
// https://github.com/wch/r-source/blob/trunk/src/main/connections.c#L5531
#ifdef _WIN32
    if (!IS_ASCII(file)) {
        ienc = CE_UTF8;
        url = translateCharUTF8(file);
    } else {
        ienc = getCharCE(file);
        url = CHAR(file);
    }
#else
    url = translateChar(file);
#endif


// https://github.com/wch/r-source/blob/trunk/src/main/connections.c#L5650
    int nh = 7;
#ifdef _WIN32
    if (strlen(url) > 9 && url[7] == '/' && url[9] == ':') nh = 8;
#endif


    _assign_default(srcfile_original, owd, ScalarString(mkCharCE(url + nh, ienc)), documentcontext, na);
}


void assign_file_uri2(SEXP srcfile_original, SEXP owd, SEXP description, SEXP documentcontext, NORMALIZE_ACTION na)
{
    const char *url = CHAR(description);
    char _buf[8 + strlen(url) + 1];
    char *buf = _buf;
#ifdef _WIN32
    if (strlen(url) > 1 && url[1] == ':') {
        strcpy(buf, "file:///");
        strcpy(buf + 8, url);
    } else {
        strcpy(buf, "file://");
        strcpy(buf + 7, url);
    }
#else
    strcpy(buf, "file://");
    strcpy(buf + 7, url);
#endif



    SEXP ofile = ScalarString(mkCharCE(_buf, getCharCE(description)));
    _assign(ofile, documentcontext);
    _assign_default(srcfile_original, owd, ScalarString(description), documentcontext, na);
}


void assign_url(SEXP ofile, SEXP file, SEXP documentcontext)
{
    _assign(ofile, documentcontext);


    const char *url;
    cetype_t ienc = CE_NATIVE;
// https://github.com/wch/r-source/blob/trunk/src/main/connections.c#L5531
#ifdef _WIN32
    if (!IS_ASCII(file)) {
        ienc = CE_UTF8;
        url = translateCharUTF8(file);
    } else {
        ienc = getCharCE(file);
        url = CHAR(file);
    }
#else
    url = translateChar(file);
#endif


    SET_PRCODE(e, LCONS(_normalizeURL_1Symbol, CONS(ScalarString(mkCharCE(url, ienc)), R_NilValue)));
    eval(e, R_EmptyEnv);  /* force the promise */
}


#undef _assign_default
#undef _assign


void overwrite_ofile(SEXP ofilearg, SEXP documentcontext)
{
    INCREMENT_NAMED_defineVar(ofileSymbol, ofilearg, documentcontext);
}


int _gui_rstudio = -1;
int _maybe_unembedded_shell = -1;
Rboolean _in_site_file = TRUE;
Rboolean _in_init_file = FALSE;
SEXP get_debugSource(void)
{
    if (!gui_rstudio) return R_UnboundValue;


    const char *what = "tools:rstudio";


    SEXP name;
    for (SEXP t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
        name = getAttrib(t, R_NameSymbol);
        if (isString(name) &&
            length(name) > 0 &&
            !strcmp(translateChar(STRING_ELT(name, 0)), what))
        {
            return getInFrame(debugSourceSymbol, t, TRUE);
        }
    }


    return R_UnboundValue;
}


SEXP duplicateEnv(SEXP env)
{
    if (TYPEOF(env) != ENVSXP)
        error("wtf are you doing? %s %d", __FILE__, __LINE__);
    if (env == R_EmptyEnv)
        return env;
    SEXP value = R_NewEnv(
        /* enclos */ ENCLOS(env),
        /* hash */ HASHTAB(env) != R_NilValue,
        /* size */ 29
    );
    PROTECT(value);
    SEXP names = R_lsInternal3(env, TRUE, FALSE);
    PROTECT(names);
    for (int i = LENGTH(names) - 1; i >= 0; i--) {
        SEXP sym = installTrChar(STRING_ELT(names, i));
#if R_version_at_least(4, 0, 0)
        if (R_BindingIsActive(sym, env))
            R_MakeActiveBinding(sym, R_ActiveBindingFunction(sym, env), value);
        else
#endif
            INCREMENT_NAMED_defineVar(sym, findVarInFrame(env, sym), value);
        if (R_BindingIsLocked(sym, env))
            R_LockBinding(sym, value);
    }
    if (R_EnvironmentIsLocked(env))
        R_LockEnvironment(value, FALSE);
    DUPLICATE_ATTRIB(/* to */ value, /* from */ env);
    if (OBJECT(env))
        SET_OBJECT(value, 1);
    if (IS_S4_OBJECT(env))
        SET_S4_OBJECT(value);
    UNPROTECT(2);
    return value;
}


// SEXP do_duplicateEnv do_formals
// {
//     do_start_no_call_op_rho("duplicateEnv", 1);
//     return duplicateEnv(CAR(args));
// }
