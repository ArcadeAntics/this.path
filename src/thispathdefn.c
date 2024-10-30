#include "thispathdefn.h"


#if R_version_at_least(3,0,0)
R_xlen_t asXLength(SEXP x)
{
    const R_xlen_t na = -999;

    if (Rf_isVectorAtomic(x) && XLENGTH(x) >= 1) {
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

    double d = Rf_asReal(x);
    if (!R_FINITE(d) || d > R_XLEN_T_MAX || d < 0)
        return na;
    else
        return (R_xlen_t) d;
}
#else
R_xlen_t asXLength(SEXP x)
{
    return Rf_asInteger(x);
}
#endif


int ddVal(SEXP symbol)
{
    const char *buf;
    char *endp;
    int rval;

    buf = R_CHAR(PRINTNAME(symbol));
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
    Rf_error(_("unimplemented type '%s' in '%s'\n"), Rf_type2char(t), s);
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
    R_ProtectWithIndex(expr = Rf_cons(R_FalseValue, R_NilValue), &indx);
    SET_TAG(expr, na_encodeSymbol);
    R_Reprotect(expr = Rf_lcons(encodeStringSymbol, Rf_cons(Rf_ScalarString(x), expr)), indx);
    SEXP value = Rf_eval(expr, R_BaseEnv);
    Rf_unprotect(1);
    return R_CHAR(STRING_ELT(value, 0));
}


SEXP getInFrame(SEXP sym, SEXP env, int unbound_ok)
{
    SEXP value = Rf_findVarInFrame(env, sym);
    if (!unbound_ok && value == R_UnboundValue)
        Rf_error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    if (TYPEOF(value) == PROMSXP) {
        if (ptr_PRVALUE(value) == R_UnboundValue) {
            Rf_protect(value);
            value = Rf_eval(value, R_EmptyEnv);
            Rf_unprotect(1);
        }
        else value = ptr_PRVALUE(value);
    }
    return value;
}


SEXP getInList(SEXP sym, SEXP list, int NULL_ok)
{
    const char *what = Rf_translateChar(PRINTNAME(sym));
    SEXP names = Rf_protect(Rf_getAttrib(list, R_NamesSymbol));
    for (R_xlen_t i = 0, n = Rf_xlength(names); i < n; i++) {
        if (!strcmp(Rf_translateChar(STRING_ELT(names, i)), what)) {
            Rf_unprotect(1);
            return VECTOR_ELT(list, i);
        }
    }
    if (!NULL_ok)
        Rf_error("element '%s' not found", EncodeChar(PRINTNAME(sym)));
    Rf_unprotect(1);
    return NULL;
}


void INCREMENT_NAMED_defineVar(SEXP symbol, SEXP value, SEXP rho)
{
    INCREMENT_NAMED(value);
    Rf_defineVar(symbol, value, rho);
}


void MARK_NOT_MUTABLE_defineVar(SEXP symbol, SEXP value, SEXP rho)
{
    MARK_NOT_MUTABLE(value);
    Rf_defineVar(symbol, value, rho);
}


SEXP findFunction3(SEXP symbol, SEXP rho, SEXP call)
{
    SEXP vl;
    for (; rho != R_EmptyEnv; rho = ENCLOS(rho)) {
        vl = Rf_findVarInFrame(rho, symbol);
        if (vl != R_UnboundValue) {
            if (TYPEOF(vl) == PROMSXP) {
                if (ptr_PRVALUE(vl) == R_UnboundValue) {
                    Rf_protect(vl);
                    vl = Rf_eval(vl, R_EmptyEnv);
                    Rf_unprotect(1);
                }
                else vl = ptr_PRVALUE(vl);
            }
            if (TYPEOF(vl) == CLOSXP ||
                TYPEOF(vl) == BUILTINSXP ||
                TYPEOF(vl) == SPECIALSXP)
            {
                return vl;
            }
            if (vl == R_MissingArg)
                // my_errorcall(call,
                //     _("argument \"%s\" is missing, with no default"),
                //     EncodeChar(PRINTNAME(symbol)));
                MissingArgError_c(EncodeChar(PRINTNAME(symbol)),
                                  call, rho, "getMissingError");

        }
    }
    my_errorcall(call,
        _("could not find function \"%s\""),
        EncodeChar(PRINTNAME(symbol)));
    return R_UnboundValue;
}


SEXP findFunction(SEXP symbol, SEXP rho)
{
    return findFunction3(symbol, rho, R_CurrentExpression);
}


SEXP summary_connection(SEXP sConn)
{
    if (!Rf_inherits(sConn, "connection")) Rf_error(_("invalid connection"));
    SEXP expr = Rf_lcons(summary_connectionSymbol, Rf_cons(sConn, R_NilValue));
    Rf_protect(expr);
    SEXP value = Rf_eval(expr, R_BaseEnv);
    Rf_unprotect(1);
    return value;
}
#if defined(R_CONNECTIONS_VERSION_1)
SEXP summary_connection_Rcon_V1(Rconnection Rcon)
{
    SEXP value, names;
    value = Rf_allocVector(VECSXP, 7);
    Rf_protect(value);
    names = Rf_allocVector(STRSXP, 7);
    Rf_setAttrib(value, R_NamesSymbol, names);
    SET_STRING_ELT(names, 0, Rf_mkChar("description"));
    SET_VECTOR_ELT(value, 0, Rf_ScalarString(
        Rf_mkCharCE(Rcon->description, (Rcon->enc == CE_UTF8) ? CE_UTF8 : CE_NATIVE)
    ));
    SET_STRING_ELT(names, 1, Rf_mkChar("class"));
    SET_VECTOR_ELT(value, 1, Rf_mkString(Rcon->class));
    SET_STRING_ELT(names, 2, Rf_mkChar("mode"));
    SET_VECTOR_ELT(value, 2, Rf_mkString(Rcon->mode));
    SET_STRING_ELT(names, 3, Rf_mkChar("text"));
    SET_VECTOR_ELT(value, 3, Rf_mkString(Rcon->text ? "text" : "binary"));
    SET_STRING_ELT(names, 4, Rf_mkChar("opened"));
    SET_VECTOR_ELT(value, 4, Rf_mkString(Rcon->isopen ? "opened" : "closed"));
    SET_STRING_ELT(names, 5, Rf_mkChar("can read"));
    SET_VECTOR_ELT(value, 5, Rf_mkString(Rcon->canread ? "yes" : "no"));
    SET_STRING_ELT(names, 6, Rf_mkChar("can write"));
    SET_VECTOR_ELT(value, 6, Rf_mkString(Rcon->canwrite ? "yes" : "no"));
    Rf_unprotect(1);
    return value;
}
#endif


SEXP DocumentContext(void)
{
    SEXP documentcontext = R_NewEnv(mynamespace, TRUE, 10);
    Rf_protect(documentcontext);
    Rf_setAttrib(documentcontext, R_ClassSymbol, DocumentContextClass);
    Rf_unprotect(1);
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
    Rf_protect(e);                                             \
    Rf_defineVar(fileSymbol, e, (documentcontext))


#define _assign_default(srcfile_original, owd, trans, documentcontext, na)\
    if (srcfile_original) {                                    \
        ptr_SET_PRCODE(                                        \
            e,                                                 \
            Rf_lcons(                                          \
                _normalizePath_srcfilealiasSymbol,             \
                Rf_cons(                                       \
                    srcfile_original,                          \
                    Rf_cons(trans, R_NilValue)                 \
                )                                              \
            )                                                  \
        );                                                     \
    }                                                          \
    else if (owd) {                                            \
        INCREMENT_NAMED_defineVar(wdSymbol, owd, documentcontext);\
        SEXP sym;                                              \
        switch (na) {                                          \
        case NA_DEFAULT: sym = _normalizePath_againstSymbol        ; break;\
        case NA_NOT_DIR: sym = _normalizePath_not_dir_againstSymbol; break;\
        case NA_FIX_DIR: sym = _normalizePath_fix_dir_againstSymbol; break;\
        default:                                               \
            Rf_errorcall(R_NilValue, _("invalid '%s' value"), "na");\
            sym = R_NilValue;                                  \
        }                                                      \
        ptr_SET_PRCODE(e, Rf_lcons(sym, Rf_cons(wdSymbol, Rf_cons(trans, R_NilValue))));\
    }                                                          \
    else {                                                     \
        SEXP sym;                                              \
        switch (na) {                                          \
        case NA_DEFAULT: sym = _normalizePathSymbol        ; break;\
        case NA_NOT_DIR: sym = _normalizePath_not_dirSymbol; break;\
        case NA_FIX_DIR: sym = _normalizePath_fix_dirSymbol; break;\
        default:                                               \
            Rf_errorcall(R_NilValue, _("invalid '%s' value"), "na");\
            sym = R_NilValue;                                  \
        }                                                      \
        ptr_SET_PRCODE(e, Rf_lcons(sym, Rf_cons(trans, R_NilValue)));\
    }


void assign_default(SEXP srcfile_original, SEXP owd, SEXP ofile, SEXP file, SEXP documentcontext, NORMALIZE_ACTION na)
{
    _assign(ofile, documentcontext);


    const char *url;
    cetype_t ienc = CE_NATIVE;
// https://github.com/wch/r-source/blob/trunk/src/main/connections.c#L5531
#if defined(_WIN32)
    if (!IS_ASCII(file)) {
        ienc = CE_UTF8;
        url = Rf_translateCharUTF8(file);
    } else {
        ienc = Rf_getCharCE(file);
        url = R_CHAR(file);
    }
#else
    url = Rf_translateChar(file);
#endif


    if (is_abs_path(url)) owd = NULL;


    _assign_default(srcfile_original, owd, Rf_ScalarString(Rf_mkCharCE(url, ienc)), documentcontext, na);
    Rf_unprotect(1);
}


void assign_file_uri(SEXP srcfile_original, SEXP owd, SEXP ofile, SEXP file, SEXP documentcontext, NORMALIZE_ACTION na)
{
    _assign(ofile, documentcontext);


    /* translate the string, then extract the string after file://
     * or file:/// on windows where path is file:///d:/path/to/file */
    const char *url;
    cetype_t ienc = CE_NATIVE;
// https://github.com/wch/r-source/blob/trunk/src/main/connections.c#L5531
#if defined(_WIN32)
    if (!IS_ASCII(file)) {
        ienc = CE_UTF8;
        url = Rf_translateCharUTF8(file);
    } else {
        ienc = Rf_getCharCE(file);
        url = R_CHAR(file);
    }
#else
    url = Rf_translateChar(file);
#endif


// https://github.com/wch/r-source/blob/trunk/src/main/connections.c#L5650
    int nh = 7;
#if defined(_WIN32)
    if (strlen(url) > 9 && url[7] == '/' && url[9] == ':') nh = 8;
#endif


    if (is_abs_path(url + nh)) owd = NULL;


    _assign_default(srcfile_original, owd, Rf_ScalarString(Rf_mkCharCE(url + nh, ienc)), documentcontext, na);
    Rf_unprotect(1);
}


void assign_file_uri2(SEXP srcfile_original, SEXP owd, SEXP description, SEXP documentcontext, NORMALIZE_ACTION na)
{
    const char *url = R_CHAR(description);
    char _buf[8 + strlen(url) + 1];
    char *buf = _buf;
#if defined(_WIN32)
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


    if (is_abs_path(url)) owd = NULL;


    SEXP ofile = Rf_ScalarString(Rf_mkCharCE(_buf, Rf_getCharCE(description)));
    _assign(ofile, documentcontext);
    _assign_default(srcfile_original, owd, Rf_ScalarString(description), documentcontext, na);
    Rf_unprotect(1);
}


void assign_url(SEXP ofile, SEXP file, SEXP documentcontext)
{
    _assign(ofile, documentcontext);


    const char *url;
    cetype_t ienc = CE_NATIVE;
// https://github.com/wch/r-source/blob/trunk/src/main/connections.c#L5531
#if defined(_WIN32)
    if (!IS_ASCII(file)) {
        ienc = CE_UTF8;
        url = Rf_translateCharUTF8(file);
    } else {
        ienc = Rf_getCharCE(file);
        url = R_CHAR(file);
    }
#else
    url = Rf_translateChar(file);
#endif


    ptr_SET_PRCODE(
        e,
        Rf_lcons(_normalizeURL_1Symbol,
            Rf_cons(Rf_ScalarString(Rf_mkCharCE(url, ienc)), R_NilValue))
    );
    Rf_eval(e, R_EmptyEnv);  /* force the promise */
    Rf_unprotect(1);
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
        name = Rf_getAttrib(t, R_NameSymbol);
        if (Rf_isString(name) &&
            Rf_length(name) > 0 &&
            !strcmp(Rf_translateChar(STRING_ELT(name, 0)), what))
        {
            return getInFrame(debugSourceSymbol, t, TRUE);
        }
    }


    return R_UnboundValue;
}


int my_HASHASH(SEXP x)
{
#if defined(R_THIS_PATH_DEVEL) || R_version_less_than(4,5,0)
    return HASHTAB(x) != R_NilValue;
#else
    static SEXP env_profileSymbol = NULL;
    if (env_profileSymbol == NULL)
        env_profileSymbol = Rf_install("env.profile");
    SEXP expr = Rf_lcons(env_profileSymbol, Rf_cons(x, R_NilValue));
    Rf_protect(expr);
    int value = Rf_eval(expr, R_BaseEnv) != R_NilValue;
    Rf_unprotect(1);
    return value;
#endif
}


SEXP duplicateEnv(SEXP env)
{
    if (TYPEOF(env) != ENVSXP)
        Rf_error("wtf are you doing? %s %d", __FILE__, __LINE__);
    if (env == R_EmptyEnv)
        return env;
    SEXP value = R_NewEnv(
        /* enclos */ ENCLOS(env),
        /* hash */ my_HASHASH(env),
        /* size */ 29
    );
    Rf_protect(value);
    SEXP names = R_lsInternal3(env, TRUE, FALSE);
    Rf_protect(names);
    for (int i = LENGTH(names) - 1; i >= 0; i--) {
        SEXP sym = Rf_installTrChar(STRING_ELT(names, i));
#if R_version_at_least(4,0,0)
        if (R_BindingIsActive(sym, env))
            R_MakeActiveBinding(sym, R_ActiveBindingFunction(sym, env), value);
        else
#endif
            INCREMENT_NAMED_defineVar(sym, Rf_findVarInFrame(env, sym), value);
        if (R_BindingIsLocked(sym, env))
            R_LockBinding(sym, value);
    }
    if (R_EnvironmentIsLocked(env))
        R_LockEnvironment(value, FALSE);
    DUPLICATE_ATTRIB(/* to */ value, /* from */ env);
    Rf_unprotect(2);
    return value;
}


// SEXP do_duplicateEnv do_formals
// {
//     do_start_no_call_op_rho("duplicateEnv", 1);
//     return duplicateEnv(CAR(args));
// }
