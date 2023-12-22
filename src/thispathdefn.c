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


SEXP as_environment_char(const char *what)
{
    SEXP name;
    for (SEXP t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
        name = getAttrib(t, R_NameSymbol);
        if (isString(name) &&
            length(name) > 0 &&
            !strcmp(translateChar(STRING_ELT(name, 0)), what))
        {
            return t;
        }
    }
    SEXP expr = LCONS(as_environmentSymbol, CONS(mkString(what), R_NilValue));
    PROTECT(expr);
    errorcall(expr, _("no item called \"%s\" on the search list"), what);
    UNPROTECT(1);
    return R_NilValue;
}


SEXP errorCondition(const char *msg, SEXP call, const char **Class, int numFields)
{
    /* 'Class' is an array of strings */


    SEXP value = allocVector(VECSXP, 2 + numFields);
    PROTECT(value);
    SEXP names = allocVector(STRSXP, 2 + numFields);
    setAttrib(value, R_NamesSymbol, names);


    SET_STRING_ELT(names, 0, mkChar("message"));
    SET_VECTOR_ELT(value, 0, mkString(msg));
    SET_STRING_ELT(names, 1, mkChar("call"));
    SET_VECTOR_ELT(value, 1, call);


    /* count the number of strings in 'Class' */
    int nClass = 0;
    while (Class[nClass]) ++nClass;


    SEXP klass = allocVector(STRSXP, nClass + 2);
    setAttrib(value, R_ClassSymbol, klass);
    for (int i = 0; i < nClass; i++)
        SET_STRING_ELT(klass, i         , mkChar(Class[i]));
    SET_STRING_ELT(    klass, nClass    , mkChar("error"));
    SET_STRING_ELT(    klass, nClass + 1, mkChar("condition"));


    UNPROTECT(1);
    return value;
}


SEXP errorCondition1(const char *msg, SEXP call, const char *Class, int numFields)
{
    /* 'Class' is a string */


    SEXP value = allocVector(VECSXP, 2 + numFields);
    PROTECT(value);
    SEXP names = allocVector(STRSXP, 2 + numFields);
    setAttrib(value, R_NamesSymbol, names);


    SET_STRING_ELT(names, 0, mkChar("message"));
    SET_VECTOR_ELT(value, 0, mkString(msg));
    SET_STRING_ELT(names, 1, mkChar("call"));
    SET_VECTOR_ELT(value, 1, call);


    SEXP klass = allocVector(STRSXP, 3);
    setAttrib(value, R_ClassSymbol, klass);
    SET_STRING_ELT(klass, 0, mkChar(Class));
    SET_STRING_ELT(klass, 1, mkChar("error"));
    SET_STRING_ELT(klass, 2, mkChar("condition"));


    UNPROTECT(1);
    return value;
}


SEXP simpleError(const char *msg, SEXP call)
{
    return errorCondition1(msg, call, "simpleError", 0);
}


#define funbody(class_as_CHARSXP, summConn)                    \
    const char *klass = EncodeChar((class_as_CHARSXP));        \
    const char *format = "'this.path' not implemented when source()-ing a connection of class '%s'";\
    const int n = strlen(format) + strlen(klass) + 1;          \
    char msg[n];                                               \
    snprintf(msg, n, format, klass);                           \
    SEXP cond = errorCondition1(msg, call, "this.path::thisPathUnrecognizedConnectionClassError", 1);\
    PROTECT(cond);                                             \
    SEXP names = getAttrib(cond, R_NamesSymbol);               \
    PROTECT(names);                                            \
    SET_STRING_ELT(names, 2, mkChar("summary"));               \
    SET_VECTOR_ELT(cond , 2, (summConn));                      \
    UNPROTECT(2);                                              \
    return cond
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


SEXP thisPathUnrecognizedConnectionClassError(SEXP call, Rconnection Rcon)
{
    funbody(mkChar(Rcon->class), summaryconnection(Rcon));
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


SEXP thisPathUnrecognizedConnectionClassError(SEXP call, SEXP summary)
{
    funbody(STRING_ELT(VECTOR_ELT(summary, 1), 0), summary);
}
#endif
#undef funbody


SEXP thisPathUnrecognizedMannerError(SEXP call)
{
    const char *msg = "R is running in an unrecognized manner";
    const char *Class[] = {
        "this.path::thisPathUnrecognizedMannerError",
        thisPathNotFoundErrorClass,
        NULL
    };
    return errorCondition(msg, call, Class, 0);
}


SEXP thisPathNotImplementedError(const char *msg, SEXP call)
{
#define thisPathNotImplementedErrorClass                       \
    "this.path::thisPathNotImplementedError",                  \
    "this.path_this.path_unimplemented_error",                 \
    "notImplementedError",                                     \
    "NotImplementedError"
    const char *Class[] = {thisPathNotImplementedErrorClass, NULL};
    return errorCondition(msg, call, Class, 0);
}


SEXP thisPathNotExistsError(const char *msg, SEXP call)
{
    const char *Class[] = {
        thisPathNotExistsErrorClass,
        "this.path::thisPathNotExistError",
        "this.path_this.path_not_exists_error",
        thisPathNotFoundErrorClass,
        NULL
    };
    return errorCondition(msg, call, Class, 0);
}


SEXP thisPathInZipFileError(SEXP call, SEXP description)
{
    const char *msg = "'this.path' cannot be used within a zip file";
    SEXP cond = errorCondition1(msg, call, "this.path::thisPathInZipFileError", 1);
    PROTECT(cond);
    SEXP names = getAttrib(cond, R_NamesSymbol);
    PROTECT(names);
    SET_STRING_ELT(names, 2, mkChar("description"));
    SET_VECTOR_ELT(cond , 2, ScalarString(description));
    UNPROTECT(2);
    return cond;
}


SEXP thisPathInAQUAError(SEXP call)
{
    const char *msg = "R is running from AQUA which is currently unimplemented\n"
                      " consider using RStudio / / VSCode until such a time when this is implemented";
    const char *Class[] = {
        "this.path::thisPathInAQUAError",
        thisPathNotFoundErrorClass,
        thisPathNotImplementedErrorClass,
        NULL
    };
    return errorCondition(msg, call, Class, 0);
}


SEXP thisPathInEmacsError(SEXP call)
{
    const char *msg = "R is running from Emacs which is currently unimplemented\n"
                      " consider using RStudio / / VSCode until such a time when this is implemented";
    const char *Class[] = {
        "thisPathInEmacsError",
        thisPathNotFoundErrorClass,
        thisPathNotImplementedErrorClass,
        NULL
    };
    return errorCondition(msg, call, Class, 0);
}


void stop(SEXP cond)
{
    SEXP expr = LCONS(stopSymbol, CONS(cond, R_NilValue));
    PROTECT(expr);
    eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return;
}


SEXP DocumentContext(void)
{
    SEXP documentcontext = R_NewEnv(mynamespace, TRUE, 10);
    PROTECT(documentcontext);
    setAttrib(documentcontext, R_ClassSymbol, DocumentContextClass);
    UNPROTECT(1);
    return documentcontext;
}


/* instead of doing:
 * document.context$ofile <- NULL
 * to declare that a document context does not refer to a file, now we do:
 * document.context <- emptyenv()
 * this is much easier to test for and to read and to debug
 */


#define _assign(file, documentcontext)                         \
    INCREMENT_NAMED_defineVar(ofileSymbol, (file), (documentcontext));\
    SEXP e = makePROMISE(R_NilValue, (documentcontext));       \
    defineVar(fileSymbol, e, (documentcontext))


static R_INLINE
SEXP NA_TYPE2sym(NA_TYPE normalize_action)
{
    switch (normalize_action) {
    case NA_DEFAULT: return _normalizePathSymbol        ;
    case NA_NOT_DIR: return _normalizeNotDirectorySymbol;
    case NA_FIX_DIR: return _normalizeFixDirectorySymbol;
    default:
        errorcall(R_NilValue, _("invalid '%s' value"), "normalize_action");
        return R_NilValue;
    }
}


void assign_default(SEXP file, SEXP documentcontext, NA_TYPE normalize_action)
{
    _assign(file, documentcontext);
    SET_PRCODE(e, LCONS(NA_TYPE2sym(normalize_action), CONS(file, R_NilValue)));
}


void assign_chdir(SEXP file, SEXP owd, SEXP documentcontext)
{
    _assign(file, documentcontext);
    INCREMENT_NAMED_defineVar(wdSymbol, owd, documentcontext);
    SET_PRCODE(e, LCONS(_normalizeAgainstSymbol,
                        CONS(wdSymbol,
                             CONS(file, R_NilValue))));
    return;
}


void assign_file_uri(SEXP ofile, SEXP file, SEXP documentcontext, NA_TYPE normalize_action)
{
    _assign(ofile, documentcontext);


    /* translate the string, then extract the string after file://
     * or file:/// on windows where path is file:///d:/path/to/file */
    const char *url;
    cetype_t ienc;
#ifdef _WIN32
    if (!IS_ASCII(file)) {
        ienc = CE_UTF8;
        url = translateCharUTF8(file);
    } else {
        ienc = getCharCE(file);
        url = CHAR(file);
    }
#else
    ienc = getCharCE(file);
    url = CHAR(file);
#endif


#ifdef _WIN32
    if (strlen(url) > 9 && url[7] == '/' && url[9] == ':')
        url += 8;
    else url += 7;
#else
    url += 7;
#endif


    SET_PRCODE(e, LCONS(NA_TYPE2sym(normalize_action),
                        CONS(ScalarString(mkCharCE(url, ienc)), R_NilValue)));
}


void assign_file_uri2(SEXP description, SEXP documentcontext, NA_TYPE normalize_action)
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



    SEXP ofile = ScalarString(mkCharCE(buf, getCharCE(description)));
    _assign(ofile, documentcontext);
    SET_PRCODE(e, LCONS(NA_TYPE2sym(normalize_action),
                        CONS(ScalarString(description), R_NilValue)));
}


void assign_url(SEXP ofile, SEXP file, SEXP documentcontext)
{
    _assign(ofile, documentcontext);


#ifdef _WIN32
    if (!IS_ASCII(file))
        ofile = ScalarString(mkCharCE(translateCharUTF8(file), CE_UTF8));
#endif


    SET_PRCODE(e, LCONS(_normalizeurl_1Symbol, CONS(ofile, R_NilValue)));
    eval(e, R_EmptyEnv);  /* force the promise */
}


#undef _assign


void overwrite_ofile(SEXP ofilearg, SEXP documentcontext)
{
    INCREMENT_NAMED_defineVar(ofileSymbol, ofilearg, documentcontext);
}


SEXP sys_call(SEXP which, SEXP rho)
{
    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = CONS(which, R_NilValue), &indx);
    REPROTECT(expr = LCONS(getFromBase(sys_callSymbol), expr), indx);
    SEXP value = eval(expr, rho);
    UNPROTECT(1);
    return value;
}


int sys_parent(int n, SEXP rho)
{
    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = CONS(ScalarInteger(n), R_NilValue), &indx);
    REPROTECT(expr = LCONS(getFromBase(sys_parentSymbol), expr), indx);
    int value = asInteger(eval(expr, rho));
    UNPROTECT(1);
    return value;
}


static Rboolean _init_tools_rstudio(void)
{
    SEXP _tools_rstudio = getFromMyNS(_tools_rstudioSymbol);
    if (_tools_rstudio != R_EmptyEnv)
        return TRUE;


    const char *what = "tools:rstudio";


    SEXP name;
    for (SEXP t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
        name = getAttrib(t, R_NameSymbol);
        if (isString(name) &&
            length(name) > 0 &&
            !strcmp(translateChar(STRING_ELT(name, 0)), what))
        {


#define check4closure(var, sym)                                \
            SEXP var = getInFrame((sym), t, FALSE);            \
            PROTECT(var);                                      \
            if (TYPEOF(var) != CLOSXP)                         \
                error(_("object '%s' of mode '%s' was not found"), EncodeChar(PRINTNAME((sym))), "closure")


            check4closure(_rs_api_getActiveDocumentContext, _rs_api_getActiveDocumentContextSymbol);
            check4closure(_rs_api_getSourceEditorContext  , _rs_api_getSourceEditorContextSymbol  );
            check4closure(debugSource                     , debugSourceSymbol                     );


#undef check4closure


#define assigninmynamespace(sym, val)                          \
            do {                                               \
                SEXP e = findVarInFrame(mynamespace, (sym));   \
                if (TYPEOF(e) == PROMSXP) {                    \
                    SET_PRCODE(e, (val));                      \
                    SET_PRENV(e, R_NilValue);                  \
                    SET_PRVALUE(e, (val));                     \
                    SET_PRSEEN(e, 0);                          \
                } else {                                       \
                    if (R_BindingIsLocked((sym), mynamespace)) {\
                        R_unLockBinding((sym), mynamespace);   \
                        INCREMENT_NAMED_defineVar((sym), makeEVPROMISE((val), (val)), mynamespace);\
                        R_LockBinding((sym), mynamespace);     \
                    }                                          \
                    else INCREMENT_NAMED_defineVar((sym), makeEVPROMISE((val), (val)), mynamespace);\
                }                                              \
            } while (0)


            assigninmynamespace(_rs_api_getActiveDocumentContextSymbol, _rs_api_getActiveDocumentContext);
            assigninmynamespace(_rs_api_getSourceEditorContextSymbol  , _rs_api_getSourceEditorContext  );
            assigninmynamespace(_debugSourceSymbol                    , debugSource                     );
            assigninmynamespace(_tools_rstudioSymbol                  , t                               );


#undef assigninmynamespace


            UNPROTECT(3);
            return TRUE;
        }
    }


    return FALSE;
}


int _gui_rstudio = -1;
Rboolean has_tools_rstudio = FALSE;


Rboolean init_tools_rstudio(Rboolean skipCheck)
{
    if (skipCheck || gui_rstudio) {
        if (!has_tools_rstudio) {
            has_tools_rstudio = _init_tools_rstudio();
        }
    }
    return has_tools_rstudio;
}


int _maybe_unembedded_shell = -1;


#ifdef _WIN32
int is_clipboard(const char *url)
{
    return strcmp (url, "clipboard"     ) == 0 ||
           strncmp(url, "clipboard-", 10) == 0;
}
const char *must_not_be_clipboard_message = "must not be \"clipboard\" nor start with \"clipboard-\"";
#else
int is_clipboard(const char *url)
{
    return strcmp(url, "clipboard"    ) == 0 ||
           strcmp(url, "X11_primary"  ) == 0 ||
           strcmp(url, "X11_secondary") == 0 ||
           strcmp(url, "X11_clipboard") == 0;
}
const char *must_not_be_clipboard_message = "must not be \"clipboard\", \"X11_primary\", \"X11_secondary\", nor \"X11_clipboard\"";
#endif


int is_url(const char *url)
{
    if      (strncmp(url, "http://" , 7) == 0)
        return 7;
    else if (strncmp(url, "https://", 8) == 0)
        return 8;
    else if (strncmp(url, "ftp://"  , 6) == 0)
        return 6;
    else if (strncmp(url, "ftps://" , 7) == 0)
        return 7;
    else
        return 0;
}


int is_file_uri(const char *url)
{
    return strncmp(url, "file://", 7) == 0;
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
