#include "thispathdefn.h"


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


#if R_version_less_than(4, 1, 0)
SEXP R_NewEnv(SEXP enclos, int hash, int size)
{
    SEXP expr = LCONS(new_envSymbol,
        CONS(/* hash */ ScalarLogical(hash),
            CONS(/* parent */ enclos,
                CONS(/* size */ ScalarInteger(size), R_NilValue))));
    PROTECT(expr);
    SEXP value = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return value;
}
#endif


#if R_version_less_than(4, 2, 0)
Rboolean R_existsVarInFrame(SEXP rho, SEXP symbol)
{
    static SEXP existsSymbol = NULL;
    if (existsSymbol == NULL) {
        existsSymbol = install("exists");
    }
    SEXP expr = R_NilValue;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = CONS(R_FalseValue, expr), &indx);
    SET_TAG(expr, inheritsSymbol);
    REPROTECT(expr = CONS(rho, expr), indx);
    SET_TAG(expr, envirSymbol);
    REPROTECT(expr = CONS(ScalarString(PRINTNAME(symbol)), expr), indx);
    REPROTECT(expr = LCONS(getFromBase(existsSymbol), expr), indx);
    SEXP value = PROTECT(eval(expr, R_EmptyEnv));
    if (TYPEOF(value) != LGLSXP || XLENGTH(value) != 1)
        error(_("invalid '%s' value"), "exists()");
    Rboolean lvalue = LOGICAL(value)[0];
    UNPROTECT(2);
    return lvalue;
}
#endif


SEXP makePROMISE(SEXP expr, SEXP env)
{
    ENSURE_NAMEDMAX(expr);
    SEXP s = allocSExp(PROMSXP);
    SET_PRCODE(s, expr);
    SET_PRENV(s, env);
    SET_PRVALUE(s, R_UnboundValue);
    SET_PRSEEN(s, 0);
    SET_ATTRIB(s, R_NilValue);
    return s;
}


SEXP makeEVPROMISE(SEXP expr, SEXP value)
{
    SEXP prom = makePROMISE(expr, R_NilValue);
    SET_PRVALUE(prom, value);
    return prom;
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


#if R_version_less_than(3, 6, 0)
SEXP R_shallow_duplicate_attr(SEXP x) { return shallow_duplicate(x); }
#endif


#if R_version_less_than(3, 6, 0)
SEXP installTrChar(SEXP x)
{
    return install(translateChar(x));
}
#endif


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


#if R_version_less_than(4, 1, 0)
int IS_ASCII(SEXP x)
{
    for (const char *s = CHAR(x); *s; s++) {
        if (*s > 0x7f) {
            return FALSE;
        }
    }
    return TRUE;
}
#endif


#if R_version_less_than(4, 0, 0)
void R_removeVarFromFrame(SEXP name, SEXP env)
{
    if (TYPEOF(env) == NILSXP)
        error(_("use of NULL environment is defunct"));

    if (!isEnvironment(env))
        error(_("argument to '%s' is not an environment"), "R_removeVarFromFrame");

    if (TYPEOF(name) != SYMSXP)
        error(_("not a symbol"));

    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = CONS(R_FalseValue, R_NilValue), &indx);
    SET_TAG(expr, inheritsSymbol);
    REPROTECT(expr = CONS(env, expr), indx);
    SET_TAG(expr, envirSymbol);
    REPROTECT(expr = LCONS(removeSymbol, CONS(name, expr)), indx);
    eval(expr, R_BaseEnv);
    UNPROTECT(1);
}
#endif


void UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t)
{
    error(_("unimplemented type '%s' in '%s'\n"), type2char(t), s);
}


void UNIMPLEMENTED_TYPE(const char *s, SEXP x)
{
    UNIMPLEMENTED_TYPEt(s, TYPEOF(x));
}


#if R_version_less_than(3, 1, 0)
SEXP lazy_duplicate(SEXP s)
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


SEXP shallow_duplicate(SEXP s)
{
    return duplicate(s);
}
#endif


SEXP getInFrame(SEXP sym, SEXP env, int unbound_ok)
{
    SEXP value = findVarInFrame(env, sym);
    if (!unbound_ok && value == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    if (TYPEOF(value) == PROMSXP) {
        if (PRVALUE(value) == R_UnboundValue)
            return eval(value, R_EmptyEnv);
        else
            return PRVALUE(value);
    }
    else return value;
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


SEXP errorCondition(const char *msg, SEXP call, const char **cls, int numFields)
{
    /* 'cls' is an array of strings */


    SEXP value = allocVector(VECSXP, 2 + numFields);
    PROTECT(value);
    SEXP names = allocVector(STRSXP, 2 + numFields);
    setAttrib(value, R_NamesSymbol, names);


    SET_STRING_ELT(names, 0, mkChar("message"));
    SET_VECTOR_ELT(value, 0, mkString(msg));
    SET_STRING_ELT(names, 1, mkChar("call"));
    SET_VECTOR_ELT(value, 1, call);


    /* count the number of strings in 'cls' */
    int numCls = 0;
    while (cls[numCls]) ++numCls;


    SEXP klass = allocVector(STRSXP, numCls + 2);
    setAttrib(value, R_ClassSymbol, klass);
    for (int i = 0; i < numCls; i++)
        SET_STRING_ELT(klass, i         , mkChar(cls[i]));
    SET_STRING_ELT(    klass, numCls    , mkChar("error"));
    SET_STRING_ELT(    klass, numCls + 1, mkChar("condition"));


    UNPROTECT(1);
    return value;
}


SEXP errorCondition1(const char *msg, SEXP call, const char *cls, int numFields)
{
    /* 'cls' is a string */


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
    SET_STRING_ELT(klass, 0, mkChar(cls));
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
SEXP thisPathUnrecognizedConnectionClassError(SEXP call, Rconnection Rcon)
{
    funbody(mkChar(get_connection_class(Rcon)), summaryconnection(Rcon));
}
#else
SEXP thisPathUnrecognizedConnectionClassError(SEXP call, SEXP summary)
{
    funbody(STRING_ELT(VECTOR_ELT(summary, 1), 0), summary);
}
#endif
#undef funbody


SEXP thisPathUnrecognizedMannerError(SEXP call)
{
    const char *msg = "R is running in an unrecognized manner";
    const char *cls[] = {
        "this.path::thisPathUnrecognizedMannerError",
        thisPathNotFoundErrorCls,
        NULL
    };
    return errorCondition(msg, call, cls, 0);
}


SEXP thisPathNotImplementedError(const char *msg, SEXP call)
{
#define thisPathNotImplementedErrorCls                         \
    "this.path::thisPathNotImplementedError",                  \
    "this.path_this.path_unimplemented_error",                 \
    "notImplementedError",                                     \
    "NotImplementedError"
    const char *cls[] = {thisPathNotImplementedErrorCls, NULL};
    return errorCondition(msg, call, cls, 0);
}


SEXP thisPathNotExistsError(const char *msg, SEXP call)
{
    const char *cls[] = {
        thisPathNotExistsErrorCls,
        "this.path::thisPathNotExistError",
        "this.path_this.path_not_exists_error",
        thisPathNotFoundErrorCls,
        NULL
    };
    return errorCondition(msg, call, cls, 0);
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
    const char *cls[] = {
        "this.path::thisPathInAQUAError",
        thisPathNotFoundErrorCls,
        thisPathNotImplementedErrorCls,
        NULL
    };
    return errorCondition(msg, call, cls, 0);
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
    setAttrib(documentcontext, R_ClassSymbol, DocumentContextCls);
    UNPROTECT(1);
    return documentcontext;
}


SEXP ofile_is_NULL(SEXP documentcontext)
{
    if (documentcontext != NULL &&
        documentcontext != R_UnboundValue &&
        TYPEOF(documentcontext) == ENVSXP &&
        findVarInFrame(documentcontext, ofileSymbol) == R_NilValue)
        return documentcontext;
    return NULL;
}


#define _assign(file, documentcontext)                         \
    INCREMENT_NAMED_defineVar(ofileSymbol, (file), (documentcontext));\
    SEXP e = makePROMISE(R_NilValue, (documentcontext));       \
    defineVar(fileSymbol, e, (documentcontext))


void assign_default(SEXP file, SEXP documentcontext, Rboolean check_not_directory)
{
    _assign(file, documentcontext);
    SET_PRCODE(e, LCONS(check_not_directory ? _normalizeNotDirectorySymbol : _normalizePathSymbol,
                        CONS(file, R_NilValue)));
}


void assign_null(SEXP documentcontext)
{
    /* make and force the promise */
    _assign(R_NilValue, documentcontext);
    eval(e, R_EmptyEnv);
}


void assign_chdir(SEXP file, SEXP owd, SEXP documentcontext)
{
    _assign(file, documentcontext);
    INCREMENT_NAMED_defineVar(wdSymbol, owd, documentcontext);
    SET_PRCODE(e, LCONS(_normalizeAgainstSymbol, CONS(wdSymbol, CONS(file, R_NilValue))));
    return;
}


void assign_file_uri(SEXP ofile, SEXP file, SEXP documentcontext, Rboolean check_not_directory)
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


    SET_PRCODE(e, LCONS(check_not_directory ? _normalizeNotDirectorySymbol : _normalizePathSymbol,
                        CONS(ScalarString(mkCharCE(url, ienc)), R_NilValue)));
}


void assign_file_uri2(SEXP description, SEXP documentcontext, Rboolean check_not_directory)
{
    char _buf[7 + strlen(CHAR(description)) + 1];
    char *buf = _buf;
    strcpy(buf, "file://");
    buf += 7;
    strcpy(buf, CHAR(description));


    SEXP ofile = ScalarString(mkCharCE(buf, getCharCE(description)));


    _assign(ofile, documentcontext);
    SET_PRCODE(e, LCONS(check_not_directory ? _normalizeNotDirectorySymbol : _normalizePathSymbol,
                        CONS(ScalarString(description), R_NilValue)));
}


void assign_url(SEXP ofile, SEXP file, SEXP documentcontext)
{
    _assign(ofile, documentcontext);


#ifdef _WIN32
    if (!IS_ASCII(file))
        ofile = ScalarString(mkCharCE(translateCharUTF8(file), CE_UTF8));
#endif


    SET_PRCODE(e, LCONS(_normalizeURL_1Symbol, CONS(ofile, R_NilValue)));


    /* force the promise */
    eval(e, R_EmptyEnv);
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
            if (R_BindingIsLocked((sym), mynamespace)) {       \
                R_unLockBinding((sym), mynamespace);           \
                INCREMENT_NAMED_defineVar((sym), (val), mynamespace);\
                R_LockBinding((sym), mynamespace);             \
            }                                                  \
            else INCREMENT_NAMED_defineVar((sym), (val), mynamespace)


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


int gui_rstudio = -1;
Rboolean has_tools_rstudio = FALSE;


Rboolean init_tools_rstudio(Rboolean skipCheck)
{
    if (skipCheck || in_rstudio) {
        if (!has_tools_rstudio) {
            has_tools_rstudio = _init_tools_rstudio();
        }
    }
    return has_tools_rstudio;
}


int maybe_unembedded_shell = -1;


int get_sys_parent(int n, SEXP rho)
{
    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = CONS(ScalarInteger(n), R_NilValue), &indx);
    REPROTECT(expr = LCONS(getFromBase(sys_parentSymbol), expr), indx);
    int value = asInteger(eval(expr, rho));
    UNPROTECT(1);
    return value;
}
