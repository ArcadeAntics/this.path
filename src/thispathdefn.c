#include "thispathdefn.h"


const char *EncodeChar(SEXP x)
{
    /* accepts a CHARSXP and escapes the special / / non-printing characters */
    SEXP expr = lang3(encodeStringSymbol, ScalarString(x), ScalarLogical(FALSE));
    PROTECT(expr);
    SET_TAG(CDDR(expr), na_encodeSymbol);
    SEXP value = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return CHAR(STRING_ELT(value, 0));
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


#if !defined(_WIN32) || R_version_less_than(4, 1, 0)
SEXP R_getNSValue(SEXP call, SEXP ns, SEXP name, int exported)
{
    SEXP expr = lang3(exported ? R_DoubleColonSymbol : R_TripleColonSymbol, ns, name);
    PROTECT(expr);
    expr = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return expr;
}
#endif


#if !defined(_WIN32) || R_version_less_than(3, 4, 0)
SEXP findFun3(SEXP symbol, SEXP rho, SEXP call)
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
#endif


#if R_version_less_than(4, 1, 0)
int IS_ASCII(SEXP x)
{
    for (const char *s = CHAR(x); *s; s++) {
        if (*s > 127) {
            return FALSE;
        }
    }
    return TRUE;
}
#endif


#if R_version_less_than(4, 0, 0)
#define R_THIS_PATH_USE_removeFromFrame
void R_removeVarFromFrame(SEXP name, SEXP env)
{
    static SEXP removeSymbol   = NULL,
                envirSymbol    = NULL,
                inheritsSymbol = NULL;
    if (removeSymbol == NULL) {
        removeSymbol   = install("remove");
        envirSymbol    = install("envir");
        inheritsSymbol = install("inherits");
    }

    if (TYPEOF(env) == NILSXP)
        error(_("use of NULL environment is defunct"));

    if (!isEnvironment(env))
        error(_("argument to '%s' is not an environment"), "R_removeVarFromFrame");

    if (TYPEOF(name) != SYMSXP)
        error(_("not a symbol"));

    SEXP expr = allocList(4);
    PROTECT(expr);
    SET_TYPEOF(expr, LANGSXP);

    SEXP ptr = expr;
                                  SETCAR(ptr, removeSymbol);         ptr = CDR(ptr);
                                  SETCAR(ptr, name);                 ptr = CDR(ptr);
    SET_TAG(ptr, envirSymbol);    SETCAR(ptr, env);                  ptr = CDR(ptr);
    SET_TAG(ptr, inheritsSymbol); SETCAR(ptr, ScalarLogical(FALSE)); ptr = CDR(ptr);

    eval(expr, R_BaseEnv);
    UNPROTECT(1);
}


void removeFromFrame(SEXP *names, SEXP env)
{
    static SEXP removeSymbol   = NULL,
                listSymbol     = NULL,
                envirSymbol    = NULL,
                inheritsSymbol = NULL;
    if (removeSymbol == NULL) {
        removeSymbol   = install("remove");
        listSymbol     = install("list");
        envirSymbol    = install("envir");
        inheritsSymbol = install("inherits");
    }

    if (TYPEOF(env) == NILSXP)
        error(_("use of NULL environment is defunct"));

    if (!isEnvironment(env))
        error(_("argument to '%s' is not an environment"), "removeFromFrame");

    int n;
    for (n = 0; names[n]; n++) {
        if (TYPEOF(names[n]) != SYMSXP)
            error(_("not a symbol"));
    }

    SEXP expr = allocList(4);
    PROTECT(expr);
    SET_TYPEOF(expr, LANGSXP);

    SEXP ptr = expr;
                                  SETCAR(ptr, removeSymbol);         ptr = CDR(ptr);
    SEXP list = allocVector(STRSXP, n);
    SET_TAG(ptr, listSymbol);     SETCAR(ptr, list);                 ptr = CDR(ptr);
    SET_TAG(ptr, envirSymbol);    SETCAR(ptr, env);                  ptr = CDR(ptr);
    SET_TAG(ptr, inheritsSymbol); SETCAR(ptr, ScalarLogical(FALSE)); ptr = CDR(ptr);

    for (n = 0; names[n]; n++)
        SET_STRING_ELT(list, n, PRINTNAME(names[n]));

    eval(expr, R_BaseEnv);
    UNPROTECT(1);
}
#else
void removeFromFrame(SEXP *names, SEXP env)
{
    if (TYPEOF(env) == NILSXP)
        error(_("use of NULL environment is defunct"));

    if (!isEnvironment(env))
        error(_("argument to '%s' is not an environment"), "removeFromFrame");

    int i;
    for (i = 0; names[i]; i++) {
        if (TYPEOF(names[i]) != SYMSXP)
            error(_("not a symbol"));
    }

    for (i = 0; names[i]; i++)
        R_removeVarFromFrame(names[i], env);
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
    errorcall(lang2(as_environmentSymbol, mkString(what)),
        _("no item called \"%s\" on the search list"), what);
    return R_NilValue;
}


#if defined(R_CONNECTIONS_VERSION_1)
SEXP summaryconnection(Rconnection Rcon)
{
    SEXP value, names;
    PROTECT(value = allocVector(VECSXP, 7));
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
    SEXP expr = lang2(summary_connectionSymbol, sConn);
    PROTECT(expr);
    SEXP value = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return value;
}
#endif


SEXP errorCondition(const char *msg, SEXP call, const char **cls, int numCls, int numFields)
{
    SEXP value = allocVector(VECSXP, 2 + numFields);
    PROTECT(value);
    SEXP names = allocVector(STRSXP, 2 + numFields);
    setAttrib(value, R_NamesSymbol, names);


    SET_STRING_ELT(names, 0, mkChar("message"));
    SET_VECTOR_ELT(value, 0, mkString(msg));
    SET_STRING_ELT(names, 1, mkChar("call"));
    SET_VECTOR_ELT(value, 1, call);


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
    const char *format = "'this.path' not implemented when source-ing a connection of class '%s'";\
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
    const char *msg =
        this_path_used_in_an_inappropriate_fashion
        "* R is being run in an unrecognized manner";
    return errorCondition1(msg, call, "this.path::thisPathUnrecognizedMannerError", 0);
}


SEXP thisPathNotImplementedError(const char *msg, SEXP call)
{
#define thispathnotimplementederrorcls                         \
    "this.path::thisPathNotImplementedError",                  \
    "this.path_this.path_unimplemented_error",                 \
    "notImplementedError",                                     \
    "NotImplementedError"
#define thispathnotimplementederrornumcls 4
    const char *cls[] = {thispathnotimplementederrorcls};
    return errorCondition(msg, call, cls, thispathnotimplementederrornumcls, 0);
}


SEXP thisPathNotExistsError(const char *msg, SEXP call)
{
    const char *cls[] = {
        thisPathNotExistsErrorCls,
        "this.path::thisPathNotExistError",
        "this.path_this.path_not_exists_error"
    };
    return errorCondition(msg, call, cls, 3, 0);
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
    const char *msg =
        "'this.path' used in an inappropriate fashion\n"
        "* no appropriate source call was found up the calling stack\n"
        "* R is being run from AQUA which is currently unimplemented\n"
        "  consider using RStudio / / VSCode until such a time when this is implemented";
    const char *cls[] = {"this.path::thisPathInAQUAError", thispathnotimplementederrorcls};
    return errorCondition(msg, call, cls, 1 + thispathnotimplementederrornumcls, 0);
}


void stop(SEXP cond)
{
    SEXP expr = lang2(stopSymbol, cond);
    PROTECT(expr);
    eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return;
}


void assign_done(SEXP frame)
{
    defineVar(thispathdoneSymbol, R_NilValue, frame);
    R_LockBinding(thispathdoneSymbol, frame);
}


#define _assign(file, frame)                                   \
    INCREMENT_NAMED((file));                                   \
    defineVar(thispathofileSymbol, (file), (frame));           \
    R_LockBinding(thispathofileSymbol, (frame));               \
    /* this would be so much easier if we were given access to mkPROMISE */\
    SEXP expr = allocList(5);                                  \
    PROTECT(expr);                                             \
    SET_TYPEOF(expr, LANGSXP);                                 \
    SETCAR   (expr, findVarInFrame(R_BaseEnv, delayedAssignSymbol));\
    SETCADR  (expr, /* x          */ ScalarString(PRINTNAME(thispathfileSymbol)));\
    SETCADDDR(expr, /* eval.env   */ R_EmptyEnv);              \
    SETCAD4R (expr, /* assign.env */ (frame));                 \
    eval(expr, R_EmptyEnv);                                    \
    UNPROTECT(1);                                              \
    R_LockBinding(thispathfileSymbol, (frame));                \
    assign_done((frame));                                      \
    SEXP e = findVarInFrame((frame), thispathfileSymbol)


void assign_default(SEXP file, SEXP frame)
{
    _assign(file, frame);
    SET_PRCODE(e, lang2(_normalizePathSymbol, file));
    SET_PRENV(e, mynamespace);
}


void assign_null(SEXP frame)
{
    /* make and force the promise */
    _assign(R_NilValue, frame);
    eval(e, R_EmptyEnv);
}


void assign_chdir(SEXP file, SEXP owd, SEXP frame)
{
    _assign(file, frame);
    SET_PRCODE(e, lang3(_normalizeAgainstSymbol, file, owd));
    SET_PRENV(e, mynamespace);
    return;
}


void assign_file_uri(SEXP ofile, SEXP file, SEXP frame)
{
    _assign(ofile, frame);


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


    SET_PRCODE(e, lang2(_normalizePathSymbol, ScalarString(mkCharCE(url, ienc))));
    SET_PRENV(e, mynamespace);
}


void assign_file_uri2(SEXP description, SEXP frame)
{
    char _buf[7 + strlen(CHAR(description)) + 1];
    char *buf = _buf;
    strcpy(buf, "file://");
    buf += 7;
    strcpy(buf, CHAR(description));


    SEXP ofile = ScalarString(mkCharCE(buf, getCharCE(description)));


    _assign(ofile, frame);
    SET_PRCODE(e, lang2(_normalizePathSymbol, ScalarString(description)));
    SET_PRENV(e, mynamespace);
}


void assign_url(SEXP ofile, SEXP file, SEXP frame)
{
    _assign(ofile, frame);


#ifdef _WIN32
    if (!IS_ASCII(file))
        ofile = ScalarString(mkCharCE(translateCharUTF8(file), CE_UTF8));
#endif


    SET_PRCODE(e, lang2(normalizeURL_1Symbol, ofile));
    SET_PRENV(e, mynamespace);


    /* force the promise */
    eval(e, R_EmptyEnv);
}


static Rboolean _init_tools_rstudio(void)
{
    SEXP tools_rstudio = getInFrame(tools_rstudioSymbol, mynamespace, FALSE);
    if (tools_rstudio != R_EmptyEnv)
        return TRUE;


    const char *what = EncodeChar(PRINTNAME(tools_rstudioSymbol));


    SEXP name;
    for (SEXP t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
        name = getAttrib(t, R_NameSymbol);
        if (isString(name) &&
            length(name) > 0 &&
            !strcmp(translateChar(STRING_ELT(name, 0)), what))
        {


#define check4closure(var, sym)                                \
            SEXP var = getInFrame((sym), t, FALSE);            \
            PROTECT((var));                                    \
            if (TYPEOF((var)) != CLOSXP)                       \
                error(_("object '%s' of mode '%s' was not found"), EncodeChar(PRINTNAME((sym))), "closure")


            check4closure(_rs_api_getActiveDocumentContext, _rs_api_getActiveDocumentContextSymbol);
            check4closure(_rs_api_getSourceEditorContext  , _rs_api_getSourceEditorContextSymbol  );
            check4closure(debugSource                     , debugSourceSymbol                     );


#undef check4closure


#define assigninmynamespace(sym, val)                          \
            if (R_BindingIsLocked((sym), mynamespace)) {       \
                R_unLockBinding((sym), mynamespace);           \
                defineVar((sym), (val), mynamespace);          \
                R_LockBinding((sym), mynamespace);             \
            }                                                  \
            else defineVar((sym), (val), mynamespace)


            assigninmynamespace(_rs_api_getActiveDocumentContextSymbol, _rs_api_getActiveDocumentContext);
            assigninmynamespace(_rs_api_getSourceEditorContextSymbol  , _rs_api_getSourceEditorContext  );
            assigninmynamespace(debugSourceSymbol                     , debugSource                     );
            assigninmynamespace(tools_rstudioSymbol                   , t                               );


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
