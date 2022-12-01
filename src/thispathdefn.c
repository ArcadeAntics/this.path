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


#if !defined(_WIN32) || R_VERSION < R_Version(4, 1, 0)
SEXP R_getNSValue(SEXP call, SEXP ns, SEXP name, int exported)
{
    SEXP expr = lang3(exported ? R_DoubleColonSymbol : R_TripleColonSymbol, ns, name);
    PROTECT(expr);
    expr = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return expr;
}
#endif


#if !defined(_WIN32)
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


#if R_VERSION < R_Version(4, 1, 0)
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


Rconnection R_GetConnection2(SEXP sConn)
{
    if (!inherits(sConn, "connection")) error(_("invalid connection"));
    SEXP expr = lang2(R_getNSValue(R_NilValue, thispathhelperSymbol, GetConnectionSymbol, FALSE), sConn);
    PROTECT(expr);
    SEXP ptr = eval(expr, R_EmptyEnv);
    UNPROTECT(1);
    return (Rconnection) R_ExternalPtrAddr(ptr);
}


Rconnection GetUnderlyingConnection(SEXP sConn)
{
    if (!inherits(sConn, "connection")) error(_("invalid connection"));
    SEXP expr = lang2(R_getNSValue(R_NilValue, thispathhelperSymbol, GetUnderlyingConnectionSymbol, FALSE), sConn);
    PROTECT(expr);
    SEXP ptr = eval(expr, R_EmptyEnv);
    UNPROTECT(1);
    return (Rconnection) R_ExternalPtrAddr(ptr);
}


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


SEXP summaryconnection(Rconnection Rcon)
{
    SEXP value, names;
    PROTECT(value = allocVector(VECSXP, 7));
    names = allocVector(STRSXP, 7);
    setAttrib(value, R_NamesSymbol, names);
    SET_STRING_ELT(names, 0, mkChar("description"));
    SET_VECTOR_ELT(value, 0, ScalarString(
        (Rcon->enc == CE_UTF8) ? mkCharCE(Rcon->description, CE_UTF8) : mkChar(Rcon->description)
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


SEXP summaryconnection2(SEXP sConn)
{
    if (!inherits(sConn, "connection")) error(_("invalid connection"));
    SEXP expr = lang2(summary_connectionSymbol, sConn);
    PROTECT(expr);
    SEXP value = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return value;
}


SEXP errorCondition(const char *msg, SEXP call, const char **cls, int n, int nfields)
{
    SEXP value = allocVector(VECSXP, 2 + nfields);
    PROTECT(value);
    SEXP names = allocVector(STRSXP, 2 + nfields);
    setAttrib(value, R_NamesSymbol, names);


    SET_STRING_ELT(names, 0, mkChar("message"));
    SET_VECTOR_ELT(value, 0, mkString(msg));
    SET_STRING_ELT(names, 1, mkChar("call"));
    SET_VECTOR_ELT(value, 1, call);


    SEXP klass = allocVector(STRSXP, n + 2);
    setAttrib(value, R_ClassSymbol, klass);
    int i;
    for (i = 0; i < n; i++) {
        SET_STRING_ELT(klass, i    , mkChar(cls[i]));
    }
    SET_STRING_ELT(    klass, n    , mkChar("error"));
    SET_STRING_ELT(    klass, n + 1, mkChar("condition"));


    UNPROTECT(1);
    return value;
}


SEXP errorCondition1(const char *msg, SEXP call, const char *cls, int nfields)
{
    SEXP value = allocVector(VECSXP, 2 + nfields);
    PROTECT(value);
    SEXP names = allocVector(STRSXP, 2 + nfields);
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


SEXP thisPathUnrecognizedConnectionClassError(SEXP call, Rconnection Rcon)
{
    const char *klass = EncodeChar(mkChar(get_class_from_Rconnection(Rcon)));
    const char *format = "'this.path' not implemented when source-ing a connection of class '%s'";
    char msg[strlen(format) - 2 + strlen(klass) + 1];
    sprintf(msg, format, klass);
    SEXP cond = errorCondition1(msg, call, "this.path::thisPathUnrecognizedConnectionClassError", 1);
    PROTECT(cond);
    SET_VECTOR_ELT(cond, 2, summaryconnection(Rcon));
    SET_STRING_ELT(getAttrib(cond, R_NamesSymbol), 2, mkChar("summary"));
    UNPROTECT(1);
    return cond;
}


SEXP thisPathUnrecognizedConnectionClassError2(SEXP call, SEXP summary)
{
    const char *klass = EncodeChar(STRING_ELT(VECTOR_ELT(summary, 1), 0));
    const char *format = "'this.path' not implemented when source-ing a connection of class '%s'";
    char msg[strlen(format) - 2 + strlen(klass) + 1];
    sprintf(msg, format, klass);
    SEXP cond = errorCondition1(msg, call, "this.path::thisPathUnrecognizedConnectionClassError", 1);
    PROTECT(cond);
    SET_VECTOR_ELT(cond, 2, summary);
    SET_STRING_ELT(getAttrib(cond, R_NamesSymbol), 2, mkChar("summary"));
    UNPROTECT(1);
    return cond;
}


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
    SET_VECTOR_ELT(cond, 2, ScalarString(description));
    SET_STRING_ELT(getAttrib(cond, R_NamesSymbol), 2, mkChar("description"));
    UNPROTECT(1);
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


SEXP _assign(SEXP file, SEXP frame)
{
    INCREMENT_NAMED(file);
    defineVar(thispathofileSymbol, file, frame);
    R_LockBinding(thispathofileSymbol, frame);
    /* this would be so much easier if we were given access to mkPROMISE */
    eval(lang5(findVarInFrame(R_BaseEnv, delayedAssignSymbol),
/* x          */    ScalarString(PRINTNAME(thispathfileSymbol)),
/* value      */    R_NilValue,
/* eval.env   */    R_EmptyEnv,
/* assign.env */    frame
    ), R_EmptyEnv);
    R_LockBinding(thispathfileSymbol, frame);
    return findVarInFrame(frame, thispathfileSymbol);
}


void assign_default(SEXP file, SEXP frame, SEXP rho)
{
    SEXP e = _assign(file, frame);
    SET_PRCODE(e, lang2(_normalizePathSymbol, file));
    SET_PRENV(e, ENCLOS(rho));
}


void assign_null(SEXP frame)
{
    /* make and force the promise */
    eval(_assign(R_NilValue, frame), R_EmptyEnv);
}


void assign_chdir(SEXP file, SEXP owd, SEXP frame, SEXP rho)
{
    SEXP e = _assign(file, frame);
    SET_PRCODE(e, lang3(_normalizeAgainstSymbol, file, owd));
    SET_PRENV(e, ENCLOS(rho));
    return;
}


void assign_file_uri(SEXP ofile, SEXP file, SEXP frame, SEXP rho)
{
    SEXP e = _assign(ofile, frame);


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
    SET_PRENV(e, ENCLOS(rho));
}


void assign_file_uri2(SEXP description, SEXP frame, SEXP rho)
{
    char _buf[7 + strlen(CHAR(description)) + 1];
    char *buf = _buf;
    strcpy(buf, "file://");
    buf += 7;
    strcpy(buf, CHAR(description));


    SEXP ofile = ScalarString(mkCharCE(buf, getCharCE(description)));


    SEXP e = _assign(ofile, frame);
    SET_PRCODE(e, lang2(_normalizePathSymbol, ScalarString(description)));
    SET_PRENV(e, ENCLOS(rho));
}


void assign_url(SEXP ofile, SEXP file, SEXP frame, SEXP rho)
{
    SEXP e = _assign(ofile, frame);


#ifdef _WIN32
    if (!IS_ASCII(file))
        ofile = ScalarString(mkCharCE(translateCharUTF8(file), CE_UTF8));
#endif


    SET_PRCODE(e, lang2(normalizeURL_1Symbol, ofile));
    SET_PRENV(e, ENCLOS(rho));


    /* force the promise */
    eval(e, R_EmptyEnv);
}


int gui_rstudio = -1;
