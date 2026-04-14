/*
this.path : Get Executing Script's Path
Copyright (C) 2022-2026   Iris Simmons
 */


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


int length_DOTS(SEXP x)
{
    return (x && TYPEOF(x) == DOTSXP) ? Rf_length(x) : 0;
}


Rboolean isUnbound(SEXP x)
{
    return TYPEOF(x) == SYMSXP && PRINTNAME(x) == R_NilValue;
}


SEXP my_UnboundValue = NULL;


#if R_version_at_least(4,6,0)


extern R_BindingType_t R_GetBindingType(SEXP sym, SEXP env);


void init_UnboundValue(void)
{
    int nprotect = 0;


    SEXP formals = Rf_allocList(1);
    Rf_protect(formals); nprotect++;
    SET_TAG(formals, R_DotsSymbol);
    SETCAR (formals, R_MissingArg);

    SEXP body = Rf_allocLang(2);
    Rf_protect(body); nprotect++;
    SETCAR (body, Rf_install("get"));
    SETCADR(body, Rf_mkString("..."));

    SEXP fun = R_mkClosure(formals, body, R_BaseEnv);
    Rf_protect(fun); nprotect++;

    SEXP expr = Rf_lcons(fun, Rf_cons(R_NilValue, R_NilValue));
    Rf_protect(expr); nprotect++;

    SEXP value = Rf_eval(expr, R_EmptyEnv);
    Rf_protect(value); nprotect++;
    if (length_DOTS(value) != 1)
        Rf_error("invalid ... list");
    value = CAR(value);
    if (TYPEOF(value) != PROMSXP)
        Rf_error("invalid ... list");
    if      (isUnbound(CAR(value))) my_UnboundValue = CAR(value);
    else if (isUnbound(CDR(value))) my_UnboundValue = CDR(value);
    else if (isUnbound(TAG(value))) my_UnboundValue = TAG(value);
    else Rf_error("invalid ... list");


    Rf_unprotect(nprotect);
}


int is_delayed(binding_info_t x) { return x.type == R_BindingTypeDelayed; }
int is_forced (binding_info_t x) { return x.type == R_BindingTypeForced ; }
int is_promise(binding_info_t x) { return is_delayed(x) || is_forced(x); }
int my_TYPEOF (binding_info_t x) { return is_promise(x) ? PROMSXP : TYPEOF(x.value); }


binding_info_t *my_findVarInFrame(SEXP env, SEXP sym, binding_info_t *x)
{
    x->env = env;
    x->sym = sym;
    switch (x->type = R_GetBindingType(sym, env)) {
    case R_BindingTypeUnbound: x->value = my_UnboundValue          ; break;
    /* make no distinction between active and inactive bindings */
    case R_BindingTypeValue  :
    case R_BindingTypeActive : x->value = R_getVar(sym, env, FALSE); break;
    case R_BindingTypeMissing: x->value = R_MissingArg             ; break;
    case R_BindingTypeDelayed:
    case R_BindingTypeForced : x->value = NULL                     ; break;
    default: Rf_error("invalid binding type");
    }
    return x;
}


binding_info_t *my_findVar(SEXP env, SEXP sym, binding_info_t *x)
{
    x->env = env;
    x->sym = sym;
    for (; env != R_EmptyEnv; env = ENCLOS(env)) {
        x->env = env;
        switch (x->type = R_GetBindingType(sym, env)) {
        case R_BindingTypeUnbound: break;
        /* make no distinction between active and inactive bindings */
        case R_BindingTypeValue  :
        case R_BindingTypeActive : x->value = R_getVar(sym, env, FALSE); return x;
        case R_BindingTypeMissing: x->value = R_MissingArg             ; return x;
        case R_BindingTypeDelayed:
        case R_BindingTypeForced : x->value = NULL                     ; return x;
        default: Rf_error("invalid binding type");
        }
    }
    if (env != R_EmptyEnv)
        Rf_error("should not happen, please report! (from %s#%d)", __FILE__, __LINE__);
    x->type = R_BindingTypeUnbound;
    x->value = my_UnboundValue;
    return x;
}


SEXP my_findValInFrame(SEXP env, SEXP sym)
{
    binding_info_t x; my_findVarInFrame(env, sym, &x);
    return is_promise(x) ? R_NilValue : x.value;
}


SEXP my_findVal(SEXP env, SEXP sym)
{
    binding_info_t x; my_findVar(env, sym, &x);
    return is_promise(x) ? R_NilValue : x.value;
}


SEXP force(binding_info_t *x)
{
    if (is_promise(*x)) {
        SEXP value = R_getVar(x->sym, x->env, FALSE);
        /* assign type AFTER 'R_getVar' in case an error is thrown */
        x->type = R_BindingTypeForced;
        return value;
    }
    /* as possibly returned by an active binding */
    else if (TYPEOF(x->value) == PROMSXP) {
        if (ptr_PRVALUE(x->value) == my_UnboundValue) {
            Rf_protect(x->value);
            SEXP value = Rf_eval(x->value, R_EmptyEnv);
            Rf_unprotect(1);
            return value;
        }
        else return ptr_PRVALUE(x->value);
    }
    else return x->value;
}


void forceInFrame(SEXP env, SEXP sym)
{
    switch (R_GetBindingType(sym, env)) {
    case R_BindingTypeUnbound:
        Rf_error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
        break;
    case R_BindingTypeValue:
        break;
    case R_BindingTypeMissing:
        break;
    case R_BindingTypeDelayed:
        R_getVar(sym, env, FALSE);
        break;
    case R_BindingTypeForced:
        break;
    case R_BindingTypeActive:
        break;
    }
}


SEXP my_PREXPR(binding_info_t x)
{
    if (is_promise(x)) {
        if (is_delayed(x))
            return R_DelayedBindingExpression(x.sym, x.env);
        else
            return R_ForcedBindingExpression(x.sym, x.env);
    }
    else if (TYPEOF(x.value) == PROMSXP)
        return ptr_R_PromiseExpr(x.value);
    else Rf_error("not a promise");
}


SEXP my_PRENV(binding_info_t x)
{
    if (is_promise(x)) {
        if (is_delayed(x))
            return R_DelayedBindingEnvironment(x.sym, x.env);
        else
            return R_NilValue;
    }
    else if (TYPEOF(x.value) == PROMSXP)
        return ptr_PRENV(x.value);
    else Rf_error("not a promise");
}


SEXP my_PRVALUE(binding_info_t x)
{
    if (is_promise(x)) {
        if (is_delayed(x))
            return my_UnboundValue;
        else
            return R_getVar(x.sym, x.env, FALSE);
    }
    else if (TYPEOF(x.value) == PROMSXP)
        return ptr_PRVALUE(x.value);
    else Rf_error("not a promise");
}


SEXP my_getRegisteredNamespace_c(const char *x)
{
    extern SEXP R_getRegisteredNamespace(const char *name);
    return R_getRegisteredNamespace(x);
}


SEXP my_getRegisteredNamespace_sym(SEXP sym)
{
    return my_getRegisteredNamespace_c(R_CHAR(PRINTNAME(sym)));
}


SEXP my_getRegisteredNamespace(const char *x, SEXP sym)
{
    return my_getRegisteredNamespace_c(x);
}


#else


void init_UnboundValue(void)
{
    my_UnboundValue = R_UnboundValue;
}


int is_delayed(binding_info_t x) { return ptr_PRVALUE(x.value) == my_UnboundValue; }
int is_forced (binding_info_t x) { return ptr_PRVALUE(x.value) != my_UnboundValue; }
int is_promise(binding_info_t x) { return TYPEOF(x.value) == PROMSXP; }
int my_TYPEOF (binding_info_t x) { return TYPEOF(x.value); }


binding_info_t *my_findVarInFrame(SEXP env, SEXP sym, binding_info_t *x)
{
    x->env = env;
    x->sym = sym;
    x->value = Rf_findVarInFrame(env, sym);
    return x;
}


binding_info_t *my_findVar(SEXP env, SEXP sym, binding_info_t *x)
{
    x->env = env;
    x->sym = sym;
    x->value = Rf_findVar(sym, env);
    return x;
}


SEXP my_findValInFrame(SEXP env, SEXP sym)
{
    return Rf_findVarInFrame(env, sym);
}


SEXP my_findVal(SEXP env, SEXP sym)
{
    return Rf_findVar(sym, env);
}


SEXP force(binding_info_t *x)
{
    if (is_promise(*x)) {
        if (is_delayed(*x)) {
            Rf_protect(x->value);
            SEXP value = Rf_eval(x->value, R_EmptyEnv);
            Rf_unprotect(1);
            return value;
        }
        else return ptr_PRVALUE(x->value);
    }
    else return x->value;
}


void forceInFrame(SEXP env, SEXP sym)
{
    SEXP value = Rf_findVarInFrame(env, sym);
    if (TYPEOF(value) == PROMSXP && ptr_PRVALUE(value) == my_UnboundValue) {
        Rf_protect(value);
        Rf_eval(value, R_EmptyEnv);
        Rf_unprotect(1);
    }
}


SEXP my_PREXPR(binding_info_t x)
{
    if (is_promise(x))
        return ptr_R_PromiseExpr(x.value);
    else {
        Rf_error("not a promise");
        return R_NilValue;
    }
}


SEXP my_PRENV(binding_info_t x)
{
    if (is_promise(x))
        return ptr_PRENV(x.value);
    else {
        Rf_error("not a promise");
        return R_NilValue;
    }
}


SEXP my_PRVALUE(binding_info_t x)
{
    if (is_promise(x))
        return ptr_PRVALUE(x.value);
    else {
        Rf_error("not a promise");
        return R_NilValue;
    }
}


SEXP my_getRegisteredNamespace_c(const char *x)
{
    return my_getRegisteredNamespace_sym(Rf_install(x));
}


SEXP my_getRegisteredNamespace_sym(SEXP sym)
{
    SEXP val = Rf_findVarInFrame(R_NamespaceRegistry, sym);
    return val == my_UnboundValue ? R_NilValue : val;
}


SEXP my_getRegisteredNamespace(const char *x, SEXP sym)
{
    return my_getRegisteredNamespace_sym(sym);
}


SEXP promiseUnwrap(SEXP x)
{
    /* uses Floyd's cycle detection */
    SEXP y = x;
    Rboolean advance = FALSE;
    while (TRUE) {
        SEXP tmp = ptr_PRCODE(x);
        if (TYPEOF(tmp) != PROMSXP)
            return x;
        x = tmp;

        if (x == y)
            Rf_error(_("cycle detected in promise chain"));

        if (advance)
            y = ptr_PRCODE(y);
        advance = !advance;
    }
}


SEXP R_DelayedBindingExpression(SEXP sym, SEXP env)
{
    SEXP x = Rf_findVarInFrame(env, sym);
    if (TYPEOF(x) != PROMSXP)
        Rf_error("not a promise");
    x = promiseUnwrap(x);
    return ptr_R_PromiseExpr(x);
}


SEXP R_ForcedBindingExpression(SEXP sym, SEXP env)
{
    return R_DelayedBindingExpression(sym, env);
}


SEXP R_DelayedBindingEnvironment(SEXP sym, SEXP env)
{
    SEXP x = Rf_findVarInFrame(env, sym);
    if (TYPEOF(x) != PROMSXP)
        Rf_error("not a promise");
    x = promiseUnwrap(x);
    return ptr_PRENV(x);
}


SEXP makePROMISE(SEXP expr, SEXP env)
{
    ENSURE_NAMEDMAX(expr);
    SEXP s = ptr_Rf_allocSExp(PROMSXP);
    ptr_SET_PRCODE(s, expr);
    ptr_SET_PRENV(s, env);
    ptr_SET_PRVALUE(s, my_UnboundValue);
    ptr_SET_PRSEEN(s, 0);
    CLEAR_ATTRIB(s);
    return s;
}


SEXP makeEVPROMISE(SEXP expr, SEXP value)
{
    SEXP prom = makePROMISE(expr, R_NilValue);
    ptr_SET_PRVALUE(prom, value);
    return prom;
}


void R_MakeDelayedBinding(SEXP sym, SEXP expr, SEXP eval_env, SEXP assign_env)
{
    Rf_defineVar(sym, makePROMISE(expr, eval_env), assign_env);
}


void R_MakeForcedBinding(SEXP sym, SEXP expr, SEXP value, SEXP assign_env)
{
    Rf_defineVar(sym, makeEVPROMISE(expr, value), assign_env);
}


#endif


SEXP getInFrame(SEXP sym, SEXP env, int unbound_ok)
{
    return my_getVarInFrame(env, sym, unbound_ok);
}


SEXP my_getVarInFrame(SEXP env, SEXP sym, int unbound_ok)
{
    binding_info_t x;
    my_findVarInFrame(env, sym, &x);
    if (!unbound_ok && x.value == my_UnboundValue)
        Rf_error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    return force(&x);
}


SEXP my_getVar(SEXP env, SEXP sym, int unbound_ok)
{
    binding_info_t x;
    my_findVar(env, sym, &x);
    if (!unbound_ok && x.value == my_UnboundValue)
        Rf_error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    return force(&x);
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
        vl = my_getVarInFrame(rho, symbol, /* unbound_ok */ TRUE);
        if (vl != my_UnboundValue) {
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
    return my_UnboundValue;
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


/*
 * we used to do:
 * document.context$ofile <- NULL
 * to indicate that a document context does not refer to a file, now we do:
 * document.context <- emptyenv()
 * this is much easier to test for, to read, and to debug
 */


void _assign_ofile(SEXP ofile, SEXP documentcontext)
{
    INCREMENT_NAMED_defineVar(ofileSymbol, ofile, documentcontext);
}


void _assign_file(SEXP expr, SEXP documentcontext)
{
    Rf_protect(expr);
    R_MakeDelayedBinding(fileSymbol, expr, documentcontext, documentcontext);
    Rf_unprotect(1);
}


void _assign_default(SEXP srcfile_original, SEXP owd, SEXP trans_rights, SEXP documentcontext, NORMALIZE_ACTION na)
{
    Rf_protect(trans_rights);
    if (srcfile_original) {
        _assign_file(
            Rf_lcons(
                _normalizePath_srcfilealiasSymbol,
                Rf_cons(
                    srcfile_original,
                    Rf_cons(trans_rights, R_NilValue)
                )
            ),
            documentcontext
        );
    }
    else if (owd) {
        INCREMENT_NAMED_defineVar(wdSymbol, owd, documentcontext);
        SEXP sym;
        switch (na) {
        case NA_DEFAULT: sym = _normalizePath_againstSymbol        ; break;
        case NA_NOT_DIR: sym = _normalizePath_not_dir_againstSymbol; break;
        case NA_FIX_DIR: sym = _normalizePath_fix_dir_againstSymbol; break;
        default:
            Rf_errorcall(R_NilValue, _("invalid '%s' value"), "na");
            sym = R_NilValue;
        }
        _assign_file(
            Rf_lcons(sym, Rf_cons(wdSymbol, Rf_cons(trans_rights, R_NilValue))),
            documentcontext
        );
    }
    else {
        SEXP sym;
        switch (na) {
        case NA_DEFAULT: sym = _normalizePathSymbol        ; break;
        case NA_NOT_DIR: sym = _normalizePath_not_dirSymbol; break;
        case NA_FIX_DIR: sym = _normalizePath_fix_dirSymbol; break;
        default:
            Rf_errorcall(R_NilValue, _("invalid '%s' value"), "na");
            sym = R_NilValue;
        }
        _assign_file(
            Rf_lcons(sym, Rf_cons(trans_rights, R_NilValue)),
            documentcontext
        );
    }
    Rf_unprotect(1);
}


void assign_default(SEXP srcfile_original, SEXP owd, SEXP ofile, SEXP file, SEXP documentcontext, NORMALIZE_ACTION na)
{
    _assign_ofile(ofile, documentcontext);


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


    if ((srcfile_original || owd) && is_abs_path(url)) {
        if (!srcfile_original && owd) {
            /* assign 'wd' but do not use it programmatically */
            INCREMENT_NAMED_defineVar(wdSymbol, owd, documentcontext);
        }
        srcfile_original = NULL;
        owd = NULL;
    }


    _assign_default(srcfile_original, owd, Rf_ScalarString(Rf_mkCharCE(url, ienc)), documentcontext, na);
}


void assign_file_uri(SEXP srcfile_original, SEXP owd, SEXP ofile, SEXP file, SEXP documentcontext, NORMALIZE_ACTION na)
{
    _assign_ofile(ofile, documentcontext);


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


    if ((srcfile_original || owd) && is_abs_path(url + nh)) {
        if (!srcfile_original && owd) {
            /* assign 'wd' but do not use it programmatically */
            INCREMENT_NAMED_defineVar(wdSymbol, owd, documentcontext);
        }
        srcfile_original = NULL;
        owd = NULL;
    }


    _assign_default(srcfile_original, owd, Rf_ScalarString(Rf_mkCharCE(url + nh, ienc)), documentcontext, na);
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


    SEXP ofile = Rf_ScalarString(Rf_mkCharCE(_buf, Rf_getCharCE(description)));
    _assign_ofile(ofile, documentcontext);


    if ((srcfile_original || owd) && is_abs_path(url)) {
        if (!srcfile_original && owd) {
            /* assign 'wd' but do not use it programmatically */
            INCREMENT_NAMED_defineVar(wdSymbol, owd, documentcontext);
        }
        srcfile_original = NULL;
        owd = NULL;
    }


    _assign_default(srcfile_original, owd, Rf_ScalarString(description), documentcontext, na);
}


void assign_url(SEXP ofile, SEXP file, SEXP documentcontext)
{
    _assign_ofile(ofile, documentcontext);


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


    _assign_file(
        Rf_lcons(_normalizeURL_1Symbol,
            Rf_cons(Rf_ScalarString(Rf_mkCharCE(url, ienc)), R_NilValue)),
        documentcontext
    );
    forceInFrame(documentcontext, fileSymbol);
}


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
    if (!gui_rstudio) return my_UnboundValue;


    const char *what = "tools:rstudio";


    SEXP name;
    for (SEXP t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
        name = Rf_getAttrib(t, R_NameSymbol);
        if (Rf_isString(name) &&
            Rf_length(name) > 0 &&
            !strcmp(Rf_translateChar(STRING_ELT(name, 0)), what))
        {
            return my_getVarInFrame(t, debugSourceSymbol, TRUE);
        }
    }


    return my_UnboundValue;
}


SEXP call_path_join(SEXP x, SEXP y)
{
    static SEXP path_join_symbol = NULL;
    if (path_join_symbol == NULL)
        path_join_symbol = Rf_install("path.join");
    SEXP expr = Rf_lcons(path_join_symbol, Rf_cons(x, Rf_cons(y, R_NilValue)));
    Rf_protect(expr);
    SEXP value = Rf_eval(expr, mynamespace);
    Rf_unprotect(1);
    return value;
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
#if R_version_at_least(4,6,0)
        switch (R_GetBindingType(sym, env)) {
        case R_BindingTypeUnbound:
            Rf_error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
            break;
        case R_BindingTypeValue:
            INCREMENT_NAMED_defineVar(sym, my_getVarInFrame(env, sym, FALSE), value);
            break;
        case R_BindingTypeMissing:
            INCREMENT_NAMED_defineVar(sym, R_MissingArg, value);
            break;
        case R_BindingTypeDelayed:
            R_MakeDelayedBinding(
                /* sym        */ sym,
                /* expr       */ sym,
                /* eval_env   */ env,
                /* assign_env */ value
            );
            break;
        case R_BindingTypeForced:
            R_MakeForcedBinding(
                /* sym        */ sym,
                /* expr       */ R_ForcedBindingExpression(sym, env),
                /* value      */ my_getVarInFrame(env, sym, FALSE),
                /* assign_env */ value
            );
            break;
        case R_BindingTypeActive:
            R_MakeActiveBinding(sym, R_ActiveBindingFunction(sym, env), value);
            break;
        }
#else
#if R_version_at_least(4,0,0)
        if (R_BindingIsActive(sym, env))
            R_MakeActiveBinding(sym, R_ActiveBindingFunction(sym, env), value);
        else
#endif
        {
            binding_info_t x; my_findVarInFrame(env, sym, &x);
            if (x.value == my_UnboundValue)
                Rf_error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
            if (my_TYPEOF(x) == PROMSXP) {
                if (my_PRVALUE(x) == my_UnboundValue)
                    R_MakeDelayedBinding(sym, sym, env, value);
                else
                    R_MakeForcedBinding(sym, ptr_PRCODE(x.value), my_PRVALUE(x), value);
            }
            else INCREMENT_NAMED_defineVar(sym, x.value, value);
        }
#endif
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
