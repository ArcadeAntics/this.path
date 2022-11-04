#include <R.h>
#include <Rinternals.h>


#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("R", String)
#else
#define _(String) (String)
#endif


extern void (SET_PRSEEN)(SEXP x, int v);





#define _get_sym(elsecode)                                     \
    sym = CADR(args);                                          \
    if (TYPEOF(sym) == SYMSXP) {}                              \
    else if (isValidStringF(sym)) {                            \
        if (XLENGTH(sym) > 1)                                  \
            errorcall(call, "first argument has length > 1");  \
        sym = installTrChar(STRING_ELT(sym, 0));               \
    }                                                          \
    else elsecode


#define get_sym _get_sym(errorcall(call, "invalid first argument");)


#define handles_nargs(one_arg_env, name)                       \
    switch (length(args) - 1) {                                \
    case 1:                                                    \
        get_sym                                                \
        env = (one_arg_env);                                   \
        break;                                                 \
    case 2:                                                    \
        get_sym                                                \
        env = CADDR(args);                                     \
        if (TYPEOF(env) != ENVSXP)                             \
            errorcall(call, "invalid second argument");        \
        break;                                                 \
    default:                                                   \
        errorcall(call, "%d arguments passed to .External(%s) which requires 1 or 2", length(args) - 1, (name));\
    }





SEXP do_isunevaluatedpromise(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* return TRUE if get(sym, env, inherits = FALSE) will force a promise,
     * return FALSE otherwise
     */


    SEXP sym, env;
    handles_nargs(rho, "C_isunevaluatedpromise");


    SEXP value = findVarInFrame(env, sym);
    if (value == R_UnboundValue)
        errorcall(call, "object '%s' not found", CHAR(PRINTNAME(sym)));


    return ScalarLogical(TYPEOF(value) == PROMSXP &&
                         PRVALUE(value) == R_UnboundValue);
}


SEXP do_getpromisewithoutwarning(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* return the result of getting a promise, silencing a possible warning
     * about "restarting interrupted promise evaluation"
     *
     * if the requested variable is not a promise, throw an error
     */


    SEXP sym, env;
    handles_nargs(ENCLOS(rho), "C_getpromisewithoutwarning");


    SEXP value = findVarInFrame(env, sym);
    if (value == R_UnboundValue)
        errorcall(call, "object '%s' not found", CHAR(PRINTNAME(sym)));


    if (TYPEOF(value) != PROMSXP)
        errorcall(call, "'%s' is not a promise", CHAR(PRINTNAME(sym)));


    if (PRVALUE(value) == R_UnboundValue) {
        if (PRSEEN(value)) {
            if (PRSEEN(value) == 1) {}
            else SET_PRSEEN(value, 0);
        }
        eval(value, env);
    }


    return PRVALUE(value);
}





SEXP PRINFO(SEXP e)
{
    if (TYPEOF(e) != PROMSXP)
        error("in PRINFO: argument is not a promise");


    /*
     * PRCODE
     * PRENV
     * PREXPR
     * PRSEEN
     * PRVALUE
     */


#define n 4
#define allocate_value_and_names(len)                          \
        value = allocVector(VECSXP, len);                      \
        PROTECT(value);                                        \
        names = allocVector(STRSXP, len);                      \
        setAttrib(value, R_NamesSymbol, names);


    SEXP value, names;
    if (PRVALUE(e) == R_UnboundValue) {
        allocate_value_and_names(n)
    }
    else {
        allocate_value_and_names(n + 1)
        SET_VECTOR_ELT(value, n, PRVALUE(e));
        SET_STRING_ELT(names, n, mkChar("PRVALUE"));
    }


#undef n
#undef allocate_value_and_names


    SET_VECTOR_ELT(value, 0,               PRCODE(e) );
    SET_VECTOR_ELT(value, 1,               PRENV (e) );
    SET_VECTOR_ELT(value, 2,               PREXPR(e) );
    SET_VECTOR_ELT(value, 3, ScalarInteger(PRSEEN(e)));


    SET_STRING_ELT(names, 0, mkChar("PRCODE"));
    SET_STRING_ELT(names, 1, mkChar("PRENV" ));
    SET_STRING_ELT(names, 2, mkChar("PREXPR"));
    SET_STRING_ELT(names, 3, mkChar("PRSEEN"));


    UNPROTECT(1);
    return value;
}


SEXP R_getS4DataSlot(SEXP obj, SEXPTYPE type)
{
    static SEXP _xDataSymbol = NULL,
                _DataSymbol  = NULL;
    if (_xDataSymbol == NULL) {
        _xDataSymbol = install(".xData");
        _DataSymbol  = install(".Data");
    }


    SEXP value = getAttrib(obj, _DataSymbol);
    if (value == R_NilValue)
        value = getAttrib(obj, _xDataSymbol);
    if (value != R_NilValue &&
        (type == ANYSXP || type == TYPEOF(value)))
    {
        return value;
    }
    else return R_NilValue;
}
#define simple_as_environment(arg) (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) : R_NilValue)


SEXP do_prinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nargs = length(args) - 1;


    SEXP sym, env = rho;
    int inherits = TRUE;


    switch (nargs) {
    case 3:
        inherits = asLogical(CADDDR(args));
        if (inherits == NA_LOGICAL)
            errorcall(call, _("invalid '%s' argument"), "inherits");
    case 2:
        env = CADDR(args);
        if (!isEnvironment(env) &&
            !isEnvironment(env = simple_as_environment(env)))
        {
            errorcall(call, _("invalid '%s' argument"), "envir");
        }
    case 1:
        _get_sym({
            if (TYPEOF(sym) != PROMSXP)
                errorcall(call, _("invalid '%s' argument"), "x");
            return PRINFO(sym);
        })
        break;
    default:
        errorcall(call, "%d arguments passed to .External(%s) which requires 1, 2, or 3", nargs, "C_prinfo");
    }


    if (sym == R_MissingArg)
        error(_("argument \"%s\" is missing, with no default"), "x");


    SEXP e = (inherits ? findVar(sym, env) : findVarInFrame(env, sym));
    if (e == R_UnboundValue)
        error(_("object '%s' not found"), CHAR(PRINTNAME(sym)));
    if (TYPEOF(e) != PROMSXP)
        error("'%s' is not a promise", CHAR(PRINTNAME(sym)));


    return PRINFO(e);
}
