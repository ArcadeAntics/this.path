#include "thispathdefn.h"





SEXP R_getS4DataSlot(SEXP obj, SEXPTYPE type)
{
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


#define _get_sym(elsecode)                                     \
    sym = CAR(args);                                           \
    if (TYPEOF(sym) == SYMSXP) {}                              \
    else if (isValidStringF(sym)) {                            \
        if (XLENGTH(sym) > 1)                                  \
            errorcall(call, _("first argument has length > 1"));\
        sym = installTrChar(STRING_ELT(sym, 0));               \
    }                                                          \
    else elsecode


#define get_sym _get_sym(errorcall(call, _("invalid first argument")))


#define get_env                                                \
        env = CADR(args);                                      \
        if (!isEnvironment(env) &&                             \
            !isEnvironment(env = simple_as_environment(env)))  \
            errorcall(call, "invalid second argument")


#define get_inherits                                           \
        inherits = asLogical(CADDR(args));                     \
        if (inherits == NA_LOGICAL)                            \
            errorcall(call, "invalid third argument")


#define handles_nargs(one_arg_env, name)                       \
    SEXP sym, env;                                             \
    Rboolean inherits;                                         \
    switch (length(args)) {                                    \
    case 1:                                                    \
        get_sym;                                               \
        env = (one_arg_env);                                   \
        inherits = FALSE;                                      \
        break;                                                 \
    case 2:                                                    \
        get_sym;                                               \
        get_env;                                               \
        inherits = FALSE;                                      \
        break;                                                 \
    case 3:                                                    \
        get_sym;                                               \
        get_env;                                               \
        get_inherits;                                          \
        break;                                                 \
    default:                                                   \
        errorcall(call, wrong_nargs_to_External(length(args), (name), "1, 2, or 3"));\
        return R_NilValue;                                     \
    }





SEXP do_isunevaluatedpromise do_formals
{
    /*
    return TRUE if get(sym, env, inherits = FALSE) will force a promise
    return FALSE otherwise
     */


    do_start_no_op("isunevaluatedpromise", -1);


    handles_nargs(rho, ".C_isunevaluatedpromise");


    SEXP value = (inherits ? findVar(sym, env) : findVarInFrame(env, sym));
    if (value == R_UnboundValue)
        errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    return ScalarLogical(TYPEOF(value) == PROMSXP &&
                         PRVALUE(value) == R_UnboundValue);
}


SEXP do_promiseisunevaluated do_formals
{
    /* similar to do_isunevaluatedpromise, but the binding MUST be a promise */


    do_start_no_op("promiseisunevaluated", -1);


    handles_nargs(ENCLOS(rho), ".C_promiseisunevaluated");


    SEXP value = (inherits ? findVar(sym, env) : findVarInFrame(env, sym));
    if (value == R_UnboundValue)
        errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    if (TYPEOF(value) != PROMSXP)
        errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    return ScalarLogical(PRVALUE(value) == R_UnboundValue);
}


SEXP do_getpromisewithoutwarning do_formals
{
    /* return the result of getting a promise, silencing a possible warning
     * about "restarting interrupted promise evaluation"
     *
     * if the requested variable is not a promise, throw an error
     */


    do_start_no_op("getpromisewithoutwarning", -1);


    handles_nargs(ENCLOS(rho), ".C_getpromisewithoutwarning");


    SEXP value = (inherits ? findVar(sym, env) : findVarInFrame(env, sym));
    if (value == R_UnboundValue)
        errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    if (TYPEOF(value) != PROMSXP)
        errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


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


SEXP do_PRINFO do_formals
{
    do_start_no_op("PRINFO", -1);


    int nargs = length(args);


    SEXP sym, env = rho;
    int inherits = TRUE;


    switch (nargs) {
    case 3:
        inherits = asLogical(CADDR(args));
        if (inherits == NA_LOGICAL)
            errorcall(call, _("invalid '%s' argument"), "inherits");
    case 2:
        env = CADR(args);
        if (!isEnvironment(env) &&
            !isEnvironment(env = simple_as_environment(env)))
        {
            errorcall(call, _("invalid '%s' argument"), "envir");
        }
    case 1:
        _get_sym({
            if (TYPEOF(sym) == PROMSXP)
                return PRINFO(sym);
            errorcall(call, _("invalid '%s' argument"), "x");
        })
        break;
    default:
        errorcall(call, wrong_nargs_to_External(nargs, ".C_PRINFO", "1, 2, or 3"));
        return R_NilValue;
    }


    if (sym == R_MissingArg)
        error(_("argument \"%s\" is missing, with no default"), "x");


    SEXP e = (inherits ? findVar(sym, env) : findVarInFrame(env, sym));
    if (e == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    if (TYPEOF(e) != PROMSXP)
        error("'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    return PRINFO(e);
}





Rboolean validJupyterRNotebook(SEXP path)
{
    SEXP expr = LCONS(_validJupyterRNotebookSymbol, CONS(path, R_NilValue));
    PROTECT(expr);
    SEXP value = eval(expr, mynamespace);
    if (TYPEOF(value) != LGLSXP || LENGTH(value) != 1L || LOGICAL(value)[0] == NA_LOGICAL)
        errorcall(expr, "invalid return value");
    UNPROTECT(1);
    return LOGICAL(value)[0];
}


#include "drivewidth.h"


SEXP do_setsyspathjupyter do_formals
{
    do_start_no_op_rho("setsyspathjupyter", -1);


    SEXP path;
    Rboolean skipCheck = FALSE;
    switch (length(args)) {
    case 1:
        path = CAR(args);
        break;
    case 2:
        path = CAR(args);
        skipCheck = asLogical(CADR(args));
        if (skipCheck == NA_LOGICAL)
            errorcall(call, _("invalid '%s' argument"), "skipCheck");
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), ".C_setsyspathjupyter", "1 or 2"));
        return R_NilValue;
    }


    if (TYPEOF(path) != STRSXP || LENGTH(path) != 1L)
        errorcall(call, _("'%s' must be a character string"), "path");
    if (STRING_ELT(path, 0) == NA_STRING) {}
#ifdef _WIN32
    else if (is_abs_path_windows(CHAR(STRING_ELT(path, 0)))) {}
#else
    else if (is_abs_path_unix(CHAR(STRING_ELT(path, 0)))) {}
#endif
    else errorcall(call, _("invalid '%s' argument"), "path");


    if (skipCheck || STRING_ELT(path, 0) == NA_STRING || validJupyterRNotebook(path)) {}
    else errorcall(call, "invalid '%s' argument; must be a valid Jupyter R notebook", "path");


    SEXP sym, env = getFromMyNS(_sys_path_jupyterSymbol);
    if (TYPEOF(env) != CLOSXP)
        errorcall(call, "'%s' is not a closure", EncodeChar(PRINTNAME(_sys_path_jupyterSymbol)));
    env = CLOENV(env);


    /* attempt to get the promise */
    sym = fileSymbol;
    SEXP e = findVarInFrame(env, sym);
    if (e == R_UnboundValue)
        errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    if (TYPEOF(e) != PROMSXP)
        errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    /* attempt to unlock the original file's binding */
    sym = ofileSymbol;
    R_unLockBinding(sym, env);


    /* restore the promise to its original state */
    SET_PRENV(e, env);
    SET_PRVALUE(e, R_UnboundValue);


    /* define the variable and re-lock the binding */
    INCREMENT_NAMED_defineVar(sym, path, env);
    R_LockBinding(sym, env);


    set_R_Visible(FALSE);
    return path;
}





SEXP do_mkPROMISE do_formals
{
    do_start_no_call_op_rho("mkPROMISE", 2);


    SEXP expr = CAR(args); args = CDR(args);
    SEXP env  = CAR(args); args = CDR(args);
    if (!isEnvironment(env)) error(_("not an environment"));


    return makePROMISE(expr, env);
}


SEXP do_mkEVPROMISE do_formals
{
    do_start_no_call_op_rho("mkEVPROMISE", 2);


    return makeEVPROMISE(CAR(args), CADR(args));
}


#define FRAME_LOCK_MASK (1<<14)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))


#if R_version_less_than(3, 2, 0)
SEXP R_lsInternal3(SEXP env, Rboolean all, Rboolean sorted)
{
    static SEXP sortedSymbol    = NULL,
                all_namesSymbol = NULL,
                lsSymbol        = NULL;
    if (sortedSymbol == NULL) {
        sortedSymbol    = install("sorted");
        all_namesSymbol = install("all.names");
        lsSymbol        = install("ls");
    }


    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = CONS(R_FalseValue, R_NilValue), &indx);
    SET_TAG(expr, sortedSymbol);
    REPROTECT(expr = CONS(R_TrueValue, expr), indx);
    SET_TAG(expr, all_namesSymbol);
    REPROTECT(expr = CONS(env, expr), indx);
    SET_TAG(expr, envirSymbol);
    REPROTECT(expr = LCONS(getFromBase(lsSymbol), expr), indx);


    SEXP value = eval(expr, R_EmptyEnv);
    UNPROTECT(1);
    return value;
}
#endif


void unLockEnvironment(SEXP env, Rboolean bindings)
{
    if (IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
	    env = R_getS4DataSlot(env, ANYSXP); /* better be an ENVSXP */

    if (TYPEOF(env) != ENVSXP)
	    error(_("not an environment"));
    if (bindings) {
        SEXP names = R_lsInternal3(env, /* all */ TRUE, /* sorted */ FALSE);
        PROTECT(names);
        for (int i = 0, n = LENGTH(names); i < n; i++)
            R_unLockBinding(installTrChar(STRING_ELT(names, i)), env);
        UNPROTECT(1);
    }
    UNLOCK_FRAME(env);
}


SEXP do_unlockEnvironment do_formals
{
    do_start_no_op_rho("unlockEnvironment", -1);


    SEXP frame;
    Rboolean bindings = FALSE;
    switch (length(args)) {
    case 2:
        bindings = asLogical(CADR(args));
    case 1:
        frame = CAR(args);
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), ".C_unlockEnvironment", "1 or 2"));
        return R_NilValue;
    }


    unLockEnvironment(frame, bindings);
    set_R_Visible(FALSE);
    return R_NilValue;
}
