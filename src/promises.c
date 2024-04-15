#include "thispathdefn.h"





SEXP R_getS4DataSlot(SEXP obj, SEXPTYPE type)
{
    SEXP value = Rf_getAttrib(obj, _DataSymbol);
    if (value == R_NilValue)
        value = Rf_getAttrib(obj, _xDataSymbol);
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
    if (TYPEOF(sym) == SYMSXP);                                \
    else if (Rf_isValidStringF(sym)) {                         \
        if (XLENGTH(sym) > 1)                                  \
            Rf_errorcall(call, _("first argument has length > 1"));\
        sym = Rf_installTrChar(STRING_ELT(sym, 0));            \
    }                                                          \
    else elsecode


#define get_sym _get_sym(Rf_errorcall(call, _("invalid first argument")))


#define get_env                                                \
        env = CADR(args);                                      \
        if (!Rf_isEnvironment(env) &&                          \
            !Rf_isEnvironment(env = simple_as_environment(env)))\
            Rf_errorcall(call, "invalid second argument")


#define get_inherits                                           \
        inherits = Rf_asLogical(CADDR(args));                  \
        if (inherits == NA_LOGICAL)                            \
            Rf_errorcall(call, "invalid third argument")


#define handles_nargs(one_arg_env, name)                       \
    SEXP sym, env;                                             \
    Rboolean inherits;                                         \
    switch (Rf_length(args)) {                                 \
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
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), (name), "1, 2, or 3"));\
        return R_NilValue;                                     \
    }





SEXP do_is_unevaluated_promise do_formals
{
    /*
    return TRUE if get(sym, env, inherits = FALSE) will force a promise
    return FALSE otherwise
     */


    do_start_no_op("is_unevaluated_promise", -1);


    handles_nargs(rho, ".C_is_unevaluated_promise");


    SEXP value;
    if (DDVAL(sym))
        value = ddfindVar(sym, env);
    else
        value = (inherits ? Rf_findVar(sym, env) : Rf_findVarInFrame(env, sym));
    if (value == R_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    return Rf_ScalarLogical(TYPEOF(value) == PROMSXP &&
                            PRVALUE(value) == R_UnboundValue);
}


SEXP do_promise_is_unevaluated do_formals
{
    /* similar to do_is_unevaluated_promise, but the binding MUST be a promise */


    do_start_no_op("promise_is_unevaluated", -1);


    handles_nargs(ENCLOS(rho), ".C_promise_is_unevaluated");


    SEXP value;
    if (DDVAL(sym))
        value = ddfindVar(sym, env);
    else
        value = (inherits ? Rf_findVar(sym, env) : Rf_findVarInFrame(env, sym));
    if (value == R_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    if (TYPEOF(value) != PROMSXP)
        Rf_errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    return Rf_ScalarLogical(PRVALUE(value) == R_UnboundValue);
}


SEXP do_forcePromise_no_warn do_formals
{
    /* return the result of getting a promise, silencing a possible warning
     * about "restarting interrupted promise evaluation"
     *
     * if the requested variable is not a promise, throw an error
     */


    do_start_no_op("forcePromise_no_warn", -1);


    handles_nargs(ENCLOS(rho), ".C_forcePromise_no_warn");


    SEXP value;
    if (DDVAL(sym))
        value = ddfindVar(sym, env);
    else
        value = (inherits ? Rf_findVar(sym, env) : Rf_findVarInFrame(env, sym));
    if (value == R_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    if (TYPEOF(value) != PROMSXP)
        Rf_errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    if (PRVALUE(value) == R_UnboundValue) {
        if (PRSEEN(value)) {
            if (PRSEEN(value) == 1);
            else SET_PRSEEN(value, 0);
        }
        Rf_protect(value);
        Rf_eval(value, env);
        Rf_unprotect(1);
    }
    return PRVALUE(value);
}





SEXP PRINFO(SEXP e)
{
    if (TYPEOF(e) != PROMSXP)
        Rf_error("in PRINFO: argument is not a promise");


    /*
     * PRCODE
     * PRENV
     * PREXPR
     * PRSEEN
     * PRVALUE
     */


#define n 4
#define allocate_value_and_names(len)                          \
        value = Rf_allocVector(VECSXP, len);                   \
        Rf_protect(value);                                     \
        names = Rf_allocVector(STRSXP, len);                   \
        Rf_setAttrib(value, R_NamesSymbol, names)


    SEXP value, names;
    if (PRVALUE(e) == R_UnboundValue) {
        allocate_value_and_names(n);
    }
    else {
        allocate_value_and_names(n + 1);
        SET_VECTOR_ELT(value, n, PRVALUE(e));
        SET_STRING_ELT(names, n, Rf_mkChar("PRVALUE"));
    }


#undef n
#undef allocate_value_and_names


    SET_VECTOR_ELT(value, 0,                  PRCODE(e) );
    SET_VECTOR_ELT(value, 1,                  PRENV (e) );
    SET_VECTOR_ELT(value, 2,                  PREXPR(e) );
    SET_VECTOR_ELT(value, 3, Rf_ScalarInteger(PRSEEN(e)));


    SET_STRING_ELT(names, 0, Rf_mkChar("PRCODE"));
    SET_STRING_ELT(names, 1, Rf_mkChar("PRENV" ));
    SET_STRING_ELT(names, 2, Rf_mkChar("PREXPR"));
    SET_STRING_ELT(names, 3, Rf_mkChar("PRSEEN"));


    Rf_unprotect(1);
    return value;
}


SEXP do_PRINFO do_formals
{
    do_start_no_op("PRINFO", -1);


    int nargs = Rf_length(args);


    SEXP sym, env = rho;
    int inherits = TRUE;


    switch (nargs) {
    case 3:
        inherits = Rf_asLogical(CADDR(args));
        if (inherits == NA_LOGICAL)
            Rf_errorcall(call, _("invalid '%s' argument"), "inherits");
    case 2:
        env = CADR(args);
        if (!Rf_isEnvironment(env) &&
            !Rf_isEnvironment(env = simple_as_environment(env)))
        {
            Rf_errorcall(call, _("invalid '%s' argument"), "envir");
        }
    case 1:
        _get_sym({
            if (TYPEOF(sym) == PROMSXP)
                return PRINFO(sym);
            Rf_errorcall(call, _("invalid '%s' argument"), "x");
        })
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(nargs, ".C_PRINFO", "1, 2, or 3"));
        return R_NilValue;
    }


    if (sym == R_MissingArg)
        Rf_error(_("argument \"%s\" is missing, with no default"), "x");


    SEXP e;
    if (DDVAL(sym))
        e = ddfindVar(sym, env);
    else
        e = (inherits ? Rf_findVar(sym, env) : Rf_findVarInFrame(env, sym));
    if (e == R_UnboundValue)
        Rf_error(_("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    if (TYPEOF(e) != PROMSXP)
        Rf_error("'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    return PRINFO(e);
}





SEXP makePROMISE(SEXP expr, SEXP env)
{
    ENSURE_NAMEDMAX(expr);
    SEXP s = Rf_allocSExp(PROMSXP);
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





SEXP do_mkPROMISE do_formals
{
    do_start_no_call_op_rho("mkPROMISE", 2);


    SEXP expr = CAR(args); args = CDR(args);
    SEXP env  = CAR(args); args = CDR(args);
    if (!Rf_isEnvironment(env)) Rf_error(_("not an environment"));


    return makePROMISE(expr, env);
}


SEXP do_mkEVPROMISE do_formals
{
    do_start_no_call_op_rho("mkEVPROMISE", 2);
    return makeEVPROMISE(CAR(args), CADR(args));
}





#define FRAME_LOCK_MASK (1<<14)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))


void unLockEnvironment(SEXP env, Rboolean bindings)
{
    if (IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
	    env = R_getS4DataSlot(env, ANYSXP); /* better be an ENVSXP */

    if (TYPEOF(env) != ENVSXP)
	    Rf_error(_("not an environment"));
    if (bindings) {
        Rf_protect(env);
        SEXP names = R_lsInternal3(env, /* all */ TRUE, /* sorted */ FALSE);
        Rf_protect(names);
        for (int i = 0, n = LENGTH(names); i < n; i++)
            R_unLockBinding(Rf_installTrChar(STRING_ELT(names, i)), env);
        Rf_unprotect(2);
    }
    UNLOCK_FRAME(env);
}


SEXP do_unlockEnvironment do_formals
{
    do_start_no_op_rho("unlockEnvironment", -1);


    SEXP frame;
    Rboolean bindings = FALSE;
    switch (Rf_length(args)) {
    case 2:
        bindings = Rf_asLogical(CADR(args));
    case 1:
        frame = CAR(args);
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), ".C_unlockEnvironment", "1 or 2"));
        return R_NilValue;
    }


    unLockEnvironment(frame, bindings);
    set_R_Visible(FALSE);
    return R_NilValue;
}





SEXP do_is_R_MissingArg do_formals
{
    do_start_no_op("is_R_MissingArg", -1);


    handles_nargs(rho, ".C_is_R_MissingArg");


    SEXP value;
    if (DDVAL(sym))
        value = ddfindVar(sym, env);
    else
        value = (inherits ? Rf_findVar(sym, env) : Rf_findVarInFrame(env, sym));
    if (value == R_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    return ((value == R_MissingArg) ? R_TrueValue : R_FalseValue);
}
