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
#if defined(R_THIS_PATH_DEVEL) || R_version_less_than(4,5,0)
#define if_DDVAL_ddfindVar                                     \
    if (DDVAL(sym))                                            \
        value = ddfindVar(sym, env);
#else
#define if_DDVAL_ddfindVar                                     \
    int i;                                                     \
    if ((i = ddVal(sym)))                                      \
        value = ddfind(i, env);
#endif
    if_DDVAL_ddfindVar
    else
        value = (inherits ? Rf_findVar(sym, env) : Rf_findVarInFrame(env, sym));
    if (value == R_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    return Rf_ScalarLogical(TYPEOF(value) == PROMSXP &&
                            ptr_PRVALUE(value) == R_UnboundValue);
}


SEXP do_promise_is_unevaluated do_formals
{
    /* similar to do_is_unevaluated_promise, but the binding MUST be a promise */


    do_start_no_op("promise_is_unevaluated", -1);


    handles_nargs(ENCLOS(rho), ".C_promise_is_unevaluated");


    SEXP value;
    if_DDVAL_ddfindVar
    else
        value = (inherits ? Rf_findVar(sym, env) : Rf_findVarInFrame(env, sym));
    if (value == R_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    if (TYPEOF(value) != PROMSXP)
        Rf_errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    return Rf_ScalarLogical(ptr_PRVALUE(value) == R_UnboundValue);
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
    if_DDVAL_ddfindVar
    else
        value = (inherits ? Rf_findVar(sym, env) : Rf_findVarInFrame(env, sym));
    if (value == R_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    if (TYPEOF(value) != PROMSXP)
        Rf_errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    if (ptr_PRVALUE(value) == R_UnboundValue) {
        Rf_protect(value);
#if defined(R_THIS_PATH_HAS_PRSEEN)
        if (PRSEEN(value)) {
            if (PRSEEN(value) == 1);
            else SET_PRSEEN(value, 0);
        }
        Rf_eval(value, env);
#else
        SEXP x = Rf_eval(ptr_PRCODE(value), ptr_PRENV(value));
        ptr_SET_PRVALUE(value, x);
        ENSURE_NAMEDMAX(x);
        ptr_SET_PRENV(value, R_NilValue);
#endif
        Rf_unprotect(1);
    }
    return ptr_PRVALUE(value);
}





SEXP makePROMISE(SEXP expr, SEXP env)
{
#if defined(R_THIS_PATH_HAS_PRSEEN)
    ENSURE_NAMEDMAX(expr);
    SEXP s = Rf_allocSExp(PROMSXP);
    ptr_SET_PRCODE(s, expr);
    ptr_SET_PRENV(s, env);
    ptr_SET_PRVALUE(s, R_UnboundValue);
    SET_PRSEEN(s, 0);
    SET_ATTRIB(s, R_NilValue);
    return s;
#else
    Rf_eval(expr_makePROMISE, R_EmptyEnv);
    SEXP s = Rf_findVarInFrame(makePROMISE_environment, xSymbol);
    Rf_protect(s);
    ptr_SET_PRCODE(s, expr);
    ptr_SET_PRENV(s, env);
    Rf_unprotect(1);
    return s;
#endif
}


SEXP makeEVPROMISE(SEXP expr, SEXP value)
{
    SEXP prom = makePROMISE(expr, R_NilValue);
    ptr_SET_PRVALUE(prom, value);
    return prom;
}





SEXP do_is_R_MissingArg do_formals
{
    do_start_no_op("is_R_MissingArg", -1);


    handles_nargs(rho, ".C_is_R_MissingArg");


    SEXP value;
    if_DDVAL_ddfindVar
    else
        value = (inherits ? Rf_findVar(sym, env) : Rf_findVarInFrame(env, sym));
    if (value == R_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    return ((value == R_MissingArg) ? R_TrueValue : R_FalseValue);
}
