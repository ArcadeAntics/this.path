/*
this.path : Get Executing Script's Path
Copyright (C) 2022-2026   Iris Simmons
 */


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


    int i;
    if ((i = ddVal(sym))) {
        SEXP value = ddfind(i, env);
        if (value == my_UnboundValue)
            Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
        return Rf_ScalarLogical(TYPEOF(value) == PROMSXP &&
                                ptr_PRVALUE(value) == my_UnboundValue);
    }
    binding_info_t value; (inherits ? my_findVar(env, sym, &value) :
                                      my_findVarInFrame(env, sym, &value));
    if (value.value == my_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    return Rf_ScalarLogical(my_TYPEOF(value) == PROMSXP &&
                            my_PRVALUE(value) == my_UnboundValue);
}


SEXP do_promise_is_unevaluated do_formals
{
    /* similar to do_is_unevaluated_promise, but the binding MUST be a promise */


    do_start_no_op("promise_is_unevaluated", -1);


    handles_nargs(ENCLOS(rho), ".C_promise_is_unevaluated");


    int i;
    if ((i = ddVal(sym))) {
        SEXP value = ddfind(i, env);
        if (value == my_UnboundValue)
            Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
        if (TYPEOF(value) != PROMSXP)
            Rf_errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));
        return Rf_ScalarLogical(ptr_PRVALUE(value) == my_UnboundValue);
    }
    binding_info_t value; (inherits ? my_findVar(env, sym, &value) :
                                      my_findVarInFrame(env, sym, &value));
    if (value.value == my_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    if (my_TYPEOF(value) != PROMSXP)
        Rf_errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    return Rf_ScalarLogical(my_PRVALUE(value) == my_UnboundValue);
}


SEXP do_forcePromise_no_warn do_formals
{
    /* return the result of getting a promise, silencing a possible warning
     * about "restarting interrupted promise evaluation"
     *
     * if the requested variable is not a promise, throw an error
     */


    do_start_no_op("forcePromise_no_warn", -1);
#if R_version_at_least(4,6,0)
    handles_nargs(ENCLOS(rho), ".C_forcePromise_no_warn");


    SEXP value;
    int i;
    if ((i = ddVal(sym))) {
        value = ddfind(i, env);
        if (value == my_UnboundValue)
            Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
        if (TYPEOF(value) != PROMSXP)
            Rf_errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));
        if (ptr_PRVALUE(value) == my_UnboundValue) {
            Rf_protect(value);
            Rf_eval(value, R_EmptyEnv);
            Rf_unprotect(1);
        }
        return ptr_PRVALUE(value);
    }
    binding_info_t tmp; (inherits ? my_findVar(sym, env, &tmp) :
                                    my_findVarInFrame(env, sym, &tmp));
    if (tmp.value == my_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    if (my_TYPEOF(tmp) != PROMSXP)
        Rf_errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    return force(&tmp);
#else
    handles_nargs(ENCLOS(rho), ".C_forcePromise_no_warn");


    SEXP value;
    int i;
    if ((i = ddVal(sym)))
        value = ddfind(i, env);
    else {
        binding_info_t tmp; (inherits ? my_findVar(sym, env, &tmp) :
                                        my_findVarInFrame(env, sym, &tmp));
        value = tmp.value;
    }
    if (value == my_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    if (TYPEOF(value) != PROMSXP)
        Rf_errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    if (ptr_PRVALUE(value) == my_UnboundValue) {
        Rf_protect(value);
        switch (ptr_PRSEEN(value)) {
        case 0: case 1: break;
        default: ptr_SET_PRSEEN(value, 0);
        }
        Rf_eval(value, env);
        Rf_unprotect(1);
    }
    return ptr_PRVALUE(value);
#endif
}


SEXP do_is_R_MissingArg do_formals
{
    do_start_no_op("is_R_MissingArg", -1);


    handles_nargs(rho, ".C_is_R_MissingArg");


    SEXP value;
    int i;
    if ((i = ddVal(sym)))
        value = ddfind(i, env);
    else
        value = (inherits ? my_findVal(env, sym) : my_findValInFrame(env, sym));
    if (value == my_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));


    return ((value == R_MissingArg) ? R_TrueValue : R_FalseValue);
}
