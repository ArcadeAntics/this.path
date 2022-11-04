#include <R.h>
#include <Rinternals.h>


#include "thispathdefn.h"


SEXP do_makepromise(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP expr, env;
    int seen;
    SEXP value;
    switch (length(args) - 1) {
    case 2:
        expr  = CADR(args);
        env   = R_EmptyEnv;
        seen  = 0;
        value = CADDR(args);
        break;
    case 3:
        expr  = CADR(args);
        env   = CADDR(args);
        if (!isEnvironment(env))
            errorcall(call, "invalid second argument, must be an environment");
        seen  = asInteger(CADDDR(args));
        value = R_UnboundValue;
        break;
    default:
        errorcall(call, "%d arguments passed to .External(%s) which requires 2 or 3",
            length(args) - 1, "C_makepromise");
    }


    SEXP assign_env = R_NewEnv(R_EmptyEnv, TRUE, 1);
    PROTECT(assign_env); nprotect++;
    SEXP tmp = lang5(
        findVarInFrame(R_BaseEnv, delayedAssignSymbol),
        mkString("x"),
        expr,
        env,
        assign_env
    );
    PROTECT(tmp); nprotect++;
    eval(tmp, R_EmptyEnv);
    set_R_Visible(1);


    SEXP promise = findVarInFrame(assign_env, install("x"));
    if (promise == R_UnboundValue ||
        TYPEOF(promise) != PROMSXP)
    {
        error("'x' is not a promise; should never happen, please report!");
    }


    if (value == R_UnboundValue) {
        SET_PRSEEN(promise, seen);
        // SET_PRVALUE(promise, value);
        // SET_PRENV(promise, env);
    } else {
        SET_PRSEEN(promise, 0);
        SET_PRVALUE(promise, value);
        SET_PRENV(promise, R_NilValue);
    }


    UNPROTECT(nprotect);
    return promise;
}


SEXP do_setprseen2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ptr = CADR(args);
    if (TYPEOF(ptr) != EXTPTRSXP)
        errorcall(call, "invalid first argument, must be an external pointer");
    SEXP promises = R_ExternalPtrProtected(ptr);
    /* 'promises' is an empty pairlist, so nothing to do */
    if (TYPEOF(promises) == NILSXP)
        return R_NilValue;
    if (TYPEOF(promises) != LISTSXP)
        errorcall(call, "invalid first argument, 'R_ExternalPtrProtected()' must be a pairlist");
    SEXP x;
    for (x = promises ; x != R_NilValue ; x = CDR(x)) {
        if (TYPEOF(CAR(x)) != PROMSXP)
            errorcall(call, "invalid first argument, 'R_ExternalPtrProtected()' must be a pairlist of promises, not type '%s'", type2char(TYPEOF(CAR(x))));
        if (PRSEEN(CAR(x)) != 1)
            errorcall(call, "invalid first argument, 'R_ExternalPtrProtected()' contains a promise in which PRSEEN is not 1");
        if (PRVALUE(CAR(x)) != R_UnboundValue)
            errorcall(call, "invalid first argument, 'R_ExternalPtrProtected()' contains a promise for which 'PRVALUE()' is defined");
    }
    /* now that we know the list is safe to modify, change all the PRSEEN
       value to 2, the value used for interrupted promises */
    for (x = promises ; x != R_NilValue ; x = CDR(x))
        SET_PRSEEN(CAR(x), 2);
    return R_NilValue;
}


SEXP findfiletheneval(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    int character_only, file_only;


    character_only = asLogical(CADR(args));
    if (character_only == NA_LOGICAL)
        error(_("invalid '%s' argument"), "character.only");
    file_only = asLogical(CADDR(args));
    if (file_only == NA_LOGICAL)
        error(_("invalid '%s' argument"), "file.only");


    SEXP promise = findVarInFrame(rho, exprSymbol);
    if (promise == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(exprSymbol)));
    if (TYPEOF(promise) != PROMSXP)
        error("invalid '%s', must be a promise; should never happen, please report!", EncodeChar(PRINTNAME(exprSymbol)));


    SEXP ptr = R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);
    PROTECT(ptr); nprotect++;


    SEXP promises = R_NilValue;


    /* we will be modifying the PRSEEN values of the promises,
     * but we need to change them to 0 or 2 depending upon whether
     * this returns a value as normal or the promises get interupted
     *
     * save the root promise, the original code of the promise,
     * and make an on.exit() call that will restore said promise
     */
    ENSURE_NAMEDMAX(ptr);
    eval(lang2(on_exitSymbol, lang3(External2Symbol, C_setprseen2Symbol, ptr)), rho);


#define check_validity                                         \
    do {                                                       \
        if (PRVALUE(promise) != R_UnboundValue)                \
            error("invalid '%s', must be a call when '%s' is missing", EncodeChar(PRINTNAME(exprSymbol)), "file");\
        R_SetExternalPtrProtected(ptr, promises = CONS(promise, promises));\
        if (PRSEEN(promise)) {                                 \
            if (PRSEEN(promise) == 1)                          \
                error(_("promise already under evaluation: recursive default argument reference or earlier problems?"));\
            else {                                             \
                SET_PRSEEN(promise, 1);                        \
                warning(_("restarting interrupted promise evaluation"));\
            }                                                  \
        }                                                      \
        else SET_PRSEEN(promise, 1);                           \
    } while (0)


#define set_prvalues_then_return(val)                          \
    do {                                                       \
        for (SEXP x = promises ; x != R_NilValue ; x = CDR(x)) {\
            SEXP p = CAR(x);                                   \
            SET_PRSEEN (p, 0);                                 \
            SET_PRVALUE(p, (val));                             \
            ENSURE_NAMEDMAX((val));                            \
            SET_PRENV  (p, R_NilValue);                        \
        }                                                      \
        R_SetExternalPtrProtected(ptr, R_NilValue);            \
        UNPROTECT(nprotect);                                   \
        return (val);                                          \
    } while (0)


    check_validity;


    /* find the root promise */
    while (TYPEOF(PREXPR(promise)) == PROMSXP) {
        promise = PREXPR(promise);
        check_validity;
    }


    SEXP expr = PREXPR(promise),
         env  = PRENV (promise);


    if (expr == R_MissingArg)
        error(_("argument \"%s\" is missing, with no default"), EncodeChar(PRINTNAME(exprSymbol)));
    if (TYPEOF(expr) != LANGSXP)
        error("invalid '%s', must be a call when '%s' is missing", EncodeChar(PRINTNAME(exprSymbol)), "file");


    if (length(expr) < 2) {
        SEXP val = eval(PRCODE(promise), env);
        set_prvalues_then_return(val);
    }


    SEXP oexpr = expr;
    expr = duplicate(expr);
    PROTECT(expr); nprotect++;


    SEXP fun     = CAR(expr),
         funargs = CDR(expr);
    if (TYPEOF(fun) == SYMSXP)
        fun = findFun3(fun, env, expr);
    else
        fun = eval(fun, env);
    PROTECT(fun); nprotect++;


    switch (TYPEOF(fun)) {
    case SPECIALSXP:
    case BUILTINSXP:
    case CLOSXP:
        break;
    default:
        error(_("attempt to apply non-function"));
    }


    /* change the function so that it is not evaluated twice */
    SETCAR(expr, fun);


    SEXP s, b, dots, dot;
    int already_checked_dots;


    SEXP tag;
    if (TYPEOF(fun) == CLOSXP)
        tag = TAG(FORMALS(fun));
    else
        tag = R_NilValue;


    int i, n = -1;


    if (tag != R_NilValue && tag != R_DotsSymbol) {


        s = NULL;
        dots = NULL;
        /* exact matches */
        already_checked_dots = 0;
        for (b = funargs, i = 0 ; b != R_NilValue ; b = CDR(b), i++) {
            if (CAR(b) == R_DotsSymbol) {
                if (already_checked_dots)
                    continue;
                already_checked_dots = 1;
                dots = findVar(R_DotsSymbol, env);
                if (TYPEOF(dots) == DOTSXP) {
                    for (dot = dots ; dot != R_NilValue ; dot = CDR(dot)) {
                        if (TAG(dot) != R_NilValue && pmatch(tag, TAG(dot), 1)) {
                            if (s != NULL)
                                errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), CHAR(PRINTNAME(tag)));
                            else
                                s = CAR(dot);
                            if (TYPEOF(s) != PROMSXP)
                                error(_("value in '...' is not a promise"));
                        }
                    }
                }
                else if (dots != R_NilValue && dots != R_MissingArg)
                    error(_("'...' used in an incorrect context"));
            }
            else if (TAG(b) != R_NilValue && pmatch(tag, TAG(b), 1)) {
                if (s != NULL)
                    errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), CHAR(PRINTNAME(tag)));
                else
                    s = CAR(b);
                n = i;
            }
        }


        if (s == NULL) {
            /* partial matches */
            already_checked_dots = 0;
            for (b = funargs, i = 0 ; b != R_NilValue ; b = CDR(b), i++) {
                if (CAR(b) == R_DotsSymbol) {
                    if (already_checked_dots)
                        continue;
                    already_checked_dots = 1;
                    if (TYPEOF(dots) == DOTSXP) {
                        for (dot = dots ; dot != R_NilValue ; dot = CDR(dot)) {
                            if (TAG(dot) != R_NilValue && pmatch(tag, TAG(dot), 0)) {
                                if (s != NULL)
                                    errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), CHAR(PRINTNAME(tag)));
                                else
                                    s = CAR(dot);
                                if (TYPEOF(s) != PROMSXP)
                                    error(_("value in '...' is not a promise"));
                            }
                        }
                    }
                    else if (dots != R_NilValue && dots != R_MissingArg)
                        error(_("'...' used in an incorrect context"));
                }
                else if (TAG(b) != R_NilValue && pmatch(tag, TAG(b), 0)) {
                    if (s != NULL)
                        errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), CHAR(PRINTNAME(tag)));
                    else
                        s = CAR(b);
                    n = i;
                }
            }
        }


        if (s == NULL) {
            /* first untagged argument */
            already_checked_dots = 0;
            for (b = funargs, i = 0 ; b != R_NilValue ; b = CDR(b), i++) {
                if (CAR(b) == R_DotsSymbol) {
                    if (already_checked_dots)
                        continue;
                    already_checked_dots = 1;
                    if (TYPEOF(dots) == DOTSXP) {
                        for (dot = dots ; dot != R_NilValue ; dot = CDR(dot)) {
                            if (TAG(dot) == R_NilValue) {
                                s = CAR(dot);
                                if (TYPEOF(s) != PROMSXP)
                                    error(_("value in '...' is not a promise"));
                                break;
                            }
                        }
                        if (s != NULL) break;  /* break out of both for loops */
                    }
                    else if (dots != R_NilValue && dots != R_MissingArg)
                        error(_("'...' used in an incorrect context"));
                }
                else if (TAG(b) == R_NilValue) {
                    s = CAR(b);
                    n = i;
                    break;
                }
            }
        }
    }
    else {
        s = NULL;
        dots = NULL;
        already_checked_dots = 0;
        for (b = funargs, i = 0 ; b != R_NilValue ; b = CDR(b), i++) {
            if (CAR(b) == R_DotsSymbol) {
                if (already_checked_dots)
                    continue;
                already_checked_dots = 1;
                dots = findVar(R_DotsSymbol, env);
                if (TYPEOF(dots) == DOTSXP) {
                    s = CAR(dots);
                    if (TYPEOF(s) != PROMSXP)
                        error(_("value in '...' is not a promise"));
                    break;
                }
                else if (dots != R_NilValue && dots != R_MissingArg)
                    error(_("'...' used in an incorrect context"));
            }
            else {
                s = CAR(b);
                n = i;
                break;
            }
        }
    }
    if (s == NULL) {
        if (character_only || file_only)
            errorcall(oexpr, "argument '%s' is missing", CHAR(PRINTNAME(tag)));
        SEXP tmp = eval(expr, env);
        set_prvalues_then_return(tmp);
    }


    SEXP e;


    switch (TYPEOF(s)) {
    case PROMSXP:
        e = eval(s, env);
        break;
    case SYMSXP:
    case LANGSXP:
        if (n == -1)
            error("found a symbol / / call but not an index; should never happen, please report!");
        e = eval(s, env);


        SEXP tmp = lang5(
            findVarInFrame(R_BaseEnv, delayedAssignSymbol),
            ScalarString(PRINTNAME(thispathtempSymbol)),
            R_NilValue,
            R_EmptyEnv,
            rho
        );
        PROTECT(tmp); nprotect++;


        eval(tmp, R_EmptyEnv);
        tmp = findVarInFrame(rho, thispathtempSymbol);
        SET_PRCODE(tmp, s);
        SET_PRSEEN(tmp, 0);
        SET_PRVALUE(tmp, e);
        ENSURE_NAMEDMAX(e);
        SET_PRENV(tmp, R_NilValue);
        R_removeVarFromFrame(thispathtempSymbol, rho);


        e = tmp;
        /* add 1 for the function at the start */
        SETCAR(nthcdr(expr, n + 1), e);
        break;
    default:
        e = s;
        break;
    }


    SEXP ofile = TYPEOF(e) == PROMSXP ? PRVALUE(e) : e;
    checkfile(
        /* const char *name   = */ "file",
        /* SEXP ofile         = */ ofile,
        /* SEXP frame         = */ rho  ,
        /* int character_only = */ character_only,
        /* int file_only      = */ file_only,
        /* SEXP rho           = */ rho  ,
        /* int forcepromise   = */ TRUE ,
        /* SEXP call          = */ R_CurrentExpression,
        /* int maybe_chdir    = */ FALSE,
        /* SEXP getowd        = */ NULL ,
        /* int hasowd         = */ FALSE,
        /* int do_enc2utf8    = */ FALSE,
        /* int normalize      = */ FALSE
    )
    set_R_Visible(1);
    SEXP tmp = eval(expr, env);
    set_prvalues_then_return(tmp);


#undef set_prvalues_then_return
}


SEXP do_wrapsource(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP expr = findVarInFrame(rho, exprSymbol);
    if (expr == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(exprSymbol)));
    if (expr == R_MissingArg)
        error(_("argument \"%s\" is missing, with no default"), EncodeChar(PRINTNAME(exprSymbol)));


    SEXP ofile;
    int character_only, file_only;


    int nargs = length(args) - 1;
    switch (nargs) {
    case 2:
        return findfiletheneval(call, op, args, rho);
    case 3:
        ofile = CADR(args);
        character_only = asLogical(CADDR(args));
        if (character_only == NA_LOGICAL)
            error(_("invalid '%s' argument"), "character.only");
        file_only = asLogical(CADDDR(args));
        if (file_only == NA_LOGICAL)
            error(_("invalid '%s' argument"), "file.only");
        checkfile(
            /* const char *name   = */ "file",
            /* SEXP ofile         = */ ofile,
            /* SEXP frame         = */ rho  ,
            /* int character_only = */ character_only,
            /* int file_only      = */ file_only,
            /* SEXP rho           = */ rho  ,
            /* int forcepromise   = */ TRUE ,
            /* SEXP call          = */ R_CurrentExpression,
            /* int maybe_chdir    = */ FALSE,
            /* SEXP getowd        = */ NULL ,
            /* int hasowd         = */ FALSE,
            /* int do_enc2utf8    = */ FALSE,
            /* int normalize      = */ FALSE
        )
        return eval(exprSymbol, rho);
    default:
        error(
            nargs == 1 ? "%d argument passed to .External(%s) which requires %s" :
                         "%d arguments passed to .External(%s) which requires %s",
            nargs, "C_wrapsource", "2 or 3"
        );
    }


    return R_NilValue;
}


SEXP do_insidesource(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (asInteger(eval(lang1(sys_nframeSymbol), rho)) < 2)
        error("inside.source() must be called within another function");


    SEXP ofile, frame;
    int character_only, file_only;


    ofile = CADR(args);
    character_only = asLogical(CADDR(args));
    if (character_only == NA_LOGICAL)
        error(_("invalid '%s' argument"), "character.only");
    file_only = asLogical(CADDDR(args));
    if (file_only == NA_LOGICAL)
        error(_("invalid '%s' argument"), "file.only");


    frame = eval(lang1(parent_frameSymbol), rho);
    PROTECT(frame);


    checkfile(
        /* const char *name   = */ "file",
        /* SEXP ofile         = */ ofile,
        /* SEXP frame         = */ frame,
        /* int character_only = */ character_only,
        /* int file_only      = */ file_only,
        /* SEXP rho           = */ rho  ,
        /* int forcepromise   = */ TRUE ,
        /* SEXP call          = */ R_CurrentExpression,
        /* int maybe_chdir    = */ FALSE,
        /* SEXP getowd        = */ NULL ,
        /* int hasowd         = */ FALSE,
        /* int do_enc2utf8    = */ FALSE,
        /* int normalize      = */ FALSE
    )
    defineVar(insidesourcewashereSymbol, R_NilValue, frame);
    R_LockBinding(insidesourcewashereSymbol, frame);
    set_R_Visible(0);
    UNPROTECT(1);  /* frame */
    return R_NilValue;
}
