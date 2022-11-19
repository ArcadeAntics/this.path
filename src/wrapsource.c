#include <R.h>
#include <Rinternals.h>


#include "thispathdefn.h"


static R_INLINE int asFlag(SEXP x, const char *name)
{
    int val = asLogical(x);
    if (val == NA_LOGICAL)
        error(_("invalid '%s' value"), name);
    return val;
}


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


#define flag_declarations                                      \
    int character_only, conv2utf8, allow_blank_string,         \
        allow_clipboard, allow_stdin, allow_url,               \
        allow_file_uri, allow_unz, allow_pipe, allow_terminal, \
        allow_textConnection, allow_rawConnection,             \
        allow_sockconn, allow_servsockconn,                    \
        allow_customConnection, ignore_blank_string,           \
        ignore_clipboard, ignore_stdin, ignore_url,            \
        ignore_file_uri


#define flag_definitions                                       \
    character_only         = asFlag(CAR(args), "character.only"        ); args = CDR(args);\
    conv2utf8              = asFlag(CAR(args), "conv2utf8"             ); args = CDR(args);\
    allow_blank_string     = asFlag(CAR(args), "allow.blank.string"    ); args = CDR(args);\
    allow_clipboard        = asFlag(CAR(args), "allow.clipboard"       ); args = CDR(args);\
    allow_stdin            = asFlag(CAR(args), "allow.stdin"           ); args = CDR(args);\
    allow_url              = asFlag(CAR(args), "allow.url"             ); args = CDR(args);\
    allow_file_uri         = asFlag(CAR(args), "allow.file.uri"        ); args = CDR(args);\
    allow_unz              = asFlag(CAR(args), "allow.unz"             ); args = CDR(args);\
    allow_pipe             = asFlag(CAR(args), "allow.pipe"            ); args = CDR(args);\
    allow_terminal         = asFlag(CAR(args), "allow.terminal"        ); args = CDR(args);\
    allow_textConnection   = asFlag(CAR(args), "allow.textConnection"  ); args = CDR(args);\
    allow_rawConnection    = asFlag(CAR(args), "allow.rawConnection"   ); args = CDR(args);\
    allow_sockconn         = asFlag(CAR(args), "allow.sockconn"        ); args = CDR(args);\
    allow_servsockconn     = asFlag(CAR(args), "allow.servsockconn"    ); args = CDR(args);\
    allow_customConnection = asFlag(CAR(args), "allow.customConnection"); args = CDR(args);\
    ignore_blank_string    = asFlag(CAR(args), "ignore.blank.string"   ); args = CDR(args);\
    ignore_clipboard       = asFlag(CAR(args), "ignore.clipboard"      ); args = CDR(args);\
    ignore_stdin           = asFlag(CAR(args), "ignore.stdin"          ); args = CDR(args);\
    ignore_url             = asFlag(CAR(args), "ignore.url"            ); args = CDR(args);\
    ignore_file_uri        = asFlag(CAR(args), "ignore.file.uri"       ); args = CDR(args)


    flag_declarations;
    args = CDR(args);
    flag_definitions;


    SEXP frame = rho;
    SEXP promise = findVarInFrame(rho, exprSymbol);
    if (promise == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(exprSymbol)));
    if (TYPEOF(promise) != PROMSXP)
        error("invalid '%s', must be a promise; should never happen, please report!",
              EncodeChar(PRINTNAME(exprSymbol)));


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
    MARK_NOT_MUTABLE(ptr);
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
        assign_null(frame);
        assign_done(frame);
        set_R_Visible(1);
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


    SEXP ofile = ((TYPEOF(e) == PROMSXP) ? PRVALUE(e) : e);


    SEXP returnvalue;  /* this is never used */


    checkfile(
        /* SEXP call                  = */ R_CurrentExpression,
        /* SEXP rho                   = */ rho,
        /* SEXP sym                   = */ fileSymbol,
        /* SEXP ofile                 = */ ofile,
        /* SEXP frame                 = */ frame,
        /* int forcepromise           = */ TRUE,
        /* int assign_returnvalue     = */ FALSE,
        /* int maybe_chdir            = */ FALSE,
        /* SEXP getowd                = */ NULL,
        /* int hasowd                 = */ FALSE,
        /* int character_only         = */ character_only,
        /* int conv2utf8              = */ conv2utf8,
        /* int allow_blank_string     = */ allow_blank_string,
        /* int allow_clipboard        = */ allow_clipboard,
        /* int allow_stdin            = */ allow_stdin,
        /* int allow_url              = */ allow_url,
        /* int allow_file_uri         = */ allow_file_uri,
        /* int allow_unz              = */ allow_unz,
        /* int allow_pipe             = */ allow_pipe,
        /* int allow_terminal         = */ allow_terminal,
        /* int allow_textConnection   = */ allow_textConnection,
        /* int allow_rawConnection    = */ allow_rawConnection,
        /* int allow_sockconn         = */ allow_sockconn,
        /* int allow_servsockconn     = */ allow_servsockconn,
        /* int allow_customConnection = */ allow_customConnection,
        /* int ignore_blank_string    = */ ignore_blank_string,
        /* int ignore_clipboard       = */ ignore_clipboard,
        /* int ignore_stdin           = */ ignore_stdin,
        /* int ignore_url             = */ ignore_url,
        /* int ignore_file_uri        = */ ignore_file_uri
    )


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
    // FIXME: move all from findfiletheneval into this function
    return findfiletheneval(call, op, args, rho);
}


SEXP do_insidesource(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    int n = asInteger(eval(lang1(sys_nframeSymbol), rho));
    if (n < 2)
        error("inside.source() must be called within another function");


    SEXP expr = lang2(sys_functionSymbol, ScalarInteger(n - 1));
    PROTECT(expr);
    SEXP function = eval(expr, rho);
    UNPROTECT(1);
    if (TYPEOF(function) != CLOSXP)
        error("inside.source() cannot be used within a '%s', possible errors with eval?", type2char(TYPEOF(function)));


    SEXP ofile, frame;


    frame = eval(lang1(parent_frameSymbol), rho);
    PROTECT(frame); nprotect++;


    args = CDR(args);
    int missing_file;
    /* why would this be NA?? idk but might as well test for it anyway */
    missing_file = asFlag(CAR(args), "missing(file)"); args = CDR(args);
    if (missing_file) {
        assign_null(frame);
        assign_done(frame);
        set_R_Visible(1);
        UNPROTECT(nprotect);
        return R_MissingArg;
    }


    flag_declarations;
    flag_definitions;


    ofile = getInFrame(fileSymbol, rho, FALSE);
    PROTECT(ofile); nprotect++;


    SEXP returnvalue = R_NilValue;
    checkfile(
        /* SEXP call                  = */ R_CurrentExpression,
        /* SEXP rho                   = */ rho,
        /* SEXP sym                   = */ fileSymbol,
        /* SEXP ofile                 = */ ofile,
        /* SEXP frame                 = */ frame,
        /* int forcepromise           = */ TRUE,
        /* int assign_returnvalue     = */ TRUE,
        /* int maybe_chdir            = */ FALSE,
        /* SEXP getowd                = */ NULL,
        /* int hasowd                 = */ FALSE,
        /* int character_only         = */ character_only,
        /* int conv2utf8              = */ conv2utf8,
        /* int allow_blank_string     = */ allow_blank_string,
        /* int allow_clipboard        = */ allow_clipboard,
        /* int allow_stdin            = */ allow_stdin,
        /* int allow_url              = */ allow_url,
        /* int allow_file_uri         = */ allow_file_uri,
        /* int allow_unz              = */ allow_unz,
        /* int allow_pipe             = */ allow_pipe,
        /* int allow_terminal         = */ allow_terminal,
        /* int allow_textConnection   = */ allow_textConnection,
        /* int allow_rawConnection    = */ allow_rawConnection,
        /* int allow_sockconn         = */ allow_sockconn,
        /* int allow_servsockconn     = */ allow_servsockconn,
        /* int allow_customConnection = */ allow_customConnection,
        /* int ignore_blank_string    = */ ignore_blank_string,
        /* int ignore_clipboard       = */ ignore_clipboard,
        /* int ignore_stdin           = */ ignore_stdin,
        /* int ignore_url             = */ ignore_url,
        /* int ignore_file_uri        = */ ignore_file_uri
    )


    defineVar(insidesourcewashereSymbol, R_NilValue, frame);
    R_LockBinding(insidesourcewashereSymbol, frame);
    UNPROTECT(nprotect + 1);  /* +1 for returnvalue */
    return returnvalue;
}


#undef flag_definitions
#undef flag_declarations
