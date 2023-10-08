#include "thispathdefn.h"


static R_INLINE
Rboolean asFlag(SEXP x, const char *name)
{
    Rboolean val = asLogical(x);
    if (val == NA_LOGICAL)
        error(_("invalid '%s' value"), name);
    return val;
}


SEXP do_SET_PRSEEN_2 do_formals
{
    do_start_no_op_rho("SET_PRSEEN_2", 1);


    SEXP ptr = CAR(args);
    if (TYPEOF(ptr) != EXTPTRSXP)
        errorcall(call, "invalid first argument, must be an external pointer");
    SEXP promises = R_ExternalPtrProtected(ptr);
    /* 'promises' is an empty pairlist, so nothing to do */
    if (TYPEOF(promises) == NILSXP)
        return R_NilValue;
    if (TYPEOF(promises) != LISTSXP)
        errorcall(call, "invalid first argument, 'R_ExternalPtrProtected()' must be a pairlist");
    SEXP x;
    for (x = promises; x != R_NilValue; x = CDR(x)) {
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


SEXP do_wrap_source do_formals
{
    do_start_no_op("wrap.source", 20);


    int nprotect = 0;


    SEXP promise = findVarInFrame(rho, exprSymbol);
    if (promise == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(exprSymbol)));
    if (promise == R_MissingArg)
        error(_("argument \"%s\" is missing, with no default"), EncodeChar(PRINTNAME(exprSymbol)));
    if (TYPEOF(promise) != PROMSXP)
        error("invalid '%s', must be a promise; should never happen, please report!",
              EncodeChar(PRINTNAME(exprSymbol)));


    /* determine context number for .External2(.C_getframenumber) */
    int context_number;
    int nframe = asInteger(eval(expr_sys_nframe, rho));
    // Rprintf("sys.nframe() = %d\n", nframe);


    if (nframe <= 0) {
        errorcall(call, "cannot be called from the global environment");
        return R_NilValue;
    }
    else if (nframe < 2)
        context_number = 1;
    else {
        int parent = sys_parent(1, rho);
        // Rprintf("\nsys.parent(): %d\n", parent);
        if (nframe - 1 != parent)
            /* this will happen for something like:
               wrapper <- function(...) {
                   force(wrap.source(sourcelike(...)))
               }

               but won't for something like this:
               wrapper <- function(...) {
                   wrap.source(sourcelike(...))
               }
             */
            context_number = nframe;
        else {
            SEXP tmp;
            PROTECT_INDEX indx;
            PROTECT_WITH_INDEX(tmp = CONS(ScalarInteger(parent), R_NilValue), &indx);
            REPROTECT(tmp = LCONS(getFromBase(sys_functionSymbol), tmp), indx);
            SEXP function = eval(tmp, rho);
            PROTECT(function);
            if (TYPEOF(function) != CLOSXP)
                context_number = nframe;
            else if (identical(function, getFromMyNS(withArgsSymbol)))
                context_number = nframe;
            else
                context_number = parent;
            UNPROTECT(2);
        }
    }
    // Rprintf("context_number = %d\n", context_number);


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
    flag_definitions;


    SEXP frame = rho;
    SEXP ptr;
    SEXP promises = R_NilValue;
    SEXP documentcontext;


    /* we will be modifying the PRSEEN values of the promises,
     * but we need to change them to 0 or 2 depending upon whether
     * this returns a value as normal or the promises get interrupted
     *
     * save the root promise, the original code of the promise,
     * and make an on.exit() call that will restore said promise
     */
    {
        SEXP expr;
        PROTECT_INDEX indx;
        PROTECT_WITH_INDEX(expr = CONS(ptr = R_MakeExternalPtr(NULL, R_NilValue, promises), R_NilValue), &indx);
#if R_version_at_least(3, 0, 0)
        /* .External2(.C_SET_PRSEEN_2, ptr) */
        REPROTECT(expr = CONS(getFromMyNS(_C_SET_PRSEEN_2Symbol), expr), indx);
        REPROTECT(expr = LCONS(getFromBase(_External2Symbol), expr), indx);
#else
        /* .SET_PRSEEN_2(ptr) */
        REPROTECT(expr = LCONS(getFromMyNS(_SET_PRSEEN_2Symbol), expr), indx);
#endif
        REPROTECT(expr = CONS(expr, R_NilValue), indx);
        REPROTECT(expr = LCONS(getFromBase(on_exitSymbol), expr), indx);
        eval(expr, rho);
        UNPROTECT(1);
    }


#define check_validity                                         \
    do {                                                       \
        if (PRVALUE(promise) != R_UnboundValue)                \
            error("invalid '%s', must be a call", EncodeChar(PRINTNAME(exprSymbol)));\
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
        error("invalid '%s', must be a call", EncodeChar(PRINTNAME(exprSymbol)));


#define set_prvalues_then_return(val)                          \
    do {                                                       \
        SEXP tmp = (val);                                      \
        PROTECT(tmp); nprotect++;                              \
        ENSURE_NAMEDMAX(tmp);                                  \
        for (SEXP x = promises; x != R_NilValue; x = CDR(x)) { \
            SEXP p = CAR(x);                                   \
            SET_PRSEEN (p, 0);                                 \
            SET_PRVALUE(p, tmp);                               \
            SET_PRENV  (p, R_NilValue);                        \
        }                                                      \
        R_SetExternalPtrProtected(ptr, R_NilValue);            \
        UNPROTECT(nprotect);                                   \
        return tmp;                                            \
    } while (0)


    if (length(expr) < 2) {
        documentcontext = R_EmptyEnv;
        INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, frame);
        R_LockBinding(documentcontextSymbol, frame);
        set_prvalues_then_return(eval(PRCODE(promise), env));
    }


    /* we will want to modify 'expr' before evaluating it, but we want the
       original version of it for possible error messages */
    SEXP oexpr = expr;
    /* we will modify a copy */
    expr = duplicate(expr);
    PROTECT(expr); nprotect++;


    SEXP fun     = CAR(expr),
         funargs = CDR(expr);
    if (TYPEOF(fun) == SYMSXP)
        fun = findFunction3(fun, env, expr);
    else
        fun = eval(fun, env);
    /* change the function so that it is not evaluated twice */
    SETCAR(expr, fun);


    switch (TYPEOF(fun)) {
    case SPECIALSXP:
    case BUILTINSXP:
    case CLOSXP:
        break;
    default:
        error(_("attempt to apply non-function"));
    }


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
        for (b = funargs, i = 0; b != R_NilValue; b = CDR(b), i++) {
            if (CAR(b) == R_DotsSymbol) {
                if (already_checked_dots)
                    continue;
                already_checked_dots = 1;
                dots = findVar(R_DotsSymbol, env);
                if (TYPEOF(dots) == DOTSXP) {
                    for (dot = dots ; dot != R_NilValue ; dot = CDR(dot)) {
                        if (TAG(dot) != R_NilValue && pmatch(tag, TAG(dot), 1)) {
                            if (s != NULL)
                                errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), EncodeChar(PRINTNAME(tag)));
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
                    errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), EncodeChar(PRINTNAME(tag)));
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
                                    errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), EncodeChar(PRINTNAME(tag)));
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
                        errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), EncodeChar(PRINTNAME(tag)));
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
        documentcontext = R_EmptyEnv;
        INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, frame);
        R_LockBinding(documentcontextSymbol, frame);
        set_R_Visible(TRUE);
        set_prvalues_then_return(eval(expr, env));
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
        e = PROTECT(eval(s, env));
        ENSURE_NAMEDMAX(e);
        /* we want to replace 's' with 'e' in the call. we could directly
           replace but then we would need to quote 's'. even if we didn't, it
           would mess up substitute(). so we make an evaluated promise
           containing 's' as PRCODE() and 'e' as PRVALUE() so that substitute()
           will work as normal but that 's' won't be evaluated twice. it would
           be bad if it was evaluated twice if 's' opens a connection or prints
           something. this means substitute() works, we don't evaluate 's'
           twice, and we don't need to quote 'e' */
        e = makeEVPROMISE(s, e);
        SETCAR(nthcdr(funargs, n), e);
        UNPROTECT(1);
        break;
    default:
        e = s;
        break;
    }


    SEXP ofile = ((TYPEOF(e) == PROMSXP) ? PRVALUE(e) : e);


    SEXP returnvalue;  /* this is never used */


    checkfile(
        /* call                   */ R_CurrentExpression,
        /* sym                    */ fileSymbol,
        /* ofile                  */ ofile,
        /* frame                  */ frame,
        /* as_binding             */ TRUE,
        /* normalize_action       */ NA_NOT_DIR,
        /* forcepromise           */ TRUE,
        /* assign_returnvalue     */ FALSE,
        /* maybe_chdir            */ FALSE,
        /* getowd                 */ NULL,
        /* hasowd                 */ FALSE,
        /* ofilearg               */ NULL,
        character_only, conv2utf8, allow_blank_string,
        allow_clipboard, allow_stdin, allow_url, allow_file_uri,
        allow_unz, allow_pipe, allow_terminal, allow_textConnection,
        allow_rawConnection, allow_sockconn, allow_servsockconn,
        allow_customConnection, ignore_blank_string, ignore_clipboard,
        ignore_stdin, ignore_url, ignore_file_uri,
        /* source                 */ mkChar("call to function wrap.source from package this.path")
    );


#define define_n(n, documentcontext)                           \
    MARK_NOT_MUTABLE_defineVar(nSymbol, ScalarInteger((n)), (documentcontext))


    if (documentcontext != R_EmptyEnv) {
        define_n(context_number, documentcontext);
    }
    set_prvalues_then_return(eval(expr, env));


#undef set_prvalues_then_return
}


typedef enum { SETPATHOP_SETSYSPATH         = 0 ,
               SETPATHOP_UNSETSYSPATH           ,
               SETPATHOP_SETENVPATH             ,
               SETPATHOP_SETSRCPATH             ,
               SETPATHOP_SETSYSPATHFUNCTION     } SETPATHOP;


SEXP set_path(SETPATHOP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    const char *name;
    switch (op) {
    case SETPATHOP_SETSYSPATH:   name = "'set.sys.path()'";   break;
    case SETPATHOP_UNSETSYSPATH: name = "'unset.sys.path()'"; break;
    case SETPATHOP_SETENVPATH:   name = "'set.env.path()'";   break;
    case SETPATHOP_SETSRCPATH:   name = "'set.src.path()'";   break;
    case SETPATHOP_SETSYSPATHFUNCTION: name = "'set.sys.path.function()'"; break;
    default:
        error(_("invalid '%s' value"), "op");
        return R_NilValue;
    }


    int parent = sys_parent(1, rho);
    if (parent < 1)
        error("%s cannot be used within the global environment", name);


    SEXP expr;
    PROTECT_INDEX indx;
    PROTECT_WITH_INDEX(expr = CONS(ScalarInteger(parent), R_NilValue), &indx);
    REPROTECT(expr = LCONS(getFromBase(sys_functionSymbol), expr), indx);
    SEXP function = eval(expr, rho);
    UNPROTECT(1);  /* expr */
    PROTECT(function);
    if (TYPEOF(function) != CLOSXP)
        error("%s cannot be used within a '%s', possible errors with eval?",
              name, type2char(TYPEOF(function)));


    /* ensure 'set.sys.path()' is not called from one of the source-like functions */
    if (identical(function, getFromBase(sourceSymbol)))
        error("%s cannot be called within %s()",
              name, EncodeChar(PRINTNAME(sourceSymbol)));


    if (identical(function, getFromBase(sys_sourceSymbol)))
        error("%s cannot be called within %s()",
              name, EncodeChar(PRINTNAME(sys_sourceSymbol)));


    if (identical(function, getFromMyNS(wrap_sourceSymbol)))
        error("%s cannot be called within %s() from package %s",
              name, EncodeChar(PRINTNAME(wrap_sourceSymbol)), "this.path");


    SEXP ns = findVarInFrame(R_NamespaceRegistry, compilerSymbol);
    if (ns != R_UnboundValue) {
        if (identical(function, getInFrame(loadcmpSymbol, ns, FALSE))) {
            error("%s cannot be called within %s() from package %s",
                  name, EncodeChar(PRINTNAME(loadcmpSymbol)), EncodeChar(PRINTNAME(compilerSymbol)));
        }
    }


    UNPROTECT(1);  /* function */


    SEXP ofile, frame, documentcontext;


    frame = eval(expr_parent_frame, rho);
    PROTECT(frame); nprotect++;


    /* ensure 'set.sys.path()' isn't evaluated in an invalid environment */
    if (frame == R_EmptyEnv)
        error("%s cannot be used within the empty environment", name);
    else if (frame == R_GlobalEnv)
        error("%s cannot be used within the global environment", name);
    else if (frame == R_BaseEnv)
        error("%s cannot be used within the base environment", name);
    else if (frame == R_BaseNamespace)
        error("%s cannot be used within the base namespace environment", name);
    else if (R_IsPackageEnv(frame))
        error("%s cannot be used within a package environment", name);
    else if (R_IsNamespaceEnv(frame))
        error("%s cannot be used within a namespace environment", name);
    else if (R_existsVarInFrame(frame, R_dot_packageName))
        error("%s cannot be used within a top level environment", name);
    else if (R_EnvironmentIsLocked(frame))
        error("%s cannot be used within a locked environment", name);


    if (op == SETPATHOP_SETSYSPATHFUNCTION) {


        if (R_existsVarInFrame(frame, documentcontextSymbol))
            error("%s cannot be called more than once within an environment", name);


        extern SEXP src_context(SEXP srcfile, SEXP rho);


        SEXP fun = CAR(args);
        if (TYPEOF(fun) != CLOSXP)
            error(_("invalid '%s' value"), "fun");
        SEXP documentcontext = src_context(fun, rho);
        INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, frame);
        R_LockBinding(documentcontextSymbol, frame);
        defineVar(setsyspathwashereSymbol, R_NilValue, documentcontext);
        define_n(parent, documentcontext);
        set_R_Visible(FALSE);
        UNPROTECT(nprotect);
        return R_NilValue;
    }
    else if (op) {
        documentcontext = findVarInFrame(frame, documentcontextSymbol);
        if (documentcontext == R_UnboundValue)
            error("%s cannot be called before set.sys.path()", name);
        if (TYPEOF(documentcontext) != ENVSXP)
            error(_("invalid '%s' value"), EncodeChar(PRINTNAME(documentcontextSymbol)));
        if (!R_existsVarInFrame(documentcontext, setsyspathwashereSymbol))
            error("%s cannot be called before set.sys.path()", name);
        SEXP returnthis = R_NilValue;
        switch (op) {
        case SETPATHOP_UNSETSYSPATH:
        {
            R_removeVarFromFrame(documentcontextSymbol, frame);
            break;
        }
        case SETPATHOP_SETENVPATH:
        {
            SEXP envir = CAR(args); args = CDR(args);
            returnthis = envir;
            if (TYPEOF(envir) != ENVSXP) break;
            SEXP target = CAR(args); args = CDR(args);
            if (target != R_NilValue && TYPEOF(target) != ENVSXP) target = R_NilValue;
            SEXP env = topenv(target, envir);
            if (env == R_GlobalEnv ||
                env == R_BaseEnv || env == R_BaseNamespace ||
                R_IsPackageEnv(env) || R_IsNamespaceEnv(env));
            else if (inherits(env, "box$ns"));
            else if (getAttrib(env, documentcontextSymbol) != R_NilValue)
                error("cannot overwrite existing '%s' attribute", EncodeChar(PRINTNAME(documentcontextSymbol)));
            else setAttrib(env, documentcontextSymbol, documentcontext);
            break;
        }
        case SETPATHOP_SETSRCPATH:
        {
            SEXP x = CAR(args); args = CDR(args);
            returnthis = x;
            SEXP srcfile = NULL;
            switch (TYPEOF(x)) {
            case EXPRSXP:
                srcfile = PROTECT(getAttrib(x, srcfileSymbol)); nprotect++;
                if (TYPEOF(srcfile) != ENVSXP) srcfile = NULL;
                break;
            case ENVSXP:
                if (inherits(x, "srcfile")) srcfile = x;
                break;
            }
            if (srcfile) {
                if (R_existsVarInFrame(srcfile, documentcontextSymbol))
                    error("cannot overwrite existing binding '%s'", EncodeChar(PRINTNAME(documentcontextSymbol)));


                extern void document_context_assign_lines(SEXP documentcontext, SEXP srcfile);


                document_context_assign_lines(documentcontext, srcfile);
                INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, srcfile);
                R_LockBinding(documentcontextSymbol, srcfile);
            }
            break;
        }
        default:
            error(_("invalid '%s' value"), "op");
            return R_NilValue;
        }
        set_R_Visible(FALSE);
        UNPROTECT(nprotect);
        return returnthis;
    }


    if (R_existsVarInFrame(frame, documentcontextSymbol))
        error("%s cannot be called more than once within an environment", name);


    /* why would this be NA??? idk but might as well test for it anyway */
    Rboolean missing_file = asFlag(eval(expr_missing_file, rho), "missing(file)");
    if (missing_file) {
        documentcontext = R_EmptyEnv;
        INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, frame);
        R_LockBinding(documentcontextSymbol, frame);
        set_R_Visible(TRUE);
        UNPROTECT(nprotect);
        // return R_MissingArg;
        /* this is better because it preserves the original value */
        SEXP value = findVarInFrame(rho, fileSymbol);
        if (value == R_UnboundValue)
            error(_("object '%s' not found"), EncodeChar(PRINTNAME(fileSymbol)));
        if (value == R_MissingArg)
            return R_MissingArg;
        if (TYPEOF(value) != PROMSXP)
            error("invalid '%s' value, expected R_MissingArg or a promise", EncodeChar(PRINTNAME(fileSymbol)));
        if (TYPEOF(PREXPR(value)) != SYMSXP)
            error("invalid '%s' value, expected a symbol", EncodeChar(PRINTNAME(fileSymbol)));
        value = findVarInFrame(PRENV(value), PREXPR(value));
        if (value == R_UnboundValue)
            error(_("object '%s' not found"), EncodeChar(PRINTNAME(PREXPR(value))));
        return value;
    }


    ofile = getInFrame(fileSymbol, rho, FALSE);
    PROTECT(ofile); nprotect++;


    flag_declarations;
    flag_definitions;
    SEXP Function = CAR(args); args = CDR(args);


    SEXP source = NULL;
    switch (TYPEOF(Function)) {
    case NILSXP:
        source = mkChar("call to function set.sys.path from package this.path");
        PROTECT(source); nprotect++;
        break;
    case SYMSXP:
    {
        Function = PRINTNAME(Function);
        if (Function == NA_STRING || Function == R_BlankString)
            error(_("invalid '%s' argument"), "Function");
        const char *fmt = "call to function '%s'";
        const char *tmp = EncodeChar(Function);
        int pwidth = 1 + ((int) strlen(fmt)) + ((int) strlen(tmp)) - 2;
        char buf[pwidth];
        snprintf(buf, pwidth, fmt, tmp);
        source = mkChar(buf);
        PROTECT(source); nprotect++;
    }
        break;
    case STRSXP:
    {
        switch (XLENGTH(Function)) {
        case 1:
        {
            if (STRING_ELT(Function, 0) == NA_STRING ||
                STRING_ELT(Function, 0) == R_BlankString)
            {
                error(_("invalid '%s' argument"), "Function");
            }
            const char *fmt = "call to function '%s'";
            const char *tmp = EncodeChar(STRING_ELT(Function, 0));
            int pwidth = 1 + ((int) strlen(fmt)) + ((int) strlen(tmp)) - 2;
            char buf[pwidth];
            snprintf(buf, pwidth, fmt, tmp);
            source = mkChar(buf);
            PROTECT(source); nprotect++;
        }
            break;
        case 2:
        {
            if (STRING_ELT(Function, 0) == NA_STRING ||
                STRING_ELT(Function, 0) == R_BlankString)
            {
                error(_("invalid '%s' value"), "Function[1]");
            }
            if (STRING_ELT(Function, 1) == NA_STRING ||
                STRING_ELT(Function, 1) == R_BlankString)
            {
                error(_("invalid '%s' value"), "Function[2]");
            }
            const char *fmt = "call to function '%s' from package '%s'";
            const char *tmp0 = EncodeChar(STRING_ELT(Function, 0));
            const char *tmp1 = EncodeChar(STRING_ELT(Function, 1));
            int pwidth = 1 + ((int) strlen(fmt)) + ((int) strlen(tmp0)) - 2 + ((int) strlen(tmp1)) - 2;
            char buf[pwidth];
            snprintf(buf, pwidth, fmt, tmp0, tmp1);
            source = mkChar(buf);
            PROTECT(source); nprotect++;
        }
            break;
        case 3:
        {
            if (STRING_ELT(Function, 0) == NA_STRING ||
                STRING_ELT(Function, 0) == R_BlankString)
            {
                error(_("invalid '%s' value"), "Function[1]");
            }
            if (STRING_ELT(Function, 1) == NA_STRING ||
                STRING_ELT(Function, 1) == R_BlankString)
            {
                error(_("invalid '%s' value"), "Function[2]");
            }
            if (STRING_ELT(Function, 2) == NA_STRING ||
                STRING_ELT(Function, 2) == R_BlankString)
            {
                error(_("invalid '%s' value"), "Function[3]");
            }
            const char *fmt;
            const char *tmp1 = CHAR(STRING_ELT(Function, 1));
            if (streql(tmp1, "package"    ) ||
                streql(tmp1, "module"     ) ||
                streql(tmp1, "namespace"  ) ||
                streql(tmp1, "environment") ||
                streql(tmp1, "library"    ) ||
                streql(tmp1, "script"     ))
            {
                fmt = "call to function '%s' from %s '%s'";
            }
            else if (streql("in", tmp1))
                fmt = "call to function '%s' %s '%s'";
            else {
                tmp1 = EncodeChar(STRING_ELT(Function, 1));
                fmt = "call to function '%s' from '%s' '%s'";
            }
            const char *tmp0 = EncodeChar(STRING_ELT(Function, 0));
            const char *tmp2 = EncodeChar(STRING_ELT(Function, 2));
            int pwidth = 1 + ((int) strlen(fmt)) + ((int) strlen(tmp0)) - 2 +
                                                   ((int) strlen(tmp1)) - 2 +
                                                   ((int) strlen(tmp2)) - 2;
            char buf[pwidth];
            snprintf(buf, pwidth, fmt, tmp0, tmp1, tmp2);
            source = mkChar(buf);
            PROTECT(source); nprotect++;
        }
            break;
        default:
            error("invalid '%s'; must be of length 1, 2, or 3", "Function");
            return R_NilValue;
        }
    }
        break;
    default:
        error("invalid '%s' argument of type %s", "Function", type2char(TYPEOF(Function)));
        return R_NilValue;
    }
    if (source && TYPEOF(source) == CHARSXP);
    else error("internal error; invalid '%s' value", "source");


    SEXP ofilearg;
    Rboolean missing_ofile = asFlag(eval(expr_missing_ofile, rho), "missing(ofile)");
    if (missing_ofile)
        ofilearg = NULL;
    else {
        ofilearg = getInFrame(ofileSymbol, rho, FALSE);
        PROTECT(ofilearg); nprotect++;
    }


    SEXP returnvalue = R_NilValue;
    checkfile(
        /* call                   */ R_CurrentExpression,
        /* sym                    */ fileSymbol,
        /* ofile                  */ ofile,
        /* frame                  */ frame,
        /* as_binding             */ TRUE,
        /* normalize_action       */ NA_FIX_DIR,
        /* forcepromise           */ TRUE,
        /* assign_returnvalue     */ TRUE,
        /* maybe_chdir            */ FALSE,
        /* getowd                 */ NULL,
        /* hasowd                 */ FALSE,
        /* ofilearg               */ ofilearg,
        character_only, conv2utf8, allow_blank_string,
        allow_clipboard, allow_stdin, allow_url, allow_file_uri,
        allow_unz, allow_pipe, allow_terminal, allow_textConnection,
        allow_rawConnection, allow_sockconn, allow_servsockconn,
        allow_customConnection, ignore_blank_string, ignore_clipboard,
        ignore_stdin, ignore_url, ignore_file_uri,
        /* source                 */ source
    );
    PROTECT(returnvalue); nprotect++;


    if (documentcontext != R_EmptyEnv) {
        defineVar(setsyspathwashereSymbol, R_NilValue, documentcontext);
        define_n(parent, documentcontext);
    }
    UNPROTECT(nprotect);
    return returnvalue;
}


#undef flag_definitions
#undef flag_declarations
#undef define_n


SEXP do_set_sys_path do_formals
{
    do_start_no_op("set.sys.path", 21);
    return set_path(SETPATHOP_SETSYSPATH, args, rho);
}


SEXP do_unset_sys_path do_formals
{
    do_start_no_call_op("unset.sys.path", 0);
    return set_path(SETPATHOP_UNSETSYSPATH, args, rho);
}


SEXP do_set_env_path do_formals
{
    do_start_no_call_op("set.env.path", 2);
    return set_path(SETPATHOP_SETENVPATH, args, rho);
}


SEXP do_set_src_path do_formals
{
    do_start_no_call_op("set.src.path", 1);
    return set_path(SETPATHOP_SETSRCPATH, args, rho);
}


SEXP do_set_src_path_function do_formals
{
    do_start_no_call_op("set.src.path.function", 1);
    return set_path(SETPATHOP_SETSYSPATHFUNCTION, args, rho);
}
