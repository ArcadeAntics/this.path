#include "thispathdefn.h"


static R_INLINE
Rboolean asFlag(SEXP x, const char *name)
{
    Rboolean val = Rf_asLogical(x);
    if (val == NA_LOGICAL)
        Rf_error(_("invalid '%s' value"), name);
    return val;
}


SEXP do_wrap_source do_formals
{
    do_start_no_op("wrap_source", 20);


    int nprotect = 0;


    SEXP promise = Rf_findVarInFrame(rho, exprSymbol);
    if (promise == R_UnboundValue)
        Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(exprSymbol)));
    if (promise == R_MissingArg)
        // Rf_error(_("argument \"%s\" is missing, with no default"), R_CHAR(PRINTNAME(exprSymbol)));
        MissingArgError(exprSymbol, R_CurrentExpression, rho, "evalError");
    if (TYPEOF(promise) != PROMSXP)
        Rf_error("invalid '%s', must be a promise; should never happen, please report!",
              R_CHAR(PRINTNAME(exprSymbol)));
    if (ptr_PRVALUE(promise) != R_UnboundValue)
        Rf_error("invalid '%s', must be an unevaluated call", R_CHAR(PRINTNAME(exprSymbol)));


    /* determine context number for .getframenumber() */
    int context_number;
    int nframe = Rf_asInteger(Rf_eval(expr_sys_nframe, rho));
    // Rprintf("sys.nframe() = %d\n", nframe);


    if (nframe <= 0) {
        Rf_errorcall(call, "cannot be called from the global environment");
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
            R_ProtectWithIndex(tmp = Rf_cons(Rf_ScalarInteger(parent), R_NilValue), &indx);
            R_Reprotect(tmp = Rf_lcons(getFromBase(sys_functionSymbol), tmp), indx);
            SEXP function = Rf_eval(tmp, rho);
            Rf_protect(function);
            if (TYPEOF(function) != CLOSXP)
                context_number = nframe;
            else if (identical(function, getFromMyNS(withArgsSymbol)))
                context_number = nframe;
            else
                context_number = parent;
            Rf_unprotect(2);
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
    SEXP documentcontext;


    /* find the root promise */
    while (TYPEOF(ptr_R_PromiseExpr(promise)) == PROMSXP) {
        promise = ptr_R_PromiseExpr(promise);
        if (ptr_PRVALUE(promise) != R_UnboundValue)
            Rf_error("invalid '%s', must be an unevaluated call", R_CHAR(PRINTNAME(exprSymbol)));
    }


    SEXP expr = ptr_R_PromiseExpr(promise),
         env  = ptr_PRENV (promise);


    if (expr == R_MissingArg)
        // Rf_error(_("argument \"%s\" is missing, with no default"), R_CHAR(PRINTNAME(exprSymbol)));
        MissingArgError(exprSymbol, R_CurrentExpression, rho, "evalError");
    if (TYPEOF(expr) != LANGSXP)
        Rf_error("invalid '%s', must be a call", R_CHAR(PRINTNAME(exprSymbol)));


#if R_version_at_least(3,0,0)
#define eval_with_visible(expr, env)                           \
    SEXP tmp = Rf_eval(expr, env);                             \
    Rf_protect(tmp); nprotect++
#else
#define eval_with_visible(expr, env)                           \
    SEXP expr2 = Rf_cons(expr, R_NilValue);                    \
    Rf_protect(expr2); nprotect++;                             \
    expr2 = Rf_lcons(getFromBase(withVisibleSymbol), expr2);   \
    Rf_protect(expr2); nprotect++;                             \
    SEXP value = Rf_eval(expr2, env);                          \
    Rf_protect(value); nprotect++;                             \
    set_this_path_value(VECTOR_ELT(value, 0));                 \
    set_this_path_visible(Rf_asLogical(VECTOR_ELT(value, 1))); \
    SEXP tmp = VECTOR_ELT(value, 0)
#endif


#define eval_then_return(expr, env)                            \
    do {                                                       \
        eval_with_visible((expr), (env));                      \
        ENSURE_NAMEDMAX(tmp);                                  \
        Rf_unprotect(nprotect);                                \
        return tmp;                                            \
    } while (0)


    if (Rf_length(expr) < 2) {
        documentcontext = R_EmptyEnv;
        INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, frame);
        R_LockBinding(documentcontextSymbol, frame);
        eval_then_return(ptr_PRCODE(promise), env);
    }


    /* we will want to modify 'expr' before evaluating it, but we want the
       original version of it for possible error messages */
    SEXP oexpr = expr;
    /* we will modify a copy */
    expr = Rf_duplicate(expr);
    Rf_protect(expr); nprotect++;


    SEXP fun     = CAR(expr),
         funargs = CDR(expr);
    if (TYPEOF(fun) == SYMSXP)
        fun = findFunction3(fun, env, expr);
    else
        fun = Rf_eval(fun, env);
    /* change the function so that it is not evaluated twice */
    SETCAR(expr, fun);


    switch (TYPEOF(fun)) {
    case SPECIALSXP:
    case BUILTINSXP:
    case CLOSXP:
        break;
    default:
        Rf_error(_("attempt to apply non-function"));
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
                dots = Rf_findVar(R_DotsSymbol, env);
                if (TYPEOF(dots) == DOTSXP) {
                    for (dot = dots; dot != R_NilValue; dot = CDR(dot)) {
                        if (TAG(dot) != R_NilValue && Rf_pmatch(tag, TAG(dot), 1)) {
                            if (s != NULL)
                                Rf_errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), EncodeChar(PRINTNAME(tag)));
                            else
                                s = CAR(dot);
                            if (TYPEOF(s) != PROMSXP)
                                Rf_error(_("value in '...' is not a promise"));
                        }
                    }
                }
                else if (dots != R_NilValue && dots != R_MissingArg)
                    Rf_error(_("'...' used in an incorrect context"));
            }
            else if (TAG(b) != R_NilValue && Rf_pmatch(tag, TAG(b), 1)) {
                if (s != NULL)
                    Rf_errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), EncodeChar(PRINTNAME(tag)));
                else
                    s = CAR(b);
                n = i;
            }
        }


        if (s == NULL) {
            /* partial matches */
            already_checked_dots = 0;
            for (b = funargs, i = 0; b != R_NilValue; b = CDR(b), i++) {
                if (CAR(b) == R_DotsSymbol) {
                    if (already_checked_dots)
                        continue;
                    already_checked_dots = 1;
                    if (TYPEOF(dots) == DOTSXP) {
                        for (dot = dots; dot != R_NilValue; dot = CDR(dot)) {
                            if (TAG(dot) != R_NilValue && Rf_pmatch(tag, TAG(dot), 0)) {
                                if (s != NULL)
                                    Rf_errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), EncodeChar(PRINTNAME(tag)));
                                else
                                    s = CAR(dot);
                                if (TYPEOF(s) != PROMSXP)
                                    Rf_error(_("value in '...' is not a promise"));
                            }
                        }
                    }
                    else if (dots != R_NilValue && dots != R_MissingArg)
                        Rf_error(_("'...' used in an incorrect context"));
                }
                else if (TAG(b) != R_NilValue && Rf_pmatch(tag, TAG(b), 0)) {
                    if (s != NULL)
                        Rf_errorcall(oexpr, _("formal argument \"%s\" matched by multiple actual arguments"), EncodeChar(PRINTNAME(tag)));
                    else
                        s = CAR(b);
                    n = i;
                }
            }
        }


        if (s == NULL) {
            /* first untagged argument */
            already_checked_dots = 0;
            for (b = funargs, i = 0; b != R_NilValue; b = CDR(b), i++) {
                if (CAR(b) == R_DotsSymbol) {
                    if (already_checked_dots)
                        continue;
                    already_checked_dots = 1;
                    if (TYPEOF(dots) == DOTSXP) {
                        for (dot = dots; dot != R_NilValue; dot = CDR(dot)) {
                            if (TAG(dot) == R_NilValue) {
                                s = CAR(dot);
                                if (TYPEOF(s) != PROMSXP)
                                    Rf_error(_("value in '...' is not a promise"));
                                break;
                            }
                        }
                        if (s != NULL) break;  /* break out of both for loops */
                    }
                    else if (dots != R_NilValue && dots != R_MissingArg)
                        Rf_error(_("'...' used in an incorrect context"));
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
                dots = Rf_findVar(R_DotsSymbol, env);
                if (TYPEOF(dots) == DOTSXP) {
                    s = CAR(dots);
                    if (TYPEOF(s) != PROMSXP)
                        Rf_error(_("value in '...' is not a promise"));
                    break;
                }
                else if (dots != R_NilValue && dots != R_MissingArg)
                    Rf_error(_("'...' used in an incorrect context"));
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
        eval_then_return(expr, env);
    }


    SEXP e;


    switch (TYPEOF(s)) {
    case PROMSXP:
        e = Rf_eval(s, env);
        break;
    case SYMSXP:
    case LANGSXP:
        if (n == -1)
            Rf_error("found a symbol / / call but not an index; should never happen, please report!");
        e = Rf_protect(Rf_eval(s, env));
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
        SETCAR(Rf_nthcdr(funargs, n), e);
        Rf_unprotect(1);
        break;
    default:
        e = s;
        break;
    }


    SEXP ofile = ((TYPEOF(e) == PROMSXP) ? ptr_PRVALUE(e) : e);


    SEXP returnvalue;  /* this is never used */


    set_documentcontext(
        /* call                   */ R_CurrentExpression,
        /* sym                    */ fileSymbol,
        /* ofile                  */ ofile,
        /* assign_here            */ frame,
        /* assign_as_binding      */ TRUE,
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
        /* source                 */ Rf_mkChar("call to function wrap.source from package @R_PACKAGE_NAME@"),
        /* srcfile_original       */ NULL
    );


#define define_n(n, documentcontext)                           \
    MARK_NOT_MUTABLE_defineVar(nSymbol, Rf_ScalarInteger((n)), (documentcontext))


    if (documentcontext != R_EmptyEnv) {
        define_n(context_number, documentcontext);
    }
    eval_then_return(expr, env);


#undef eval_then_return
}


typedef enum {
    SPA_SET_SYS_PATH           = 0,
    SPA_UNSET_SYS_PATH            ,
    SPA_SET_ENV_PATH              ,
    SPA_SET_SRC_PATH              ,
    SPA_SET_SYS_PATH_FUNCTION
} SET_PATH_ACTION;


SEXP set_path(SET_PATH_ACTION spa, SEXP args, SEXP rho)
{
    int nprotect = 0;


    const char *name;
    switch (spa) {
    case SPA_SET_SYS_PATH:   name = "'set.sys.path()'";   break;
    case SPA_UNSET_SYS_PATH: name = "'unset.sys.path()'"; break;
    case SPA_SET_ENV_PATH:   name = "'set.env.path()'";   break;
    case SPA_SET_SRC_PATH:   name = "'set.src.path()'";   break;
    case SPA_SET_SYS_PATH_FUNCTION: name = "'set.sys.path.function()'"; break;
    default:
        Rf_error(_("invalid '%s' value"), "spa");
        return R_NilValue;
    }


    int parent = sys_parent(1, rho);
    if (parent < 1)
        Rf_error("%s cannot be used within the global environment", name);


    {
        INTEGER(CADR(expr_sys_function_which))[0] = parent;
        SEXP function = Rf_eval(expr_sys_function_which, rho);
        Rf_protect(function);
        if (function == eval_op)
            Rf_error("%s cannot be used within '%s'",
                name, R_CHAR(PRINTNAME(R_EvalSymbol)));
        else if (TYPEOF(function) != CLOSXP)
            Rf_error("%s cannot be used within a '%s', possible errors with eval?",
                name, Rf_type2char(TYPEOF(function)));
        else if (identical(function, getFromMyNS(wrap_sourceSymbol)))
            Rf_error("%s cannot be called within %s() from package %s",
                name, R_CHAR(PRINTNAME(wrap_sourceSymbol)), "@R_PACKAGE_NAME@");
        Rf_unprotect(1);
    }


    SEXP ofile, frame, documentcontext;


    frame = Rf_eval(expr_parent_frame, rho);
    Rf_protect(frame); nprotect++;


    /* ensure 'set.sys.path()' isn't evaluated in an invalid environment */
    if (frame == R_EmptyEnv)
        Rf_error("%s cannot be used within the empty environment", name);
    else if (frame == R_GlobalEnv)
        Rf_error("%s cannot be used within the global environment", name);
    else if (frame == R_BaseEnv)
        Rf_error("%s cannot be used within the base environment", name);
    else if (frame == R_BaseNamespace)
        Rf_error("%s cannot be used within the base namespace environment", name);
    else if (R_IsPackageEnv(frame))
        Rf_error("%s cannot be used within a package environment", name);
    else if (R_IsNamespaceEnv(frame))
        Rf_error("%s cannot be used within a namespace environment", name);
    else if (R_existsVarInFrame(frame, R_dot_packageName))
        Rf_error("%s cannot be used within a top level environment", name);
    else if (R_EnvironmentIsLocked(frame))
        Rf_error("%s cannot be used within a locked environment", name);


    if (spa == SPA_SET_SYS_PATH_FUNCTION) {


        if (R_existsVarInFrame(frame, documentcontextSymbol))
            Rf_error("%s cannot be called more than once within an environment", name);


        extern SEXP src_context(SEXP srcfile, SEXP rho);


        SEXP fun = CAR(args);
        if (TYPEOF(fun) != CLOSXP)
            Rf_error(_("invalid '%s' value"), "fun");
        SEXP documentcontext = src_context(fun, rho);
        if (documentcontext == R_EmptyEnv) {
            INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, frame);
            R_LockBinding(documentcontextSymbol, frame);
            set_R_Visible(FALSE);
            Rf_unprotect(nprotect);
            return R_NilValue;
        }
        documentcontext = duplicateEnv(documentcontext);
        INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, frame);
        R_LockBinding(documentcontextSymbol, frame);
        SEXP osource = Rf_findVarInFrame(documentcontext, sourceSymbol);
        if (osource == R_UnboundValue)
            Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(sourceSymbol)));
        if (TYPEOF(osource) != CHARSXP)
            Rf_error(_("invalid '%s' value"), R_CHAR(PRINTNAME(sourceSymbol)));
        const char *ostr = R_CHAR(osource);
        const char *str = "call to function 'set.sys.path.function'";
        SEXP source;
        if (!LENGTH(osource))
            source = Rf_mkChar(str);
        else {
            const char *fmt = "%s, copied from %s";
            int pwidth = 1 + ((int) strlen(fmt)) + ((int) strlen(str)) + ((int) strlen(ostr)) - 4;
            char buf[pwidth];
            snprintf(buf, pwidth, fmt, str, ostr);
            source = Rf_mkChar(buf);
        }
        INCREMENT_NAMED_defineVar(sourceSymbol, source, documentcontext);
        Rf_defineVar(setsyspathwashereSymbol, R_NilValue, documentcontext);
        define_n(parent, documentcontext);
        set_R_Visible(FALSE);
        Rf_unprotect(nprotect);
        return R_NilValue;
    }
    else if (spa) {
        documentcontext = Rf_findVarInFrame(frame, documentcontextSymbol);
        if (documentcontext == R_UnboundValue)
            Rf_error("%s cannot be called before set.sys.path()", name);
        if (TYPEOF(documentcontext) != ENVSXP)
            Rf_error(_("invalid '%s' value"), R_CHAR(PRINTNAME(documentcontextSymbol)));
        if (documentcontext != R_EmptyEnv &&
            !R_existsVarInFrame(documentcontext, setsyspathwashereSymbol))
        {
            Rf_error("%s cannot be called before set.sys.path()", name);
        }
        SEXP returnthis = R_NilValue;
        switch (spa) {
        case SPA_UNSET_SYS_PATH:
        {
            R_removeVarFromFrame(documentcontextSymbol, frame);
            break;
        }
        case SPA_SET_ENV_PATH:
        {
            SEXP envir = CAR(args); args = CDR(args);
            returnthis = envir;
            if (TYPEOF(envir) != ENVSXP) break;
            SEXP target = CAR(args); args = CDR(args);
            if (target != R_NilValue && TYPEOF(target) != ENVSXP) target = R_NilValue;
            SEXP env = Rf_topenv(target, envir);
            if (env == R_GlobalEnv ||
                env == R_BaseEnv || env == R_BaseNamespace ||
                R_IsPackageEnv(env) || R_IsNamespaceEnv(env));
            else if (Rf_inherits(env, "box$ns"));
            else if (!ISNULL(Rf_getAttrib(env, documentcontextSymbol)))
                Rf_error("cannot overwrite existing '%s' attribute", R_CHAR(PRINTNAME(documentcontextSymbol)));
            else Rf_setAttrib(env, documentcontextSymbol, documentcontext);
            break;
        }
        case SPA_SET_SRC_PATH:
        {
            SEXP x = CAR(args); args = CDR(args);
            returnthis = x;
            SEXP srcfile = NULL;
            switch (TYPEOF(x)) {
            case EXPRSXP:
                srcfile = Rf_protect(Rf_getAttrib(x, srcfileSymbol)); nprotect++;
                if (TYPEOF(srcfile) != ENVSXP) srcfile = NULL;
                break;
            case ENVSXP:
                if (Rf_inherits(x, "srcfile")) srcfile = x;
                break;
            }
            if (srcfile) {
                if (R_existsVarInFrame(srcfile, documentcontextSymbol))
                    Rf_error("cannot overwrite existing binding '%s'", R_CHAR(PRINTNAME(documentcontextSymbol)));


                extern void document_context_assign_lines(SEXP documentcontext, SEXP srcfile);


                document_context_assign_lines(documentcontext, srcfile);
                INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, srcfile);
                R_LockBinding(documentcontextSymbol, srcfile);
            }
            break;
        }
        default:
            Rf_error(_("invalid '%s' value"), "spa");
            return R_NilValue;
        }
        set_R_Visible(FALSE);
        Rf_unprotect(nprotect);
        return returnthis;
    }


    if (R_existsVarInFrame(frame, documentcontextSymbol))
        Rf_error("%s cannot be called more than once within an environment", name);


    /* why would this be NA??? idk but might as well test for it anyway */
    Rboolean missing_file = asFlag(Rf_eval(expr_missing_file, rho), "missing(file)");
    if (missing_file) {
        documentcontext = R_EmptyEnv;
        INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, frame);
        R_LockBinding(documentcontextSymbol, frame);
        Rf_unprotect(nprotect);
        // return R_MissingArg;
        /* this is better because it preserves the original value */
        SEXP value = Rf_findVarInFrame(rho, fileSymbol);
        if (value == R_UnboundValue)
            Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(fileSymbol)));
        if (value == R_MissingArg)
            return R_MissingArg;
        if (TYPEOF(value) != PROMSXP)
            Rf_error("invalid '%s' value, expected R_MissingArg or a promise", R_CHAR(PRINTNAME(fileSymbol)));
        if (TYPEOF(ptr_R_PromiseExpr(value)) != SYMSXP)
            Rf_error("invalid '%s' value, expected a symbol", R_CHAR(PRINTNAME(fileSymbol)));
        value = Rf_findVarInFrame(ptr_PRENV(value), ptr_R_PromiseExpr(value));
        if (value == R_UnboundValue)
            Rf_error(_("object '%s' not found"), EncodeChar(PRINTNAME(ptr_R_PromiseExpr(value))));
        return value;
    }


    ofile = getInFrame(fileSymbol, rho, FALSE);
    Rf_protect(ofile); nprotect++;


    flag_declarations;
    flag_definitions;
    SEXP Function = CAR(args); args = CDR(args);


    SEXP source = NULL;
    switch (TYPEOF(Function)) {
    case NILSXP:
        source = Rf_mkChar("call to function set.sys.path from package @R_PACKAGE_NAME@");
        Rf_protect(source); nprotect++;
        break;
    case SYMSXP:
    {
        Function = PRINTNAME(Function);
        if (Function == NA_STRING || Function == R_BlankString)
            Rf_error(_("invalid '%s' argument"), "Function");
        const char *fmt = "call to function '%s'";
        const char *tmp = EncodeChar(Function);
        int pwidth = 1 + ((int) strlen(fmt)) + ((int) strlen(tmp)) - 2;
        char buf[pwidth];
        snprintf(buf, pwidth, fmt, tmp);
        source = Rf_mkChar(buf);
        Rf_protect(source); nprotect++;
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
                Rf_error(_("invalid '%s' argument"), "Function");
            }
            const char *fmt = "call to function '%s'";
            const char *tmp = EncodeChar(STRING_ELT(Function, 0));
            int pwidth = 1 + ((int) strlen(fmt)) + ((int) strlen(tmp)) - 2;
            char buf[pwidth];
            snprintf(buf, pwidth, fmt, tmp);
            source = Rf_mkChar(buf);
            Rf_protect(source); nprotect++;
        }
            break;
        case 2:
        {
            if (STRING_ELT(Function, 0) == NA_STRING ||
                STRING_ELT(Function, 0) == R_BlankString)
            {
                Rf_error(_("invalid '%s' value"), "Function[1]");
            }
            if (STRING_ELT(Function, 1) == NA_STRING ||
                STRING_ELT(Function, 1) == R_BlankString)
            {
                Rf_error(_("invalid '%s' value"), "Function[2]");
            }
            const char *fmt = "call to function '%s' from package '%s'";
            const char *tmp0 = EncodeChar(STRING_ELT(Function, 0));
            const char *tmp1 = EncodeChar(STRING_ELT(Function, 1));
            int pwidth = 1 + ((int) strlen(fmt)) + ((int) strlen(tmp0)) - 2 + ((int) strlen(tmp1)) - 2;
            char buf[pwidth];
            snprintf(buf, pwidth, fmt, tmp0, tmp1);
            source = Rf_mkChar(buf);
            Rf_protect(source); nprotect++;
        }
            break;
        case 3:
        {
            if (STRING_ELT(Function, 0) == NA_STRING ||
                STRING_ELT(Function, 0) == R_BlankString)
            {
                Rf_error(_("invalid '%s' value"), "Function[1]");
            }
            if (STRING_ELT(Function, 1) == NA_STRING ||
                STRING_ELT(Function, 1) == R_BlankString)
            {
                Rf_error(_("invalid '%s' value"), "Function[2]");
            }
            if (STRING_ELT(Function, 2) == NA_STRING ||
                STRING_ELT(Function, 2) == R_BlankString)
            {
                Rf_error(_("invalid '%s' value"), "Function[3]");
            }
            const char *fmt;
            const char *tmp1 = R_CHAR(STRING_ELT(Function, 1));
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
            source = Rf_mkChar(buf);
            Rf_protect(source); nprotect++;
        }
            break;
        default:
            Rf_error("invalid '%s'; must be of length 1, 2, or 3", "Function");
            return R_NilValue;
        }
    }
        break;
    default:
        Rf_error("invalid '%s' argument of type %s", "Function", Rf_type2char(TYPEOF(Function)));
        return R_NilValue;
    }
    if (source && TYPEOF(source) == CHARSXP);
    else Rf_error("internal error; invalid '%s' value", "source");


    SEXP ofilearg;
    Rboolean missing_ofile = asFlag(Rf_eval(expr_missing_ofile, rho), "missing(ofile)");
    if (missing_ofile)
        ofilearg = NULL;
    else {
        ofilearg = getInFrame(ofileSymbol, rho, FALSE);
        Rf_protect(ofilearg); nprotect++;
    }


    SEXP returnvalue = R_NilValue;
    set_documentcontext(
        /* call                   */ R_CurrentExpression,
        /* sym                    */ fileSymbol,
        /* ofile                  */ ofile,
        /* assign_here            */ frame,
        /* assign_as_binding      */ TRUE,
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
        /* source                 */ source,
        /* srcfile_original       */ NULL
    );
    Rf_protect(returnvalue); nprotect++;


    if (documentcontext != R_EmptyEnv) {
        Rf_defineVar(setsyspathwashereSymbol, R_NilValue, documentcontext);
        define_n(parent, documentcontext);
    }
    Rf_unprotect(nprotect);
    return returnvalue;
}


#undef flag_definitions
#undef flag_declarations
#undef define_n


SEXP do_set_sys_path do_formals
{
    do_start_no_call_op("set_sys_path", 21);
    return set_path(SPA_SET_SYS_PATH, args, rho);
}


SEXP do_unset_sys_path do_formals
{
    do_start_no_call_op("unset_sys_path", 0);
    return set_path(SPA_UNSET_SYS_PATH, args, rho);
}


SEXP do_set_env_path do_formals
{
    do_start_no_call_op("set_env_path", 2);
    return set_path(SPA_SET_ENV_PATH, args, rho);
}


SEXP do_set_src_path do_formals
{
    do_start_no_call_op("set_src_path", 1);
    return set_path(SPA_SET_SRC_PATH, args, rho);
}


SEXP do_set_sys_path_function do_formals
{
    do_start_no_call_op("set_sys_path_function", 1);
    return set_path(SPA_SET_SYS_PATH_FUNCTION, args, rho);
}
