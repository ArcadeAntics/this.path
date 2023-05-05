#include "thispathdefn.h"


#if R_version_less_than(3, 0, 0)
#define XLENGTH LENGTH
#endif


static R_INLINE int asFlag(SEXP x, const char *name)
{
    int val = asLogical(x);
    if (val == NA_LOGICAL)
        error(_("invalid '%s' value"), name);
    return val;
}


SEXP do_setprseen2 do_formals
{
    do_start_no_op_rho("setprseen2", 1);


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


SEXP do_wrapsource do_formals
{
    do_start_no_call_op("wrapsource", 20);


    SEXP expr = findVarInFrame(rho, exprSymbol);
    if (expr == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(exprSymbol)));
    if (expr == R_MissingArg)
        error(_("argument \"%s\" is missing, with no default"), EncodeChar(PRINTNAME(exprSymbol)));


    int nprotect = 0;


    /* determine context number for .External2(.C_getframenumber) */
    int context_number;
    int nframe = asInteger(eval(lang1(sys_nframeSymbol), rho));
    // Rprintf("sys.nframe() = %d\n", nframe);


    if (nframe < 2)
        context_number = 1;
    else {
        int sys_parent = get_sys_parent(1, rho);
        // Rprintf("\nsys.parent(): %d\n", sys_parent);
        if (nframe - 1 != sys_parent)
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
            SEXP tmp = lang2(sys_functionSymbol, ScalarInteger(sys_parent));
            PROTECT(tmp);
            SEXP function = eval(tmp, rho);
            PROTECT(function);
            if (TYPEOF(function) != CLOSXP)
                context_number = nframe;
            else if (identical(function, getInFrame(withArgsSymbol, mynamespace, FALSE)))
                context_number = nframe;
            else
                context_number = sys_parent;
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
    SEXP promise = findVarInFrame(rho, exprSymbol);
    if (promise == R_UnboundValue)
        error(_("object '%s' not found"), EncodeChar(PRINTNAME(exprSymbol)));
    if (TYPEOF(promise) != PROMSXP)
        error("invalid '%s', must be a promise; should never happen, please report!",
              EncodeChar(PRINTNAME(exprSymbol)));


    SEXP ptr = R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);
    PROTECT(ptr); nprotect++;
    MARK_NOT_MUTABLE(ptr);


    SEXP promises = R_NilValue;


    /* we will be modifying the PRSEEN values of the promises,
     * but we need to change them to 0 or 2 depending upon whether
     * this returns a value as normal or the promises get interrupted
     *
     * save the root promise, the original code of the promise,
     * and make an on.exit() call that will restore said promise
     */
    {
#if R_version_at_least(3, 0, 0)
        /* .External2(.C_setprseen2, ptr) */
        SEXP expr = allocList(3);
        PROTECT(expr);
        SET_TYPEOF(expr, LANGSXP);
        SETCAR(expr, External2Symbol);
        SETCADR(expr, _C_setprseen2Symbol);
        SETCADDR(expr, ptr);
#else
        /* .setprseen2(ptr) */
        SEXP expr = allocList(2);
        PROTECT(expr);
        SET_TYPEOF(expr, LANGSXP);
        SETCAR(expr, _setprseen2Symbol);
        SETCADR(expr, ptr);
#endif


        SEXP expr2 = allocList(2);
        PROTECT(expr2);
        SET_TYPEOF(expr2, LANGSXP);
        SETCAR(expr2, on_exitSymbol);
        SETCADR(expr2, expr);


        eval(expr2, rho);
        UNPROTECT(2);
    }


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


#define set_thispathn(n, frame)                                \
    do {                                                       \
        INCREMENT_NAMED_defineVar(thispathnSymbol, ScalarInteger((n)), (frame));\
        R_LockBinding(thispathnSymbol, (frame));               \
    } while (0)


#define set_prvalues_then_return(val)                          \
    do {                                                       \
        set_thispathn(context_number, frame);                  \
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


    check_validity;


    /* find the root promise */
    while (TYPEOF(PREXPR(promise)) == PROMSXP) {
        promise = PREXPR(promise);
        check_validity;
    }


         expr = PREXPR(promise);
    SEXP env  = PRENV (promise);


    if (expr == R_MissingArg)
        error(_("argument \"%s\" is missing, with no default"), EncodeChar(PRINTNAME(exprSymbol)));
    if (TYPEOF(expr) != LANGSXP)
        error("invalid '%s', must be a call when '%s' is missing", EncodeChar(PRINTNAME(exprSymbol)), "file");


    if (length(expr) < 2) {
        assign_null(frame);
        set_R_Visible(TRUE);
        set_prvalues_then_return(eval(PRCODE(promise), env));
    }


    SEXP oexpr = expr;
    expr = duplicate(expr);
    PROTECT(expr); nprotect++;


    SEXP fun     = CAR(expr),
         funargs = CDR(expr);
    if (TYPEOF(fun) == SYMSXP)
        fun = findFunction(fun, env, expr);
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
        assign_null(frame);
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
        e = eval(s, env);


        SEXP tmp = allocList(5);
        PROTECT(tmp); nprotect++;
        SET_TYPEOF(tmp, LANGSXP);
        SETCAR   (tmp, findVarInFrame(R_BaseEnv, delayedAssignSymbol));
        SETCADR  (tmp, /* x          */ ScalarString(PRINTNAME(thispathtempSymbol)));
        SETCADDDR(tmp, /* eval.env   */ R_EmptyEnv);
        SETCAD4R (tmp, /* assign.env */ rho);


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


#if R_version_at_least(3, 4, 0)
#define checkfile_call R_CurrentExpression
#else
#define checkfile_call R_NilValue
#endif
    checkfile(
        /* call                   */ checkfile_call,
        /* sym                    */ fileSymbol,
        /* ofile                  */ ofile,
        /* frame                  */ frame,
        /* check_not_directory    */ TRUE,
        /* forcepromise           */ TRUE,
        /* assign_returnvalue     */ FALSE,
        /* maybe_chdir            */ FALSE,
        /* getowd                 */ NULL,
        /* hasowd                 */ FALSE,
        /* character_only         */ character_only,
        /* conv2utf8              */ conv2utf8,
        /* allow_blank_string     */ allow_blank_string,
        /* allow_clipboard        */ allow_clipboard,
        /* allow_stdin            */ allow_stdin,
        /* allow_url              */ allow_url,
        /* allow_file_uri         */ allow_file_uri,
        /* allow_unz              */ allow_unz,
        /* allow_pipe             */ allow_pipe,
        /* allow_terminal         */ allow_terminal,
        /* allow_textConnection   */ allow_textConnection,
        /* allow_rawConnection    */ allow_rawConnection,
        /* allow_sockconn         */ allow_sockconn,
        /* allow_servsockconn     */ allow_servsockconn,
        /* allow_customConnection */ allow_customConnection,
        /* ignore_blank_string    */ ignore_blank_string,
        /* ignore_clipboard       */ ignore_clipboard,
        /* ignore_stdin           */ ignore_stdin,
        /* ignore_url             */ ignore_url,
        /* ignore_file_uri        */ ignore_file_uri
    );


    set_prvalues_then_return(eval(expr, env));


#undef set_prvalues_then_return
}


SEXP insidesource(SEXP args, SEXP rho, const char *name, Rboolean unset, SEXP backup)
{
    int nprotect = 0;


    int sys_parent = get_sys_parent(1, rho);
    if (sys_parent < 1)
        error("%s() cannot be used within the global environment", name);


    SEXP expr = lang2(sys_functionSymbol, ScalarInteger(sys_parent));
    PROTECT(expr);
    SEXP function = eval(expr, rho);
    UNPROTECT(1);  /* expr */
    PROTECT(function);
    if (TYPEOF(function) != CLOSXP)
        error("%s() cannot be used within a '%s', possible errors with eval?",
              name, type2char(TYPEOF(function)));


    /* ensure 'inside.source()' is not called from one of the source-like functions */
    if (identical(function, getInFrame(sourceSymbol, R_BaseEnv, FALSE)))
        error("%s() cannot be called within %s()",
              name, EncodeChar(PRINTNAME(sourceSymbol)));
    else if (identical(function, getInFrame(sys_sourceSymbol, R_BaseEnv, FALSE)))
        error("%s() cannot be called within %s()",
              name, EncodeChar(PRINTNAME(sys_sourceSymbol)));


    init_tools_rstudio(FALSE);


    if (has_tools_rstudio) {
        if (identical(function, get_debugSource)) {
            error("%s() cannot be called within %s() in RStudio",
                  name, EncodeChar(PRINTNAME(debugSourceSymbol)));
        }
    }


    SEXP ns;


    ns = findVarInFrame(R_NamespaceRegistry, testthatSymbol);
    if (ns == R_UnboundValue ? FALSE : TRUE) {
        if (identical(function, getInFrame(source_fileSymbol, ns, FALSE))) {
            error("%s() cannot be called within %s() from package %s",
                  name, EncodeChar(PRINTNAME(source_fileSymbol)), EncodeChar(PRINTNAME(testthatSymbol)));
        }
    }


    ns = findVarInFrame(R_NamespaceRegistry, knitrSymbol);
    if (ns == R_UnboundValue ? FALSE : TRUE) {
        if (identical(function, getInFrame(knitSymbol, ns, FALSE))) {
            error("%s() cannot be called within %s() from package %s",
                  name, EncodeChar(PRINTNAME(knitSymbol)), EncodeChar(PRINTNAME(knitrSymbol)));
        }
    }


    if (identical(function, getInFrame(wrap_sourceSymbol, mynamespace, FALSE))) {
        error("%s() cannot be called within %s() from package %s",
              name, EncodeChar(PRINTNAME(wrap_sourceSymbol)), "this.path");
    }


    ns = findVarInFrame(R_NamespaceRegistry, boxSymbol);
    if (ns == R_UnboundValue ? FALSE : TRUE) {
        if (identical(function, getInFrame(load_from_sourceSymbol, ns, FALSE))) {
            error("%s() cannot be called within %s() from package %s",
                  name, EncodeChar(PRINTNAME(load_from_sourceSymbol)), EncodeChar(PRINTNAME(boxSymbol)));
        }
    }


    ns = findVarInFrame(R_NamespaceRegistry, compilerSymbol);
    if (ns == R_UnboundValue ? FALSE : TRUE) {
        if (identical(function, getInFrame(loadcmpSymbol, ns, FALSE))) {
            error("%s() cannot be called within %s() from package %s",
                  name, EncodeChar(PRINTNAME(loadcmpSymbol)), EncodeChar(PRINTNAME(compilerSymbol)));
        }
    }


    UNPROTECT(1);  /* function */


    SEXP ofile, frame;


    frame = eval(lang1(parent_frameSymbol), rho);
    PROTECT(frame); nprotect++;


    /* ensure 'inside.source()' isn't evaluated in an invalid environment */
    if (frame == R_GlobalEnv)
        error("%s() cannot be used within the global environment", name);
    else if (frame == R_BaseEnv)
        error("%s() cannot be used within the base environment", name);
    else if (frame == R_EmptyEnv)
        error("%s() cannot be used within the empty environment", name);
    else if (R_IsPackageEnv(frame))
        error("%s() cannot be used within a package environment", name);
    else if (R_IsNamespaceEnv(frame))
        error("%s() cannot be used within a namespace environment", name);
    else if (R_EnvironmentIsLocked(frame))
        error("%s() cannot be used within a locked environment", name);


    if (unset) {
        if (findVarInFrame(frame, insidesourcewashereSymbol) == R_UnboundValue)
            error("%s() cannot be called before inside.source() / / set.this.path()", name);
        if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue)
            error("%s() cannot be called within this environment", name);
#if defined(R_THIS_PATH_USE_removeFromFrame)
        SEXP names[] = {
            thispathofileSymbol      ,
            thispathfileSymbol       ,
            thispathformsgSymbol     ,
            thispatherrorSymbol      ,
            thispathassocwfileSymbol ,
            thispathdoneSymbol       ,
            insidesourcewashereSymbol,
            thispathnSymbol          ,
            NULL
        };
        removeFromFrame(names, frame);
#else
        R_removeVarFromFrame(thispathofileSymbol      , frame);
        R_removeVarFromFrame(thispathfileSymbol       , frame);
        R_removeVarFromFrame(thispathformsgSymbol     , frame);
        R_removeVarFromFrame(thispatherrorSymbol      , frame);
        R_removeVarFromFrame(thispathassocwfileSymbol , frame);
        R_removeVarFromFrame(thispathdoneSymbol       , frame);
        R_removeVarFromFrame(insidesourcewashereSymbol, frame);
        R_removeVarFromFrame(thispathnSymbol          , frame);
#endif
        set_R_Visible(FALSE);
        UNPROTECT(nprotect);
        return R_NilValue;
    }


    if (findVarInFrame(frame, insidesourcewashereSymbol) != R_UnboundValue)
        error("%s() cannot be called more than once within an environment", name);
    if (findVarInFrame(frame, thispathdoneSymbol) != R_UnboundValue)
        error("%s() cannot be called within this environment", name);


    /* why would this be NA?? idk but might as well test for it anyway */
    int missing_file = asFlag(eval(lang2(missingSymbol, fileSymbol), rho), "missing(file)");
    if (missing_file) {
        assign_null(frame);
        defineVar(insidesourcewashereSymbol, R_MissingArg, frame);
        R_LockBinding(insidesourcewashereSymbol, frame);
        set_thispathn(sys_parent, frame);
        set_R_Visible(TRUE);
        UNPROTECT(nprotect);
        return R_MissingArg;
    }


    flag_declarations;
    flag_definitions;
    SEXP Function = CAR(args); args = CDR(args);
    SEXP fun_name;
    switch (TYPEOF(Function)) {
    case NILSXP:
        fun_name = PRINTNAME(backup);
        PROTECT(fun_name); nprotect++;
        break;
    case SYMSXP:
        {
            Function = PRINTNAME(Function);
            if (Function == NA_STRING || Function == R_BlankString)
                error(_("invalid '%s' argument"), "Function");
            const char *tmp = EncodeChar(Function);
            int pwidth = 1 + ((int) strlen(tmp)) + 2;
            char buf[pwidth];
            snprintf(buf, pwidth, "'%s'", tmp);
            fun_name = mkChar(buf);
            PROTECT(fun_name); nprotect++;
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
                    const char *tmp = EncodeChar(STRING_ELT(Function, 0));
                    int pwidth = 1 + ((int) strlen(tmp)) + 2;
                    char buf[pwidth];
                    snprintf(buf, pwidth, "'%s'", tmp);
                    fun_name = mkChar(buf);
                    PROTECT(fun_name); nprotect++;
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
                    const char *tmp0 = EncodeChar(STRING_ELT(Function, 0));
                    const char *tmp1 = EncodeChar(STRING_ELT(Function, 1));
                    int pwidth = 1 + ((int) strlen(tmp0)) + 16 + ((int) strlen(tmp1)) + 2;
                    char buf[pwidth];
                    snprintf(buf, pwidth, "'%s' from package '%s'", tmp0, tmp1);
                    fun_name = mkChar(buf);
                    PROTECT(fun_name); nprotect++;
                }
                break;
            default:
                error("invalid '%s'; must be of length 1 or 2", "Function");
                return R_NilValue;
            }
        }
        break;
    default:
        error("invalid '%s' argument of type %s", "Function", type2char(TYPEOF(Function)));
        return R_NilValue;
    }


    ofile = getInFrame(fileSymbol, rho, FALSE);
    PROTECT(ofile); nprotect++;


    SEXP returnvalue = R_NilValue;
    checkfile(
        /* call                   */ checkfile_call,
        /* sym                    */ fileSymbol,
        /* ofile                  */ ofile,
        /* frame                  */ frame,
        /* check_not_directory    */ TRUE,
        /* forcepromise           */ TRUE,
        /* assign_returnvalue     */ TRUE,
        /* maybe_chdir            */ FALSE,
        /* getowd                 */ NULL,
        /* hasowd                 */ FALSE,
        /* character_only         */ character_only,
        /* conv2utf8              */ conv2utf8,
        /* allow_blank_string     */ allow_blank_string,
        /* allow_clipboard        */ allow_clipboard,
        /* allow_stdin            */ allow_stdin,
        /* allow_url              */ allow_url,
        /* allow_file_uri         */ allow_file_uri,
        /* allow_unz              */ allow_unz,
        /* allow_pipe             */ allow_pipe,
        /* allow_terminal         */ allow_terminal,
        /* allow_textConnection   */ allow_textConnection,
        /* allow_rawConnection    */ allow_rawConnection,
        /* allow_sockconn         */ allow_sockconn,
        /* allow_servsockconn     */ allow_servsockconn,
        /* allow_customConnection */ allow_customConnection,
        /* ignore_blank_string    */ ignore_blank_string,
        /* ignore_clipboard       */ ignore_clipboard,
        /* ignore_stdin           */ ignore_stdin,
        /* ignore_url             */ ignore_url,
        /* ignore_file_uri        */ ignore_file_uri
    );


    INCREMENT_NAMED_defineVar(insidesourcewashereSymbol, fun_name, frame);
    R_LockBinding(insidesourcewashereSymbol, frame);
    set_thispathn(sys_parent, frame);
    UNPROTECT(nprotect + 1);  /* +1 for returnvalue */
    return returnvalue;
}


SEXP do_insidesource do_formals
{
    do_start_no_call_op("insidesource", 21);
    return insidesource(args, rho, "inside.source", FALSE, insidesourcefrompackageSymbol);
}


SEXP do_setthispath do_formals
{
    do_start_no_call_op("setthispath", 21);
    return insidesource(args, rho, "set.this.path", FALSE, setthispathfrompackageSymbol);
}


SEXP do_unsetthispath do_formals
{
    do_start_no_call_op("unsetthispath", 0);


    return insidesource(args, rho, "unset.this.path", TRUE, NULL);
}


#undef flag_definitions
#undef flag_declarations
#undef set_thispathn
