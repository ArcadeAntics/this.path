#include "drivewidth.h"
#include "thispathdefn.h"


SEXP do_is_clipboard do_formals
{
    do_start_no_call_op_rho("is_clipboard", 1);


    SEXP file = CAR(args);
    if (TYPEOF(file) != STRSXP)
        error(_("a character vector argument expected"));
    int n = LENGTH(file);
    SEXP value = allocVector(LGLSXP, n);
    PROTECT(value);
    int *ivalue = INTEGER(value);
    for (int i = 0; i < n; i++)
        ivalue[i] = is_clipboard(CHAR(STRING_ELT(file, i)));
    UNPROTECT(1);
    return value;
}


#define one   "'%s' must be FALSE when '%s' is TRUE"
#define two   "'%s' and '%s' must be FALSE when '%s' is TRUE"
#define three "'%s', '%s', and '%s' must be FALSE when '%s' is TRUE"
#define four  "'%s', '%s', '%s', and '%s' must be FALSE when '%s' is TRUE"


static R_INLINE
void check_arguments4(Rboolean verbose , Rboolean original, Rboolean for_msg ,
                      Rboolean contents)
{
    if (verbose == NA_LOGICAL)
        error(_("invalid '%s' value"), "verbose");
    /* original is allowed to be NA */
    if (for_msg == NA_LOGICAL)
        error(_("invalid '%s' value"), "for.msg");
    if (contents == NA_LOGICAL)
        error(_("invalid '%s' value"), "contents");


    if (contents) {
        if (original)
            error(one, "original", "contents");
        else
            ;
    }
}


static R_INLINE
void check_arguments5(Rboolean verbose , Rboolean original, Rboolean for_msg ,
                      Rboolean contents, Rboolean local   )
{
    check_arguments4(verbose, original, for_msg, contents);


    if (local == NA_LOGICAL)
        error(_("invalid '%s' value"), "local");
}


/* function is currently unused
static R_INLINE
void check_arguments7(Rboolean verbose         , Rboolean original        ,
                      Rboolean for_msg         , Rboolean contents        ,
                      Rboolean local           , int N                    ,
                      Rboolean get_frame_number)
{
    check_arguments5(verbose, original, for_msg, contents, local);


    if (N == NA_INTEGER);
    else if (N < 0) error(_("invalid '%s' argument"), "N");
    if (get_frame_number == NA_LOGICAL)
        error(_("invalid '%s' value"), "get.frame.number");


    if (get_frame_number) {
        if (original) {
            if (for_msg) {
                if (contents) {
                    if (local)
                        error(four, "original", "for.msg", "contents", "local", "get.frame.number");
                    else
                        error(three, "original", "for.msg", "contents", "get.frame.number");
                }
                else {
                    if (local)
                        error(three, "original", "for.msg", "local", "get.frame.number");
                    else
                        error(two, "original", "for.msg", "get.frame.number");
                }
            }
            else {
                if (contents) {
                    if (local)
                        error(three, "original", "contents", "local", "get.frame.number");
                    else
                        error(two, "original", "contents", "get.frame.number");
                }
                else {
                    if (local)
                        error(two, "original", "local", "get.frame.number");
                    else
                        error(one, "original", "get.frame.number");
                }
            }
        }
        else {
            if (for_msg) {
                if (contents) {
                    if (local)
                        error(three "for.msg", "contents", "local", "get.frame.number");
                    else
                        error(two, "for.msg", "contents", "get.frame.number");
                }
                else {
                    if (local)
                        error(two, "for.msg", "local", "get.frame.number");
                    else
                        error(one, "for.msg", "get.frame.number");
                }
            }
            else {
                if (contents) {
                    if (local)
                        error(two, "contents", "local", "get.frame.number");
                    else
                        error(one, "contents", "get.frame.number");
                }
                else {
                    if (local)
                        error(one, "local", "get.frame.number");
                    else
                        ;
                }
            }
        }
    }
}
*/


#undef four
#undef three
#undef two
#undef one


#include "get_file_from_closure.h"


SEXP do_jupyter_path do_formals
{
    do_start_no_op_rho("jupyter_path", -1);


    Rboolean verbose  = FALSE,
             original = FALSE,
             for_msg  = FALSE,
             contents = FALSE;


    switch (length(args)) {
    case 0:
        break;
    case 4:
        verbose  = asLogical(CAR(args)); args = CDR(args);
        original = asLogical(CAR(args)); args = CDR(args);
        for_msg  = asLogical(CAR(args)); args = CDR(args);
        contents = asLogical(CAR(args)); args = CDR(args);
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), ".C_jupyter_path", "0 or 4"));
        return R_NilValue;
    }


    check_arguments4(verbose, original, for_msg, contents);


    if (verbose) Rprintf("Source: document in Jupyter\n");


    if (contents) {
        for_msg = FALSE;
        SEXP value = allocVector(VECSXP, 1);
        PROTECT(value);
        SEXP file = get_file_from_closure(original, for_msg, _jupyter_pathSymbol);
        SEXP expr = LCONS(_get_jupyter_notebook_contentsSymbol, CONS(file, R_NilValue));
        PROTECT(expr);
        SET_VECTOR_ELT(value, 0, eval(expr, mynamespace));
        UNPROTECT(2);
        return value;
    }
    return get_file_from_closure(original, for_msg, _jupyter_pathSymbol);
}


Rboolean validJupyterRNotebook(SEXP path)
{
    SEXP expr = LCONS(_get_jupyter_R_notebook_contentsSymbol, CONS(path, R_NilValue));
    PROTECT(expr);
    SEXP value = eval(expr, mynamespace);
    UNPROTECT(1);
    return (value != R_NilValue);
}


SEXP do_set_jupyter_path do_formals
{
    do_start_no_op_rho("set_jupyter_path", -1);


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
        errorcall(call, wrong_nargs_to_External(length(args), ".C_set_jupyter_path", "1 or 2"));
        return R_NilValue;
    }


    if (!IS_SCALAR(path, STRSXP))
        errorcall(call, _("'%s' must be a character string"), "path");
    if (STRING_ELT(path, 0) == NA_STRING);
    else if (is_abs_path(CHAR(STRING_ELT(path, 0))));
    else errorcall(call, _("invalid '%s' argument"), "path");


    if (skipCheck || STRING_ELT(path, 0) == NA_STRING || validJupyterRNotebook(path));
    else errorcall(call, "invalid '%s' argument; must be a valid Jupyter R notebook", "path");


    SEXP sym, env = getFromMyNS(_jupyter_pathSymbol);
    if (TYPEOF(env) != CLOSXP)
        errorcall(call, "'%s' is not a closure", CHAR(PRINTNAME(_jupyter_pathSymbol)));
    env = CLOENV(env);


    /* get the promises */
    sym = ofileSymbol;
    SEXP ofile = findVarInFrame(env, sym);
    if (ofile == R_UnboundValue)
        errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    if (TYPEOF(ofile) != PROMSXP)
        errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    sym = fileSymbol;
    SEXP file = findVarInFrame(env, sym);
    if (file == R_UnboundValue)
        errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(sym)));
    if (TYPEOF(file) != PROMSXP)
        errorcall(call, "'%s' is not a promise", EncodeChar(PRINTNAME(sym)));


    /* restore and evaluate the promise 'ofile' */
    SET_PRCODE(ofile, path);
    SET_PRENV(ofile, R_NilValue);
    SET_PRVALUE(ofile, path);
    SET_PRSEEN(ofile, 0);


    /* restore the promise 'file' to its original state */
    // SET_PRCODE(file, );
    SET_PRENV(file, env);
    SET_PRVALUE(file, R_UnboundValue);
    SET_PRSEEN(file, 0);


    set_R_Visible(FALSE);
    return path;
}


SEXP do_Rgui_path do_formals
{
    do_start_no_op("Rgui_path", 6);


    Rboolean verbose, original, for_msg, contents;
    SEXP untitled, r_editor;


    verbose  = asLogical(CAR(args)); args = CDR(args);
    original = asLogical(CAR(args)); args = CDR(args);
    for_msg  = asLogical(CAR(args)); args = CDR(args);
    contents = asLogical(CAR(args)); args = CDR(args);
    check_arguments4(verbose, original, for_msg, contents);


    /* strings representing non-existent files in RGui */
    untitled = CAR(args); args = CDR(args);
    if (!(TYPEOF(untitled) == STRSXP || untitled == R_NilValue))
        errorcall(call, "%s, must be %s", "invalid second argument", "'character' / / NULL");


    /* strings representing R scripts in RGui */
    r_editor = CAR(args); args = CDR(args);
    if (!(TYPEOF(r_editor) == STRSXP || r_editor == R_NilValue))
        errorcall(call, "%s, must be %s", "invalid third argument", "'character' / / NULL");


    extern SEXP Rgui_path(Rboolean verbose, Rboolean original, Rboolean for_msg,
                          Rboolean contents, SEXP untitled, SEXP r_editor, SEXP rho);


    return Rgui_path(verbose, original, for_msg, contents, untitled, r_editor, rho);
}


SEXP env_or_NULL(SEXP x)
{
    return (x != R_UnboundValue && TYPEOF(x) == ENVSXP) ? x : NULL;
}


void document_context_assign_lines(SEXP documentcontext, SEXP srcfile)
{
    if (documentcontext != R_EmptyEnv) {
        if (R_existsVarInFrame(documentcontext, linesSymbol))
            R_removeVarFromFrame(linesSymbol, documentcontext);
        if (inherits(srcfile, "srcfilecopy") ||
            (
                inherits(srcfile, "srcfilealias") &&
                (srcfile = env_or_NULL(findVarInFrame(srcfile, originalSymbol))) &&
                inherits(srcfile, "srcfilecopy")
            )
           )
        {
            SEXP tmp = findVarInFrame(srcfile, fixedNewlinesSymbol);
            if (tmp == R_UnboundValue || tmp == R_NilValue) {
                SEXP expr = LCONS(_fixNewlinesSymbol, CONS(srcfile, R_NilValue));
                PROTECT(expr);
                defineVar(linesSymbol, makePROMISE(expr, documentcontext), documentcontext);
                UNPROTECT(1);
            }
            else {
                SEXP lines = findVarInFrame(srcfile, linesSymbol);
                if (lines == R_UnboundValue)
                    error(_("object '%s' not found"), CHAR(PRINTNAME(linesSymbol)));
                if (TYPEOF(lines) != STRSXP)
                    error(_("object '%s' of mode '%s' was not found"),
                        CHAR(PRINTNAME(linesSymbol)), "character");
                INCREMENT_NAMED_defineVar(linesSymbol, lines, documentcontext);
            }
        }
    }
}


static R_INLINE
SEXP error_no_associated_path(SEXP rho)
{
    const char *msg = "no associated path";
    SEXP call = getCurrentCall(rho);
    PROTECT(call);
    SEXP cond = ThisPathNotExistsError(msg, call);
    PROTECT(cond);
    stop(cond);
    UNPROTECT(2);
    return R_NilValue;
}


SEXP do_remove_trailing_blank_string do_formals
{
    /* if the last line is a blank string, remove it */


    do_start_no_op_rho("remove_trailing_blank_string", 1);


    SEXP x = CAR(args);
    if (TYPEOF(x) != STRSXP)
        errorcall(call, _("a character vector argument expected"));
    R_xlen_t n = XLENGTH(x);
    if (n) {
        n--;
        if (STRING_ELT(x, n) == R_BlankString) {
            SEXP tmp = allocVector(STRSXP, n);
            PROTECT(tmp);
            for (R_xlen_t i = 0; i < n; i++)
                SET_STRING_ELT(tmp, i, STRING_ELT(x, i));
            x = tmp;
            UNPROTECT(1);
        }
    }
    return x;
}


SEXP fixNewlines(SEXP x)
{
    PROTECT(x);
    R_xlen_t n = XLENGTH(x);
    if (n) {
        Rboolean found_newlines = FALSE;
        R_xlen_t num_new_strings = 0;
        for (R_xlen_t i = 0; i < n; i++) {
            SEXP x0 = STRING_ELT(x, i);
            if (x0 == NA_STRING)
                error("invalid '%s' argument; must not contain NA", "x");
            else if (x0 == R_BlankString);
            else {
                const char *str = CHAR(x0);
                const char *p = strchr(str, '\n');
                if (p) {
                    found_newlines = TRUE;
                    do {
                        str = p + 1;
                        if (*str) {
                            num_new_strings++;
                            p = strchr(str, '\n');
                        }
                        else break;
                    } while (p);
                }
            }
        }
        if (found_newlines) {
            SEXP y = allocVector(STRSXP, n + num_new_strings);
            PROTECT(y);
            for (R_xlen_t ix = 0, iy = 0; ix < n; ix++) {
                SEXP x0 = STRING_ELT(x, ix);
                if (x0 == R_BlankString);
                else {
                    const char *str = CHAR(x0);
                    const char *p = strchr(str, '\n');
                    if (p) {
                        cetype_t enc = getCharCE(x0);
                        do {
                            SET_STRING_ELT(y, iy++, mkCharLenCE(str, p - str, enc));
                            str = p + 1;
                            if (*str) {
                                p = strchr(str, '\n');
                            }
                            else break;
                        } while (p);
                        if (*str) SET_STRING_ELT(y, iy++, mkCharCE(str, enc));
                    }
                    else SET_STRING_ELT(y, iy++, x0);
                }
            }
            UNPROTECT(1);
            x = y;
        }
    }
    UNPROTECT(1);
    return x;
}


SEXP do_fixNewlines do_formals
{
    do_start_no_op_rho("fixNewlines", 1);
    SEXP x = CAR(args);
    if (TYPEOF(x) != STRSXP)
        errorcall(call, _("a character vector argument expected"));
    return fixNewlines(x);
}


// SEXP do_splitlines do_formals
// {
//     do_start_no_op_rho("splitlines", 1);
//     SEXP x = CAR(args);
//     if (!IS_SCALAR(x, STRSXP))
//         errorcall(call, _("argument must be a character string"));
//     const char *str = CHAR(STRING_ELT(x, 0));
//     const char *cr = strchr(str, '\r');
//     // if there are no carriage returns, just split the lines by \n, much easier
//     if (!cr) return fixNewlines(x);
//     return R_NilValue;
// }


typedef enum {
    GUIPATH_DEFAULT  ,
    GUIPATH_FUNCTION ,
    GUIPATH_CHARACTER
} GUIPATH_ACTION;


GUIPATH_ACTION gui_path = GUIPATH_DEFAULT;


SEXP do_set_gui_path do_formals
{
    do_start_no_call_op("set_gui_path", 0);


    SEXP dots = findVarInFrame(rho, R_DotsSymbol);
    if (dots == R_UnboundValue)
        error(_("object '%s' not found"), "...");


    int dots_length = ((TYPEOF(dots) == DOTSXP) ? length(dots) : 0);


    switch (dots_length) {
    case 0:
        gui_path = GUIPATH_DEFAULT;
        break;
    case 1:
    {
        SEXP fun = eval(CAR(dots), R_EmptyEnv);
        ENSURE_NAMEDMAX(fun);
        if (TYPEOF(fun) != CLOSXP)
            error("expected a function; got a %s", type2char(TYPEOF(fun)));


        SEXP args = FORMALS(fun);
        if (TYPEOF(args) == LISTSXP   && TAG(args) == verboseSymbol  &&
            !ISNULL(args = CDR(args)) && TAG(args) == originalSymbol &&
            !ISNULL(args = CDR(args)) && TAG(args) == for_msgSymbol  &&
            !ISNULL(args = CDR(args)) && TAG(args) == contentsSymbol &&
            ISNULL(CDR(args)));
        else error("invalid '%s' argument; must accept the following arguments:\n  (verbose, original, for.msg, contents)", "fun");


        defineVar(_custom_gui_path_functionSymbol, fun, _custom_gui_path_function_environment);
    }
        gui_path = GUIPATH_FUNCTION;
        break;
    case 2:
    case 3:
    {
        SEXP guiname = eval(CAR(dots), R_EmptyEnv);
        if (!IS_SCALAR(guiname, STRSXP) || STRING_ELT(guiname, 0) == NA_STRING)
            error(_("invalid first argument"));
        SEXP path = eval(CADR(dots), R_EmptyEnv);
        if (!IS_SCALAR(path, STRSXP))
            error("invalid '%s' argument; expected a character string", "path");
        if (!is_abs_path(CHAR(STRING_ELT(path, 0))))
            error("invalid '%s' argument; expected an absolute path", "path");
        SEXP _getContents = CDDR(dots);
        if (_getContents == R_NilValue);
        else {
            _getContents = eval(CAR(_getContents), R_EmptyEnv);
            if (_getContents == R_NilValue);
            else if (TYPEOF(_getContents) == CLOSXP) {
                if (FORMALS(_getContents) == R_NilValue)
                    error("invalid '%s' argument; expected a function with at least one formal argument", "getContents");
            }
            else error("invalid '%s' argument; expected a function", "getContents");
        }


        SEXP ofile = findVarInFrame(_custom_gui_path_character_environment, ofileSymbol);
        if (TYPEOF(ofile) != PROMSXP)
            error(_("'%s' is not a promise"), "ofile");
        SEXP file = findVarInFrame(_custom_gui_path_character_environment, fileSymbol);
        if (TYPEOF(file) != PROMSXP)
            error(_("'%s' is not a promise"), "file");


        defineVar(guinameSymbol, STRING_ELT(guiname, 0), _custom_gui_path_character_environment);


        /* restore and evaluate the promise 'ofile' */
        SET_PRCODE(ofile, path);
        SET_PRENV(ofile, R_NilValue);
        SET_PRVALUE(ofile, path);
        SET_PRSEEN(ofile, 0);


        /* restore the promise 'file' to its original state */
        // SET_PRCODE(file, );
        SET_PRENV(file, _custom_gui_path_character_environment);
        SET_PRVALUE(file, R_UnboundValue);
        SET_PRSEEN(file, 0);


        defineVar(_get_contentsSymbol, _getContents, _custom_gui_path_character_environment);
    }
        gui_path = GUIPATH_CHARACTER;
        break;
    default:
        error("%d arguments passed to %s which requires %s",
            dots_length, "set.gui.path()", "0, 1, 2, or 3");
    }


    set_R_Visible(FALSE);
    return R_NilValue;
}


static R_INLINE
SEXP make_path_call(SEXP sym, Rboolean verbose , Rboolean original,
                              Rboolean for_msg , Rboolean contents)
{
    SEXP expr = R_NilValue;
    if (contents) {
        expr = CONS(ScalarLogical(verbose),
                    CONS(ScalarLogical(original),
                         CONS(ScalarLogical(for_msg),
                              CONS(ScalarLogical(contents), expr))));
    } else if (for_msg) {
        expr = CONS(ScalarLogical(verbose),
                    CONS(ScalarLogical(original),
                         CONS(ScalarLogical(for_msg), expr)));
    } else if (original) {
        expr = CONS(ScalarLogical(verbose),
                    CONS(ScalarLogical(original), expr));
    } else if (verbose) {
        expr = CONS(ScalarLogical(verbose), expr);
    }
    return LCONS(sym, expr);
}


SEXP _sys_path(Rboolean verbose         , Rboolean original        ,
               Rboolean for_msg         , Rboolean contents        ,
               Rboolean local           , Rboolean *gave_contents  ,
               int N                    , Rboolean get_frame_number,
               SEXP rho                 )
{
    *gave_contents = FALSE;


    static const char *name = "'this.path(local = TRUE)'";


    SEXP returnvalue;  /* set_documentcontext() creates a variable 'returnvalue'
                          that is used in setpath() (see ./src/setsyspath.c)
                          not used elsewhere but must be declared */
    SEXP documentcontext;


    if (N == NA_INTEGER) {
        if (local)
            N = sys_parent(1, rho);
        else {
            N = asInteger(eval(expr_sys_nframe, rho));
            if (N) --N;
        }
        if (N == NA_INTEGER)
            error(_("invalid '%s' argument"), "N");
    }


    if (local) {


        if (N <= 0) error("%s cannot be used within the global environment", name);
        SEXP frame = eval(expr_parent_frame, rho);


        /* ensure 'this.path(local = TRUE)' isn't evaluated in an invalid environment */
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


        if (!R_existsVarInFrame(frame, documentcontextSymbol))
            error("%s cannot be called within this environment", name);


        /* sys.function(N) */
        INTEGER(CADR(expr_sys_function_which))[0] = N;
        SEXP function = eval(expr_sys_function_which, rho);
        if (function == eval_op)
            error("%s cannot be used within '%s'",
                  name, CHAR(PRINTNAME(R_EvalSymbol)));
        else if (TYPEOF(function) != CLOSXP)
            error("%s cannot be used within a '%s', possible errors with eval?",
                  name, type2char(TYPEOF(function)));
    }


    if (N <= 0) {


#define toplevel                                               \
        if (in_site_file) {                                    \
            if (get_frame_number) return ScalarInteger(-1);    \
            SEXP expr = make_path_call(_site_file_pathSymbol,  \
                verbose, original, for_msg, contents);         \
            PROTECT(expr);                                     \
            SEXP value = eval(expr, mynamespace);              \
            UNPROTECT(1);                                      \
            return value;                                      \
        }                                                      \
        else if (in_init_file) {                               \
            if (get_frame_number) return ScalarInteger(-1);    \
            SEXP expr = make_path_call(_init_file_pathSymbol,  \
                verbose, original, for_msg, contents);         \
            PROTECT(expr);                                     \
            SEXP value = eval(expr, mynamespace);              \
            UNPROTECT(1);                                      \
            return value;                                      \
        }                                                      \
        else {                                                 \
            if (get_frame_number) return ScalarInteger(0);     \
            else return R_UnboundValue;                        \
        }


        toplevel;
    }


    SEXP srcfile, errcnd, ofile, file, lines;


    int nprotect = 0;


    SEXP source      = getFromBase(sourceSymbol     ),
         sys_source  = getFromBase(sys_sourceSymbol ),
         wrap_source = getFromMyNS(wrap_sourceSymbol),
         debugSource = get_debugSource()             ;
    Rboolean rstudio_loaded = (debugSource != R_UnboundValue);


    SEXP ns;
    Rboolean utils_loaded   ; SEXP Sweave;
    Rboolean compiler_loaded; SEXP loadcmp;
    Rboolean box_loaded     ; SEXP load_from_source;
    Rboolean knitr_loaded   ; SEXP knit;
    Rboolean plumber_loaded ; SEXP plumber_sourceUTF8, Plumber_public_methods_initialize;
    Rboolean shiny_loaded   ; SEXP shiny_sourceUTF8;
    Rboolean targets_loaded ; SEXP tar_callr_inner_try, tar_load_globals, tar_source, tar_workspace;
    Rboolean testthat_loaded; SEXP source_file;


    ns = findVarInFrame(R_NamespaceRegistry, utilsSymbol);
    utils_loaded = (ns != R_UnboundValue);
    Sweave = (utils_loaded ? getInFrame(SweaveSymbol, ns, FALSE) : R_UnboundValue);


    ns = findVarInFrame(R_NamespaceRegistry, compilerSymbol);
    compiler_loaded = (ns != R_UnboundValue);
    loadcmp = (compiler_loaded ? getInFrame(loadcmpSymbol, ns, FALSE) : R_UnboundValue);


    ns = findVarInFrame(R_NamespaceRegistry, boxSymbol);
    box_loaded = (ns != R_UnboundValue);
    load_from_source = (box_loaded ? getInFrame(load_from_sourceSymbol, ns, FALSE) : R_UnboundValue);


    ns = findVarInFrame(R_NamespaceRegistry, knitrSymbol);
    knitr_loaded = (ns != R_UnboundValue);
    knit = (knitr_loaded ? getInFrame(knitSymbol, ns, FALSE) : R_UnboundValue);


    ns = findVarInFrame(R_NamespaceRegistry, plumberSymbol);
    plumber_loaded = (ns != R_UnboundValue);
    plumber_sourceUTF8 = (plumber_loaded ? getInFrame(sourceUTF8Symbol, ns, FALSE) : R_UnboundValue);
    Plumber_public_methods_initialize = R_UnboundValue;
    if (plumber_loaded) {
        SEXP tmp = getInFrame(PlumberSymbol, ns, FALSE);
        if (TYPEOF(tmp) == ENVSXP) {
            tmp = getInFrame(public_methodsSymbol, tmp, FALSE);
            if (TYPEOF(tmp) == VECSXP) {
                tmp = getInList(initializeSymbol, tmp, TRUE);
                if (tmp && TYPEOF(tmp) == CLOSXP)
                    Plumber_public_methods_initialize = tmp;
            }
        }
    }


    ns = findVarInFrame(R_NamespaceRegistry, shinySymbol);
    shiny_loaded = (ns != R_UnboundValue);
    shiny_sourceUTF8 = (shiny_loaded ? getInFrame(sourceUTF8Symbol, ns, FALSE) : R_UnboundValue);


    ns = findVarInFrame(R_NamespaceRegistry, targetsSymbol);
    targets_loaded = (ns != R_UnboundValue);
    if (targets_loaded) {
        tar_callr_inner_try = getInFrame(tar_callr_inner_trySymbol, ns, FALSE);
        tar_load_globals    = getInFrame(tar_load_globalsSymbol   , ns, FALSE);
        tar_source          = getInFrame(tar_sourceSymbol         , ns, FALSE);
        tar_workspace       = getInFrame(tar_workspaceSymbol      , ns, FALSE);
    } else {
        tar_callr_inner_try = R_UnboundValue;
        tar_load_globals    = R_UnboundValue;
        tar_source          = R_UnboundValue;
        tar_workspace       = R_UnboundValue;
    }


    ns = findVarInFrame(R_NamespaceRegistry, testthatSymbol);
    testthat_loaded = (ns != R_UnboundValue);
    source_file = (testthat_loaded ? getInFrame(source_fileSymbol, ns, FALSE) : R_UnboundValue);


    SEXP which = allocVector(INTSXP, 1);
    PROTECT(which); nprotect++;
    int *iwhich = INTEGER(which);
    SEXP getframe;
    {
        PROTECT_INDEX indx;
        PROTECT_WITH_INDEX(getframe = CONS(which, R_NilValue), &indx); nprotect++;
        REPROTECT(getframe = LCONS(getFromBase(sys_frameSymbol), getframe), indx);
    }
    SEXP getfunction;
    {
        PROTECT_INDEX indx;
        PROTECT_WITH_INDEX(getfunction = CONS(which, R_NilValue), &indx); nprotect++;
        REPROTECT(getfunction = LCONS(getFromBase(sys_functionSymbol), getfunction), indx);
    }


    SEXP frame, function;


    int minimum_which = ((local) ? N : (1 + asInteger(eval(expr__toplevel_nframe, R_EmptyEnv))));


/* the number of objects protected in each iteration (that must be unprotected
   before moving to the next iteration) */
#define nprotect_loop 1


    for (iwhich[0] = N; iwhich[0] >= minimum_which; iwhich[0]--, UNPROTECT(nprotect_loop)) {
        frame = eval(getframe, rho);
        // PROTECT(frame);
        function = eval(getfunction, rho);
        PROTECT(function);


        /* it might be tempting to put:
         * documentcontext = findVarInFrame(frame, documentcontextSymbol);
         *
         * right here to avoid copy and pasting the same code into each `if`
         * block, but targets::tar_source specifically does not need this, so
         * do it later
         */


        if (identical(function, source)) {
#define source_char "call to function 'source'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = NULL;
            if (R_existsVarInFrame(frame, NeSymbol)) {
                SEXP exprs = getInFrame(exprsSymbol, frame, TRUE);
                if (exprs != R_UnboundValue && TYPEOF(exprs) == EXPRSXP)
                    srcfile = env_or_NULL(getAttrib(exprs, srcfileSymbol));
            }
            if (documentcontext != R_UnboundValue) {


#define check_documentcontext_env                              \
                if (TYPEOF(documentcontext) != ENVSXP)         \
                    error("invalid '%s' value; expected an object of class \"environment\", found \"%s\"",\
                        CHAR(PRINTNAME(documentcontextSymbol)), type2char(TYPEOF(documentcontext)))


/* not used here but used later */
#define check_documentcontext_not_emptyenv                     \
                if (documentcontext == R_EmptyEnv)             \
                    error("invalid '%s' value; expected non-empty document context",\
                        CHAR(PRINTNAME(documentcontextSymbol)))


#define define_no_lock_srcfile_documentcontext                 \
                document_context_assign_lines(documentcontext, srcfile);\
                INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, srcfile)



#define define_srcfile_documentcontext                         \
                define_no_lock_srcfile_documentcontext;        \
                R_LockBinding(documentcontextSymbol, srcfile)


#define if_srcfile_then_define_documentcontext                 \
                if (srcfile) {                                 \
                    define_srcfile_documentcontext;            \
                }


#define ifndef_srcfile_documentcontext_then_define             \
                if (srcfile && !R_existsVarInFrame(srcfile, documentcontextSymbol)) {\
                    define_srcfile_documentcontext;            \
                }


#define overwrite_srcfile_documentcontext                      \
                if (R_BindingIsLocked(documentcontextSymbol, srcfile)) {\
                    R_unLockBinding(documentcontextSymbol, srcfile);\
                    define_srcfile_documentcontext;            \
                }                                              \
                else {                                         \
                    define_no_lock_srcfile_documentcontext;    \
                }


#define maybe_overwrite_srcfile_documentcontext                \
                if (srcfile) {                                 \
                    SEXP tmp = findVarInFrame(srcfile, documentcontextSymbol);\
                    if (tmp == R_UnboundValue) {               \
                        define_srcfile_documentcontext;        \
                    }                                          \
                    else if (tmp == documentcontext);          \
                    else if (tmp == R_EmptyEnv) {              \
                        overwrite_srcfile_documentcontext;     \
                    }                                          \
                    else if (TYPEOF(tmp) != ENVSXP) {          \
                        errorcall(sys_call(which, rho),        \
                            "invalid '%s' value found in srcfile; expected an object of class \"environment\", found \"%s\"",\
                            CHAR(PRINTNAME(documentcontextSymbol)), type2char(TYPEOF(tmp)));\
                    }                                          \
                }


                check_documentcontext_env;
                maybe_overwrite_srcfile_documentcontext;
            }
            else if (srcfile &&
                     !ISUNBOUND(documentcontext = findVarInFrame(srcfile, documentcontextSymbol)) &&
                     documentcontext != R_EmptyEnv)
            {


#define define_frame_documentcontext                           \
                INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, frame);\
                R_LockBinding(documentcontextSymbol, frame)


                check_documentcontext_env;
                define_frame_documentcontext;
            }
            else {
                ofile = findVarInFrame(frame, ofileSymbol);
                if (ofile == R_UnboundValue) continue;


                SEXP wd = findVarInFrame(frame, owdSymbol);
                if (srcfile && wd == R_UnboundValue)
                    wd = findVarInFrame(srcfile, wdSymbol);


#define set_documentcontext2(call, sym, ofile, assign_here, assign_as_binding,\
    normalize_action, maybe_chdir, getowd, hasowd,             \
    character_only, conv2utf8, allow_blank_string,             \
    allow_clipboard, allow_stdin, allow_url, allow_file_uri,   \
    ignore_all, srcfile_original)                              \
                set_documentcontext(                           \
                    call, sym, ofile, assign_here, assign_as_binding, normalize_action,\
                    /* forcepromise           */ FALSE,        \
                    /* assign_returnvalue     */ FALSE,        \
                    maybe_chdir, getowd, hasowd,               \
                    /* ofilearg               */ NULL,         \
                    character_only, conv2utf8, allow_blank_string, allow_clipboard,\
                    allow_stdin, allow_url, allow_file_uri,    \
                    /* allow_unz              */ ( !(character_only) ),\
                    /* allow_pipe             */ ( !(character_only) ),\
                    /* allow_terminal         */ ( !(character_only) ),\
                    /* allow_textConnection   */ ( !(character_only) ),\
                    /* allow_rawConnection    */ ( !(character_only) ),\
                    /* allow_sockconn         */ ( !(character_only) ),\
                    /* allow_servsockconn     */ ( !(character_only) ),\
                    /* allow_customConnection */ ( !(character_only) ),\
                    /* ignore_blank_string    */ (ignore_all), \
                    /* ignore_clipboard       */ (ignore_all), \
                    /* ignore_stdin           */ (ignore_all), \
                    /* ignore_url             */ (ignore_all), \
                    /* ignore_file_uri        */ (ignore_all), \
                    /* source                 */ mkChar(source_char),\
                    srcfile_original                           \
                )


                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ ofileSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* character_only         */ FALSE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ TRUE,
                    /* allow_stdin            */ TRUE,
                    /* allow_url              */ TRUE,
                    /* allow_file_uri         */ TRUE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );


                maybe_overwrite_srcfile_documentcontext;
            }


#define _returnfile(which, source, unprotect) do {             \
            if (documentcontext == R_EmptyEnv) break;          \
            SEXP returnthis = NULL;                            \
            errcnd = findVarInFrame(documentcontext, errcndSymbol);\
            if (errcnd != R_UnboundValue) {                    \
                if (for_msg) {                                 \
                    if (contents) {                            \
                        returnthis = ScalarString(NA_STRING);  \
                    } else {                                   \
                        returnthis = findVarInFrame(documentcontext, for_msgSymbol);\
                        if (returnthis == R_UnboundValue)      \
                            error(_("object '%s' not found"), CHAR(PRINTNAME(for_msgSymbol)));\
                    }                                          \
                }                                              \
                else if (get_frame_number) {                   \
                    if (R_existsVarInFrame(documentcontext, associated_with_fileSymbol))\
                        returnthis = (which);                  \
                    else                                       \
                        returnthis = ScalarInteger(NA_INTEGER);\
                }                                              \
                else {                                         \
                    if (TYPEOF(errcnd) != VECSXP ||            \
                        LENGTH(errcnd) < 2)                    \
                    {                                          \
                        error(_("invalid '%s' value"), CHAR(PRINTNAME(errcndSymbol)));\
                    }                                          \
                    errcnd = duplicate(errcnd);                \
                    PROTECT(errcnd);                           \
                    SET_VECTOR_ELT(errcnd, 1, getCurrentCall(rho));\
                    stop(errcnd);                              \
                    UNPROTECT(1);  /* errcnd */                \
                    /* should not reach here */                \
                    returnthis = R_NilValue;                   \
                }                                              \
            }                                                  \
            else if (contents && !ISUNBOUND(lines = findVarInFrame(documentcontext, linesSymbol))) {\
                if (TYPEOF(lines) == PROMSXP) {                \
                    if (PRVALUE(lines) == R_UnboundValue) {    \
                        PROTECT(lines);                        \
                        lines = eval(lines, R_EmptyEnv);       \
                        UNPROTECT(1);                          \
                    }                                          \
                    else lines = PRVALUE(lines);               \
                }                                              \
                if (TYPEOF(lines) != STRSXP)                   \
                    error(_("invalid '%s' value"), CHAR(PRINTNAME(linesSymbol)));\
                returnthis = lines;                            \
                *gave_contents = TRUE;                         \
            }                                                  \
            else if (get_frame_number) {                       \
                returnthis = (which);                          \
            }                                                  \
            else if (original == TRUE) {                       \
                ofile = findVarInFrame(documentcontext, ofileSymbol);\
                if (ofile == R_UnboundValue)                   \
                    error(_("object '%s' not found"), CHAR(PRINTNAME(ofileSymbol)));\
                returnthis = ofile;                            \
            }                                                  \
            else {                                             \
                file = findVarInFrame(documentcontext, fileSymbol);\
                if (file == R_UnboundValue)                    \
                    error(_("object '%s' not found"), CHAR(PRINTNAME(fileSymbol)));\
                if (TYPEOF(file) != PROMSXP)                   \
                    error("invalid '%s', is not a promise; should never happen, please report!", CHAR(PRINTNAME(fileSymbol)));\
                if (PRVALUE(file) == R_UnboundValue) {         \
                    if (original || for_msg) {                 \
                        ofile = findVarInFrame(documentcontext, ofileSymbol);\
                        if (ofile == R_UnboundValue)           \
                            error(_("object '%s' not found"), CHAR(PRINTNAME(ofileSymbol)));\
                        returnthis = ofile;                    \
                    }                                          \
                    else {                                     \
                        if (PRSEEN(file)) {                    \
                            if (PRSEEN(file) == 1);            \
                            else SET_PRSEEN(file, 0);          \
                        }                                      \
                        returnthis = eval(file, R_EmptyEnv);   \
                    }                                          \
                }                                              \
                else returnthis = PRVALUE(file);               \
            }                                                  \
            if (verbose) {                                     \
                SEXP osource = findVarInFrame(documentcontext, sourceSymbol);\
                if (osource == R_UnboundValue)                 \
                    error(_("object '%s' not found"), CHAR(PRINTNAME(sourceSymbol)));\
                if (TYPEOF(osource) != CHARSXP)                \
                    error(_("invalid '%s' value"), CHAR(PRINTNAME(sourceSymbol)));\
                if (streql(source, CHAR(osource)))             \
                    Rprintf("Source: %s\n", source);           \
                else                                           \
                    Rprintf("Source: %s, copied from %s\n", source, CHAR(osource));\
            }                                                  \
            UNPROTECT((unprotect));                            \
            return returnthis;                                 \
                                    } while (0)


#define returnfile(which, source) _returnfile((which), (source), (nprotect_loop + nprotect))


            returnfile(which, source_char);
        }


        else if (identical(function, sys_source)) {
#undef source_char
#define source_char "call to function 'sys.source'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = env_or_NULL(findVarInFrame(frame, srcfileSymbol));
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = findVarInFrame(srcfile, documentcontextSymbol))) {


#define check_path_only do {                                   \
                ofile = getInFrame(fileSymbol, frame, FALSE);  \
                if (!IS_SCALAR(ofile, STRSXP))                 \
                    error(_("'%s' must be a character string"), CHAR(PRINTNAME(fileSymbol)));\
                file = STRING_ELT(ofile, 0);                   \
                const char *url = CHAR(file);                  \
                if (!(LENGTH(file) > 0))                       \
                    errorcall(sys_call(which, rho), "invalid '%s', must not be \"\""     , CHAR(PRINTNAME(fileSymbol)));\
                else if (is_clipboard(url))                    \
                    errorcall(sys_call(which, rho), "invalid '%s', %s"                   , CHAR(PRINTNAME(fileSymbol)), must_not_be_clipboard_message);\
                else if (streql(url, "stdin"))                 \
                    errorcall(sys_call(which, rho), "invalid '%s', must not be \"stdin\"", CHAR(PRINTNAME(fileSymbol)));\
                else if (is_url(url))                          \
                    errorcall(sys_call(which, rho), "invalid '%s', cannot be a URL"      , CHAR(PRINTNAME(fileSymbol)));\
                else if (is_file_uri(url))                     \
                    errorcall(sys_call(which, rho), "invalid '%s', cannot be a file URI" , CHAR(PRINTNAME(fileSymbol)));\
                        } while (0)


                check_documentcontext_env;
                check_path_only;
                check_documentcontext_not_emptyenv;
                define_frame_documentcontext;
            }
            else {
                ofile = findVarInFrame(frame, fileSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), CHAR(PRINTNAME(fileSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                SEXP wd = findVarInFrame(frame, owdSymbol);
                if (srcfile && wd == R_UnboundValue)
                    wd = findVarInFrame(srcfile, wdSymbol);
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
                if (srcfile) {
                    define_srcfile_documentcontext;
                }
            }
            returnfile(which, source_char);
        }


        else if (identical(function, wrap_source)) {
#undef source_char
#define source_char "call to function wrap.source from package @R_PACKAGE_NAME@"
            if (local)
                error("%s cannot be called within %s() from package %s",
                      name, CHAR(PRINTNAME(wrap_sourceSymbol)), "@R_PACKAGE_NAME@");
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = NULL;
            if (documentcontext == R_UnboundValue)
                error(_("object '%s' not found"), CHAR(PRINTNAME(documentcontextSymbol)));
            if (documentcontext == R_EmptyEnv) continue;
            check_documentcontext_env;
            SEXP n = findVarInFrame(documentcontext, nSymbol);
            if (!IS_SCALAR(n, INTSXP))
                error(_("invalid '%s' value"), CHAR(PRINTNAME(nSymbol)));
            returnfile(n, source_char);
        }


        else if (rstudio_loaded && identical(function, debugSource)) {
#undef source_char
#define source_char "call to function 'debugSource' in 'RStudio'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = NULL;
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
            }
            else {
                ofile = findVarInFrame(frame, fileNameSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), CHAR(PRINTNAME(fileNameSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileNameSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ FALSE,
                    /* getowd                 */ NULL,
                    /* hasowd                 */ FALSE,
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ TRUE,
                    /* allow_blank_string     */ TRUE,
                    /* allow_clipboard        */ TRUE,
                    /* allow_stdin            */ TRUE,
                    /* allow_url              */ TRUE,
                    /* allow_file_uri         */ TRUE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
            }
            returnfile(which, source_char);
        }


        else if (utils_loaded && identical(function, Sweave)) {
#undef source_char
#define source_char "call to function 'Sweave' from package 'utils'"
            SEXP documentcontexts = findVarInFrame(frame, documentcontextsSymbol);
            SEXP names;
            if (documentcontexts == R_UnboundValue) {
                documentcontexts = allocVector(VECSXP, 8);
                INCREMENT_NAMED_defineVar(documentcontextsSymbol, documentcontexts, frame);
                R_LockBinding(documentcontextsSymbol, frame);
                names = allocVector(STRSXP, XLENGTH(documentcontexts));
                setAttrib(documentcontexts, R_NamesSymbol, names);


#if R_version_less_than(3, 4, 0)
#define SET_GROWABLE_BIT(x)
#endif


                SET_GROWABLE_BIT(documentcontexts);
                SET_TRUELENGTH(documentcontexts, XLENGTH(documentcontexts));
                SETLENGTH(documentcontexts, 0);


                SET_GROWABLE_BIT(names);
                SET_TRUELENGTH(names, TRUELENGTH(documentcontexts));
                SETLENGTH(names, XLENGTH(documentcontexts));
            }
            else {
                if (TYPEOF(documentcontexts) != VECSXP)
                    error("invalid '%s' value; expected an object of class \"list\", found \"%s\"",
                        CHAR(PRINTNAME(documentcontextsSymbol)), type2char(TYPEOF(documentcontexts)));
                names = getAttrib(documentcontexts, R_NamesSymbol);
                if (TYPEOF(names) != STRSXP)
                    error("invalid '%s' value; expected an object of class \"character\", found \"%s\"",
                        CHAR(PRINTNAME(R_NamesSymbol)), type2char(TYPEOF(names)));
            }
            SEXP ofile = findVarInFrame(frame, fileSymbol);
            if (!IS_SCALAR(ofile, STRSXP)) continue;
            SEXP file = STRING_ELT(ofile, 0);
            R_xlen_t indx = -999;
            for (R_xlen_t i = 0, n = xlength(names); i < n; i++) {
                if (STRING_ELT(names, i) == file) {
                    indx = i;
                    break;
                }
            }
            if (indx < 0) {
                indx = XLENGTH(documentcontexts);
                if (indx >= TRUELENGTH(documentcontexts)) {
                    double dindx = 2.0 * (double) indx;
                    if (dindx > R_XLEN_T_MAX) error("too many files");
                    SEXP xdocumentcontexts = allocVector(VECSXP, 2 * indx);
                    PROTECT(xdocumentcontexts);
                    SEXP xnames = allocVector(STRSXP, XLENGTH(xdocumentcontexts));
                    setAttrib(xdocumentcontexts, R_NamesSymbol, xnames);


                    SET_GROWABLE_BIT(xdocumentcontexts);
                    SET_TRUELENGTH(xdocumentcontexts, XLENGTH(xdocumentcontexts));
                    SETLENGTH(xdocumentcontexts, indx);


                    SET_GROWABLE_BIT(xnames);
                    SET_TRUELENGTH(xnames, TRUELENGTH(xdocumentcontexts));
                    SETLENGTH(xnames, XLENGTH(xdocumentcontexts));


                    for (R_xlen_t i = 0; i < indx; i++) {
                        SET_VECTOR_ELT(xdocumentcontexts, i, VECTOR_ELT(documentcontexts, i));
                        SET_STRING_ELT(xnames, i, STRING_ELT(names, i));
                    }


                    documentcontexts = xdocumentcontexts;
                    names = xnames;
                    if (R_BindingIsLocked(documentcontextsSymbol, frame)) {
                        R_unLockBinding(documentcontextsSymbol, frame);
                        INCREMENT_NAMED_defineVar(documentcontextsSymbol, documentcontexts, frame);
                        R_LockBinding(documentcontextsSymbol, frame);
                    }
                    // if the user unlocked the binding, leave it unlocked,
                    // they probably have a good reason for it being so
                    else INCREMENT_NAMED_defineVar(documentcontextsSymbol, documentcontexts, frame);
                    UNPROTECT(1);
                }
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ NULL,
                    /* assign_as_binding      */ (error("invalid; %s %d", __FILE__, __LINE__), TRUE),
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ FALSE,
                    /* getowd                 */ NULL,
                    /* hasowd                 */ FALSE,
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
                SETLENGTH(documentcontexts, indx + 1);
                SETLENGTH(names, indx + 1);
                SET_VECTOR_ELT(documentcontexts, indx, documentcontext);
                SET_STRING_ELT(names, indx, file);
            }
            else {
                documentcontext = VECTOR_ELT(documentcontexts, indx);
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
            }
            returnfile(which, source_char);
        }


        else if (compiler_loaded && identical(function, loadcmp)) {
#undef source_char
#define source_char "call to function 'loadcmp' from package 'compiler'"
            /* much the same as sys.source() */
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = NULL;
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
            }
            else {
                ofile = findVarInFrame(frame, fileSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), CHAR(PRINTNAME(fileSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ findVarInFrame(frame, owdSymbol),
                    /* hasowd                 */ ((owd) != R_UnboundValue),
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
            }
            returnfile(which, source_char);
        }


        else if (box_loaded && identical(function, load_from_source)) {
#undef source_char
#define source_char "call to function 'load_from_source' from package 'box'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = NULL;
            SEXP exprs = findVarInFrame(frame, exprsSymbol);
            if (exprs != R_UnboundValue && TYPEOF(exprs) == EXPRSXP)
                srcfile = env_or_NULL(getAttrib(exprs, srcfileSymbol));
            SEXP mod_ns = getInFrame(mod_nsSymbol, frame, TRUE);
            if (mod_ns == R_UnboundValue || TYPEOF(mod_ns) != ENVSXP) mod_ns = NULL;
            if (documentcontext != R_UnboundValue) {


#define if_mod_ns_then_define_documentcontext                  \
                if (mod_ns) {                                  \
                    setAttrib(mod_ns, documentcontextSymbol, documentcontext);\
                }


#define ifndef_mod_ns_documentcontext_then_define              \
                if (mod_ns && ISNULL(getAttrib(mod_ns, documentcontextSymbol))) {\
                    setAttrib(mod_ns, documentcontextSymbol, documentcontext);\
                }


                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
                ifndef_mod_ns_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = findVarInFrame(srcfile, documentcontextSymbol))) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                define_frame_documentcontext;
                ifndef_mod_ns_documentcontext_then_define;
            }
            else if (mod_ns && !ISNULL(documentcontext = getAttrib(mod_ns, documentcontextSymbol))) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                define_frame_documentcontext;
                if_srcfile_then_define_documentcontext;
            }
            else {
                SEXP ofile = eval(expr_info_dollar_source_path, frame);
                PROTECT(ofile);
                SEXP wd = srcfile ? findVarInFrame(srcfile, wdSymbol) : R_UnboundValue;
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ info_source_pathSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
                UNPROTECT(1);  /* ofile */
                if_srcfile_then_define_documentcontext;
                if_mod_ns_then_define_documentcontext;
            }
            returnfile(which, source_char);
        }


        else if (knitr_loaded && identical(function, knit)) {
#undef source_char
#define source_char "call to function 'knit' from package 'knitr'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = NULL;
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
            }
            else {
                if (!R_existsVarInFrame(frame, oenvirSymbol)) continue;
                int missing_input = asLogical(eval(expr_missing_input, frame));
                if (missing_input) {
                    documentcontext = R_EmptyEnv;
                    define_frame_documentcontext;
                    continue;
                }
                ofile = getInFrame(inputSymbol, frame, FALSE);
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ inputSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ eval(expr_knitr_output_dir, R_EmptyEnv),
                    /* hasowd                 */ ((owd) != R_NilValue),
                    /* character_only         */ FALSE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ TRUE,
                    /* allow_stdin            */ TRUE,
                    /* allow_url              */ TRUE,
                    /* allow_file_uri         */ TRUE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
            }
            returnfile(which, source_char);
        }


        else if (plumber_loaded && identical(function, plumber_sourceUTF8)) {
#undef source_char
#define source_char "call to function 'sourceUTF8' from package 'plumber'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = NULL;
            SEXP exprs = findVarInFrame(frame, exprsSymbol);
            if (exprs != R_UnboundValue && TYPEOF(exprs) == EXPRSXP)
                srcfile = env_or_NULL(getAttrib(exprs, srcfileSymbol));
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = findVarInFrame(srcfile, documentcontextSymbol))) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                define_frame_documentcontext;
            }
            else {
                SEXP ofile = findVarInFrame(frame, fileSymbol);
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                SEXP wd = srcfile ? findVarInFrame(srcfile, wdSymbol) : R_UnboundValue;
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
                if_srcfile_then_define_documentcontext;
            }
            returnfile(which, source_char);
        }


        else if (shiny_loaded && identical(function, shiny_sourceUTF8)) {
#undef source_char
#define source_char "call to function 'sourceUTF8' from package 'shiny'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = env_or_NULL(findVarInFrame(frame, srcSymbol));
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = findVarInFrame(srcfile, documentcontextSymbol))) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                define_frame_documentcontext;
            }
            else {
                SEXP wd, sym;
                if (srcfile) {
                    ofile = findVarInFrame(srcfile, sym = filenameSymbol);
                    wd = findVarInFrame(srcfile, wdSymbol);
                }
                else {
                    ofile = findVarInFrame(frame, sym = fileSymbol);
                    wd = R_UnboundValue;
                }
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ sym,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
                if_srcfile_then_define_documentcontext;
            }
            returnfile(which, source_char);
        }


        else if (targets_loaded && identical(function, tar_callr_inner_try)) {
#undef source_char
#define source_char "call to function 'tar_callr_inner_try' from package 'targets'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = NULL;
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
            }
            else {
                SEXP ofile = findVarInFrame(frame, scriptSymbol);
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ scriptSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ FALSE,
                    /* getowd                 */ NULL,
                    /* hasowd                 */ FALSE,
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
            }
            returnfile(which, source_char);
        }


        else if (targets_loaded && identical(function, tar_load_globals)) {
#undef source_char
#define source_char "call to function 'tar_load_globals' from package 'targets'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = NULL;
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
            }
            else {
                SEXP ofile = findVarInFrame(frame, scriptSymbol);
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ scriptSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ FALSE,
                    /* getowd                 */ NULL,
                    /* hasowd                 */ FALSE,
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
            }
            returnfile(which, source_char);
        }


        else if (targets_loaded && identical(function, tar_source)) {
#undef source_char
#define source_char "call to function 'tar_source' from package 'targets'"
            /* this one is a legitimate use of 'continue', do not edit out */
            if (!R_existsVarInFrame(frame, non_r_scriptsSymbol)) continue;
            iwhich[0] += 2;
            frame = eval(getframe, rho);
            iwhich[0] -= 2;
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = NULL;
            SEXP expr = findVarInFrame(frame, exprSymbol);
            if (expr != R_UnboundValue && TYPEOF(expr) == EXPRSXP)
                srcfile = env_or_NULL(getAttrib(expr, srcfileSymbol));
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = findVarInFrame(srcfile, documentcontextSymbol))) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                define_frame_documentcontext;
                defineVar(setsyspathwashereSymbol, R_FalseValue, documentcontext);
            }
            else {
                SEXP ofile = findVarInFrame(frame, scriptSymbol);
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                SEXP wd = srcfile ? findVarInFrame(srcfile, wdSymbol) : findVarInFrame(frame, oldSymbol);
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ scriptSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
                defineVar(setsyspathwashereSymbol, R_FalseValue, documentcontext);
                if_srcfile_then_define_documentcontext;
            }
            if (get_frame_number) {
                if (documentcontext == R_EmptyEnv) continue;
                iwhich[0] += 2;
            }
            returnfile(which, source_char);
        }


        else if (targets_loaded && identical(function, tar_workspace)) {
#undef source_char
#define source_char "call to function 'tar_workspace' from package 'targets'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = NULL;
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
            }
            else {
                {
                    SEXP tmp = findVarInFrame(frame, sourceSymbol);
                    if (TYPEOF(tmp) == PROMSXP) {
                        /* if tmp is a promise already under evaluation */
                        if (PRSEEN(tmp) == 1) continue;
                        /* we expect this promise to already be forced */
                        if (PRVALUE(tmp) == R_UnboundValue) continue;
                        tmp = PRVALUE(tmp);
                    }
                    if (asLogical(tmp) != TRUE) continue;
                }
                SEXP ofile = findVarInFrame(frame, scriptSymbol);
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ scriptSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ FALSE,
                    /* getowd                 */ NULL,
                    /* hasowd                 */ FALSE,
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
            }
            returnfile(which, source_char);
        }


        else if (testthat_loaded && identical(function, source_file)) {
#undef source_char
#define source_char "call to function 'source_file' from package 'testthat'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            srcfile = env_or_NULL(findVarInFrame(frame, srcfileSymbol));
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                maybe_overwrite_srcfile_documentcontext;
            }
            else if (srcfile &&
                     !ISUNBOUND(documentcontext = findVarInFrame(srcfile, documentcontextSymbol)) &&
                     documentcontext != R_EmptyEnv)
            {
                check_documentcontext_env;
                define_frame_documentcontext;
            }
            else {
                ofile = findVarInFrame(frame, pathSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), CHAR(PRINTNAME(pathSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                int ignore_all = asLogical(eval(expr_testthat_source_file_uses_brio_read_lines, R_EmptyEnv));
                SEXP wd = findVarInFrame(frame, old_dirSymbol);
                if (srcfile && wd == R_UnboundValue)
                    wd = findVarInFrame(srcfile, wdSymbol);
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ pathSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    ignore_all,
                    /* srcfile_original       */ NULL
                );
                maybe_overwrite_srcfile_documentcontext;
            }
            returnfile(which, source_char);
        }


        /* this condition must be second last */
        else if (plumber_loaded && identical_ignore_bytecode_ignore_environment(function, Plumber_public_methods_initialize)) {
#undef source_char
#define source_char "call to function 'Plumber$public_methods$initialize' from package 'plumber'"
            documentcontext = findVarInFrame(frame, documentcontextSymbol);
            SEXP private_env = findVarInFrame(CLOENV(function), privateSymbol);
            srcfile = NULL;
            if (TYPEOF(private_env) == ENVSXP) {
                SEXP parsed = findVarInFrame(private_env, parsedSymbol);
                if (TYPEOF(parsed) == EXPRSXP) {
                    srcfile = env_or_NULL(getAttrib(parsed, srcfileSymbol));
                }
            }
            if (documentcontext != R_UnboundValue) {
                check_documentcontext_env;
                check_documentcontext_not_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = findVarInFrame(srcfile, documentcontextSymbol))) {
                check_documentcontext_env;
                check_path_only;
                check_documentcontext_not_emptyenv;
                define_frame_documentcontext;
            }
            else {
                if (!private_env) continue;
                ofile = findVarInFrame(private_env, filenameSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), CHAR(PRINTNAME(filenameSymbol)));
                SEXP wd = findVarInFrame(frame, old_wdSymbol);
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ filenameSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != R_NilValue),
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
                if_srcfile_then_define_documentcontext;
            }
            returnfile(which, source_char);
        }


        /* this condition must be last */
        else if (!ISUNBOUND(documentcontext = findVarInFrame(frame, documentcontextSymbol))) {
            if (documentcontext == R_EmptyEnv) continue;
            check_documentcontext_env;


            {
                SEXP tmp = findVarInFrame(documentcontext, setsyspathwashereSymbol);
                if (tmp == R_UnboundValue)
                    error(_("object '%s' not found"), CHAR(PRINTNAME(setsyspathwashereSymbol)));
                if (asLogical(tmp) == FALSE) {
                    if (local) error("expected TRUE value for '%s'", CHAR(PRINTNAME(setsyspathwashereSymbol)));
                    continue;
                }
            }


            srcfile = NULL;


            SEXP source = findVarInFrame(documentcontext, sourceSymbol);
            if (source == R_UnboundValue)
                error(_("object '%s' not found"), CHAR(PRINTNAME(sourceSymbol)));
            if (TYPEOF(source) != CHARSXP)
                error(_("invalid '%s' value"), CHAR(PRINTNAME(sourceSymbol)));


            SEXP n = findVarInFrame(documentcontext, nSymbol);
            if (!IS_SCALAR(n, INTSXP))
                error(_("invalid '%s' value"), CHAR(PRINTNAME(nSymbol)));
            /* this could happen with eval() or similar */
            if (iwhich[0] != INTEGER(n)[0]) continue;


            returnfile(n, CHAR(source));


#undef returnfile
        }
    }


    UNPROTECT(nprotect);


    if (local) {
        if (for_msg) return ScalarString(NA_STRING);
        return error_no_associated_path(rho);
    }


    toplevel;


#undef toplevel
}


SEXP sys_path8(Rboolean verbose         , Rboolean original        ,
               Rboolean for_msg         , Rboolean contents        ,
               Rboolean local           , int N                    ,
               Rboolean get_frame_number, SEXP rho                 )
{
    Rboolean gave_contents;
    SEXP value = _sys_path(verbose         , original        , for_msg         ,
                           contents        , local           , &gave_contents  ,
                           N               , get_frame_number, rho             );
    if (value != R_UnboundValue) {
        if (!contents)
            return value;
        if (gave_contents)
            return value;
        if (!IS_SCALAR(value, STRSXP))
            error("internal error; invalid '%s' value", "_sys_path()");
        if (STRING_ELT(value, 0) == NA_STRING)
            return R_NilValue;
        SEXP expr = LCONS(_get_contentsSymbol, CONS(value, R_NilValue));
        PROTECT(expr);
        value = eval(expr, mynamespace);
        UNPROTECT(1);
        return value;
    }


    switch (gui_path) {
    case GUIPATH_DEFAULT:
    {
        SEXP expr = make_path_call(_gui_pathSymbol, verbose , original,
                                                    for_msg , contents);
        PROTECT(expr);
        SEXP value = eval(expr, mynamespace);
        UNPROTECT(1);


        if (!contents)
            return value;
        if (IS_SCALAR(value, VECSXP))
            return VECTOR_ELT(value, 0);
        if (!IS_SCALAR(value, STRSXP))
            error("internal error; invalid '%s()' value", CHAR(PRINTNAME(_gui_pathSymbol)));
        if (STRING_ELT(value, 0) == NA_STRING)
            return R_NilValue;
        expr = LCONS(_get_contentsSymbol, CONS(value, R_NilValue));
        PROTECT(expr);
        value = eval(expr, mynamespace);
        UNPROTECT(1);
        return value;
    }
    case GUIPATH_FUNCTION:
    {
        SEXP expr = LCONS(_custom_gui_path_functionSymbol,
                          CONS(ScalarLogical(verbose),
                               CONS(ScalarLogical(original),
                                    CONS(ScalarLogical(for_msg),
                                         CONS(ScalarLogical(contents), R_NilValue)))));
        PROTECT(expr);
        SEXP value = eval(expr, _custom_gui_path_function_environment);
        PROTECT(value);
        if (contents) {
            if (for_msg && IS_SCALAR(value, STRSXP) && STRING_ELT(value, 0) == NA_STRING)
                value = R_NilValue;
            else if (TYPEOF(value) == STRSXP)
                value = fixNewlines(value);
        }
        else {
            if (!IS_SCALAR(value, STRSXP))
                errorcall(expr, "invalid return value; must be a character string");
            if (for_msg);
            else if (is_abs_path(CHAR(STRING_ELT(value, 0))));
            else errorcall(expr, "invalid return value; must be an absolute path");
        }
        UNPROTECT(2);
        return value;
    }
    case GUIPATH_CHARACTER:
    {
        SEXP env = _custom_gui_path_character_environment;


        if (verbose) {
            SEXP guiname = findVarInFrame(env, guinameSymbol);
            if (TYPEOF(guiname) != CHARSXP)
                error(_("object '%s' of mode '%s' was not found"),
                    CHAR(PRINTNAME(guinameSymbol)), "char");
            Rprintf("Source: document in %s\n", CHAR(guiname));
        }


        if (contents) {
            for_msg = FALSE;
            SEXP file = get_file_from_closure(original, for_msg, env);
            SEXP expr = LCONS(_get_contentsSymbol, CONS(file, R_NilValue));
            PROTECT(expr);
            SEXP value;
            SEXP _getContents = findVarInFrame(env, _get_contentsSymbol);
            if (_getContents != R_NilValue) {
                if (TYPEOF(_getContents) != CLOSXP)
                    error(_("object '%s' of mode '%s' was not found"),
                        CHAR(PRINTNAME(_get_contentsSymbol)), "function");
                value = eval(expr, env);
                if (TYPEOF(value) == STRSXP)
                    value = fixNewlines(value);
            }
            else {
                value = eval(expr, mynamespace);
            }
            UNPROTECT(1);
            return value;
        }
        return get_file_from_closure(original, for_msg, env);
    }
    default:
        errorcall(R_NilValue, "internal error; invalid 'gui_path' value");
    }


    return R_NilValue;
}


SEXP sys_path6(Rboolean verbose , Rboolean original, Rboolean for_msg ,
               Rboolean contents, Rboolean local   , SEXP rho         )
{
    return sys_path8(verbose, original, for_msg, contents, local,
                     /* N */ NA_INTEGER, /* get_frame_number */ FALSE,  rho);
}


SEXP do_sys_path do_formals
{
    do_start_no_op("sys_path", -1);


    Rboolean verbose  = FALSE,
             original = FALSE,
             for_msg  = FALSE,
             contents = FALSE,
             local    = FALSE;


    switch (length(args)) {
    case 0:
        break;
    case 1:
        local    = asLogical(CAR(args)); args = CDR(args);
        break;
    case 2:
        verbose  = asLogical(CAR(args)); args = CDR(args);
        local    = asLogical(CAR(args)); args = CDR(args);
        break;
    case 5:
        verbose  = asLogical(CAR(args)); args = CDR(args);
        original = asLogical(CAR(args)); args = CDR(args);
        for_msg  = asLogical(CAR(args)); args = CDR(args);
        contents = asLogical(CAR(args)); args = CDR(args);
        local    = asLogical(CAR(args)); args = CDR(args);
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), ".C_sys_path", "0, 1, 2, or 5"));
        return R_NilValue;
    }


    check_arguments5(verbose, original, for_msg, contents, local);
    return sys_path6(verbose, original, for_msg, contents, local, rho);
}


SEXP do_getframenumber do_formals
{
    do_start_no_call_op("getframenumber", 0);
    return sys_path8(/* verbose */ FALSE, /* original */ FALSE,
                     /* for_msg */ FALSE, /* contents */ FALSE,
                     /* local */ FALSE, /* N */ NA_INTEGER,
                     /* get_frame_number */ TRUE, rho);
}


SEXP invalid_get_frame_number_value(void)
{
    error(_("invalid '%s' value"), "get_frame_number");
    return R_NilValue;
}


SEXP _env_path(Rboolean verbose, Rboolean original, Rboolean for_msg,
               Rboolean contents, SEXP target, SEXP envir,
               Rboolean *gave_contents, Rboolean unbound_ok, SEXP rho)
{
    *gave_contents = FALSE;
    Rboolean get_frame_number = FALSE;


    int nprotect = 0;


    if (envir == NULL) envir = eval(expr_parent_frame, rho);
    if (TYPEOF(envir) != ENVSXP) envir = rho;


    SEXP env;
    if (target == NULL) {
        target = PROTECT(eval(expr_getOption_topLevelEnvironment, rho));


#define getenv                                                 \
        if (target != R_NilValue && TYPEOF(target) != ENVSXP) target = R_NilValue;\
        env = topenv(target, envir)


        getenv;


        UNPROTECT(1);
    } else {
        getenv;


#undef getenv
    }


    SEXP returnvalue;
    SEXP documentcontext;


    SEXP errcnd, ofile, file, lines, path;


    if (env == R_GlobalEnv ||
        env == R_BaseEnv || env == R_BaseNamespace ||
        R_IsPackageEnv(env) || R_IsNamespaceEnv(env));
    else if (inherits(env, "box$ns")) {
#undef source_char
#define source_char "path of a 'package:box' namespace"
        documentcontext = getAttrib(env, documentcontextSymbol);
        if (documentcontext != R_NilValue) {
            check_documentcontext_env;
        }
        else {
            SEXP info = findVarInFrame(env, moduleSymbol);
            if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
                SEXP spec = findVarInFrame(info, infoSymbol);
                if (spec != R_UnboundValue && TYPEOF(spec) == VECSXP) {
                    SEXP names = getAttrib(spec, R_NamesSymbol);
                    if (names != R_NilValue && TYPEOF(names) == STRSXP) {
                        for (R_xlen_t i = 0, n = XLENGTH(spec); i < n; i++) {
                            if (!strcmp(CHAR(STRING_ELT(names, i)), "source_path")) {
                                path = VECTOR_ELT(spec, i);
                                if (TYPEOF(path) == STRSXP && XLENGTH(path) > 0 &&
                                    is_abs_path(CHAR(STRING_ELT(path, 0))))
                                {
                                    PROTECT(documentcontext = DocumentContext());
                                    PROTECT(ofile = ScalarString(STRING_ELT(path, 0)));
                                    assign_default(NULL, NULL, ofile, ofile, documentcontext, NA_DEFAULT);
                                    INCREMENT_NAMED_defineVar(sourceSymbol, mkChar(source_char), documentcontext);
                                    setAttrib(env, documentcontextSymbol, documentcontext);
                                    UNPROTECT(2);
                                }
                            }
                        }
                    }
                }
            }
            if (documentcontext == R_NilValue)
                error("invalid 'package:box' namespace without an associated path");
        }
#define returnfile _returnfile((invalid_get_frame_number_value()), (source_char), (nprotect))


        returnfile;
    }
    else if (!ISNULL(documentcontext = getAttrib(env, documentcontextSymbol)))
    {
#undef source_char
#define source_char "path of top level environment"
        check_documentcontext_env;
        returnfile;
    }
    else if (isString(path = getAttrib(env, pathSymbol)) && XLENGTH(path) > 0)
    {
        PROTECT(ofile = ScalarString(STRING_ELT(path, 0))); nprotect++;
        const char *str = CHAR(STRING_ELT(ofile, 0));
        if (is_abs_path(str) || is_url(str) || is_file_uri(str)) {
            set_documentcontext2(
                /* call                   */ R_CurrentExpression,
                /* sym                    */ pathSymbol,
                /* ofile                  */ ofile,
                /* assign_here            */ env,
                /* assign_as_binding      */ FALSE,
                /* normalize_action       */ NA_NOT_DIR,
                /* maybe_chdir            */ FALSE,
                /* getowd                 */ NULL,
                /* hasowd                 */ FALSE,
                /* character_only         */ TRUE,
                /* conv2utf8              */ FALSE,
                /* allow_blank_string     */ FALSE,
                /* allow_clipboard        */ FALSE,
                /* allow_stdin            */ FALSE,
                /* allow_url              */ TRUE,
                /* allow_file_uri         */ TRUE,
                /* ignore_all             */ FALSE,
                /* srcfile_original       */ NULL
            );
            returnfile;
        }
    }


    UNPROTECT(nprotect);


    if (unbound_ok)
        return R_UnboundValue;
    else if (for_msg)
        return ScalarString(NA_STRING);
    else return error_no_associated_path(rho);
}


SEXP env_path8(Rboolean verbose, Rboolean original, Rboolean for_msg,
               Rboolean contents, SEXP target, SEXP envir, Rboolean unbound_ok,
               SEXP rho)
{
    Rboolean gave_contents;
    SEXP value = _env_path(verbose       , original      , for_msg       ,
                           contents      , target        , envir         ,
                           &gave_contents, unbound_ok    , rho           );
    if (unbound_ok && value == R_UnboundValue)
        return value;
    if (!contents)
        return value;
    if (gave_contents)
        return value;
    if (!IS_SCALAR(value, STRSXP))
        error("internal error; invalid '%s' value", "_env_path()");
    if (STRING_ELT(value, 0) == NA_STRING)
        return R_NilValue;
    SEXP expr = LCONS(_get_contentsSymbol, CONS(value, R_NilValue));
    PROTECT(expr);
    value = eval(expr, mynamespace);
    UNPROTECT(1);
    return value;
}


SEXP env_path7(Rboolean verbose, Rboolean original, Rboolean for_msg,
               Rboolean contents, SEXP target, SEXP envir, SEXP rho)
{
    return env_path8(verbose, original, for_msg, contents, target, envir,
                     /* unbound_ok */ FALSE, rho);
}


SEXP do_env_path do_formals
{
    do_start_no_op("env_path", -1);


    Rboolean verbose  = FALSE,
             original = FALSE,
             for_msg  = FALSE,
             contents = FALSE;
    SEXP envir  = NULL,
         target = NULL;


    switch (length(args)) {
    case 0:
        break;
    case 2:
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        break;
    case 3:
        verbose  = asLogical(CAR(args)); args = CDR(args);
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        break;
    case 6:
        verbose  = asLogical(CAR(args)); args = CDR(args);
        original = asLogical(CAR(args)); args = CDR(args);
        for_msg  = asLogical(CAR(args)); args = CDR(args);
        contents = asLogical(CAR(args)); args = CDR(args);
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), ".C_env_path", "0, 2, 3, or 6"));
        return R_NilValue;
    }


    check_arguments4(verbose, original, for_msg, contents);
    return env_path7(verbose, original, for_msg, contents, target, envir, rho);
}


SEXP _src_path(Rboolean verbose, Rboolean original, Rboolean for_msg,
               Rboolean contents, SEXP srcfile, Rboolean *gave_contents,
               Rboolean unbound_ok, Rboolean get_lineno, Rboolean get_context,
               SEXP rho)
{
    *gave_contents = FALSE;
    Rboolean get_frame_number = FALSE;


    int nprotect = 0;


    SEXP x, srcref, wholeSrcref;
    x = srcfile;
    srcfile = NULL;


    if (get_lineno) {
        if (x == NULL || IS_SCALAR(x, INTSXP)) {
            PROTECT(srcref = sys_srcref(x ? INTEGER(x)[0] : 0, rho));
            SEXP returnthis = ScalarInteger(
                ISNULL(srcref) ? (NA_INTEGER) : (INTEGER(srcref)[0])
            );
            UNPROTECT(1);
            return returnthis;
        }
        switch (TYPEOF(x)) {
        case SYMSXP:
        case CLOSXP:
            srcref = PROTECT(getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == INTSXP) {


#define return_first_line_from_srcref do {                     \
                    SEXP returnthis = ScalarInteger(INTEGER(srcref)[0]);\
                    UNPROTECT(nprotect);                       \
                    return returnthis;                         \
                } while (0)


                return_first_line_from_srcref;
            }
            break;
        case LANGSXP:
            wholeSrcref = PROTECT(getAttrib(x, wholeSrcrefSymbol)); nprotect++;
            if (TYPEOF(wholeSrcref) == INTSXP) {
                srcref = wholeSrcref;
                return_first_line_from_srcref;
            }
            srcref = PROTECT(getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == INTSXP)
                return_first_line_from_srcref;
            if (TYPEOF(srcref) == VECSXP && LENGTH(srcref) &&
                TYPEOF(srcref = VECTOR_ELT(srcref, 0)) == INTSXP)
            {
                return_first_line_from_srcref;
            }
            break;
        case EXPRSXP:
            wholeSrcref = PROTECT(getAttrib(x, wholeSrcrefSymbol)); nprotect++;
            if (TYPEOF(wholeSrcref) == INTSXP) {
                srcref = wholeSrcref;
                return_first_line_from_srcref;
            }
            srcref = PROTECT(getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == VECSXP && LENGTH(srcref) &&
                TYPEOF(srcref = VECTOR_ELT(srcref, 0)) == INTSXP)
            {
                return_first_line_from_srcref;
            }
            break;
        case INTSXP:
            if (LENGTH(x) == 8) {
                srcref = x;
                return_first_line_from_srcref;
            }
            break;
        }
        UNPROTECT(nprotect);
        return ScalarInteger(NA_INTEGER);
    }


    if (x == NULL || IS_SCALAR(x, INTSXP)) {


#define get_srcfile_from_srcref do {                           \
            srcfile = PROTECT(getAttrib(srcref, srcfileSymbol)); nprotect++;\
            if (TYPEOF(srcfile) != ENVSXP) srcfile = NULL;     \
        } while (0)


        srcfile = sys_srcfile(x ? INTEGER(x)[0] : 0, rho);
        if (TYPEOF(srcfile) != ENVSXP) srcfile = NULL;
        else { PROTECT(srcfile); nprotect++; }
    }
    else switch (TYPEOF(x)) {
    case SYMSXP:
    case CLOSXP:
        srcref = PROTECT(getAttrib(x, srcrefSymbol)); nprotect++;
        if (TYPEOF(srcref) == INTSXP)
            get_srcfile_from_srcref;
        break;
    case LANGSXP:
        srcfile = PROTECT(getAttrib(x, srcfileSymbol)); nprotect++;
        if (TYPEOF(srcfile) == ENVSXP);
        else {
            srcfile = NULL;
            srcref = PROTECT(getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == INTSXP)
                get_srcfile_from_srcref;
        }
        break;
    case EXPRSXP:
        srcfile = PROTECT(getAttrib(x, srcfileSymbol)); nprotect++;
        if (TYPEOF(srcfile) == ENVSXP);
        else {
            srcfile = NULL;
            srcref = PROTECT(getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == VECSXP && LENGTH(srcref) &&
                TYPEOF(srcref = VECTOR_ELT(srcref, 0)) == INTSXP)
            {
                get_srcfile_from_srcref;
            }
        }
        break;
    case INTSXP:
        if (LENGTH(x) == 8) {
            srcref = x;
            get_srcfile_from_srcref;
        }
        break;
    case ENVSXP:
        if (inherits(x, "srcfile")) srcfile = x;
        break;
    }


    SEXP returnvalue;


    SEXP ofile, file, errcnd, lines;


#undef source_char
#define source_char "path of srcfile"
    if (srcfile) {
        SEXP documentcontext = findVarInFrame(srcfile, documentcontextSymbol);
        if (documentcontext != R_UnboundValue) {
            check_documentcontext_env;
        }
        else if (inherits(srcfile, "srcfilecopy") &&
                 asLogical(findVarInFrame(srcfile, isFileSymbol)) != TRUE)
        {
            documentcontext = R_EmptyEnv;
            define_srcfile_documentcontext;
        }
        else {
            ofile = findVarInFrame(srcfile, filenameSymbol);
            if (ofile == R_UnboundValue)
                error(_("object '%s' not found"), CHAR(PRINTNAME(filenameSymbol)));
            SEXP srcfile_original = NULL;
            if (inherits(srcfile, "srcfilealias")) {
                SEXP tmp = findVarInFrame(srcfile, originalSymbol);
                // declare this as a new SEXP so as to not overwrite it in the previous context
                SEXP srcfile = tmp;
                if (TYPEOF(srcfile) != ENVSXP)
                    error(_("object '%s' of mode '%s' was not found"),
                        CHAR(PRINTNAME(originalSymbol)), "environment");
                if (!inherits(srcfile, "srcfile"))
                    error("object '%s' is not a srcfile", CHAR(PRINTNAME(originalSymbol)));
                // declare this as a new SEXP so as to not overwrite it in the previous context
                SEXP documentcontext = findVarInFrame(srcfile, documentcontextSymbol);
                if (documentcontext != R_UnboundValue) {
                    check_documentcontext_env;
                }
                else if (inherits(srcfile, "srcfilecopy") &&
                         asLogical(findVarInFrame(srcfile, isFileSymbol)) != TRUE)
                {
                    documentcontext = R_EmptyEnv;
                    define_srcfile_documentcontext;
                }
                else {
                    // declare this as a new SEXP so as to not overwrite it in the previous context
                    SEXP ofile = findVarInFrame(srcfile, filenameSymbol);
                    if (ofile == R_UnboundValue)
                        error(_("object '%s' not found"), CHAR(PRINTNAME(filenameSymbol)));
                    set_documentcontext2(
                        /* call                   */ R_NilValue,
                        /* sym                    */ original_filenameSymbol,
                        /* ofile                  */ ofile,
                        /* assign_here            */ srcfile,
                        /* assign_as_binding      */ TRUE,
                        /* normalize_action       */ NA_FIX_DIR,
                        /* maybe_chdir            */ TRUE,
                        /* getowd                 */ findVarInFrame(srcfile, wdSymbol),
                        /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                        /* character_only         */ TRUE,
                        /* conv2utf8              */ FALSE,
                        /* allow_blank_string     */ TRUE,
                        /* allow_clipboard        */ TRUE,
                        /* allow_stdin            */ TRUE,
                        /* allow_url              */ TRUE,
                        /* allow_file_uri         */ TRUE,
                        /* ignore_all             */ FALSE,
                        /* srcfile_original       */ NULL
                    );
                    document_context_assign_lines(documentcontext, srcfile);
                }
                if (documentcontext != R_EmptyEnv)
                    srcfile_original = srcfile;
            }
            set_documentcontext2(
                /* call                   */ R_NilValue,
                /* sym                    */ filenameSymbol,
                /* ofile                  */ ofile,
                /* assign_here            */ srcfile,
                /* assign_as_binding      */ TRUE,
                /* normalize_action       */ NA_FIX_DIR,
                /* maybe_chdir            */ TRUE,
                /* getowd                 */ findVarInFrame(srcfile, wdSymbol),
                /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                /* character_only         */ TRUE,
                /* conv2utf8              */ FALSE,
                /* allow_blank_string     */ TRUE,
                /* allow_clipboard        */ TRUE,
                /* allow_stdin            */ TRUE,
                /* allow_url              */ TRUE,
                /* allow_file_uri         */ TRUE,
                /* ignore_all             */ FALSE,
                /* srcfile_original       */ srcfile_original
            );
            document_context_assign_lines(documentcontext, srcfile);
        }
        if (get_context) {
            if (documentcontext == R_EmptyEnv)
                return error_no_associated_path(rho);
            UNPROTECT(nprotect);
            return documentcontext;
        }
        returnfile;
    }


    UNPROTECT(nprotect);


    if (unbound_ok)
        return R_UnboundValue;
    else if (for_msg)
        return ScalarString(NA_STRING);
    else return error_no_associated_path(rho);
}


SEXP src_context(SEXP srcfile, SEXP rho)
{
    Rboolean gave_contents;
    return _src_path(/* verbose */ FALSE, /* original */ FALSE,
                     /* for_msg */ FALSE, /* contents */ FALSE,
                     srcfile, &gave_contents,
                     /* unbound_ok */ FALSE, /* get_lineno */ FALSE,
                     /* get_context */ TRUE, rho);
}


SEXP src_path7(Rboolean verbose, Rboolean original, Rboolean for_msg,
               Rboolean contents, SEXP srcfile, Rboolean unbound_ok, SEXP rho)
{
    Rboolean gave_contents, get_lineno = FALSE, get_context = FALSE;
    SEXP value = _src_path(verbose       , original      , for_msg       ,
                           contents      , srcfile       , &gave_contents,
                           unbound_ok    , get_lineno    , get_context   ,
                           rho           );
    if (unbound_ok && value == R_UnboundValue)
        return value;
    if (!contents)
        return value;
    if (gave_contents)
        return value;
    if (!IS_SCALAR(value, STRSXP))
        error("internal error; invalid '%s' value", "_src_path()");
    if (STRING_ELT(value, 0) == NA_STRING)
        return R_NilValue;
    SEXP expr = LCONS(_get_contentsSymbol, CONS(value, R_NilValue));
    PROTECT(expr);
    value = eval(expr, mynamespace);
    UNPROTECT(1);
    return value;
}


SEXP src_path6(Rboolean verbose, Rboolean original, Rboolean for_msg,
               Rboolean contents, SEXP srcfile, SEXP rho)
{
    return src_path7(verbose, original, for_msg, contents, srcfile,
                     /* unbound_ok */ FALSE, rho);
}


SEXP src_path3(Rboolean verbose, SEXP srcfile, SEXP rho)
{
    return src_path6(verbose, /* original */ FALSE, /* for_msg */ FALSE,
                     /* contents */ FALSE, srcfile, rho);
}


SEXP src_path2(SEXP srcfile, SEXP rho)
{
    return src_path3(/* verbose */ FALSE, srcfile, rho);
}


SEXP src_path1(SEXP rho)
{
    return src_path2(/* srcfile */ NULL, rho);
}


SEXP do_src_path do_formals
{
    do_start_no_op("src_path", -1);


    Rboolean verbose  = FALSE,
             original = FALSE,
             for_msg  = FALSE,
             contents = FALSE;
    SEXP srcfile = NULL;


    switch (length(args)) {
    case 0:
        break;
    case 1:
        srcfile  = CAR(args); args = CDR(args);
        break;
    case 2:
        verbose  = asLogical(CAR(args)); args = CDR(args);
        srcfile  = CAR(args); args = CDR(args);
        break;
    case 5:
        verbose  = asLogical(CAR(args)); args = CDR(args);
        original = asLogical(CAR(args)); args = CDR(args);
        for_msg  = asLogical(CAR(args)); args = CDR(args);
        contents = asLogical(CAR(args)); args = CDR(args);
        srcfile  = CAR(args); args = CDR(args);
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), ".C_src_path", "0, 1, 2, or 5"));
        return R_NilValue;
    }


    check_arguments4(verbose, original, for_msg, contents);
    return src_path6(verbose, original, for_msg, contents, srcfile, rho);
}


SEXP do_src_LINENO do_formals
{
    do_start_no_op("src_LINENO", -1);


    SEXP srcfile = NULL;


    switch (length(args)) {
    case 0:
        break;
    case 1:
        srcfile = CAR(args); args = CDR(args);
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), ".C_src_LINENO", "0 or 1"));
        return R_NilValue;
    }


    Rboolean gave_contents;
    return _src_path(/* verbose */ FALSE, /* original */ FALSE,
                     /* for_msg */ FALSE, /* contents */ FALSE,
                     srcfile, &gave_contents,
                     /* unbound_ok */ FALSE, /* get_lineno */ TRUE,
                     /* get_context */ FALSE, rho);
}


SEXP do_this_path do_formals
{
    do_start_no_op("this_path", -1);


    Rboolean verbose  = FALSE,
             original = FALSE,
             for_msg  = FALSE,
             contents = FALSE,
             local    = FALSE;
    SEXP envir   = NULL,
         target  = NULL,
         srcfile = NULL;


    switch (length(args)) {
    case 0:
        break;
    case 4:
        local    = asLogical(CAR(args)); args = CDR(args);
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        srcfile  = CAR(args); args = CDR(args);
        break;
    case 5:
        verbose  = asLogical(CAR(args)); args = CDR(args);
        local    = asLogical(CAR(args)); args = CDR(args);
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        srcfile  = CAR(args); args = CDR(args);
        break;
    case 8:
        verbose  = asLogical(CAR(args)); args = CDR(args);
        original = asLogical(CAR(args)); args = CDR(args);
        for_msg  = asLogical(CAR(args)); args = CDR(args);
        contents = asLogical(CAR(args)); args = CDR(args);
        local    = asLogical(CAR(args)); args = CDR(args);
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        srcfile  = CAR(args); args = CDR(args);
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), ".C_this_path", "0, 4, 5, or 8"));
        return R_NilValue;
    }


    check_arguments5(verbose, original, for_msg, contents, local);


    if (local) return sys_path6(verbose, original, for_msg, contents, local, rho);


    SEXP value = src_path7(verbose, original, for_msg, contents, srcfile,
                           /* unbound_ok */ TRUE, rho);
    if (value == R_UnboundValue) {
        value = env_path8(verbose, original, for_msg, contents, target, envir,
                          /* unbound_ok */ TRUE, rho);
        if (value == R_UnboundValue) {
            value = sys_path6(verbose, original, for_msg, contents, local, rho);
        }
    }
    return value;
}
