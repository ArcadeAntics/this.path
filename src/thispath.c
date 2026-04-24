/*
this.path : Get Executing Script's Path
Copyright (C) 2022-2026   Iris Simmons
 */


#include "thispathdefn.h"


#define one   "'%s' must be FALSE when '%s' is TRUE"
#define two   "'%s' and '%s' must be FALSE when '%s' is TRUE"
#define three "'%s', '%s', and '%s' must be FALSE when '%s' is TRUE"
#define four  "'%s', '%s', '%s', and '%s' must be FALSE when '%s' is TRUE"


static R_INLINE
void check_arguments4(Rboolean verbose , Rboolean original, Rboolean for_msg ,
                      Rboolean contents)
{
    if (verbose == NA_LOGICAL)
        Rf_error(_("invalid '%s' value"), "verbose");
    /* original is allowed to be NA */
    if (for_msg == NA_LOGICAL)
        Rf_error(_("invalid '%s' value"), "for.msg");
    if (contents == NA_LOGICAL)
        Rf_error(_("invalid '%s' value"), "contents");


    if (contents) {
        if (original)
            Rf_error(one, "original", "contents");
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
        Rf_error(_("invalid '%s' value"), "local");
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
    else if (N < 0) Rf_error(_("invalid '%s' argument"), "N");
    if (get_frame_number == NA_LOGICAL)
        Rf_error(_("invalid '%s' value"), "get.frame.number");


    if (get_frame_number) {
        if (original) {
            if (for_msg) {
                if (contents) {
                    if (local)
                        Rf_error(four, "original", "for.msg", "contents", "local", "get.frame.number");
                    else
                        Rf_error(three, "original", "for.msg", "contents", "get.frame.number");
                }
                else {
                    if (local)
                        Rf_error(three, "original", "for.msg", "local", "get.frame.number");
                    else
                        Rf_error(two, "original", "for.msg", "get.frame.number");
                }
            }
            else {
                if (contents) {
                    if (local)
                        Rf_error(three, "original", "contents", "local", "get.frame.number");
                    else
                        Rf_error(two, "original", "contents", "get.frame.number");
                }
                else {
                    if (local)
                        Rf_error(two, "original", "local", "get.frame.number");
                    else
                        Rf_error(one, "original", "get.frame.number");
                }
            }
        }
        else {
            if (for_msg) {
                if (contents) {
                    if (local)
                        Rf_error(three "for.msg", "contents", "local", "get.frame.number");
                    else
                        Rf_error(two, "for.msg", "contents", "get.frame.number");
                }
                else {
                    if (local)
                        Rf_error(two, "for.msg", "local", "get.frame.number");
                    else
                        Rf_error(one, "for.msg", "get.frame.number");
                }
            }
            else {
                if (contents) {
                    if (local)
                        Rf_error(two, "contents", "local", "get.frame.number");
                    else
                        Rf_error(one, "contents", "get.frame.number");
                }
                else {
                    if (local)
                        Rf_error(one, "local", "get.frame.number");
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


SEXP fixNewlines(SEXP x)
{
    Rf_protect(x);
    R_xlen_t n = XLENGTH(x);
    if (n) {
        Rboolean found_newlines = FALSE;
        R_xlen_t num_new_strings = 0;
        for (R_xlen_t i = 0; i < n; i++) {
            SEXP x0 = STRING_ELT(x, i);
            if (x0 == NA_STRING)
                Rf_error("invalid '%s' argument; must not contain NA", "x");
            else if (x0 == R_BlankString);
            else {
                const char *str = R_CHAR(x0);
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
            SEXP y = Rf_allocVector(STRSXP, n + num_new_strings);
            Rf_protect(y);
            for (R_xlen_t ix = 0, iy = 0; ix < n; ix++) {
                SEXP x0 = STRING_ELT(x, ix);
                if (x0 == R_BlankString);
                else {
                    const char *str = R_CHAR(x0);
                    const char *p = strchr(str, '\n');
                    if (p) {
                        cetype_t enc = Rf_getCharCE(x0);
                        do {
                            SET_STRING_ELT(y, iy++, Rf_mkCharLenCE(str, p - str, enc));
                            str = p + 1;
                            if (*str) {
                                p = strchr(str, '\n');
                            }
                            else break;
                        } while (p);
                        if (*str) SET_STRING_ELT(y, iy++, Rf_mkCharCE(str, enc));
                    }
                    else SET_STRING_ELT(y, iy++, x0);
                }
            }
            Rf_unprotect(1);
            x = y;
        }
    }
    Rf_unprotect(1);
    return x;
}


SEXP do_fixNewlines do_formals
{
    do_start_no_op_rho("fixNewlines", 1);
    SEXP x = CAR(args);
    if (TYPEOF(x) != STRSXP)
        Rf_errorcall(call, _("a character vector argument expected"));
    return fixNewlines(x);
}


SEXP do_splitlines do_formals
{
    do_start_no_op_rho("splitlines", 1);
    SEXP x = CAR(args);
    if (!IS_SCALAR(x, STRSXP))
        Rf_errorcall(call, _("argument must be a character string"));
    SEXP x0 = STRING_ELT(x, 0);
    const char *str = R_CHAR(x0);
    if (!*str) return Rf_allocVector(STRSXP, 0);
    const char *p = str;
    R_xlen_t num_lines = 1;
    int processing = 1;
    while (processing) {
        switch (*p) {
        case '\r':
            switch (*++p) {
            case '\n':
                if (*++p) num_lines++;
                else processing = 0;
                break;
            case '\0':
                processing = 0;
                break;
            default:
                num_lines++;
            }
            break;
        case '\n':
            if (*++p) num_lines++;
            else processing = 0;
            break;
        case '\0':
            processing = 0;
            break;
        default:
            p++;
        }
    }
    SEXP y = Rf_allocVector(STRSXP, num_lines);
    cetype_t enc = Rf_getCharCE(x0);
    Rf_protect(y);
    p = str;
    R_xlen_t i = 0;
    processing = 1;
    while (processing) {
        switch (*p) {
        case '\r':
            SET_STRING_ELT(y, i++, Rf_mkCharLenCE(str, p - str, enc));
            switch (*++p) {
            case '\n':
                if (*++p) str = p;
                else processing = 0;
                break;
            case '\0':
                processing = 0;
                break;
            default:
                str = p;
            }
            break;
        case '\n':
            SET_STRING_ELT(y, i++, Rf_mkCharLenCE(str, p - str, enc));
            if (*++p) str = p;
            else processing = 0;
            break;
        case '\0':
            SET_STRING_ELT(y, i++, Rf_mkCharLenCE(str, p - str, enc));
            processing = 0;
            break;
        default:
            p++;
        }
    }
    Rf_unprotect(1);
    return y;
}


SEXP do_remove_trailing_blank_string do_formals
{
    /* if the last line is a blank string, remove it */


    do_start_no_op_rho("remove_trailing_blank_string", 1);


    SEXP x = CAR(args);
    if (TYPEOF(x) != STRSXP)
        Rf_errorcall(call, _("a character vector argument expected"));
    R_xlen_t n = XLENGTH(x);
    if (n) {
        n--;
        if (STRING_ELT(x, n) == R_BlankString) {
            SEXP tmp = Rf_allocVector(STRSXP, n);
            Rf_protect(tmp);
            for (R_xlen_t i = 0; i < n; i++)
                SET_STRING_ELT(tmp, i, STRING_ELT(x, i));
            x = tmp;
            Rf_unprotect(1);
        }
    }
    return x;
}


SEXP do_Rgui_path do_formals
{
    do_start_no_op("Rgui_path", 6);


    Rboolean verbose, original, for_msg, contents;
    SEXP untitled, r_editor;


    verbose  = Rf_asLogical(CAR(args)); args = CDR(args);
    original = Rf_asLogical(CAR(args)); args = CDR(args);
    for_msg  = Rf_asLogical(CAR(args)); args = CDR(args);
    contents = Rf_asLogical(CAR(args)); args = CDR(args);
    check_arguments4(verbose, original, for_msg, contents);


    /* strings representing non-existent files in RGui */
    untitled = CAR(args); args = CDR(args);
    if (!(TYPEOF(untitled) == STRSXP || untitled == R_NilValue))
        Rf_errorcall(call, "%s, must be %s", "invalid second argument", "'character' / / NULL");


    /* strings representing R scripts in RGui */
    r_editor = CAR(args); args = CDR(args);
    if (!(TYPEOF(r_editor) == STRSXP || r_editor == R_NilValue))
        Rf_errorcall(call, "%s, must be %s", "invalid third argument", "'character' / / NULL");


    extern SEXP Rgui_path(Rboolean verbose, Rboolean original, Rboolean for_msg,
                          Rboolean contents, SEXP untitled, SEXP r_editor, SEXP rho);


    return Rgui_path(verbose, original, for_msg, contents, untitled, r_editor, rho);
}


#include "get_file_from_closure.h"


SEXP do_jupyter_path do_formals
{
    do_start_no_call_op_rho("jupyter_path", 4);


    Rboolean verbose, original, for_msg, contents;
    verbose  = Rf_asLogical(CAR(args)); args = CDR(args);
    original = Rf_asLogical(CAR(args)); args = CDR(args);
    for_msg  = Rf_asLogical(CAR(args)); args = CDR(args);
    contents = Rf_asLogical(CAR(args)); args = CDR(args);


    check_arguments4(verbose, original, for_msg, contents);


    if (verbose) Rprintf("Source: document in Jupyter\n");


    if (contents) {
        for_msg = FALSE;
        SEXP file = get_file_from_closure(original, for_msg, _jupyter_pathSymbol);
        SEXP expr = Rf_lcons(_get_jupyter_notebook_contentsSymbol, Rf_cons(file, R_NilValue));
        Rf_protect(expr);
        SEXP value = Rf_eval(expr, mynamespace);
        Rf_unprotect(1);
        return value;
    }
    return get_file_from_closure(original, for_msg, _jupyter_pathSymbol);
}


static R_INLINE
Rboolean validJupyterRNotebook(SEXP path)
{
    SEXP expr = Rf_lcons(_get_jupyter_R_notebook_contentsSymbol, Rf_cons(path, R_NilValue));
    Rf_protect(expr);
    SEXP value = Rf_eval(expr, mynamespace);
    Rf_unprotect(1);
    return (value != R_NilValue);
}


SEXP do_set_jupyter_path do_formals
{
    do_start_no_op_rho("set_jupyter_path", -1);


    SEXP path;
    Rboolean skipCheck = FALSE;
    switch (Rf_length(args)) {
    case 1:
        path = CAR(args);
        break;
    case 2:
        path = CAR(args);
        skipCheck = Rf_asLogical(CADR(args));
        if (skipCheck == NA_LOGICAL)
            Rf_errorcall(call, _("invalid '%s' argument"), "skipCheck");
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), ".C_set_jupyter_path", "1 or 2"));
        return R_NilValue;
    }


    if (!IS_SCALAR(path, STRSXP))
        Rf_errorcall(call, _("'%s' must be a character string"), "path");
    if (STRING_ELT(path, 0) == NA_STRING);
    else if (is_abs_path(R_CHAR(STRING_ELT(path, 0))));
    else Rf_errorcall(call, _("invalid '%s' argument"), "path");


    if (skipCheck || STRING_ELT(path, 0) == NA_STRING || validJupyterRNotebook(path));
    else Rf_errorcall(call, "invalid '%s' argument; must be a valid Jupyter R notebook", "path");


    SEXP env = getFromMyNS(_jupyter_pathSymbol);
    if (TYPEOF(env) != CLOSXP)
        Rf_errorcall(call, "'%s' is not a closure", R_CHAR(PRINTNAME(_jupyter_pathSymbol)));
    env = CLOENV(env);


    binding_info_t x; my_findVarInFrame(env, ofileSymbol, &x);
    Rf_protect(x.value);
    if (x.value == my_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(x.sym)));
    if (!is_promise(x))
        Rf_errorcall(call, "invalid '%s' binding", EncodeChar(PRINTNAME(x.sym)));


    binding_info_t y; my_findVarInFrame(env, fileSymbol, &y);
    Rf_protect(y.value);
    if (y.value == my_UnboundValue)
        Rf_errorcall(call, _("object '%s' not found"), EncodeChar(PRINTNAME(y.sym)));
    if (!is_promise(y))
        Rf_errorcall(call, "invalid '%s' binding", EncodeChar(PRINTNAME(y.sym)));


    {
        Rboolean locked = R_BindingIsLocked(x.sym, x.env);
        if (locked) R_unLockBinding(x.sym, x.env);
        R_MakeForcedBinding(x.sym, path, path, x.env);
        if (locked) R_LockBinding(x.sym, x.env);
    }


    {
        Rboolean locked = R_BindingIsLocked(y.sym, y.env);
        if (locked) R_unLockBinding(y.sym, y.env);
        R_MakeDelayedBinding(y.sym, my_PREXPR(y), y.env, y.env);
        if (locked) R_LockBinding(y.sym, y.env);
    }


    set_R_Visible(FALSE);
    Rf_unprotect(2);
    return path;
}


typedef enum {
    GUIPATH_DEFAULT  ,
    GUIPATH_FUNCTION ,
    GUIPATH_CHARACTER
} GUIPATH_ACTION;


GUIPATH_ACTION gui_path = GUIPATH_DEFAULT;


void reset_gui_path(void)
{
    gui_path = GUIPATH_DEFAULT;


    Rf_defineVar(_custom_gui_path_functionSymbol, R_NilValue, _custom_gui_path_function_environment);
    Rf_defineVar(_toplevel_nframeSymbol         , R_NilValue, _custom_gui_path_function_environment);


    Rf_defineVar(guinameSymbol         , R_NilValue, _custom_gui_path_character_environment);
    Rf_defineVar(ofileSymbol           , R_NilValue, _custom_gui_path_character_environment);
    Rf_defineVar(fileSymbol            , R_NilValue, _custom_gui_path_character_environment);
    Rf_defineVar(_get_contentsSymbol   , R_NilValue, _custom_gui_path_character_environment);
    Rf_defineVar(_toplevel_nframeSymbol, R_NilValue, _custom_gui_path_character_environment);
}


SEXP do_set_gui_path do_formals
{
    do_start_no_op("set_gui_path", 0);


    int nprotect = 0;


    SEXP dots = my_findValInFrame(rho, R_DotsSymbol);
    Rf_protect(dots); nprotect++;
    if (dots == my_UnboundValue)
        Rf_error(_("object '%s' not found"), "...");


    SEXP value;
    switch (gui_path) {
    case GUIPATH_DEFAULT:
        value = Rf_allocVector(VECSXP, 0);
        Rf_protect(value); nprotect++;
        break;
    case GUIPATH_FUNCTION:
        value = Rf_allocVector(VECSXP, 2);
        Rf_protect(value); nprotect++;
        SET_VECTOR_ELT(value, 0, my_getVarInFrame(
            _custom_gui_path_function_environment,
            _custom_gui_path_functionSymbol,
            TRUE
        ));
        SET_VECTOR_ELT(value, 1, my_getVarInFrame(
            _custom_gui_path_function_environment,
            _toplevel_nframeSymbol,
            TRUE
        ));
        break;
    case GUIPATH_CHARACTER:
    {
        value = Rf_allocVector(VECSXP, 4);
        Rf_protect(value); nprotect++;
        SET_VECTOR_ELT(value, 0, Rf_ScalarString(my_getVarInFrame(
            _custom_gui_path_character_environment,
            guinameSymbol,
            TRUE
        )));
        SET_VECTOR_ELT(value, 1, my_getVarInFrame(
            _custom_gui_path_character_environment,
            ofileSymbol,
            TRUE
        ));
        SET_VECTOR_ELT(value, 2, my_getVarInFrame(
            _custom_gui_path_character_environment,
            _get_contentsSymbol,
            TRUE
        ));
        SET_VECTOR_ELT(value, 3, my_getVarInFrame(
            _custom_gui_path_character_environment,
            _toplevel_nframeSymbol,
            TRUE
        ));
    }
        break;
    default:
        Rf_errorcall(R_NilValue, "internal error; invalid 'gui_path' value");
        value = R_NilValue;
        Rf_protect(value); nprotect++;
    }


    int n = length_DOTS(dots);
    if (n == 0) {
        gui_path = GUIPATH_DEFAULT;
        set_R_Visible(FALSE);
        Rf_unprotect(nprotect);
        return value;
    }


    SEXP dd1 = CAR(dots);
    if (dd1 == R_MissingArg)
        // Rf_errorcall(call, _("argument \"%s\" is missing, with no default"), "..1");
        MissingArgError_c("..1", call, rho, "evalError");
    dd1 = Rf_eval(dd1, R_EmptyEnv);


    SEXPTYPE t = DOTSXP;
    if (n == 1 && (Rf_isPairList(dd1) || Rf_isVectorList(dd1))
        && TAG(dots) == R_NilValue) {
        dots = dd1;
        n = ((dots == R_NilValue) ? 0 : Rf_length(dots));
        t = ((Rf_isPairList(dd1)) ? LISTSXP : VECSXP);
    }


    if (n <= 0) {
        gui_path = GUIPATH_DEFAULT;
        set_R_Visible(FALSE);
        Rf_unprotect(nprotect);
        return value;
    }


    SEXP arg1 = R_NilValue;  /* for -Wall */
    switch (t) {
    case DOTSXP: arg1 = dd1; break;
    case LISTSXP: arg1 = CAR(dots); break;
    case VECSXP: arg1 = VECTOR_ELT(dots, 0); break;
    }


    switch (TYPEOF(arg1)) {
    case CLOSXP:
    {
        if (n > 2)
            Rf_error("%d arguments passed to %s which requires %s",
                n, "set.gui.path()", "at most 2");


        SEXP env = _custom_gui_path_function_environment;


        SEXP fun = arg1;
        ENSURE_NAMEDMAX(fun);


        SEXP args = FORMALS(fun);
        if (TYPEOF(args) == LISTSXP   && (TAG(args) == R_DotsSymbol || (TAG(args) == verboseSymbol  &&
            !ISNULL(args = CDR(args)) && (TAG(args) == R_DotsSymbol || (TAG(args) == originalSymbol &&
            !ISNULL(args = CDR(args)) && (TAG(args) == R_DotsSymbol || (TAG(args) == for_msgSymbol  &&
            !ISNULL(args = CDR(args)) && (TAG(args) == R_DotsSymbol || TAG(args) == contentsSymbol))))))));
        else Rf_error("invalid '%s' argument; must accept the following arguments:\n  (verbose, original, for.msg, contents)", "fun");


        SEXP nframe = R_NilValue;
        if (n > 1)
            switch (t) {
            case DOTSXP:
                nframe = CADDR(dots);
                /* if the argument is missing */
                if (nframe == R_MissingArg)
                    nframe = R_NilValue;
                else {
                    nframe = Rf_eval(nframe, R_EmptyEnv);
                    Rf_protect(nframe); nprotect++;
                }
                break;
            case LISTSXP: nframe = CADDR(dots); break;
            case VECSXP: nframe = VECTOR_ELT(dots, 1); break;
            }
        if (nframe == R_NilValue);
        else if (IS_SCALAR(nframe, INTSXP)) {
            int x = INTEGER_ELT(nframe, 0);
            if (x == NA_INTEGER || x < 0)
                Rf_error("invalid '%s' argument; expected a non-negative integer", EncodeChar(PRINTNAME(_toplevel_nframeSymbol)));
        }
        else if (IS_SCALAR(nframe, REALSXP)) {
            double x = REAL_ELT(nframe, 0);
            if (R_IsNA(x) || x < 0 || x > INT_MAX)
                Rf_error("invalid '%s' argument; expected a non-negative integer", EncodeChar(PRINTNAME(_toplevel_nframeSymbol)));
            nframe = Rf_ScalarInteger(x);
            Rf_protect(nframe); nprotect++;
        }
        else if (TYPEOF(nframe) == CLOSXP);
        else Rf_error("invalid '%s' argument; expected an integer", EncodeChar(PRINTNAME(_toplevel_nframeSymbol)));


        reset_gui_path();


        Rf_defineVar(_custom_gui_path_functionSymbol, fun   , env);
        Rf_defineVar(_toplevel_nframeSymbol         , nframe, env);
    }
        gui_path = GUIPATH_FUNCTION;
        break;
    case STRSXP:
    {
        if (n < 2)
            Rf_error("%d arguments passed to %s which requires %s",
                n, "set.gui.path()", "at least 2");
        if (n > 4)
            Rf_error("%d arguments passed to %s which requires %s",
                n, "set.gui.path()", "at most 4");


        SEXP env = _custom_gui_path_character_environment;


        SEXP guiname = arg1;
        if (!IS_SCALAR(guiname, STRSXP) || STRING_ELT(guiname, 0) == NA_STRING)
            Rf_error(_("invalid first argument"));


        SEXP path = R_NilValue;  /* for -Wall */
        switch (t) {
        case DOTSXP:
            path = CADR(dots);
            if (path == R_MissingArg)
                // Rf_errorcall(call, _("argument \"%s\" is missing, with no default"), "..2");
                MissingArgError_c("..2", call, rho, "evalError");
            path = Rf_eval(path, R_EmptyEnv);
            Rf_protect(path); nprotect++;
            break;
        case LISTSXP: path = CADR(dots); break;
        case VECSXP: path = VECTOR_ELT(dots, 1); break;
        }
        if (!IS_SCALAR(path, STRSXP))
            Rf_error("invalid '%s' argument; expected a character string", "path");
        if (!is_abs_path(R_CHAR(STRING_ELT(path, 0))))
            Rf_error("invalid '%s' argument; expected an absolute path", "path");


        SEXP _getContents = R_NilValue;
        if (n > 2)
            switch (t) {
            case DOTSXP:
                _getContents = CADDR(dots);
                if (_getContents == R_MissingArg)
                    _getContents = R_NilValue;
                else {
                    _getContents = Rf_eval(_getContents, R_EmptyEnv);
                    Rf_protect(_getContents); nprotect++;
                }
                break;
            case LISTSXP: _getContents = CADDR(dots); break;
            case VECSXP: _getContents = VECTOR_ELT(dots, 2); break;
            }
        if (_getContents == R_NilValue);
        else if (TYPEOF(_getContents) == CLOSXP) {
            if (FORMALS(_getContents) == R_NilValue)
                Rf_error("invalid '%s' argument; expected a function with at least one formal argument", "getContents");
        }
        else Rf_error("invalid '%s' argument; expected a function", "getContents");


        SEXP nframe = R_NilValue;
        if (n > 3)
            switch (t) {
            case DOTSXP:
                nframe = CADDDR(dots);
                if (nframe == R_MissingArg)
                    nframe = R_NilValue;
                else {
                    nframe = Rf_eval(nframe, R_EmptyEnv);
                    Rf_protect(nframe); nprotect++;
                }
                break;
            case LISTSXP: nframe = CADDDR(dots); break;
            case VECSXP: nframe = VECTOR_ELT(dots, 3); break;
            }
        if (nframe == R_NilValue);
        else if (IS_SCALAR(nframe, INTSXP)) {
            int x = INTEGER_ELT(nframe, 0);
            if (x == NA_INTEGER || x < 0)
                Rf_error("invalid '%s' argument; expected a non-negative integer", EncodeChar(PRINTNAME(_toplevel_nframeSymbol)));
        }
        else if (IS_SCALAR(nframe, REALSXP)) {
            double x = REAL_ELT(nframe, 0);
            if (R_IsNA(x) || x < 0 || x > INT_MAX)
                Rf_error("invalid '%s' argument; expected a non-negative integer", EncodeChar(PRINTNAME(_toplevel_nframeSymbol)));
            nframe = Rf_ScalarInteger(x);
            Rf_protect(nframe); nprotect++;
        }
        else if (TYPEOF(nframe) == CLOSXP);
        else Rf_error("invalid '%s' argument; expected an integer", EncodeChar(PRINTNAME(_toplevel_nframeSymbol)));


        reset_gui_path();


        Rf_defineVar(guinameSymbol, STRING_ELT(guiname, 0), env);


        {
            SEXP sym = ofileSymbol;
            Rboolean locked = R_BindingIsLocked(sym, env);
            if (locked) R_unLockBinding(sym, env);
            R_MakeForcedBinding(sym, path, path, env);
            if (locked) R_LockBinding(sym, env);
        }


        {
            SEXP sym = fileSymbol;
            Rboolean locked = R_BindingIsLocked(sym, env);
            if (locked) R_unLockBinding(sym, env);
            SEXP expr = Rf_lcons(_normalizePath_not_dirSymbol, Rf_cons(ofileSymbol, R_NilValue));
            R_MakeDelayedBinding(sym, expr, env, env);
            if (locked) R_LockBinding(sym, env);
        }


        Rf_defineVar(_get_contentsSymbol   , _getContents, env);
        Rf_defineVar(_toplevel_nframeSymbol, nframe      , env);
    }
        gui_path = GUIPATH_CHARACTER;
        break;
    default:
        Rf_error("invalid first argument; expected a function or a character string");
        return R_NilValue;
    }


    set_R_Visible(FALSE);
    Rf_unprotect(nprotect);
    return value;
}


static R_INLINE
SEXP env_or_NULL(SEXP x)
{
    return (x != my_UnboundValue && TYPEOF(x) == ENVSXP) ? x : NULL;
}


void document_context_assign_lines(SEXP documentcontext, SEXP srcfile)
{
    if (documentcontext != R_EmptyEnv) {
        if (R_existsVarInFrame(documentcontext, linesSymbol))
            R_removeVarFromFrame(linesSymbol, documentcontext);
        if (Rf_inherits(srcfile, "srcfilecopy") ||
            (
                Rf_inherits(srcfile, "srcfilealias") &&
                (srcfile = env_or_NULL(my_findValInFrame(srcfile, originalSymbol))) &&
                Rf_inherits(srcfile, "srcfilecopy")
            )
           )
        {
            binding_info_t tmp; my_findVarInFrame(srcfile, fixedNewlinesSymbol, &tmp);
            if (tmp.value == my_UnboundValue || tmp.value == R_NilValue) {
                SEXP expr = Rf_lcons(_fixNewlinesSymbol, Rf_cons(srcfile, R_NilValue));
                Rf_protect(expr);
                R_MakeDelayedBinding(linesSymbol, expr, documentcontext, documentcontext);
                Rf_unprotect(1);
            }
            else {
                SEXP lines = my_getVarInFrame(srcfile, linesSymbol, FALSE);
                if (TYPEOF(lines) != STRSXP)
                    Rf_error(_("object '%s' of mode '%s' was not found"),
                        R_CHAR(PRINTNAME(linesSymbol)), "character");
                INCREMENT_NAMED_defineVar(linesSymbol, lines, documentcontext);
            }
        }
    }
}


static R_INLINE
SEXP error_no_associated_path(SEXP rho)
{
    stop(ThisPathNotExistsError(R_CurrentExpression, rho, "no associated path"));
    return R_NilValue;
}


static R_INLINE
SEXP make_path_call(SEXP sym, Rboolean verbose , Rboolean original,
                              Rboolean for_msg , Rboolean contents)
{
    SEXP expr = R_NilValue;
    if (contents) {
        expr = Rf_cons(
            Rf_ScalarLogical(verbose),
            Rf_cons(
                Rf_ScalarLogical(original),
                Rf_cons(
                    Rf_ScalarLogical(for_msg),
                    Rf_cons(Rf_ScalarLogical(contents), expr)
                )
            )
        );
    } else if (for_msg) {
        expr = Rf_cons(
            Rf_ScalarLogical(verbose),
            Rf_cons(
                Rf_ScalarLogical(original),
                Rf_cons(Rf_ScalarLogical(for_msg), expr)
            )
        );
    } else if (original) {
        expr = Rf_cons(
            Rf_ScalarLogical(verbose),
            Rf_cons(Rf_ScalarLogical(original), expr)
        );
    } else if (verbose) {
        expr = Rf_cons(Rf_ScalarLogical(verbose), expr);
    }
    return Rf_lcons(sym, expr);
}


SEXP _sys_path(Rboolean verbose         , Rboolean original        ,
               Rboolean for_msg         , Rboolean contents        ,
               Rboolean local           , Rboolean *gave_contents  ,
               int N                    , Rboolean get_frame_number,
               SEXP rho                 )
{
    *gave_contents = FALSE;


    static const char *name = "'this.path(local = TRUE)'";


    /*
     * set_documentcontext() creates a variable 'returnvalue'
     * that is used in setpath() (see ./src/setsyspath.c)
     * not used elsewhere but must be declared
     */
    SEXP returnvalue; (void) returnvalue;
    SEXP documentcontext;


    if (N == NA_INTEGER) {
        if (local)
            N = sys_parent(1, rho);
        else {
            N = sys_nframe(rho);
            if (N) --N;
        }
        if (N == NA_INTEGER)
            Rf_error(_("invalid '%s' argument"), "N");
    }


    if (local) {


        if (N <= 0) Rf_error("%s cannot be used within the global environment", name);
        SEXP frame = parent_frame(rho);


        /* ensure 'this.path(local = TRUE)' isn't evaluated in an invalid environment */
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


        if (!R_existsVarInFrame(frame, documentcontextSymbol))
            Rf_error("%s cannot be called within this environment", name);


        /* sys.function(N) */
        INTEGER(CADR(expr_sys_function_which))[0] = N;
        SEXP function = Rf_eval(expr_sys_function_which, rho);
        if (function == eval_op)
            Rf_error("%s cannot be used within '%s'",
                  name, R_CHAR(PRINTNAME(R_EvalSymbol)));
        else if (TYPEOF(function) != CLOSXP)
            Rf_error("%s cannot be used within a '%s', possible errors with eval?",
                  name, Rf_type2char(TYPEOF(function)));
    }


    if (N <= 0) {


#define toplevel                                               \
        if (in_site_file) {                                    \
            if (get_frame_number) return Rf_ScalarInteger(-1); \
            SEXP expr = make_path_call(_site_file_pathSymbol,  \
                verbose, original, for_msg, contents);         \
            Rf_protect(expr);                                  \
            SEXP value = Rf_eval(expr, mynamespace);           \
            Rf_unprotect(1);                                   \
            return value;                                      \
        }                                                      \
        else if (in_init_file) {                               \
            if (get_frame_number) return Rf_ScalarInteger(-1); \
            SEXP expr = make_path_call(_init_file_pathSymbol,  \
                verbose, original, for_msg, contents);         \
            Rf_protect(expr);                                  \
            SEXP value = Rf_eval(expr, mynamespace);           \
            Rf_unprotect(1);                                   \
            return value;                                      \
        }                                                      \
        else {                                                 \
            if (get_frame_number) return Rf_ScalarInteger(0);  \
            else return my_UnboundValue;                       \
        }


        toplevel;
    }


    SEXP srcfile, errcnd, ofile, file, lines;


    int nprotect = 0;


    SEXP source      = getFromBase(sourceSymbol     ),
         sys_source  = getFromBase(sys_sourceSymbol ),
         wrap_source = getFromMyNS(wrap_sourceSymbol),
         debugSource = get_debugSource()             ;
    Rboolean rstudio_loaded = (debugSource != my_UnboundValue);


    SEXP _rs_sourceWithProgress = my_UnboundValue;
    Rboolean rstudio_background_job_tools_loaded = FALSE;
    if (getenv("RSTUDIOAPI_IPC_REQUESTS_FILE")) {
        SEXP name;
        for (SEXP t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
            name = Rf_getAttrib(t, R_NameSymbol);
            if (Rf_isString(name) &&
                Rf_length(name) > 0 &&
                !strcmp(Rf_translateChar(STRING_ELT(name, 0)), "tools:rstudio"))
            {
                rstudio_background_job_tools_loaded = TRUE;
                _rs_sourceWithProgress = my_getVarInFrame(t, _rs_sourceWithProgressSymbol, TRUE);
            }
        }
    }


    SEXP ns;
    Rboolean compiler_loaded; SEXP loadcmp;
    Rboolean utils_loaded   ; SEXP Sweave;
    Rboolean box_loaded     ; SEXP load_from_source;
    Rboolean knitr_loaded   ; SEXP knit;
    Rboolean plumber_loaded ; SEXP plumber_sourceUTF8, Plumber_public_methods_initialize;
    Rboolean shiny_loaded   ; SEXP shiny_sourceUTF8;
    Rboolean targets_loaded ; SEXP tar_callr_inner_try, tar_load_globals, tar_source, tar_workspace;
    Rboolean testthat_loaded; SEXP source_file;


    ns = my_getRegisteredNamespace("compiler", compilerSymbol);
    compiler_loaded = (ns != R_NilValue);
    loadcmp = (compiler_loaded ? my_getVarInFrame(ns, loadcmpSymbol, FALSE) : my_UnboundValue);


    ns = my_getRegisteredNamespace("utils", utilsSymbol);
    utils_loaded = (ns != R_NilValue);
    Sweave = (utils_loaded ? my_getVarInFrame(ns, SweaveSymbol, FALSE) : my_UnboundValue);


    ns = my_getRegisteredNamespace("box", boxSymbol);
    box_loaded = (ns != R_NilValue);
    load_from_source = (box_loaded ? my_getVarInFrame(ns, load_from_sourceSymbol, FALSE) : my_UnboundValue);


    ns = my_getRegisteredNamespace("knitr", knitrSymbol);
    knitr_loaded = (ns != R_NilValue);
    knit = (knitr_loaded ? my_getVarInFrame(ns, knitSymbol, FALSE) : my_UnboundValue);


    ns = my_getRegisteredNamespace("plumber", plumberSymbol);
    plumber_loaded = (ns != R_NilValue);
    plumber_sourceUTF8 = (plumber_loaded ? my_getVarInFrame(ns, sourceUTF8Symbol, FALSE) : my_UnboundValue);
    Plumber_public_methods_initialize = my_UnboundValue;
    if (plumber_loaded) {
        SEXP tmp = my_getVarInFrame(ns, PlumberSymbol, FALSE);
        if (TYPEOF(tmp) == ENVSXP) {
            tmp = my_getVarInFrame(tmp, public_methodsSymbol, FALSE);
            if (TYPEOF(tmp) == VECSXP) {
                tmp = getInList(initializeSymbol, tmp, TRUE);
                if (tmp && TYPEOF(tmp) == CLOSXP)
                    Plumber_public_methods_initialize = tmp;
            }
        }
    }


    ns = my_getRegisteredNamespace("shiny", shinySymbol);
    shiny_loaded = (ns != R_NilValue);
    shiny_sourceUTF8 = (shiny_loaded ? my_getVarInFrame(ns, sourceUTF8Symbol, FALSE) : my_UnboundValue);


    ns = my_getRegisteredNamespace("targets", targetsSymbol);
    targets_loaded = (ns != R_NilValue);
    if (targets_loaded) {
        tar_callr_inner_try = my_getVarInFrame(ns, tar_callr_inner_trySymbol, FALSE);
        tar_load_globals    = my_getVarInFrame(ns, tar_load_globalsSymbol   , FALSE);
        tar_source          = my_getVarInFrame(ns, tar_sourceSymbol         , FALSE);
        tar_workspace       = my_getVarInFrame(ns, tar_workspaceSymbol      , FALSE);
    } else {
        tar_callr_inner_try = my_UnboundValue;
        tar_load_globals    = my_UnboundValue;
        tar_source          = my_UnboundValue;
        tar_workspace       = my_UnboundValue;
    }


    ns = my_getRegisteredNamespace("testthat", testthatSymbol);
    testthat_loaded = (ns != R_NilValue);
    source_file = (testthat_loaded ? my_getVarInFrame(ns, source_fileSymbol, FALSE) : my_UnboundValue);


    SEXP which = Rf_allocVector(INTSXP, 1);
    Rf_protect(which); nprotect++;
    int *iwhich = INTEGER(which);
    SEXP getframe;
    {
        PROTECT_INDEX indx;
        R_ProtectWithIndex(getframe = Rf_cons(which, R_NilValue), &indx); nprotect++;
        R_Reprotect(getframe = Rf_lcons(getFromBase(sys_frameSymbol), getframe), indx);
    }
    SEXP getfunction;
    {
        PROTECT_INDEX indx;
        R_ProtectWithIndex(getfunction = Rf_cons(which, R_NilValue), &indx); nprotect++;
        R_Reprotect(getfunction = Rf_lcons(getFromBase(sys_functionSymbol), getfunction), indx);
    }


    SEXP frame, function;


    int minimum_which = ((local) ? N : (1 + _toplevel_nframe()));


/* the number of objects protected in each iteration (that must be unprotected
   before moving to the next iteration) */
#define nprotect_loop 1


    for (iwhich[0] = N; iwhich[0] >= minimum_which; iwhich[0]--, Rf_unprotect(nprotect_loop)) {
        frame = Rf_eval(getframe, rho);
        // Rf_protect(frame);
        function = Rf_eval(getfunction, rho);
        Rf_protect(function);


#define find_documentcontext(x) ( my_findValInFrame((x), documentcontextSymbol) )


        /* it might be tempting to put:
         * documentcontext = find_documentcontext(frame);
         *
         * right here to avoid copy and pasting the same code into each `if`
         * block, but targets::tar_source specifically does not need this, so
         * do it later
         */


        if (identical(function, source)) {
#define source_char "call to function 'source'"
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            if (R_existsVarInFrame(frame, NeSymbol)) {
                SEXP exprs = my_getVarInFrame(frame, exprsSymbol, TRUE);
                if (exprs != my_UnboundValue && TYPEOF(exprs) == EXPRSXP)
                    srcfile = env_or_NULL(Rf_getAttrib(exprs, srcfileSymbol));
            }
            if (documentcontext != my_UnboundValue) {


#define stopifnot_documentcontext_env                          \
                if (TYPEOF(documentcontext) != ENVSXP)         \
                    Rf_error(_("object '%s' of mode '%s' was not found"),\
                        R_CHAR(PRINTNAME(documentcontextSymbol)), "environment")


/* not used here but used later */
#define stopif_documentcontext_emptyenv                        \
                if (documentcontext == R_EmptyEnv)             \
                    Rf_error("invalid '%s' value; expected non-empty document context",\
                        R_CHAR(PRINTNAME(documentcontextSymbol)))


#define define_documentcontext_in_srcfile_no_lock              \
                document_context_assign_lines(documentcontext, srcfile);\
                INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, srcfile)



#define define_documentcontext_in_srcfile                      \
                define_documentcontext_in_srcfile_no_lock;     \
                R_LockBinding(documentcontextSymbol, srcfile)


#define if_srcfile_then_define_documentcontext                 \
                if (srcfile) {                                 \
                    define_documentcontext_in_srcfile;         \
                }


#define ifndef_srcfile_documentcontext_then_define             \
                if (srcfile && !R_existsVarInFrame(srcfile, documentcontextSymbol)) {\
                    define_documentcontext_in_srcfile;         \
                }


#define overwrite_documentcontext_in_srcfile                   \
                if (R_BindingIsLocked(documentcontextSymbol, srcfile)) {\
                    R_unLockBinding(documentcontextSymbol, srcfile);\
                    define_documentcontext_in_srcfile;         \
                }                                              \
                else {                                         \
                    define_documentcontext_in_srcfile_no_lock; \
                }


#define maybe_overwrite_documentcontext_in_srcfile             \
                if (srcfile) {                                 \
                    SEXP tmp = my_findValInFrame(srcfile, documentcontextSymbol);\
                    if (tmp == my_UnboundValue) {              \
                        define_documentcontext_in_srcfile;     \
                    }                                          \
                    else if (tmp == documentcontext);          \
                    else if (tmp == R_EmptyEnv) {              \
                        overwrite_documentcontext_in_srcfile;  \
                    }                                          \
                    else if (TYPEOF(tmp) != ENVSXP) {          \
                        Rf_errorcall(sys_call(which, rho),     \
                            _("object '%s' of mode '%s' was not found"),\
                            R_CHAR(PRINTNAME(documentcontextSymbol)), "environment");\
                    }                                          \
                }


                stopifnot_documentcontext_env;
                maybe_overwrite_documentcontext_in_srcfile;
            }
            else if (srcfile &&
                     !ISUNBOUND(documentcontext = find_documentcontext(srcfile)) &&
                     documentcontext != R_EmptyEnv)
            {


#define define_documentcontext_in_frame                        \
                INCREMENT_NAMED_defineVar(documentcontextSymbol, documentcontext, frame);\
                R_LockBinding(documentcontextSymbol, frame)


                stopifnot_documentcontext_env;
                define_documentcontext_in_frame;
            }
            else {
                ofile = my_findValInFrame(frame, ofileSymbol);
                if (ofile == my_UnboundValue) continue;


                SEXP wd = my_getVarInFrame(frame, owdSymbol, TRUE);
                if (srcfile && wd == my_UnboundValue)
                    wd = my_getVarInFrame(srcfile, wdSymbol, TRUE);


#define set_documentcontext2(call, sym, ofile, assign_here, assign_as_binding,\
    normalize_action, maybe_chdir, getowd, hasowd,             \
    character_only, conv2utf8, allow_blank_string,             \
    allow_clipboard, allow_stdin, allow_url, allow_file_uri,   \
    ignore_all, srcfile_original)                              \
                set_documentcontext(                           \
                    call, sym, ofile, assign_here, assign_as_binding, normalize_action,\
                    /* forcepromise           */ FALSE,        \
                    /* assign_returnvalue     */ FALSE,        \
                    /* assign_to              */ NULL,         \
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
                    /* source                 */ Rf_mkChar(source_char),\
                    srcfile_original                           \
                )


                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != my_UnboundValue && (owd) != R_NilValue),
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


                maybe_overwrite_documentcontext_in_srcfile;
            }


#define _returnfile(which, source, unprotect) do {             \
            if (documentcontext == R_EmptyEnv) break;          \
            SEXP returnthis = NULL;                            \
            errcnd = my_findValInFrame(documentcontext, errcndSymbol);\
            if (errcnd != my_UnboundValue) {                   \
                if (for_msg) {                                 \
                    if (contents) {                            \
                        returnthis = Rf_ScalarString(NA_STRING);\
                    } else {                                   \
                        returnthis = my_getVarInFrame(documentcontext, for_msgSymbol, TRUE);\
                        if (returnthis == my_UnboundValue)     \
                            Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(for_msgSymbol)));\
                    }                                          \
                }                                              \
                else if (get_frame_number) {                   \
                    if (R_existsVarInFrame(documentcontext, associated_with_fileSymbol))\
                        returnthis = (which);                  \
                    else                                       \
                        returnthis = Rf_ScalarInteger(NA_INTEGER);\
                }                                              \
                else {                                         \
                    if (TYPEOF(errcnd) != VECSXP ||            \
                        LENGTH(errcnd) < 2)                    \
                    {                                          \
                        Rf_error(_("invalid '%s' value"), R_CHAR(PRINTNAME(errcndSymbol)));\
                    }                                          \
                    errcnd = Rf_duplicate(errcnd);             \
                    Rf_protect(errcnd);                        \
                    SET_VECTOR_ELT(errcnd, 1, getCurrentCall(rho));\
                    stop(errcnd);                              \
                    Rf_unprotect(1);  /* errcnd */             \
                    /* should not reach here */                \
                    returnthis = R_NilValue;                   \
                }                                              \
            }                                                  \
            else if (contents && !ISUNBOUND(lines = my_getVarInFrame(documentcontext, linesSymbol, TRUE))) {\
                if (TYPEOF(lines) != STRSXP)                   \
                    Rf_error(_("invalid '%s' value"), R_CHAR(PRINTNAME(linesSymbol)));\
                returnthis = lines;                            \
                *gave_contents = TRUE;                         \
            }                                                  \
            else if (get_frame_number) {                       \
                returnthis = (which);                          \
            }                                                  \
            else if (original == TRUE) {                       \
                returnthis = my_getVarInFrame(documentcontext, ofileSymbol, FALSE);\
            }                                                  \
            else {                                             \
                binding_info_t tmp; my_findVarInFrame(documentcontext, fileSymbol, &tmp);\
                if (tmp.value == my_UnboundValue)              \
                    Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(fileSymbol)));\
                if (my_TYPEOF(tmp) != PROMSXP)                 \
                    Rf_error("invalid '%s', is not a promise; should never happen, please report!", R_CHAR(PRINTNAME(fileSymbol)));\
                if (my_PRVALUE(tmp) == my_UnboundValue) {      \
                    if (original || for_msg)                   \
                        returnthis = my_getVarInFrame(documentcontext, ofileSymbol, FALSE);\
                    else returnthis = force(&tmp);             \
                }                                              \
                else returnthis = my_PRVALUE(tmp);             \
            }                                                  \
            if (verbose) {                                     \
                SEXP osource = my_findValInFrame(documentcontext, sourceSymbol);\
                if (osource == my_UnboundValue)                \
                    Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(sourceSymbol)));\
                if (TYPEOF(osource) != CHARSXP)                \
                    Rf_error(_("invalid '%s' value"), R_CHAR(PRINTNAME(sourceSymbol)));\
                if (streql(source, R_CHAR(osource)))           \
                    Rprintf("Source: %s\n", source);           \
                else                                           \
                    Rprintf("Source: %s, copied from %s\n", source, R_CHAR(osource));\
            }                                                  \
            Rf_unprotect((unprotect));                         \
            return returnthis;                                 \
                                    } while (0)


#define returnfile(which, source) _returnfile((which), (source), (nprotect_loop + nprotect))


            returnfile(which, source_char);
        }


        else if (identical(function, sys_source)) {
#undef source_char
#define source_char "call to function 'sys.source'"
            documentcontext = find_documentcontext(frame);
            srcfile = env_or_NULL(my_findValInFrame(frame, srcfileSymbol));
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = find_documentcontext(srcfile))) {


#define stopifnot_is_path do {                                 \
                ofile = my_getVarInFrame(frame, fileSymbol, FALSE);\
                if (!IS_SCALAR(ofile, STRSXP))                 \
                    Rf_error(_("'%s' must be a character string"), R_CHAR(PRINTNAME(fileSymbol)));\
                file = STRING_ELT(ofile, 0);                   \
                const char *url = R_CHAR(file);                \
                if (!(LENGTH(file) > 0))                       \
                    Rf_errorcall(sys_call(which, rho), "invalid '%s', must not be \"\""     , R_CHAR(PRINTNAME(fileSymbol)));\
                else if (is_clipboard(url))                    \
                    Rf_errorcall(sys_call(which, rho), "invalid '%s', %s"                   , R_CHAR(PRINTNAME(fileSymbol)), must_not_be_clipboard_message);\
                else if (streql(url, "stdin"))                 \
                    Rf_errorcall(sys_call(which, rho), "invalid '%s', must not be \"stdin\"", R_CHAR(PRINTNAME(fileSymbol)));\
                else if (is_url(url))                          \
                    Rf_errorcall(sys_call(which, rho), "invalid '%s', cannot be a URL"      , R_CHAR(PRINTNAME(fileSymbol)));\
                else if (is_file_uri(url))                     \
                    Rf_errorcall(sys_call(which, rho), "invalid '%s', cannot be a file URI" , R_CHAR(PRINTNAME(fileSymbol)));\
                        } while (0)


                stopifnot_documentcontext_env;
                stopifnot_is_path;
                stopif_documentcontext_emptyenv;
                define_documentcontext_in_frame;
            }
            else {
#define get_ofile_if_delayed_continue(environment, symbol)     \
                {                                              \
                    binding_info_t tmp; my_findVarInFrame((environment), (symbol), &tmp);\
                    ofile = tmp.value;                         \
                    if (ofile == my_UnboundValue)              \
                        Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(tmp.sym)));\
                    if (my_TYPEOF(tmp) == PROMSXP && ISUNBOUND(ofile = my_PRVALUE(tmp))) continue;\
                }
                get_ofile_if_delayed_continue(frame, fileSymbol);
                SEXP wd = my_getVarInFrame(frame, owdSymbol, TRUE);
                if (srcfile && wd == my_UnboundValue)
                    wd = my_getVarInFrame(srcfile, wdSymbol, TRUE);
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != my_UnboundValue && (owd) != R_NilValue),
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


        else if (identical(function, wrap_source)) {
#undef source_char
#define source_char "call to function wrap.source from package @R_PACKAGE_NAME@"
            if (local)
                Rf_error("%s cannot be called within %s() from package %s",
                      name, R_CHAR(PRINTNAME(wrap_sourceSymbol)), "@R_PACKAGE_NAME@");
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            if (documentcontext == my_UnboundValue)
                Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(documentcontextSymbol)));
            if (documentcontext == R_EmptyEnv) continue;
            stopifnot_documentcontext_env;
            SEXP n = my_findValInFrame(documentcontext, nSymbol);
            if (!IS_SCALAR(n, INTSXP))
                Rf_error(_("invalid '%s' value"), R_CHAR(PRINTNAME(nSymbol)));
            returnfile(n, source_char);
        }


        else if (rstudio_loaded && identical(function, debugSource)) {
#undef source_char
#define source_char "call to function 'debugSource' in 'RStudio'"
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
            }
            else {
                get_ofile_if_delayed_continue(frame, fileNameSymbol);
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


        else if (rstudio_background_job_tools_loaded && identical(function, _rs_sourceWithProgress)) {
#undef source_char
#define source_char ((iwhich[0] == 1) ? "background job in 'RStudio'" : "call to function '.rs.sourceWithProgress' in 'RStudio'")
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            if (R_existsVarInFrame(frame, statementsSymbol)) {
                SEXP statements = my_getVarInFrame(frame, statementsSymbol, TRUE);
                if (statements != my_UnboundValue && TYPEOF(statements) == EXPRSXP)
                    srcfile = env_or_NULL(Rf_getAttrib(statements, srcfileSymbol));
            }
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
                ifndef_srcfile_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = find_documentcontext(srcfile))) {
                stopifnot_documentcontext_env;
                define_documentcontext_in_frame;
            }
            else {
                get_ofile_if_delayed_continue(frame, scriptSymbol);
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
                    /* allow_blank_string     */ TRUE,
                    /* allow_clipboard        */ TRUE,
                    /* allow_stdin            */ TRUE,
                    /* allow_url              */ TRUE,
                    /* allow_file_uri         */ TRUE,
                    /* ignore_all             */ FALSE,
                    /* srcfile_original       */ NULL
                );
                if_srcfile_then_define_documentcontext;
            }
            returnfile(which, source_char);
        }


        else if (compiler_loaded && identical(function, loadcmp)) {
#undef source_char
#define source_char "call to function 'loadcmp' from package 'compiler'"
            /* much the same as sys.source() */
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
            }
            else {
                get_ofile_if_delayed_continue(frame, fileSymbol);
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ my_getVarInFrame(frame, owdSymbol, TRUE),
                    /* hasowd                 */ ((owd) != my_UnboundValue),
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


        else if (utils_loaded && identical(function, Sweave)) {
#undef source_char
#define source_char "call to function 'Sweave' from package 'utils'"
            SEXP vec_of_documentcontext = find_documentcontext(frame);
            SEXP names;
            if (vec_of_documentcontext == my_UnboundValue) {
                SEXP srcFilenames = my_findValInFrame(frame, srcFilenamesSymbol);
                if (srcFilenames == my_UnboundValue)
                    Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(srcFilenamesSymbol)));
                if (TYPEOF(srcFilenames) != STRSXP)
                    Rf_error(_("object '%s' of mode '%s' was not found"),
                        R_CHAR(PRINTNAME(srcFilenamesSymbol)), "character");


                R_xlen_t n = XLENGTH(srcFilenames);


                vec_of_documentcontext = Rf_allocVector(VECSXP, n);
                INCREMENT_NAMED_defineVar(documentcontextSymbol, vec_of_documentcontext, frame);
                R_LockBinding(documentcontextSymbol, frame);


                /* copy names to new object */
                names = Rf_allocVector(STRSXP, n);
                Rf_setAttrib(vec_of_documentcontext, R_NamesSymbol, names);
                for (R_xlen_t i = 0; i < n; i++)
                    SET_STRING_ELT(names, i, STRING_ELT(srcFilenames, i));
            }
            else {
                if (TYPEOF(vec_of_documentcontext) != VECSXP)
                    Rf_error(_("object '%s' of mode '%s' was not found"),
                        R_CHAR(PRINTNAME(documentcontextSymbol)), "list");
                names = Rf_getAttrib(vec_of_documentcontext, R_NamesSymbol);
                if (TYPEOF(names) != STRSXP)
                    Rf_error("object '%s(%s)' of mode '%s' was not found",
                        R_CHAR(PRINTNAME(R_NamesSymbol)), R_CHAR(PRINTNAME(documentcontextSymbol)), "character");
            }
            SEXP ofile = my_findValInFrame(frame, fileSymbol);
            if (!IS_SCALAR(ofile, STRSXP)) continue;
            SEXP file = STRING_ELT(ofile, 0);
            R_xlen_t indx = -999;
            for (R_xlen_t i = 0, n = Rf_xlength(names); i < n; i++) {
                if (STRING_ELT(names, i) == file) {
                    indx = i;
                    break;
                }
            }
            if (indx < 0)
                Rf_error(_("invalid '%s' value"), R_CHAR(PRINTNAME(documentcontextSymbol)));
            documentcontext = VECTOR_ELT(vec_of_documentcontext, indx);
            if (documentcontext == R_NilValue) {
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ NULL,
                    /* assign_as_binding      */ (Rf_error("invalid; %s %d", __FILE__, __LINE__), TRUE),
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
                SET_VECTOR_ELT(vec_of_documentcontext, indx, documentcontext);
            }
            else {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
            }
            returnfile(which, source_char);
        }


        else if (box_loaded && identical(function, load_from_source)) {
#undef source_char
#define source_char "call to function 'load_from_source' from package 'box'"
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            SEXP exprs = my_findValInFrame(frame, exprsSymbol);
            if (exprs != my_UnboundValue && TYPEOF(exprs) == EXPRSXP)
                srcfile = env_or_NULL(Rf_getAttrib(exprs, srcfileSymbol));
            SEXP mod_ns = my_getVarInFrame(frame, mod_nsSymbol, TRUE);
            if (mod_ns == my_UnboundValue || TYPEOF(mod_ns) != ENVSXP) mod_ns = NULL;
            if (documentcontext != my_UnboundValue) {


#define if_mod_ns_then_define_documentcontext                  \
                if (mod_ns) {                                  \
                    Rf_setAttrib(mod_ns, documentcontextSymbol, documentcontext);\
                }


#define ifndef_mod_ns_documentcontext_then_define              \
                if (mod_ns && ISNULL(Rf_getAttrib(mod_ns, documentcontextSymbol))) {\
                    Rf_setAttrib(mod_ns, documentcontextSymbol, documentcontext);\
                }


                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
                ifndef_mod_ns_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = find_documentcontext(srcfile))) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                define_documentcontext_in_frame;
                ifndef_mod_ns_documentcontext_then_define;
            }
            else if (mod_ns && !ISNULL(documentcontext = Rf_getAttrib(mod_ns, documentcontextSymbol))) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                define_documentcontext_in_frame;
                if_srcfile_then_define_documentcontext;
            }
            else {
                SEXP ofile = info_dollar_source_path(frame);
                Rf_protect(ofile);
                SEXP wd = srcfile ? my_getVarInFrame(srcfile, wdSymbol, TRUE) : my_UnboundValue;
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ info_source_pathSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != my_UnboundValue && (owd) != R_NilValue),
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
                Rf_unprotect(1);  /* ofile */
                if_srcfile_then_define_documentcontext;
                if_mod_ns_then_define_documentcontext;
            }
            returnfile(which, source_char);
        }


        else if (knitr_loaded && identical(function, knit)) {
#undef source_char
#define source_char "call to function 'knit' from package 'knitr'"
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
            }
            else {
                if (!R_existsVarInFrame(frame, oenvirSymbol)) continue;
                if (missing_input(frame)) {
                    documentcontext = R_EmptyEnv;
                    define_documentcontext_in_frame;
                    continue;
                }
                ofile = my_getVarInFrame(frame, inputSymbol, FALSE);
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ inputSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ knitr_output_dir(),
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
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            SEXP exprs = my_findValInFrame(frame, exprsSymbol);
            if (exprs != my_UnboundValue && TYPEOF(exprs) == EXPRSXP)
                srcfile = env_or_NULL(Rf_getAttrib(exprs, srcfileSymbol));
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = find_documentcontext(srcfile))) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                define_documentcontext_in_frame;
            }
            else {
                get_ofile_if_delayed_continue(frame, fileSymbol);
                SEXP wd = srcfile ? my_getVarInFrame(srcfile, wdSymbol, TRUE) : my_UnboundValue;
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != my_UnboundValue && (owd) != R_NilValue),
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
            documentcontext = find_documentcontext(frame);
            srcfile = env_or_NULL(my_findValInFrame(frame, srcSymbol));
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = find_documentcontext(srcfile))) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                define_documentcontext_in_frame;
            }
            else {
                SEXP wd, sym;
                if (srcfile) {
                    ofile = my_findValInFrame(srcfile, sym = filenameSymbol);
                    wd = my_getVarInFrame(srcfile, wdSymbol, TRUE);
                }
                else {
                    ofile = my_findValInFrame(frame, sym = fileSymbol);
                    wd = my_UnboundValue;
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
                    /* hasowd                 */ ((owd) != my_UnboundValue && (owd) != R_NilValue),
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
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
            }
            else {
                get_ofile_if_delayed_continue(frame, scriptSymbol);
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
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
            }
            else {
                get_ofile_if_delayed_continue(frame, scriptSymbol);
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
            frame = Rf_eval(getframe, rho);
            iwhich[0] -= 2;
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            SEXP expr = my_findValInFrame(frame, exprSymbol);
            if (expr != my_UnboundValue && TYPEOF(expr) == EXPRSXP)
                srcfile = env_or_NULL(Rf_getAttrib(expr, srcfileSymbol));
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = find_documentcontext(srcfile))) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                define_documentcontext_in_frame;
                Rf_defineVar(setsyspathwashereSymbol, R_FalseValue, documentcontext);
            }
            else {
                get_ofile_if_delayed_continue(frame, scriptSymbol);
                SEXP wd = srcfile ? my_getVarInFrame(srcfile, wdSymbol, TRUE) : my_getVarInFrame(frame, oldSymbol, TRUE);
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ scriptSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != my_UnboundValue && (owd) != R_NilValue),
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
                Rf_defineVar(setsyspathwashereSymbol, R_FalseValue, documentcontext);
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
            documentcontext = find_documentcontext(frame);
            srcfile = NULL;
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
            }
            else {
                {
                    binding_info_t tmp; my_findVarInFrame(frame, sourceSymbol, &tmp);
                    if (my_TYPEOF(tmp) == PROMSXP) {
                        /* we expect this promise to already be forced */
                        if (my_PRVALUE(tmp) == my_UnboundValue) continue;
                    }
                    SEXP tmp2 = force(&tmp);
                    if (Rf_asLogical(tmp2) != TRUE) continue;
                }
                get_ofile_if_delayed_continue(frame, scriptSymbol);
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
            documentcontext = find_documentcontext(frame);
            srcfile = env_or_NULL(my_findValInFrame(frame, srcfileSymbol));
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                maybe_overwrite_documentcontext_in_srcfile;
            }
            else if (srcfile &&
                     !ISUNBOUND(documentcontext = find_documentcontext(srcfile)) &&
                     documentcontext != R_EmptyEnv)
            {
                stopifnot_documentcontext_env;
                define_documentcontext_in_frame;
            }
            else {
                get_ofile_if_delayed_continue(frame, pathSymbol);
                int ignore_all = testthat_source_file_uses_brio_read_lines();
                SEXP wd = my_getVarInFrame(frame, old_dirSymbol, TRUE);
                if (srcfile && wd == my_UnboundValue)
                    wd = my_getVarInFrame(srcfile, wdSymbol, TRUE);
                set_documentcontext2(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ pathSymbol,
                    /* ofile                  */ ofile,
                    /* assign_here            */ frame,
                    /* assign_as_binding      */ TRUE,
                    /* normalize_action       */ NA_DEFAULT,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ wd,
                    /* hasowd                 */ ((owd) != my_UnboundValue && (owd) != R_NilValue),
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
                maybe_overwrite_documentcontext_in_srcfile;
            }
            returnfile(which, source_char);
        }


        /* this condition must be second last */
        else if (plumber_loaded && identical_ignore_bytecode_ignore_environment(function, Plumber_public_methods_initialize)) {
#undef source_char
#define source_char "call to function 'Plumber$public_methods$initialize' from package 'plumber'"
            documentcontext = find_documentcontext(frame);
            SEXP private_env = my_findValInFrame(CLOENV(function), privateSymbol);
            srcfile = NULL;
            if (TYPEOF(private_env) == ENVSXP) {
                SEXP parsed = my_findValInFrame(private_env, parsedSymbol);
                if (TYPEOF(parsed) == EXPRSXP) {
                    srcfile = env_or_NULL(Rf_getAttrib(parsed, srcfileSymbol));
                }
            }
            if (documentcontext != my_UnboundValue) {
                stopifnot_documentcontext_env;
                stopif_documentcontext_emptyenv;
                ifndef_srcfile_documentcontext_then_define;
            }
            else if (srcfile && !ISUNBOUND(documentcontext = find_documentcontext(srcfile))) {
                stopifnot_documentcontext_env;
                stopifnot_is_path;
                stopif_documentcontext_emptyenv;
                define_documentcontext_in_frame;
            }
            else {
                if (TYPEOF(private_env) != ENVSXP) continue;
                ofile = my_findValInFrame(private_env, filenameSymbol);
                if (ofile == my_UnboundValue)
                    Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(filenameSymbol)));
                SEXP wd = my_getVarInFrame(frame, old_wdSymbol, TRUE);
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
        else if (!ISUNBOUND(documentcontext = find_documentcontext(frame))) {
            if (documentcontext == R_EmptyEnv) continue;
            stopifnot_documentcontext_env;


            {
                SEXP tmp = my_findValInFrame(documentcontext, setsyspathwashereSymbol);
                if (tmp == my_UnboundValue)
                    Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(setsyspathwashereSymbol)));
                if (Rf_asLogical(tmp) == FALSE) {
                    if (local) Rf_error("expected TRUE value for '%s'", R_CHAR(PRINTNAME(setsyspathwashereSymbol)));
                    continue;
                }
            }


            srcfile = NULL;


            SEXP source = my_findValInFrame(documentcontext, sourceSymbol);
            if (source == my_UnboundValue)
                Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(sourceSymbol)));
            if (TYPEOF(source) != CHARSXP)
                Rf_error(_("invalid '%s' value"), R_CHAR(PRINTNAME(sourceSymbol)));


            SEXP n = my_findValInFrame(documentcontext, nSymbol);
            if (!IS_SCALAR(n, INTSXP))
                Rf_error(_("invalid '%s' value"), R_CHAR(PRINTNAME(nSymbol)));
            /* this could happen with eval() or similar */
            if (iwhich[0] != INTEGER(n)[0]) continue;


            returnfile(n, R_CHAR(source));


#undef returnfile
        }
    }


    Rf_unprotect(nprotect);


    if (local) {
        if (for_msg) return Rf_ScalarString(NA_STRING);
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
    if (value != my_UnboundValue) {
        if (!contents)
            return value;
        if (gave_contents)
            return value;
        if (!IS_SCALAR(value, STRSXP))
            Rf_error("internal error; invalid '%s' value", "_sys_path()");
        if (STRING_ELT(value, 0) == NA_STRING)
            return R_NilValue;
        SEXP expr = Rf_lcons(_get_contentsSymbol, Rf_cons(value, R_NilValue));
        Rf_protect(expr);
        value = Rf_eval(expr, mynamespace);
        Rf_unprotect(1);
        return value;
    }


    switch (gui_path) {
    case GUIPATH_DEFAULT:
    {
        SEXP expr = make_path_call(_gui_pathSymbol, verbose , original,
                                                    for_msg , contents);
        Rf_protect(expr);
        SEXP value = Rf_eval(expr, mynamespace);
        Rf_unprotect(1);
        if (contents && for_msg && IS_SCALAR(value, STRSXP) && STRING_ELT(value, 0) == NA_STRING)
            value = R_NilValue;
        return value;
    }
    case GUIPATH_FUNCTION:
    {
        SEXP expr = Rf_lcons(
            _custom_gui_path_functionSymbol,
            Rf_cons(
                Rf_ScalarLogical(verbose),
                Rf_cons(
                    Rf_ScalarLogical(original),
                    Rf_cons(
                        Rf_ScalarLogical(for_msg),
                        Rf_cons(
                            Rf_ScalarLogical(contents),
                            R_NilValue
                        )
                    )
                )
            )
        );
        Rf_protect(expr);
        SEXP value = Rf_eval(expr, _custom_gui_path_function_environment);
        Rf_protect(value);
        if (contents) {
            if (for_msg && IS_SCALAR(value, STRSXP) && STRING_ELT(value, 0) == NA_STRING)
                value = R_NilValue;
            else if (TYPEOF(value) == STRSXP)
                value = fixNewlines(value);
        }
        else {
            if (!IS_SCALAR(value, STRSXP))
                Rf_errorcall(expr, "invalid return value; must be a character string");
            if (for_msg);
            else if (is_abs_path(R_CHAR(STRING_ELT(value, 0))));
            else Rf_errorcall(expr, "invalid return value; must be an absolute path");
        }
        set_R_Visible(TRUE);
        Rf_unprotect(2);
        return value;
    }
    case GUIPATH_CHARACTER:
    {
        SEXP env = _custom_gui_path_character_environment;


        if (verbose) {
            SEXP guiname = my_findValInFrame(env, guinameSymbol);
            if (TYPEOF(guiname) != CHARSXP)
                Rf_error(_("object '%s' of mode '%s' was not found"),
                    R_CHAR(PRINTNAME(guinameSymbol)), "char");
            Rprintf("Source: document in %s\n", R_CHAR(guiname));
        }


        if (contents) {
            for_msg = FALSE;
            SEXP file = get_file_from_closure(original, for_msg, env);
            SEXP expr = Rf_lcons(_get_contentsSymbol, Rf_cons(file, R_NilValue));
            Rf_protect(expr);
            SEXP value;
            SEXP _getContents = my_getVarInFrame(env, _get_contentsSymbol, /* unbound_ok */ TRUE);
            if (_getContents != R_NilValue) {
                if (TYPEOF(_getContents) != CLOSXP)
                    Rf_error(_("object '%s' of mode '%s' was not found"),
                        R_CHAR(PRINTNAME(_get_contentsSymbol)), "function");
                value = Rf_eval(expr, env);
                if (TYPEOF(value) == STRSXP)
                    value = fixNewlines(value);
            }
            else {
                value = Rf_eval(expr, mynamespace);
            }
            set_R_Visible(TRUE);
            Rf_unprotect(1);
            return value;
        }
        set_R_Visible(TRUE);
        return get_file_from_closure(original, for_msg, env);
    }
    default:
        Rf_errorcall(R_NilValue, "internal error; invalid 'gui_path' value");
    }


    return R_NilValue;
}


static R_INLINE
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


    switch (Rf_length(args)) {
    case 0:
        break;
    case 1:
        local    = Rf_asLogical(CAR(args)); args = CDR(args);
        break;
    case 2:
        verbose  = Rf_asLogical(CAR(args)); args = CDR(args);
        local    = Rf_asLogical(CAR(args)); args = CDR(args);
        break;
    case 5:
        verbose  = Rf_asLogical(CAR(args)); args = CDR(args);
        original = Rf_asLogical(CAR(args)); args = CDR(args);
        for_msg  = Rf_asLogical(CAR(args)); args = CDR(args);
        contents = Rf_asLogical(CAR(args)); args = CDR(args);
        local    = Rf_asLogical(CAR(args)); args = CDR(args);
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), ".C_sys_path", "0, 1, 2, or 5"));
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


SEXP _env_path(Rboolean verbose, Rboolean original, Rboolean for_msg,
               Rboolean contents, SEXP target, SEXP envir,
               Rboolean *gave_contents, Rboolean unbound_ok, SEXP rho)
{
    *gave_contents = FALSE;
    Rboolean get_frame_number = FALSE;


    int nprotect = 0;


    if (envir == NULL) envir = parent_frame(rho);
    if (TYPEOF(envir) != ENVSXP) envir = rho;


    SEXP env;
    if (target == NULL) target = Rf_GetOption1(topLevelEnvironmentSymbol);
    if (target != R_NilValue && TYPEOF(target) != ENVSXP) target = R_NilValue;
    env = Rf_topenv(target, envir);


    SEXP returnvalue; (void) returnvalue;
    SEXP documentcontext;


    SEXP errcnd, ofile, lines, path;


    if (env == R_GlobalEnv ||
        env == R_BaseEnv || env == R_BaseNamespace ||
        R_IsPackageEnv(env) || R_IsNamespaceEnv(env));
    else if (Rf_inherits(env, "box$ns")) {
#undef source_char
#define source_char "path of a 'package:box' namespace"
        documentcontext = Rf_getAttrib(env, documentcontextSymbol);
        if (documentcontext != R_NilValue) {
            stopifnot_documentcontext_env;
        }
        else {
            SEXP info = my_findValInFrame(env, moduleSymbol);
            if (info != my_UnboundValue && TYPEOF(info) == ENVSXP) {
                SEXP spec = my_findValInFrame(info, infoSymbol);
                if (spec != my_UnboundValue && TYPEOF(spec) == VECSXP) {
                    SEXP names = Rf_getAttrib(spec, R_NamesSymbol);
                    if (names != R_NilValue && TYPEOF(names) == STRSXP) {
                        for (R_xlen_t i = 0, n = XLENGTH(spec); i < n; i++) {
                            if (!strcmp(R_CHAR(STRING_ELT(names, i)), "source_path")) {
                                path = VECTOR_ELT(spec, i);
                                if (TYPEOF(path) == STRSXP && XLENGTH(path) > 0 &&
                                    is_abs_path(R_CHAR(STRING_ELT(path, 0))))
                                {
                                    Rf_protect(documentcontext = DocumentContext());
                                    Rf_protect(ofile = Rf_ScalarString(STRING_ELT(path, 0)));
                                    assign_default(NULL, NULL, ofile, STRING_ELT(ofile, 0), documentcontext, NA_DEFAULT);
                                    INCREMENT_NAMED_defineVar(sourceSymbol, Rf_mkChar(source_char), documentcontext);
                                    Rf_setAttrib(env, documentcontextSymbol, documentcontext);
                                    Rf_unprotect(2);
                                }
                            }
                        }
                    }
                }
            }
            if (documentcontext == R_NilValue)
                Rf_error("invalid 'package:box' namespace without an associated path");
        }
#define returnfile _returnfile((Rf_error(_("invalid '%s' value"), "get_frame_number"), R_NilValue), (source_char), (nprotect))


        returnfile;
    }
    else if (!ISNULL(documentcontext = Rf_getAttrib(env, documentcontextSymbol)))
    {
#undef source_char
#define source_char "path of top level environment"
        stopifnot_documentcontext_env;
        returnfile;
    }
    else if (Rf_isString(path = Rf_getAttrib(env, pathSymbol)) && XLENGTH(path) > 0)
    {
        Rf_protect(ofile = Rf_ScalarString(STRING_ELT(path, 0))); nprotect++;
        const char *str = R_CHAR(STRING_ELT(ofile, 0));
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


    Rf_unprotect(nprotect);


    if (unbound_ok)
        return my_UnboundValue;
    else if (for_msg)
        return Rf_ScalarString(NA_STRING);
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
    if (unbound_ok && value == my_UnboundValue)
        return value;
    if (!contents)
        return value;
    if (gave_contents)
        return value;
    if (!IS_SCALAR(value, STRSXP))
        Rf_error("internal error; invalid '%s' value", "_env_path()");
    if (STRING_ELT(value, 0) == NA_STRING)
        return R_NilValue;
    SEXP expr = Rf_lcons(_get_contentsSymbol, Rf_cons(value, R_NilValue));
    Rf_protect(expr);
    value = Rf_eval(expr, mynamespace);
    Rf_unprotect(1);
    return value;
}


static R_INLINE
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


    switch (Rf_length(args)) {
    case 0:
        break;
    case 2:
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        break;
    case 3:
        verbose  = Rf_asLogical(CAR(args)); args = CDR(args);
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        break;
    case 6:
        verbose  = Rf_asLogical(CAR(args)); args = CDR(args);
        original = Rf_asLogical(CAR(args)); args = CDR(args);
        for_msg  = Rf_asLogical(CAR(args)); args = CDR(args);
        contents = Rf_asLogical(CAR(args)); args = CDR(args);
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), ".C_env_path", "0, 2, 3, or 6"));
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
            Rf_protect(srcref = sys_srcref(x ? INTEGER(x)[0] : 0, rho));
            SEXP returnthis = Rf_ScalarInteger(
                ISNULL(srcref) ? (NA_INTEGER) : (INTEGER(srcref)[0])
            );
            Rf_unprotect(1);
            return returnthis;
        }
        switch (TYPEOF(x)) {
        case SYMSXP:
        case CLOSXP:
            srcref = Rf_protect(Rf_getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == INTSXP) {


#define return_first_line_from_srcref do {                     \
                    SEXP returnthis = Rf_ScalarInteger(INTEGER(srcref)[0]);\
                    Rf_unprotect(nprotect);                    \
                    return returnthis;                         \
                } while (0)


                return_first_line_from_srcref;
            }
            break;
        case LANGSXP:
            wholeSrcref = Rf_protect(Rf_getAttrib(x, wholeSrcrefSymbol)); nprotect++;
            if (TYPEOF(wholeSrcref) == INTSXP) {
                srcref = wholeSrcref;
                return_first_line_from_srcref;
            }
            srcref = Rf_protect(Rf_getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == INTSXP)
                return_first_line_from_srcref;
            if (TYPEOF(srcref) == VECSXP && LENGTH(srcref) &&
                TYPEOF(srcref = VECTOR_ELT(srcref, 0)) == INTSXP)
            {
                return_first_line_from_srcref;
            }
            break;
        case EXPRSXP:
            wholeSrcref = Rf_protect(Rf_getAttrib(x, wholeSrcrefSymbol)); nprotect++;
            if (TYPEOF(wholeSrcref) == INTSXP) {
                srcref = wholeSrcref;
                return_first_line_from_srcref;
            }
            srcref = Rf_protect(Rf_getAttrib(x, srcrefSymbol)); nprotect++;
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
        Rf_unprotect(nprotect);
        return Rf_ScalarInteger(NA_INTEGER);
    }


    if (x == NULL || IS_SCALAR(x, INTSXP)) {


#define get_srcfile_from_srcref do {                           \
            srcfile = Rf_protect(Rf_getAttrib(srcref, srcfileSymbol)); nprotect++;\
            if (TYPEOF(srcfile) != ENVSXP) srcfile = NULL;     \
        } while (0)


        srcfile = sys_srcfile(x ? INTEGER(x)[0] : 0, rho);
        if (TYPEOF(srcfile) != ENVSXP) srcfile = NULL;
        else { Rf_protect(srcfile); nprotect++;
            SEXP tmp, tmp2;
            /*
             * if we're in Positron
             * and 'srcfile' is a "srcfilealias"
             * and 'srcfile$original' is a "srcfilecopy"
             * and 'srcfile$original$filename' is a blank string
             * and 'srcfile$original$Enc' is unbound
             * and 'srcfile$original$isFile' is unbound
             * and 'srcfile$original$timestamp' is unbound
             * and 'srcfile$original$wd' is unbound
             * and the first line of 'srcfile$original$lines' starts with "#line "
             *
             * then ignore 'srcfile'
             */
            if (
                gui_positron &&
                Rf_inherits(srcfile, "srcfilealias") &&
                TYPEOF(tmp = my_findValInFrame(srcfile, originalSymbol)) == ENVSXP &&
                Rf_inherits(tmp, "srcfilecopy")
            ) {
                Rf_protect(tmp);
                if (TYPEOF(tmp2 = my_findValInFrame(tmp, filenameSymbol)) == STRSXP &&
                    Rf_xlength(tmp2) == 1 &&
                    Rf_xlength(STRING_ELT(tmp2, 0)) == 0 &&
                    my_findValInFrame(tmp, EncSymbol) == my_UnboundValue &&
                    my_findValInFrame(tmp, isFileSymbol) == my_UnboundValue &&
                    my_findValInFrame(tmp, timestampSymbol) == my_UnboundValue &&
                    my_findValInFrame(tmp, wdSymbol) == my_UnboundValue &&
                    TYPEOF(tmp = my_getVarInFrame(tmp, linesSymbol, FALSE)) == STRSXP &&
                    Rf_xlength(tmp) > 0 &&
                    !strncmp(R_CHAR(STRING_ELT(tmp, 0)), "#line ", 6)
                ) {
                    /* then the source reference is not valid, disregard it */
                    srcfile = NULL;
                }
                Rf_unprotect(1);
            }
        }
    }
    else switch (TYPEOF(x)) {
    case SYMSXP:
    case CLOSXP:
        srcref = Rf_protect(Rf_getAttrib(x, srcrefSymbol)); nprotect++;
        if (TYPEOF(srcref) == INTSXP)
            get_srcfile_from_srcref;
        break;
    case LANGSXP:
        srcfile = Rf_protect(Rf_getAttrib(x, srcfileSymbol)); nprotect++;
        if (TYPEOF(srcfile) == ENVSXP);
        else {
            srcfile = NULL;
            srcref = Rf_protect(Rf_getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == INTSXP)
                get_srcfile_from_srcref;
        }
        break;
    case EXPRSXP:
        srcfile = Rf_protect(Rf_getAttrib(x, srcfileSymbol)); nprotect++;
        if (TYPEOF(srcfile) == ENVSXP);
        else {
            srcfile = NULL;
            srcref = Rf_protect(Rf_getAttrib(x, srcrefSymbol)); nprotect++;
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
        if (Rf_inherits(x, "srcfile")) srcfile = x;
        break;
    }


    SEXP returnvalue; (void) returnvalue;


    SEXP ofile, errcnd, lines;


#undef source_char
#define source_char "path of srcfile"
    if (srcfile) {
        SEXP documentcontext = find_documentcontext(srcfile);
        if (documentcontext != my_UnboundValue) {
            stopifnot_documentcontext_env;
        }
        else if (Rf_inherits(srcfile, "srcfilecopy") &&
                 Rf_asLogical(my_findValInFrame(srcfile, isFileSymbol)) != TRUE)
        {
            documentcontext = R_EmptyEnv;
            define_documentcontext_in_srcfile;
        }
        else {
            ofile = my_findValInFrame(srcfile, filenameSymbol);
            if (ofile == my_UnboundValue)
                Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(filenameSymbol)));
            SEXP srcfile_original = NULL;
            if (Rf_inherits(srcfile, "srcfilealias")) {
                SEXP tmp = my_findValInFrame(srcfile, originalSymbol);
                // declare this as a new SEXP so as to not overwrite it in the previous context
                SEXP srcfile = tmp;
                if (TYPEOF(srcfile) != ENVSXP)
                    Rf_error(_("object '%s' of mode '%s' was not found"),
                        R_CHAR(PRINTNAME(originalSymbol)), "environment");
                if (!Rf_inherits(srcfile, "srcfile"))
                    Rf_error("object '%s' is not a srcfile", R_CHAR(PRINTNAME(originalSymbol)));
                // declare this as a new SEXP so as to not overwrite it in the previous context
                SEXP documentcontext = find_documentcontext(srcfile);
                if (documentcontext != my_UnboundValue) {
                    stopifnot_documentcontext_env;
                }
                else if (Rf_inherits(srcfile, "srcfilecopy") &&
                         Rf_asLogical(my_findValInFrame(srcfile, isFileSymbol)) != TRUE)
                {
                    documentcontext = R_EmptyEnv;
                    define_documentcontext_in_srcfile;
                }
                else {
                    // declare this as a new SEXP so as to not overwrite it in the previous context
                    SEXP ofile = my_findValInFrame(srcfile, filenameSymbol);
                    if (ofile == my_UnboundValue)
                        Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(filenameSymbol)));
                    set_documentcontext2(
                        /* call                   */ R_NilValue,
                        /* sym                    */ original_filenameSymbol,
                        /* ofile                  */ ofile,
                        /* assign_here            */ srcfile,
                        /* assign_as_binding      */ TRUE,
                        /* normalize_action       */ NA_FIX_DIR,
                        /* maybe_chdir            */ TRUE,
                        /* getowd                 */ my_getVarInFrame(srcfile, wdSymbol, TRUE),
                        /* hasowd                 */ ((owd) != my_UnboundValue && (owd) != R_NilValue),
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
                /* getowd                 */ my_getVarInFrame(srcfile, wdSymbol, TRUE),
                /* hasowd                 */ ((owd) != my_UnboundValue && (owd) != R_NilValue),
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
            Rf_unprotect(nprotect);
            return documentcontext;
        }
        returnfile;
    }


    Rf_unprotect(nprotect);


    if (unbound_ok)
        return my_UnboundValue;
    else if (for_msg)
        return Rf_ScalarString(NA_STRING);
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
    if (unbound_ok && value == my_UnboundValue)
        return value;
    if (!contents)
        return value;
    if (gave_contents)
        return value;
    if (!IS_SCALAR(value, STRSXP))
        Rf_error("internal error; invalid '%s' value", "_src_path()");
    if (STRING_ELT(value, 0) == NA_STRING)
        return R_NilValue;
    SEXP expr = Rf_lcons(_get_contentsSymbol, Rf_cons(value, R_NilValue));
    Rf_protect(expr);
    value = Rf_eval(expr, mynamespace);
    Rf_unprotect(1);
    return value;
}


static R_INLINE
SEXP src_path6(Rboolean verbose, Rboolean original, Rboolean for_msg,
               Rboolean contents, SEXP srcfile, SEXP rho)
{
    return src_path7(verbose, original, for_msg, contents, srcfile,
                     /* unbound_ok */ FALSE, rho);
}


SEXP do_src_path do_formals
{
    do_start_no_op("src_path", -1);


    Rboolean verbose  = FALSE,
             original = FALSE,
             for_msg  = FALSE,
             contents = FALSE;
    SEXP srcfile = NULL;


    switch (Rf_length(args)) {
    case 0:
        break;
    case 1:
        srcfile  = CAR(args); args = CDR(args);
        break;
    case 2:
        verbose  = Rf_asLogical(CAR(args)); args = CDR(args);
        srcfile  = CAR(args); args = CDR(args);
        break;
    case 5:
        verbose  = Rf_asLogical(CAR(args)); args = CDR(args);
        original = Rf_asLogical(CAR(args)); args = CDR(args);
        for_msg  = Rf_asLogical(CAR(args)); args = CDR(args);
        contents = Rf_asLogical(CAR(args)); args = CDR(args);
        srcfile  = CAR(args); args = CDR(args);
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), ".C_src_path", "0, 1, 2, or 5"));
        return R_NilValue;
    }


    check_arguments4(verbose, original, for_msg, contents);
    return src_path6(verbose, original, for_msg, contents, srcfile, rho);
}


SEXP do_src_LINENO do_formals
{
    do_start_no_op("src_LINENO", -1);


    SEXP srcfile = NULL;


    switch (Rf_length(args)) {
    case 0:
        break;
    case 1:
        srcfile = CAR(args); args = CDR(args);
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), ".C_src_LINENO", "0 or 1"));
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


    switch (Rf_length(args)) {
    case 0:
        break;
    case 4:
        local    = Rf_asLogical(CAR(args)); args = CDR(args);
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        srcfile  = CAR(args); args = CDR(args);
        break;
    case 5:
        verbose  = Rf_asLogical(CAR(args)); args = CDR(args);
        local    = Rf_asLogical(CAR(args)); args = CDR(args);
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        srcfile  = CAR(args); args = CDR(args);
        break;
    case 8:
        verbose  = Rf_asLogical(CAR(args)); args = CDR(args);
        original = Rf_asLogical(CAR(args)); args = CDR(args);
        for_msg  = Rf_asLogical(CAR(args)); args = CDR(args);
        contents = Rf_asLogical(CAR(args)); args = CDR(args);
        local    = Rf_asLogical(CAR(args)); args = CDR(args);
        envir    = CAR(args); args = CDR(args);
        target   = CAR(args); args = CDR(args);
        srcfile  = CAR(args); args = CDR(args);
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), ".C_this_path", "0, 4, 5, or 8"));
        return R_NilValue;
    }


    check_arguments5(verbose, original, for_msg, contents, local);


    if (local) return sys_path6(verbose, original, for_msg, contents, local, rho);


    SEXP value = src_path7(verbose, original, for_msg, contents, srcfile,
                           /* unbound_ok */ TRUE, rho);
    if (value == my_UnboundValue) {
        value = env_path8(verbose, original, for_msg, contents, target, envir,
                          /* unbound_ok */ TRUE, rho);
        if (value == my_UnboundValue) {
            value = sys_path6(verbose, original, for_msg, contents, local, rho);
        }
    }
    return value;
}
