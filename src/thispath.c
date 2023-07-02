#include "thispathdefn.h"
#include "drivewidth.h"


#define errorbody                                              \
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1 ||      \
        STRING_ELT(CAR(args), 0) == NA_STRING)                 \
    {                                                          \
        errorcall(call, _("invalid first argument"));          \
    }                                                          \
    const char *msg = translateChar(STRING_ELT(CAR(args), 0)); args = CDR(args);\
    SEXP call2 = CAR(args); args = CDR(args);                  \
    ENSURE_NAMEDMAX(call2)


SEXP do_thisPathUnrecognizedConnectionClassError do_formals
{
    do_start_no_call_op_rho("thisPathUnrecognizedConnectionClassError", 2);


    SEXP call2 = CAR(args); args = CDR(args);
    ENSURE_NAMEDMAX(call2);
#if defined(R_CONNECTIONS_VERSION_1)
    return thisPathUnrecognizedConnectionClassError(call2,
        /* as I said before, R_GetConnection() is not a part of the R API.
           DO NOT USE IT unless you are certain of what you are doing and
           accept the potential consequences and drawbacks */
        R_GetConnection(CAR(args)));
#else
    return thisPathUnrecognizedConnectionClassError(call2, summaryconnection(CAR(args)));
#endif
}


SEXP do_thisPathUnrecognizedMannerError do_formals
{
    do_start_no_call_op_rho("thisPathUnrecognizedMannerError", 1);


    ENSURE_NAMEDMAX(CAR(args));
    return thisPathUnrecognizedMannerError(CAR(args));
}


SEXP do_thisPathNotImplementedError do_formals
{
    do_start_no_op_rho("thisPathNotImplementedError", 2);


    errorbody;
    return thisPathNotImplementedError(msg, call2);
}


SEXP do_thisPathNotExistsError do_formals
{
    do_start_no_op_rho("thisPathNotExistsError", 2);


    errorbody;
    return thisPathNotExistsError(msg, call2);
}


SEXP do_thisPathInZipFileError do_formals
{
    do_start_no_op_rho("thisPathInZipFileError", 2);


    SEXP call2 = CAR(args); args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1 ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        errorcall(call, "invalid second argument");
    }
    SEXP description = STRING_ELT(CAR(args), 0);
    return thisPathInZipFileError(call2, description);
}


SEXP do_thisPathInAQUAError do_formals
{
    do_start_no_call_op_rho("thisPathInAQUAError", 1);


    return thisPathInAQUAError(lazy_duplicate(CAR(args)));
}


#undef errorbody


SEXP do_isclipboard do_formals
{
    do_start_no_call_op_rho("isclipboard", 1);


    SEXP file = CAR(args);
    if (TYPEOF(file) != STRSXP)
        error(_("a character vector argument expected"));
    int n = LENGTH(file);
    SEXP value = allocVector(LGLSXP, n);
    PROTECT(value);
    int *ivalue = INTEGER(value);
    const char *url;
    for (int i = 0; i < n; i++) {
        url = CHAR(STRING_ELT(file, i));
        ivalue[i] = isclipboard(url);
    }
    UNPROTECT(1);
    return value;
}


SEXP do_inittoolsrstudio do_formals
{
    do_start_no_op_rho("inittoolsrstudio", -1);


    Rboolean skipCheck = FALSE;
    switch (length(args)) {
    case 0:
        break;
    case 1:
        skipCheck = asLogical(CAR(args));
        if (skipCheck == NA_LOGICAL)
            errorcall(call, _("invalid '%s' argument"), "skipCheck");
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), ".C_inittoolsrstudio", "0 or 1"));
    }
    return ScalarLogical(init_tools_rstudio(skipCheck));
}


void check_arguments7(Rboolean verbose         , Rboolean original        ,
                      Rboolean for_msg         , Rboolean contents        ,
                      Rboolean local           , int N                    ,
                      Rboolean get_frame_number)
{
    if (verbose == NA_LOGICAL)
        error(_("invalid '%s' value"), "verbose");
    /* original is allowed to be NA */
    if (for_msg == NA_LOGICAL)
        error(_("invalid '%s' value"), "for.msg");
    if (contents == NA_LOGICAL)
        error(_("invalid '%s' value"), "contents");
    if (local == NA_LOGICAL)
        error(_("invalid '%s' value"), "local");
    if (N == NA_INTEGER);
    else if (N < 0) error(_("invalid '%s' argument"), "N");
    if (get_frame_number == NA_LOGICAL)
        error(_("invalid '%s' value"), "get.frame.number");


#define one   "'%s' must be FALSE when '%s' is TRUE"
#define two   "'%s' and '%s' must be FALSE when '%s' is TRUE"
#define three "'%s', '%s', and '%s' must be FALSE when '%s' is TRUE"
#define four  "'%s', '%s', '%s', and '%s' must be FALSE when '%s' is TRUE"


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


    if (contents) {
        if (original)
            error(one, "original", "contents");
        else
            ;
    }


#undef four
#undef three
#undef two
#undef one
}


void check_arguments5(Rboolean verbose , Rboolean original, Rboolean for_msg ,
                      Rboolean contents, Rboolean local   )
{
    check_arguments7(verbose, original, for_msg, contents, local,
                     /* N */ NA_INTEGER, /* get_frame_number */ FALSE);
}


void check_arguments4(Rboolean verbose , Rboolean original, Rboolean for_msg ,
                      Rboolean contents)
{
    check_arguments5(verbose, original, for_msg, contents, /* local */ FALSE);
}


void check_arguments1(Rboolean verbose)
{
    check_arguments4(verbose, /* original */ FALSE, /* for_msg */ FALSE,
                     /* contents */ FALSE);
}


SEXP do_syspathjupyter do_formals
{
    do_start_no_call_op("syspathjupyter", 4);


    Rboolean verbose, original, for_msg, contents;


    verbose  = asLogical(CAR(args)); args = CDR(args);
    original = asLogical(CAR(args)); args = CDR(args);
    for_msg  = asLogical(CAR(args)); args = CDR(args);
    contents = asLogical(CAR(args)); args = CDR(args);
    check_arguments4(verbose, original, for_msg, contents);


    if (verbose) Rprintf("Source: document in Jupyter\n");


    if (contents) {


#define get_thispathfilejupyter                                \
        SEXP var = findVarInFrame(ENCLOS(rho), thispathfilejupyterSymbol);\
        if (var == R_UnboundValue)                             \
            error(_("object '%s' not found"), EncodeChar(PRINTNAME(thispathfilejupyterSymbol)));\
        if (TYPEOF(var) != PROMSXP)                            \
            error("invalid '%s', must be a promise", EncodeChar(PRINTNAME(thispathfilejupyterSymbol)))


        get_thispathfilejupyter;


        if (PRVALUE(var) == R_UnboundValue) {
            if (PRSEEN(var)) {
                if (PRSEEN(var) == 1) {}
                else SET_PRSEEN(var, 0);
            }
        }


        SEXP expr = LCONS(_getJupyterNotebookContentsSymbol, CONS(var, R_NilValue));
        PROTECT(expr);
        SEXP value = eval(expr, mynamespace);
        UNPROTECT(1);
        return value;
    }


    if (for_msg && !original) original = NA_LOGICAL;


    if (original == NA_LOGICAL) {
        get_thispathfilejupyter;


        if (PRVALUE(var) == R_UnboundValue)
            original = TRUE;
        else
            return PRVALUE(var);
    }


    if (original) {
        return getInFrame(thispathofilejupyterSymbol, ENCLOS(rho), FALSE);
    }
    else {
        get_thispathfilejupyter;
        if (PRVALUE(var) == R_UnboundValue) {
            if (PRSEEN(var)) {
                if (PRSEEN(var) == 1) {}
                else SET_PRSEEN(var, 0);
            }
            return eval(var, R_EmptyEnv);
        }
        else return PRVALUE(var);
    }
}


SEXP do_syspathrgui do_formals
{
    do_start_no_op("syspathrgui", 7);


    Rboolean verbose, original, for_msg, contents;
    SEXP wintitle, untitled, r_editor;


    verbose  = asLogical(CAR(args)); args = CDR(args);
    original = asLogical(CAR(args)); args = CDR(args);
    for_msg  = asLogical(CAR(args)); args = CDR(args);
    contents = asLogical(CAR(args)); args = CDR(args);
    check_arguments4(verbose, original, for_msg, contents);


    /* titles of the windows in RGui */
    wintitle = CAR(args); args = CDR(args);
    if (!(TYPEOF(wintitle) == STRSXP || wintitle == R_NilValue))
        errorcall(call, "%s, must be %s", _("invalid first argument"), "'character' / / NULL");


    /* strings representing non-existent files in RGui */
    untitled = CAR(args); args = CDR(args);
    if (!(TYPEOF(untitled) == STRSXP || untitled == R_NilValue))
        errorcall(call, "%s, must be %s", "invalid second argument", "'character' / / NULL");


    /* strings representing R scripts in RGui */
    r_editor = CAR(args); args = CDR(args);
    if (!(TYPEOF(r_editor) == STRSXP || r_editor == R_NilValue))
        errorcall(call, "%s, must be %s", "invalid third argument", "'character' / / NULL");


    Rboolean active = TRUE;
    int n = LENGTH(wintitle);
    int length_untitled = LENGTH(untitled);
    int length_r_editor = LENGTH(r_editor);
    for (int i = 0; i < n; i++) {
        SEXP wintitle0 = STRING_ELT(wintitle, i);
        if (wintitle0 == NA_STRING || wintitle0 == R_BlankString) continue;
        const char *title = CHAR(wintitle0);
        int nchar_title = (int) strlen(title);


        /* if the title and untitled strings are equal (byte-wise, do
         * not care if encodings match), then the script does not exist
         */
        for (int j = 0; j < length_untitled; j++) {
            SEXP untitled0 = STRING_ELT(untitled, j);
            if (untitled0 == NA_STRING || untitled0 == R_BlankString) continue;
            // if (wintitle0 == untitled0) {
            if (!strcmp(title, CHAR(untitled0))) {
                if (for_msg) {
                    if (contents)
                        return ScalarString(NA_STRING);
                    else
                        return mkString(_RGui("Untitled"));
                }
                error(active ? "active document in Rgui does not exist" :
                               "source document in Rgui does not exist");
            }
        }


        /* if the title ends with R Editor strings (again, bit-wise),
         * then it is an R script, remove the suffix and return
         */
        for (int j = 0; j < length_r_editor; j++) {
            SEXP r_editor0 = STRING_ELT(r_editor, j);
            if (r_editor0 == NA_STRING || r_editor0 == R_BlankString) continue;
            const char *suffix = CHAR(r_editor0);
            int nchar_suffix = (int) strlen(suffix);
            int off = nchar_title - nchar_suffix;
            if (off > 0) {
                if (memcmp(title + off, suffix, nchar_suffix) == 0) {
                    SEXP ans = mkCharLenCE(title, off, getCharCE(wintitle0));
                    if (!is_abs_path_windows(CHAR(ans)))
                        error("invalid title, path preceding '%s' must be absolute", suffix);


#define return_abs_path(charsxp)                               \
                    if (verbose)                               \
                        Rprintf((active) ? "Source: active document in Rgui\n" :\
                                           "Source: source document in Rgui\n");\
                    if (original)                              \
                        return ScalarString((charsxp));        \
                    SEXP expr = LCONS(_normalizePathSymbol,    \
                                      CONS(ScalarString((charsxp)), R_NilValue));\
                    PROTECT(expr);                             \
                    SEXP returnthis = eval(expr, mynamespace); \
                    UNPROTECT(1);                              \
                    return returnthis


                    return_abs_path(ans);
                }
            }
        }


        /* if found an absolute path, return it */
        if (is_abs_path_windows(title)) {
            active = FALSE;
            return_abs_path(wintitle0);
        }


        /* determine if the executing script is active */
        if (active) {
            if (!strcmp(title, "R Console") ||
                !strcmp(title, "R Console (64-bit)") ||
                !strcmp(title, "R Console (32-bit)"))
            {
                active = FALSE;
            }
        }
    }


    if (for_msg) return ScalarString(NA_STRING);
    if (active) error("no windows in Rgui; should never happen, please report!");


    const char *msg = "R is running from Rgui with no documents open";
    SEXP cond = thisPathNotExistsError(msg, PROTECT(getCurrentCall(rho)));
    PROTECT(cond);
    stop(cond);
    UNPROTECT(2);
    return R_NilValue;  /* should not be reached */
}


SEXP _syspath(Rboolean verbose         , Rboolean original        ,
              Rboolean for_msg         , Rboolean contents        ,
              Rboolean local           , int N                    ,
              Rboolean get_frame_number, SEXP rho                 )
{
    static const char *name = "'sys.path(local = TRUE)'";


    SEXP returnthis = NULL;
    SEXP returnvalue;  /* checkfile() creates a variable 'returnvalue' that is
                          used in setsyspath() (see ./src/wrapsource.c).
                          not used elsewhere but must be declared */


    if (N == NA_INTEGER) {
        if (local)
            N = get_sys_parent(1, rho);
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
        PROTECT(frame);


        /* ensure 'sys.path(local = TRUE)' isn't evaluated in an invalid environment */
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


        if (!existsInFrame(frame, setsyspathwashereSymbol))
            error("%s cannot be called within this environment", name);
        if (!existsInFrame(frame, thispathdoneSymbol))
            error("%s cannot be called within this environment", name);


        UNPROTECT(1);  /* frame */


        SEXP expr;
        PROTECT_INDEX indx;
        PROTECT_WITH_INDEX(expr = CONS(ScalarInteger(N), R_NilValue), &indx);
        REPROTECT(expr = LCONS(getFromBase(sys_functionSymbol), expr), indx);
        SEXP function = eval(expr, rho);
        if (TYPEOF(function) != CLOSXP)
            error("%s cannot be used within a '%s', possible errors with eval?",
                  name, type2char(TYPEOF(function)));
        UNPROTECT(1);
    }


    if (N <= 0) {


#define toplevel                                               \
        if (get_frame_number) return ScalarInteger(0);         \
        SEXP expr;                                             \
        if (contents) {                                        \
            expr = LCONS(_sys_path_toplevelSymbol,             \
                         CONS(ScalarLogical(verbose),          \
                              CONS(ScalarLogical(original),    \
                                   CONS(ScalarLogical(for_msg),\
                                        CONS(ScalarLogical(contents), R_NilValue)))));\
        } else if (for_msg) {                                  \
            expr = LCONS(_sys_path_toplevelSymbol,             \
                         CONS(ScalarLogical(verbose),          \
                              CONS(ScalarLogical(original),    \
                                   CONS(ScalarLogical(for_msg), R_NilValue))));\
        } else if (original) {                                 \
            expr = LCONS(_sys_path_toplevelSymbol,             \
                         CONS(ScalarLogical(verbose),          \
                              CONS(ScalarLogical(original), R_NilValue)));\
        } else if (verbose) {                                  \
            expr = LCONS(_sys_path_toplevelSymbol,             \
                         CONS(ScalarLogical(verbose), R_NilValue));\
        } else {                                               \
            return eval(expr__sys_path_toplevel, mynamespace); \
        }                                                      \
        PROTECT(expr);                                         \
        returnthis = eval(expr, mynamespace);                  \
        UNPROTECT(1);                                          \
        return returnthis


        toplevel;
    }


    SEXP thispathofile    , thispathfile     , thispathformsg   ,
         thispatherror    , setsyspathwashere;


    int nprotect = 0;


    init_tools_rstudio(/* skipCheck */ FALSE);


    SEXP source      = getFromBase(sourceSymbol    ),
         sys_source  = getFromBase(sys_sourceSymbol),
         debugSource = get_debugSource,
         wrap_source = getFromMyNS(wrap_sourceSymbol);


    SEXP ns;
    Rboolean testthat_loaded, knitr_loaded, compiler_loaded, box_loaded      ;
    SEXP     source_file    , knit        , loadcmp        , load_from_source;


    ns = findVarInFrame(R_NamespaceRegistry, testthatSymbol);
    testthat_loaded = (ns == R_UnboundValue ? FALSE : TRUE);
    source_file = (testthat_loaded ? getInFrame(source_fileSymbol, ns, FALSE) : R_UnboundValue);


    ns = findVarInFrame(R_NamespaceRegistry, knitrSymbol);
    knitr_loaded = (ns == R_UnboundValue ? FALSE : TRUE);
    knit = (knitr_loaded ? getInFrame(knitSymbol, ns, FALSE) : R_UnboundValue);


    ns = findVarInFrame(R_NamespaceRegistry, boxSymbol);
    box_loaded = (ns == R_UnboundValue ? FALSE : TRUE);
    load_from_source = (box_loaded ? getInFrame(load_from_sourceSymbol, ns, FALSE) : R_UnboundValue);


    ns = findVarInFrame(R_NamespaceRegistry, compilerSymbol);
    compiler_loaded = (ns == R_UnboundValue ? FALSE : TRUE);
    loadcmp = (compiler_loaded ? getInFrame(loadcmpSymbol, ns, FALSE) : R_UnboundValue);


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
    SEXP ofile;


    int to = ((local) ? N : (1 + asInteger(eval(expr__toplevel_context_number, R_EmptyEnv))));


/* the number of objects protected in each iteration (that must be unprotected
   before moving to the next iteration) */
#define nprotect_loop 1


    for (iwhich[0] = N; iwhich[0] >= to; iwhich[0]--, UNPROTECT(nprotect_loop)) {
        frame = eval(getframe, rho);
        // PROTECT(frame);
        function = eval(getfunction, rho);
        PROTECT(function);


        if (identical(function, source)) {
            if (local)
                error("%s cannot be called within %s()",
                      name, EncodeChar(PRINTNAME(sourceSymbol)));
            if (!existsInFrame(frame, thispathdoneSymbol)) {
                ofile = findVarInFrame(frame, ofileSymbol);
                if (ofile == R_UnboundValue) continue;
                if (TYPEOF(ofile) == PROMSXP) {
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                checkfile(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ ofileSymbol,
                    /* ofile                  */ ofile,
                    /* frame                  */ frame,
                    /* check_not_directory    */ FALSE,
                    /* forcepromise           */ FALSE,
                    /* assign_returnvalue     */ FALSE,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ findVarInFrame(frame, owdSymbol),
                    /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* ofilearg               */ NULL,
                    /* character_only         */ FALSE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ TRUE,
                    /* allow_stdin            */ TRUE,
                    /* allow_url              */ TRUE,
                    /* allow_file_uri         */ TRUE,
                    /* allow_unz              */ TRUE,
                    /* allow_pipe             */ TRUE,
                    /* allow_terminal         */ TRUE,
                    /* allow_textConnection   */ TRUE,
                    /* allow_rawConnection    */ TRUE,
                    /* allow_sockconn         */ TRUE,
                    /* allow_servsockconn     */ TRUE,
                    /* allow_customConnection */ TRUE,
                    /* ignore_blank_string    */ FALSE,
                    /* ignore_clipboard       */ FALSE,
                    /* ignore_stdin           */ FALSE,
                    /* ignore_url             */ FALSE,
                    /* ignore_file_uri        */ FALSE
                );
            }


#define returnfile(character_only, file_only, which, fun_name) \
            thispathofile = findVarInFrame(frame, thispathofileSymbol);\
            /* don't check right away that this is missing, in case another\
               error must be thrown or the frame number is requested */\
            if (!file_only) {                                  \
                /* if file_only is TRUE, thispathofile cannot be NULL */\
                if (thispathofile == R_NilValue) continue;     \
            }                                                  \
            /* if character_only is TRUE, there cannot be a delayed error */\
            if (!character_only &&                             \
                (thispatherror = findVarInFrame(frame, thispatherrorSymbol)) != R_UnboundValue)\
            {                                                  \
                if (for_msg) {                                 \
                    if (contents) {                            \
                        returnthis = ScalarString(NA_STRING);  \
                    } else {                                   \
                        thispathformsg = findVarInFrame(frame, thispathformsgSymbol);\
                        if (thispathformsg == R_UnboundValue)  \
                            error(_("object '%s' not found"), EncodeChar(PRINTNAME(thispathformsgSymbol)));\
                        returnthis = thispathformsg;           \
                    }                                          \
                }                                              \
                else if (get_frame_number) {                   \
                    if (existsInFrame(frame, thispathassocwfileSymbol))\
                        returnthis = (which);                  \
                    else                                       \
                        returnthis = ScalarInteger(NA_INTEGER);\
                }                                              \
                else {                                         \
                    thispatherror = duplicate(thispatherror);  \
                    PROTECT(thispatherror);                    \
                    SET_VECTOR_ELT(thispatherror, 1, getCurrentCall(rho));\
                    stop(thispatherror);                       \
                    UNPROTECT(1);  /* thispatherror */         \
                    /* should not reach here */                \
                    returnthis = R_NilValue;                   \
                }                                              \
            }                                                  \
            else if (get_frame_number)                         \
                returnthis = (which);                          \
            else if (thispathofile == R_UnboundValue)          \
                error(_("object '%s' not found"), EncodeChar(PRINTNAME(thispathofileSymbol)));\
            else if (original == TRUE)                         \
                returnthis = thispathofile;                    \
            else {                                             \
                thispathfile = findVarInFrame(frame, thispathfileSymbol);\
                if (thispathfile == R_UnboundValue)            \
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(thispathfileSymbol)));\
                if (TYPEOF(thispathfile) != PROMSXP)           \
                    error("invalid '%s', is not a promise; should never happen, please report!", EncodeChar(PRINTNAME(thispathfileSymbol)));\
                if (PRVALUE(thispathfile) == R_UnboundValue) { \
                    if (original || for_msg)                   \
                        returnthis = thispathofile;            \
                    else {                                     \
                        if (PRSEEN(thispathfile)) {            \
                            if (PRSEEN(thispathfile) == 1) {}  \
                            else SET_PRSEEN(thispathfile, 0);  \
                        }                                      \
                        returnthis = eval(thispathfile, R_EmptyEnv);\
                    }                                          \
                }                                              \
                else returnthis = PRVALUE(thispathfile);       \
            }                                                  \
            if (verbose) Rprintf("Source: call to function %s\n", fun_name);\
            UNPROTECT(nprotect_loop + nprotect);               \
            return returnthis


            returnfile(
                /* character_only         */ FALSE,
                /* file_only              */ FALSE,
                /* which                  */ which,
                /* fun_name               */ "source"
            );
        }


        else if (identical(function, sys_source)) {
            if (local)
                error("%s cannot be called within %s()",
                      name, EncodeChar(PRINTNAME(sys_sourceSymbol)));
            if (!existsInFrame(frame, thispathdoneSymbol)) {
                ofile = findVarInFrame(frame, fileSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(fileSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                checkfile(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileSymbol,
                    /* ofile                  */ ofile,
                    /* frame                  */ frame,
                    /* check_not_directory    */ FALSE,
                    /* forcepromise           */ FALSE,
                    /* assign_returnvalue     */ FALSE,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ findVarInFrame(frame, owdSymbol),
                    /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* ofilearg               */ NULL,
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* allow_unz              */ FALSE,
                    /* allow_pipe             */ FALSE,
                    /* allow_terminal         */ FALSE,
                    /* allow_textConnection   */ FALSE,
                    /* allow_rawConnection    */ FALSE,
                    /* allow_sockconn         */ FALSE,
                    /* allow_servsockconn     */ FALSE,
                    /* allow_customConnection */ FALSE,
                    /* ignore_blank_string    */ FALSE,
                    /* ignore_clipboard       */ FALSE,
                    /* ignore_stdin           */ FALSE,
                    /* ignore_url             */ FALSE,
                    /* ignore_file_uri        */ FALSE
                );
            }
            returnfile(
                /* character_only         */ TRUE,
                /* file_only              */ TRUE,
                /* which                  */ which,
                /* fun_name               */ "sys.source"
            );
        }


        else if (has_tools_rstudio && identical(function, debugSource)) {
            if (local)
                error("%s cannot be called within %s() in RStudio",
                      name, EncodeChar(PRINTNAME(debugSourceSymbol)));
            if (!existsInFrame(frame, thispathdoneSymbol)) {
                ofile = findVarInFrame(frame, fileNameSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(fileNameSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                checkfile(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileNameSymbol,
                    /* ofile                  */ ofile,
                    /* frame                  */ frame,
                    /* check_not_directory    */ FALSE,
                    /* forcepromise           */ FALSE,
                    /* assign_returnvalue     */ FALSE,
                    /* maybe_chdir            */ FALSE,
                    /* getowd                 */ NULL,
                    /* hasowd                 */ FALSE,
                    /* ofilearg               */ NULL,
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ TRUE,
                    /* allow_blank_string     */ TRUE,
                    /* allow_clipboard        */ TRUE,
                    /* allow_stdin            */ TRUE,
                    /* allow_url              */ TRUE,
                    /* allow_file_uri         */ TRUE,
                    /* allow_unz              */ FALSE,
                    /* allow_pipe             */ FALSE,
                    /* allow_terminal         */ FALSE,
                    /* allow_textConnection   */ FALSE,
                    /* allow_rawConnection    */ FALSE,
                    /* allow_sockconn         */ FALSE,
                    /* allow_servsockconn     */ FALSE,
                    /* allow_customConnection */ FALSE,
                    /* ignore_blank_string    */ FALSE,
                    /* ignore_clipboard       */ FALSE,
                    /* ignore_stdin           */ FALSE,
                    /* ignore_url             */ FALSE,
                    /* ignore_file_uri        */ FALSE
                );
            }
            returnfile(
                /* character_only         */ TRUE,
                /* file_only              */ FALSE,
                /* which                  */ which,
                /* fun_name               */ "debugSource in RStudio"
            );
        }


        else if (testthat_loaded && identical(function, source_file)) {
            if (local)
                error("%s cannot be called within %s() from package %s",
                      name, EncodeChar(PRINTNAME(source_fileSymbol)), EncodeChar(PRINTNAME(testthatSymbol)));
            if (!existsInFrame(frame, thispathdoneSymbol)) {
                ofile = findVarInFrame(frame, pathSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(pathSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                int ignore_all = asLogical(eval(expr_testthat_source_file_uses_brio_read_lines, R_EmptyEnv));
                checkfile(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ pathSymbol,
                    /* ofile                  */ ofile,
                    /* frame                  */ frame,
                    /* check_not_directory    */ FALSE,
                    /* forcepromise           */ FALSE,
                    /* assign_returnvalue     */ FALSE,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ findVarInFrame(frame, old_dirSymbol),
                    /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* ofilearg               */ NULL,
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* allow_unz              */ FALSE,
                    /* allow_pipe             */ FALSE,
                    /* allow_terminal         */ FALSE,
                    /* allow_textConnection   */ FALSE,
                    /* allow_rawConnection    */ FALSE,
                    /* allow_sockconn         */ FALSE,
                    /* allow_servsockconn     */ FALSE,
                    /* allow_customConnection */ FALSE,
                    /* ignore_blank_string    */ ignore_all,
                    /* ignore_clipboard       */ ignore_all,
                    /* ignore_stdin           */ ignore_all,
                    /* ignore_url             */ ignore_all,
                    /* ignore_file_uri        */ ignore_all
                );
            }
            returnfile(
                /* character_only         */ TRUE,
                /* file_only              */ TRUE,
                /* which                  */ which,
                /* fun_name               */ "source_file from package testthat"
            );
        }


        else if (knitr_loaded && identical(function, knit)) {
            if (local)
                error("%s cannot be called within %s() from package %s",
                      name, EncodeChar(PRINTNAME(knitSymbol)), EncodeChar(PRINTNAME(knitrSymbol)));
            if (!existsInFrame(frame, thispathdoneSymbol)) {
                if (!existsInFrame(frame, oenvirSymbol)) continue;
                int missing_input = asLogical(eval(expr_missing_input, frame));
                if (missing_input) {
                    assign_null(frame);
                    continue;
                }
                ofile = getInFrame(inputSymbol, frame, FALSE);
                checkfile(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ inputSymbol,
                    /* ofile                  */ ofile,
                    /* frame                  */ frame,
                    /* check_not_directory    */ FALSE,
                    /* forcepromise           */ FALSE,
                    /* assign_returnvalue     */ FALSE,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ eval(expr_knitr_output_dir, R_EmptyEnv),
                    /* hasowd                 */ ((owd) != R_NilValue),
                    /* ofilearg               */ NULL,
                    /* character_only         */ FALSE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ TRUE,
                    /* allow_stdin            */ TRUE,
                    /* allow_url              */ TRUE,
                    /* allow_file_uri         */ TRUE,
                    /* allow_unz              */ TRUE,
                    /* allow_pipe             */ TRUE,
                    /* allow_terminal         */ TRUE,
                    /* allow_textConnection   */ TRUE,
                    /* allow_rawConnection    */ TRUE,
                    /* allow_sockconn         */ TRUE,
                    /* allow_servsockconn     */ TRUE,
                    /* allow_customConnection */ TRUE,
                    /* ignore_blank_string    */ FALSE,
                    /* ignore_clipboard       */ FALSE,
                    /* ignore_stdin           */ FALSE,
                    /* ignore_url             */ FALSE,
                    /* ignore_file_uri        */ FALSE
                );
            }
            returnfile(
                /* character_only         */ FALSE,
                /* file_only              */ FALSE,
                /* which                  */ which,
                /* fun_name               */ "knit from package knitr"
            );
        }


        else if (identical(function, wrap_source)) {
            if (local)
                error("%s cannot be called within %s() from package %s",
                      name, EncodeChar(PRINTNAME(wrap_sourceSymbol)), "this.path");
            if (!existsInFrame(frame, thispathdoneSymbol)) continue;
            returnfile(
                /* character_only         */ FALSE,
                /* file_only              */ FALSE,
                /* which                  */ getInFrame(thispathnSymbol, frame, FALSE),
                /* fun_name               */ "wrap.source from package this.path"
            );
        }


        else if (box_loaded && identical(function, load_from_source)) {
            if (local)
                error("%s cannot be called within %s() from package %s",
                      name, EncodeChar(PRINTNAME(load_from_sourceSymbol)), EncodeChar(PRINTNAME(boxSymbol)));
            if (!existsInFrame(frame, thispathdoneSymbol)) {
                SEXP ofile = eval(expr_info_dollar_source_path, frame);
                PROTECT(ofile);
                checkfile(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ info_source_pathSymbol,
                    /* ofile                  */ ofile,
                    /* frame                  */ frame,
                    /* check_not_directory    */ FALSE,
                    /* forcepromise           */ TRUE,
                    /* assign_returnvalue     */ FALSE,
                    /* maybe_chdir            */ FALSE,
                    /* getowd                 */ NULL,
                    /* hasowd                 */ FALSE,
                    /* ofilearg               */ NULL,
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* allow_unz              */ FALSE,
                    /* allow_pipe             */ FALSE,
                    /* allow_terminal         */ FALSE,
                    /* allow_textConnection   */ FALSE,
                    /* allow_rawConnection    */ FALSE,
                    /* allow_sockconn         */ FALSE,
                    /* allow_servsockconn     */ FALSE,
                    /* allow_customConnection */ FALSE,
                    /* ignore_blank_string    */ FALSE,
                    /* ignore_clipboard       */ FALSE,
                    /* ignore_stdin           */ FALSE,
                    /* ignore_url             */ FALSE,
                    /* ignore_file_uri        */ FALSE
                );
                UNPROTECT(1);  /* ofile */
            }
            returnfile(
                /* character_only         */ TRUE,
                /* file_only              */ TRUE,
                /* which                  */ which,
                /* fun_name               */ "load_from_source from package box"
            );
        }


        else if (compiler_loaded && identical(function, loadcmp)) {
            /* much the same as sys.source() */
            if (local)
                error("%s cannot be called within %s() from package %s",
                      name, EncodeChar(PRINTNAME(loadcmpSymbol)), EncodeChar(PRINTNAME(compilerSymbol)));
            if (!existsInFrame(frame, thispathdoneSymbol)) {
                ofile = findVarInFrame(frame, fileSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(fileSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    /* if ofile is a promise already under evaluation */
                    if (PRSEEN(ofile) == 1) continue;
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                checkfile(
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ fileSymbol,
                    /* ofile                  */ ofile,
                    /* frame                  */ frame,
                    /* check_not_directory    */ FALSE,
                    /* forcepromise           */ FALSE,
                    /* assign_returnvalue     */ FALSE,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ findVarInFrame(frame, owdSymbol),
                    /* hasowd                 */ ((owd) != R_UnboundValue),
                    /* ofilearg               */ NULL,
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ FALSE,
                    /* allow_clipboard        */ FALSE,
                    /* allow_stdin            */ FALSE,
                    /* allow_url              */ FALSE,
                    /* allow_file_uri         */ FALSE,
                    /* allow_unz              */ FALSE,
                    /* allow_pipe             */ FALSE,
                    /* allow_terminal         */ FALSE,
                    /* allow_textConnection   */ FALSE,
                    /* allow_rawConnection    */ FALSE,
                    /* allow_sockconn         */ FALSE,
                    /* allow_servsockconn     */ FALSE,
                    /* allow_customConnection */ FALSE,
                    /* ignore_blank_string    */ FALSE,
                    /* ignore_clipboard       */ FALSE,
                    /* ignore_stdin           */ FALSE,
                    /* ignore_url             */ FALSE,
                    /* ignore_file_uri        */ FALSE
                );
            }
            returnfile(
                /* character_only         */ TRUE,
                /* file_only              */ TRUE,
                /* which                  */ which,
                /* fun_name               */ "loadcmp from package compiler"
            );
        }


        else if ((setsyspathwashere = findVarInFrame(frame, setsyspathwashereSymbol)) != R_UnboundValue) {
            if (setsyspathwashere == R_MissingArg) continue;
            SEXP thispathn = findVarInFrame(frame, thispathnSymbol);
            if (TYPEOF(thispathn) != INTSXP || LENGTH(thispathn) != 1)
                error(_("invalid '%s' value"), EncodeChar(PRINTNAME(thispathnSymbol)));
            /* this could happen with eval() or similar */
            if (iwhich[0] != INTEGER(thispathn)[0]) continue;
            returnfile(
                /* character_only         */ FALSE,
                /* file_only              */ FALSE,
                /* which                  */ thispathn,
                /* fun_name               */ CHAR(setsyspathwashere)
            );
        }
    }


    UNPROTECT(nprotect);


    if (local) {
        if (for_msg) return ScalarString(NA_STRING);
        const char *msg = "no associated path";
        SEXP call = getCurrentCall(rho);
        PROTECT(call);
        SEXP cond = thisPathNotExistsError(msg, call);
        PROTECT(cond);
        stop(cond);
        UNPROTECT(2);
        return R_NilValue;
    }


    toplevel;


#undef toplevel
#undef returnfile
}


SEXP syspath8(Rboolean verbose         , Rboolean original        ,
              Rboolean for_msg         , Rboolean contents        ,
              Rboolean local           , int N                    ,
              Rboolean get_frame_number, SEXP rho                 )
{
    SEXP value = _syspath(verbose         , original        , for_msg         ,
                          contents        , local           , N               ,
                          get_frame_number, rho             );
    if (!contents)
        return value;
    if (TYPEOF(value) == VECSXP) {
        if (XLENGTH(value) == 1) {
            PROTECT(value);
            SEXP names = getAttrib(value, R_NamesSymbol);
            if (TYPEOF(names) == STRSXP && XLENGTH(names) == 1 && STRING_ELT(names, 0) == mkChar("contents")) {
                /* do not need to protect */
                value = VECTOR_ELT(value, 0);
                if (TYPEOF(value) != STRSXP)
                    error("internal error; invalid '%s' value", "_syspath()");
                R_xlen_t n = XLENGTH(value) - 1;
                if (n) {
                    /* if the last line is a blank string, remove it */
                    if (STRING_ELT(value, n) == R_BlankString) {
                        SEXP tmp = allocVector(STRSXP, n);
                        PROTECT(tmp);
                        for (R_xlen_t i = 0; i < n; i++) {
                            SET_STRING_ELT(tmp, i, STRING_ELT(value, i));
                        }
                        value = tmp;
                        UNPROTECT(1); /* tmp */
                    }
                }
                else {
                    value = allocVector(STRSXP, 0);
                }
            }
            UNPROTECT(1); /* value */
        }
        return value;
    }
    if (TYPEOF(value) == STRSXP) {
        if (XLENGTH(value) != 1)
            error("internal error; invalid '%s' value", "_syspath()");
        if (STRING_ELT(value, 0) == NA_STRING)
            return R_NilValue;
        SEXP expr = LCONS(_getContentsSymbol, CONS(value, R_NilValue));
        PROTECT(expr);
        value = eval(expr, mynamespace);
        UNPROTECT(1);
        return value;
    }
    error("internal error; invalid '%s' value", "_syspath()");
    return R_NilValue;
}


SEXP syspath6(Rboolean verbose , Rboolean original, Rboolean for_msg ,
              Rboolean contents, Rboolean local   , SEXP rho         )
{
    return syspath8(verbose, original, for_msg, contents, local,
                    /* N */ NA_INTEGER, /* get_frame_number */ FALSE,  rho);
}


SEXP syspath3(Rboolean verbose, Rboolean local, SEXP rho)
{
    return syspath6(verbose, /* original */ FALSE, /* for_msg */ FALSE,
                    /* contents */ FALSE, local, rho);
}


SEXP syspath2(Rboolean local, SEXP rho)
{
    return syspath3(/* verbose */ FALSE, local, rho);
}


SEXP syspath1(SEXP rho)
{
    return syspath2(/* local */ FALSE, rho);
}


SEXP getframenumber(SEXP rho)
{
    return syspath8(/* verbose */ FALSE, /* original */ FALSE,
                    /* for_msg */ FALSE, /* contents */ FALSE,
                    /* local */ FALSE, /* N */ NA_INTEGER,
                    /* get_frame_number */ TRUE, rho);
}


SEXP do_syspath do_formals
{
    do_start_no_op("syspath", -1);


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
        errorcall(call, wrong_nargs_to_External(length(args), ".C_syspath", "0, 1, 2, or 5"));
        return R_NilValue;
    }


    check_arguments5(verbose, original, for_msg, contents, local);
    return syspath6(verbose, original, for_msg, contents, local, rho);
}


SEXP do_getframenumber do_formals
{
    do_start_no_call_op("getframenumber", 0);
    return getframenumber(rho);
}


#if R_version_at_least(3, 2, 0)
extern SEXP topenv(SEXP target, SEXP envir);
#else
SEXP topenv(SEXP target, SEXP envir)
{
    static SEXP topenvSymbol = NULL;
    if (topenvSymbol == NULL) {
        topenvSymbol = install("topenv");
    }
    SEXP expr = LCONS(topenvSymbol, CONS(envir, CONS(target, R_NilValue)));
    PROTECT(expr);
    SEXP value = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return value;
}
#endif


Rboolean IsModuleEnv(SEXP rho)
{
    if (TYPEOF(rho) == ENVSXP) {
        SEXP info = findVarInFrame(rho, ModuleSymbol);
        if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
            SEXP spec = findVarInFrame(info, specSymbol);
            if (spec != R_UnboundValue && TYPEOF(spec) == STRSXP && LENGTH(spec) > 0)
                return TRUE;
        }
    }
    return FALSE;
}


SEXP _envpath(Rboolean verbose, Rboolean original, Rboolean for_msg,
              Rboolean contents, SEXP target, SEXP envir, Rboolean unbound_ok,
              SEXP rho)
{
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


    SEXP path;


    if (env == R_GlobalEnv ||
        env == R_BaseEnv || env == R_BaseNamespace ||
        R_IsPackageEnv(env) || R_IsNamespaceEnv(env));
    else if (inherits(env, "box$ns")) {
        SEXP thispathfile = getAttrib(env, original ? thispathofileSymbol : thispathfileSymbol);
        if (isString(thispathfile)) {
            if (verbose) Rprintf("Source: source path of a {box} namespace\n");
            return thispathfile;
        }
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
#ifdef _WIN32
                                is_abs_path_windows(CHAR(STRING_ELT(path, 0)))
#else
                                is_abs_path_unix(CHAR(STRING_ELT(path, 0)))
#endif
                            )
                            {
                                SEXP thispathofile = ScalarString(STRING_ELT(path, 0));
                                setAttrib(env, thispathofileSymbol, thispathofile);


                                SEXP expr = LCONS(_normalizePathSymbol,
                                                  CONS(thispathofile, R_NilValue));
                                PROTECT(expr);
                                thispathfile = eval(expr, mynamespace);
                                UNPROTECT(1);
                                setAttrib(env, thispathfileSymbol, thispathfile);


                                if (verbose) Rprintf("Source: source path of a {box} namespace\n");
                                return (original ? thispathofile : thispathfile);
                            }
                        }
                    }
                }
            }
        }
        error("invalid {box} namespace without an associated path");
    }
    else if (IsModuleEnv(env)) {
        SEXP info = findVarInFrame(env, ModuleSymbol);
        if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
            path = findVarInFrame(info, pathSymbol);
            if (path != R_UnboundValue && TYPEOF(path) == STRSXP && XLENGTH(path) == 1) {
                if (verbose) Rprintf("Source: path of a {module} namespace\n");
                return ScalarString(STRING_ELT(path, 0));
            }
        }
    }
    else if (isString(path = getAttrib(env, original ? thispathofileSymbol : thispathfileSymbol))) {
        if (verbose) Rprintf("Source: attr(,\"path\") of the top level environment\n");
        return path;
    }
    else if (isString(path = getAttrib(env, pathSymbol)) && XLENGTH(path) > 0)
    {
        const char *str = CHAR(STRING_ELT(path, 0));
        SEXP normalize = NULL;
        if (
#ifdef _WIN32
             is_abs_path_windows(str)
#else
             is_abs_path_unix(str)
#endif
        )
            normalize = _normalizeNotDirectorySymbol;
#define looks_like_url(_STR_, _SCHEME_, _NCHAR_SCHEME_)        \
        (                                                      \
        !strncmp((_STR_), (_SCHEME_), (_NCHAR_SCHEME_)) &&     \
        (_STR_)[(_NCHAR_SCHEME_)] != '\0'               &&     \
        (_STR_)[(_NCHAR_SCHEME_)] != '/'                       \
        )
        else if (looks_like_url(str, "https://", 8) ||
                 looks_like_url(str, "http://" , 7) ||
                 looks_like_url(str, "ftp://"  , 6) ||
                 looks_like_url(str, "ftps://" , 7)
        )
            normalize = _normalizeURL_1Symbol;
        if (normalize) {
            SEXP thispathofile = ScalarString(STRING_ELT(path, 0));
            setAttrib(env, thispathofileSymbol, thispathofile);


            SEXP thispathfile;
            SEXP expr = LCONS(normalize, CONS(thispathofile, R_NilValue));
            PROTECT(expr);
            thispathfile = eval(expr, mynamespace);
            UNPROTECT(1);
            setAttrib(env, thispathfileSymbol, thispathfile);


            if (verbose) Rprintf("Source: attr(,\"path\") of the top level environment\n");
            return (original ? thispathofile : thispathfile);
        }
    }


    if (unbound_ok)
        return R_UnboundValue;
    else if (for_msg)
        return ScalarString(NA_STRING);
    else {
        const char *msg = "no associated path";
        SEXP call = getCurrentCall(rho);
        PROTECT(call);
        SEXP cond = thisPathNotExistsError(msg, call);
        PROTECT(cond);
        stop(cond);
        UNPROTECT(2);
        return R_NilValue;
    }
}


SEXP envpath8(Rboolean verbose, Rboolean original, Rboolean for_msg,
              Rboolean contents, SEXP target, SEXP envir, Rboolean unbound_ok,
              SEXP rho)
{
    SEXP value = _envpath(verbose   , original  , for_msg   , contents,
                          target    , envir     , unbound_ok, rho       );
    if (unbound_ok && value == R_UnboundValue)
        return value;
    if (!contents)
        return value;
    if (TYPEOF(value) == STRSXP) {
        if (XLENGTH(value) != 1)
            error("internal error; invalid '%s' value", "_envpath()");
        if (STRING_ELT(value, 0) == NA_STRING)
            return R_NilValue;
        SEXP expr = LCONS(_getContentsSymbol, CONS(value, R_NilValue));
        PROTECT(expr);
        value = eval(expr, mynamespace);
        UNPROTECT(1);
        return value;
    }
    error("internal error; invalid '%s' value", "_envpath()");
    return R_NilValue;
}


SEXP envpath7(Rboolean verbose, Rboolean original, Rboolean for_msg,
              Rboolean contents, SEXP target, SEXP envir, SEXP rho)
{
    return envpath8(verbose, original, for_msg, contents, target, envir,
                    /* unbound_ok */ FALSE, rho);
}


SEXP envpath4(Rboolean verbose, SEXP target, SEXP envir, SEXP rho)
{
    return envpath7(verbose, /* original */ FALSE, /* for_msg */ FALSE,
                    /* contents */ FALSE, target, envir, rho);
}


SEXP envpath3(SEXP target, SEXP envir, SEXP rho)
{
    return envpath4(/* verbose */ FALSE, target, envir, rho);
}


SEXP envpath1(SEXP rho)
{
    return envpath3(/* target */ NULL, /* envir */ NULL, rho);
}


SEXP do_envpath do_formals
{
    do_start_no_op("envpath", -1);


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
        errorcall(call, wrong_nargs_to_External(length(args), ".C_envpath", "0, 2, 3, or 6"));
        return R_NilValue;
    }


    check_arguments4(verbose, original, for_msg, contents);
    return envpath7(verbose, original, for_msg, contents, target, envir, rho);
}


SEXP _srcpath(Rboolean verbose, Rboolean original, Rboolean for_msg,
              Rboolean contents, SEXP srcfile, Rboolean *gave_contents,
              Rboolean get_lineno, Rboolean unbound_ok, SEXP rho)
{
    *gave_contents = FALSE;


    int nprotect = 0;


    SEXP x, srcref, wholeSrcref;
    x = srcfile;
    srcfile = NULL;


    if (x == NULL) {
        x = PROTECT(eval(expr_sys_call, rho)); nprotect++;
        // {
        //     Rprintf("\n> sys.call()\n");
        //     SEXP expr;
        //     PROTECT_INDEX indx;
        //     PROTECT_WITH_INDEX(expr = makeEVPROMISE(x, x), &indx);
        //     REPROTECT(expr = CONS(expr, R_NilValue), indx);
        //     REPROTECT(expr = LCONS(getFromBase(install("print")), expr), indx);
        //     eval(expr, rho);
        //     UNPROTECT(1);
        // }
        // {
        //     Rprintf("\n> sys.call()\n");
        //     SEXP expr;
        //     PROTECT_INDEX indx;
        //     PROTECT_WITH_INDEX(expr = duplicate(x), &indx);
        //     setAttrib(expr, srcrefSymbol, R_NilValue);
        //     REPROTECT(expr = makeEVPROMISE(expr, expr), indx);
        //     REPROTECT(expr = CONS(expr, R_NilValue), indx);
        //     REPROTECT(expr = LCONS(getFromBase(install("print")), expr), indx);
        //     eval(expr, rho);
        //     UNPROTECT(1);
        // }
    }


    SEXP returnthis = NULL;


    if (get_lineno) {
        switch (TYPEOF(x)) {
        case SYMSXP:
        case CLOSXP:
            srcref = PROTECT(getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == INTSXP) {


#define return_lineno                                          \
                returnthis = ScalarInteger(INTEGER(srcref)[0]);\
                UNPROTECT(nprotect);                           \
                return returnthis


                return_lineno;
            }
            break;
        case LANGSXP:
            wholeSrcref = PROTECT(getAttrib(x, wholeSrcrefSymbol)); nprotect++;
            if (TYPEOF(wholeSrcref) == INTSXP) {
                srcref = wholeSrcref;
                return_lineno;
            }
            srcref = PROTECT(getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == INTSXP) {
                return_lineno;
            }
            if (TYPEOF(srcref) == VECSXP && LENGTH(srcref) &&
                TYPEOF(srcref = VECTOR_ELT(srcref, 0)) == INTSXP)
            {
                return_lineno;
            }
            break;
        case EXPRSXP:
            wholeSrcref = PROTECT(getAttrib(x, wholeSrcrefSymbol)); nprotect++;
            if (TYPEOF(wholeSrcref) == INTSXP) {
                srcref = wholeSrcref;
                return_lineno;
            }
            srcref = PROTECT(getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == VECSXP && LENGTH(srcref) &&
                TYPEOF(srcref = VECTOR_ELT(srcref, 0)) == INTSXP)
            {
                return_lineno;
            }
            break;
        case INTSXP:
            srcref = x;
            if (LENGTH(srcref) == 8) {
                return_lineno;
            }
            break;
        }
        UNPROTECT(nprotect);
        return ScalarInteger(NA_INTEGER);
    }


    switch (TYPEOF(x)) {
    case SYMSXP:
    case CLOSXP:
        srcref = PROTECT(getAttrib(x, srcrefSymbol)); nprotect++;
        if (TYPEOF(srcref) == INTSXP) {


#define set_srcfile                                            \
            srcfile = PROTECT(getAttrib(srcref, srcfileSymbol)); nprotect++;\
            if (TYPEOF(srcfile) != ENVSXP) srcfile = NULL


            set_srcfile;
        }
        break;
    case LANGSXP:
        srcfile = PROTECT(getAttrib(x, srcfileSymbol)); nprotect++;
        if (TYPEOF(srcfile) == ENVSXP);
        else {
            srcfile = NULL;
            srcref = PROTECT(getAttrib(x, srcrefSymbol)); nprotect++;
            if (TYPEOF(srcref) == INTSXP) {
                set_srcfile;
            }
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
                set_srcfile;
            }
        }
        break;
    case INTSXP:
        srcref = x;
        if (LENGTH(srcref) == 8) {
            set_srcfile;
        }
        break;
    case ENVSXP:
        if (inherits(x, "srcfile")) srcfile = x;
        break;
    }


    SEXP returnvalue;


    SEXP thispathofile, thispathfile;


    SEXP ofile;


    SEXP srcfile_original;


    Rboolean _is_srcfilecopy = NA_LOGICAL;
#define is_srcfilecopy  ((_is_srcfilecopy == NA_LOGICAL) ?     \
                         (_is_srcfilecopy = inherits(srcfile, "srcfilecopy")) :\
                         (_is_srcfilecopy))


    if (srcfile) {
        if (!existsInFrame(srcfile, thispathdoneSymbol)) {
            if (is_srcfilecopy && asLogical(findVarInFrame(srcfile, isFileSymbol)) != TRUE) {
                assign_null(srcfile);
            } else {
                ofile = findVarInFrame(srcfile, filenameSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(filenameSymbol)));
                checkfile(
                    /* call                   */ R_NilValue,
                    /* sym                    */ filenameSymbol,
                    /* ofile                  */ ofile,
                    /* frame                  */ srcfile,
                    /* check_not_directory    */ TRUE,
                    /* forcepromise           */ FALSE,
                    /* assign_returnvalue     */ FALSE,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ findVarInFrame(srcfile, wdSymbol),
                    /* hasowd                 */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* ofilearg               */ NULL,
                    /* character_only         */ TRUE,
                    /* conv2utf8              */ FALSE,
                    /* allow_blank_string     */ TRUE,
                    /* allow_clipboard        */ TRUE,
                    /* allow_stdin            */ TRUE,
                    /* allow_url              */ TRUE,
                    /* allow_file_uri         */ TRUE,
                    /* allow_unz              */ FALSE,
                    /* allow_pipe             */ FALSE,
                    /* allow_terminal         */ FALSE,
                    /* allow_textConnection   */ FALSE,
                    /* allow_rawConnection    */ FALSE,
                    /* allow_sockconn         */ FALSE,
                    /* allow_servsockconn     */ FALSE,
                    /* allow_customConnection */ FALSE,
                    /* ignore_blank_string    */ FALSE,
                    /* ignore_clipboard       */ FALSE,
                    /* ignore_stdin           */ FALSE,
                    /* ignore_url             */ FALSE,
                    /* ignore_file_uri        */ FALSE
                );
            }
        }
        thispathofile = findVarInFrame(srcfile, thispathofileSymbol);
        if (thispathofile != R_NilValue) {
            if (thispathofile == R_UnboundValue)
                error(_("object '%s' not found"), EncodeChar(PRINTNAME(thispathofileSymbol)));
            else if (contents && (is_srcfilecopy || (inherits(srcfile, "srcfilealias") &&
                     inherits(srcfile_original = findVarInFrame(srcfile, originalSymbol), "srcfilecopy") &&
                     (srcfile = srcfile_original))))
            {
                SEXP tmp = findVarInFrame(srcfile, fixedNewlinesSymbol);
                if (tmp == R_UnboundValue || tmp == R_NilValue) {
                    SEXP expr = LCONS(_fixNewlinesSymbol, CONS(srcfile, R_NilValue));
                    PROTECT(expr);
                    eval(expr, mynamespace);
                    set_R_Visible(TRUE);
                    UNPROTECT(1);
                }
                returnthis = findVarInFrame(srcfile, linesSymbol);
                if (returnthis == R_UnboundValue)
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(linesSymbol)));
                if (TYPEOF(returnthis) != STRSXP)
                    error(_("object '%s' of mode '%s' was not found"),
                          EncodeChar(PRINTNAME(linesSymbol)), "character");
                *gave_contents = TRUE;
            }
            else if (original == TRUE)
                returnthis = thispathofile;
            else {
                thispathfile = findVarInFrame(srcfile, thispathfileSymbol);
                if (thispathfile == R_UnboundValue)
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(thispathfileSymbol)));
                if (TYPEOF(thispathfile) != PROMSXP)
                    error("invalid '%s', is not a promise; should never happen, please report!", EncodeChar(PRINTNAME(thispathfileSymbol)));
                if (PRVALUE(thispathfile) == R_UnboundValue) {
                    if (original || for_msg)
                        returnthis = thispathofile;
                    else {
                        if (PRSEEN(thispathfile)) {
                            if (PRSEEN(thispathfile) == 1) {}
                            else SET_PRSEEN(thispathfile, 0);
                        }
                        returnthis = eval(thispathfile, R_EmptyEnv);
                    }
                }
                else returnthis = PRVALUE(thispathfile);
            }
            if (verbose) Rprintf("Source: attr(,\"srcref\")attr(,\"srcfile\") of current expression\n");
            UNPROTECT(nprotect);
            return returnthis;
        }
    }


    UNPROTECT(nprotect);


    if (unbound_ok)
        return R_UnboundValue;
    else if (for_msg)
        return ScalarString(NA_STRING);
    else {
        if (for_msg) return ScalarString(NA_STRING);
        const char *msg = "no associated path";
        SEXP call = getCurrentCall(rho);
        PROTECT(call);
        SEXP cond = thisPathNotExistsError(msg, call);
        PROTECT(cond);
        stop(cond);
        UNPROTECT(2);
        return R_NilValue;
    }
}


SEXP srcpath8(Rboolean verbose, Rboolean original, Rboolean for_msg,
              Rboolean contents, SEXP srcfile, Rboolean lineno,
              Rboolean unbound_ok, SEXP rho)
{
    Rboolean gave_contents;
    SEXP value = _srcpath(verbose       , original      , for_msg       ,
                          contents      , srcfile       , &gave_contents,
                          lineno        , unbound_ok    , rho           );
    if (unbound_ok && value == R_UnboundValue)
        return value;
    if (lineno)
        return value;
    if (!contents)
        return value;
    if (gave_contents)
        return value;
    if (XLENGTH(value) != 1)
        error("internal error; invalid '%s' value", "_envpath()");
    if (STRING_ELT(value, 0) == NA_STRING)
        return R_NilValue;
    SEXP expr = LCONS(_getContentsSymbol, CONS(value, R_NilValue));
    PROTECT(expr);
    value = eval(expr, mynamespace);
    UNPROTECT(1);
    return value;
}


SEXP srcpath6(Rboolean verbose, Rboolean original, Rboolean for_msg,
              Rboolean contents, SEXP srcfile, SEXP rho)
{
    return srcpath8(verbose, original, for_msg, contents, srcfile,
                    /* lineno */ FALSE, /* unbound_ok */ FALSE, rho);
}


SEXP srcpath3(Rboolean verbose, SEXP srcfile, SEXP rho)
{
    return srcpath6(verbose, /* original */ FALSE, /* for_msg */ FALSE,
                    /* contents */ FALSE, srcfile, rho);
}


SEXP srcpath2(SEXP srcfile, SEXP rho)
{
    return srcpath3(/* verbose */ FALSE, srcfile, rho);
}


SEXP srcpath1(SEXP rho)
{
    return srcpath2(/* srcfile */ NULL, rho);
}


SEXP do_srcpath do_formals
{
    do_start_no_op("srcpath", -1);


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
        errorcall(call, wrong_nargs_to_External(length(args), ".C_srcpath", "0, 1, 2, or 5"));
        return R_NilValue;
    }


    check_arguments4(verbose, original, for_msg, contents);
    return srcpath6(verbose, original, for_msg, contents, srcfile, rho);
}


SEXP do_srclineno do_formals
{
    do_start_no_op("srclineno", -1);


    SEXP srcfile = NULL;


    switch (length(args)) {
    case 0:
        break;
    case 1:
        srcfile = CAR(args); args = CDR(args);
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), ".C_srclineno", "0 or 1"));
        return R_NilValue;
    }


    return srcpath8(/* verbose */ FALSE, /* original */ FALSE,
                    /* for_msg */ FALSE, /* contents */ FALSE, srcfile,
                    /* lineno */ TRUE, /* unbound_ok */ FALSE, rho);
}


SEXP do_thispath do_formals
{
    do_start_no_op("thispath", -1);


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
        errorcall(call, wrong_nargs_to_External(length(args), ".C_thispath", "0, 4, 5, or 8"));
        return R_NilValue;
    }


    check_arguments5(verbose, original, for_msg, contents, local);


    if (local) return syspath6(verbose, original, for_msg, contents, local, rho);


    SEXP value = srcpath8(verbose, original, for_msg, contents, srcfile,
                          /* lineno */ FALSE, /* unbound_ok */ TRUE, rho);
    if (value == R_UnboundValue) {
        value = envpath8(verbose, original, for_msg, contents, target, envir,
                         /* unbound_ok */ TRUE, rho);
        if (value == R_UnboundValue) {
            value = syspath6(verbose, original, for_msg, contents, local, rho);
        }
    }
    return value;
}


SEXP do_istrue do_formals
{
    do_start_no_call_op_rho("istrue", 1);


    return ScalarLogical(asLogical(CAR(args)) == TRUE);
}


SEXP do_isfalse do_formals
{
    do_start_no_call_op_rho("isfalse", 1);


    return ScalarLogical(asLogical(CAR(args)) == FALSE);
}



SEXP do_asInteger do_formals
{
    do_start_no_call_op_rho("asInteger", 1);


    return ScalarInteger(asInteger(CAR(args)));
}



SEXP do_asIntegerGE0 do_formals
{
    do_start_no_call_op_rho("asIntegerGE0", 1);


    int value = asInteger(CAR(args));
    if (value == NA_INTEGER || value < 0)
        error(_("invalid '%s' value"), "n");
    return ScalarInteger(value);
}
