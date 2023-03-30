#include "thispathdefn.h"
#include "drivewidth.h"


static R_INLINE int asFlag(SEXP x, const char *name)
{
    int val = asLogical(x);
    if (val == NA_LOGICAL)
        error(_("invalid '%s' value"), name);
    return val;
}


#define errorbody                                              \
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1 ||      \
        STRING_ELT(CAR(args), 0) == NA_STRING)                 \
    {                                                          \
        errorcall(call, _("invalid first argument"));          \
    }                                                          \
    const char *msg = translateChar(STRING_ELT(CAR(args), 0)); args = CDR(args);                                          \
    SEXP call2 = CAR(args); args = CDR(args);                  \
    ENSURE_NAMEDMAX(call2)


SEXP do_thispathunrecognizedconnectionclasserror do_formals
{
    do_start("thispathunrecognizedconnectionclasserror", 2);


    SEXP call2 = CAR(args); args = CDR(args);
    ENSURE_NAMEDMAX(call2);
#if defined(R_CONNECTIONS_VERSION_1)
    return thisPathUnrecognizedConnectionClassError(call2, R_GetConnection(CAR(args)));
#else
    return thisPathUnrecognizedConnectionClassError(call2, summaryconnection(CAR(args)));
#endif
}


SEXP do_thispathunrecognizedmannererror do_formals
{
    do_start("thispathunrecognizedmannererror", 1);


    op = CAR(args);
    ENSURE_NAMEDMAX(op);
    return thisPathUnrecognizedMannerError(op);
}


SEXP do_thispathnotimplementederror do_formals
{
    do_start("thispathnotimplementederror", 2);


    errorbody;
    return thisPathNotImplementedError(msg, call2);
}


SEXP do_thispathnotexistserror do_formals
{
    do_start("thispathnotexistserror", 2);


    errorbody;
    return thisPathNotExistsError(msg, call2);
}


SEXP do_thispathinzipfileerror do_formals
{
    do_start("thispathinzipfileerror", 2);


    SEXP call2 = CAR(args); args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1 ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        errorcall(call, "invalid second argument");
    }
    SEXP description = STRING_ELT(CAR(args), 0);
    return thisPathInZipFileError(call2, description);
}


SEXP do_thispathinaquaerror do_formals
{
    do_start("thispathinaquaerror", 1);


    return thisPathInAQUAError(lazy_duplicate(CAR(args)));
}


#undef errorbody


SEXP do_isclipboard do_formals
{
    do_start("isclipboard", 1);


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


SEXP do_thispath do_formals
{
    do_start("thispath", 5);


    SEXP returnthis = NULL;
    SEXP returnvalue;  /* this is never used */


    int verbose  = asFlag   (CAR(args), "verbose"); args = CDR(args);
    int original = asLogical(CAR(args));            args = CDR(args);
    int for_msg  = asFlag   (CAR(args), "for.msg"); args = CDR(args);
    int N        = asInteger(CAR(args));            args = CDR(args);
    if (N == NA_INTEGER || N < 0)
        error(_("invalid '%s' argument"), "N");
    int get_frame_number = asFlag(CAR(args), "get.frame.number"); args = CDR(args);


    if (get_frame_number && (original || for_msg)) {
        if (!for_msg)
            error("'%s' must be FALSE when '%s' is TRUE", "original", "get.frame.number");
        else if (!original)
            error("'%s' must be FALSE when '%s' is TRUE", "for.msg", "get.frame.number");
        else
            error("'%s' and '%s' must be FALSE when '%s' is TRUE", "original", "for.msg", "get.frame.number");
    }


    if (N <= 0) {


#define toplevel                                               \
        if (get_frame_number) return ScalarInteger(0);         \
        SEXP expr;                                             \
        if (for_msg)                                           \
            expr = lang4(this_path_toplevelSymbol, ScalarLogical(verbose), ScalarLogical(original), ScalarLogical(for_msg));\
        else if (original)                                     \
            expr = lang3(this_path_toplevelSymbol, ScalarLogical(verbose), ScalarLogical(original));\
        else if (verbose)                                      \
            expr = lang2(this_path_toplevelSymbol, ScalarLogical(verbose));\
        else                                                   \
            expr = lang1(this_path_toplevelSymbol);            \
        PROTECT(expr);                                         \
        returnthis = eval(expr, mynamespace);                  \
        UNPROTECT(1);                                          \
        return returnthis


        toplevel;
    }


    SEXP thispathofile ,
         thispathfile  ,
         thispathformsg,
         thispatherror ,
         insidesourcewashere;


    int nprotect = 0;


    init_tools_rstudio(FALSE);


    int testthat_loaded, knitr_loaded, box_loaded;


    SEXP source     = getInFrame(sourceSymbol    , R_BaseEnv, FALSE),
         sys_source = getInFrame(sys_sourceSymbol, R_BaseEnv, FALSE),
         debugSource,
         source_file,
         knit       ,
         wrap_source,
         load_from_source;


    debugSource = get_debugSource;
    source_file = get_source_file(testthat_loaded);
    knit        = get_knit(knitr_loaded);
    wrap_source = get_wrap_source,
    load_from_source = get_load_from_source(box_loaded);


    SEXP which = allocVector(INTSXP, 1);
    int *iwhich = INTEGER(which);
    SEXP getframe = lang2(getInFrame(sys_frameSymbol, R_BaseEnv, FALSE), which);
    PROTECT(getframe); nprotect++;
    SEXP getfunction = lang2(getInFrame(sys_functionSymbol, R_BaseEnv, FALSE), which);
    PROTECT(getfunction); nprotect++;


    SEXP frame, function;
    SEXP ofile;
    for (iwhich[0] = N; iwhich[0] >= 1; iwhich[0]--) {
        frame = eval(getframe, rho);
        // PROTECT(frame);
        function = eval(getfunction, rho);
        PROTECT(function);
/* the number of objects protected in each iteration (that must be unprotected
   before moving to the next iteration) */
#define nprotect_loop 1
        if (identical(function, source)) {
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                ofile = findVarInFrame(frame, ofileSymbol);
                if (ofile == R_UnboundValue) {
                    UNPROTECT(nprotect_loop);
                    continue;
                }
                if (TYPEOF(ofile) == PROMSXP) {
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                checkfile(
                    /* SEXP call                  = */ sys_call(which, rho),
                    /* SEXP sym                   = */ ofileSymbol,
                    /* SEXP ofile                 = */ ofile,
                    /* SEXP frame                 = */ frame,
                    /* int forcepromise           = */ FALSE,
                    /* int assign_returnvalue     = */ FALSE,
                    /* int maybe_chdir            = */ TRUE,
                    /* SEXP getowd                = */ findVarInFrame(frame, owdSymbol),
                    /* int hasowd                 = */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* int character_only         = */ FALSE,
                    /* int conv2utf8              = */ FALSE,
                    /* int allow_blank_string     = */ FALSE,
                    /* int allow_clipboard        = */ TRUE,
                    /* int allow_stdin            = */ TRUE,
                    /* int allow_url              = */ TRUE,
                    /* int allow_file_uri         = */ TRUE,
                    /* int allow_unz              = */ TRUE,
                    /* int allow_pipe             = */ TRUE,
                    /* int allow_terminal         = */ TRUE,
                    /* int allow_textConnection   = */ TRUE,
                    /* int allow_rawConnection    = */ TRUE,
                    /* int allow_sockconn         = */ TRUE,
                    /* int allow_servsockconn     = */ TRUE,
                    /* int allow_customConnection = */ TRUE,
                    /* int ignore_blank_string    = */ FALSE,
                    /* int ignore_clipboard       = */ FALSE,
                    /* int ignore_stdin           = */ FALSE,
                    /* int ignore_url             = */ FALSE,
                    /* int ignore_file_uri        = */ FALSE
                )
            }


#define returnfile(character_only, file_only, which,           \
    promise_must_be_forced, fun_name)                          \
            thispathofile = findVarInFrame(frame, thispathofileSymbol);\
            /* don't check right away that this is missing, in case another */\
            /* error must be thrown or the user requests the frame number   */\
            if (!file_only) {                                  \
                /* if file_only is TRUE, thispathofile cannot be NULL */\
                if (thispathofile == R_NilValue) {             \
                    UNPROTECT(nprotect_loop);                  \
                    continue;                                  \
                }                                              \
            }                                                  \
            if (!character_only) {                             \
                /* if character_only is TRUE, there cannot be a delayed error */\
                thispatherror = findVarInFrame(frame, thispatherrorSymbol);\
                /* if there is an error which needs to be thrown */\
                if (thispatherror != R_UnboundValue) {         \
                    if (for_msg) {                             \
                        thispathformsg = findVarInFrame(frame, thispathformsgSymbol);\
                        if (thispathformsg == R_UnboundValue)  \
                            error(_("object '%s' not found"), EncodeChar(PRINTNAME(thispathformsgSymbol)));\
                        UNPROTECT(nprotect + nprotect_loop);   \
                        return thispathformsg;                 \
                    }                                          \
                    else if (get_frame_number) {               \
                        UNPROTECT(nprotect + nprotect_loop);   \
                        if (findVarInFrame(frame, thispathassocwfileSymbol) == R_UnboundValue)\
                            return ScalarInteger(NA_INTEGER);  \
                        return (which);                        \
                    }                                          \
                    else {                                     \
                        thispatherror = duplicate(thispatherror);\
                        PROTECT(thispatherror);                \
                        SET_VECTOR_ELT(thispatherror, 1, getCurrentCall(rho));\
                        stop(thispatherror);                   \
                        UNPROTECT(1);  /* thispatherror */     \
                        /* should not reach here */            \
                        UNPROTECT(nprotect + nprotect_loop);   \
                        return R_NilValue;                     \
                    }                                          \
                }                                              \
            }                                                  \
            if (get_frame_number) {                            \
                UNPROTECT(nprotect + nprotect_loop);           \
                return (which);                                \
            }                                                  \
            if (thispathofile == R_UnboundValue)               \
                error(_("object '%s' not found"), EncodeChar(PRINTNAME(thispathofileSymbol)));\
            if (for_msg) {                                     \
                if (original == TRUE) {                        \
                    UNPROTECT(nprotect + nprotect_loop);       \
                    return thispathofile;                      \
                }                                              \
                thispathfile = findVarInFrame(frame, thispathfileSymbol);\
                if (thispathfile == R_UnboundValue)            \
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(thispathfileSymbol)));\
                if (TYPEOF(thispathfile) != PROMSXP)           \
                    error("invalid '%s', is not a promise; should never happen, please report!", EncodeChar(PRINTNAME(thispathfileSymbol)));\
                if (promise_must_be_forced) {                  \
                    if (PRVALUE(thispathfile) == R_UnboundValue)\
                        error("invalid '%s', this promise should have already been forced", EncodeChar(PRINTNAME(thispathfileSymbol)));\
                    UNPROTECT(nprotect + nprotect_loop);       \
                    return PRVALUE(thispathfile);              \
                }                                              \
                else {                                         \
                    UNPROTECT(nprotect + nprotect_loop);       \
                    /* if thispathfile has already been evaluated, return it */\
                    /* otherwise, return the original file */  \
                    if (PRVALUE(thispathfile) == R_UnboundValue)\
                        return thispathofile;                  \
                    else                                       \
                        return PRVALUE(thispathfile);          \
                }                                              \
            }                                                  \
            if (original == TRUE)                              \
                returnthis = thispathofile;                    \
            else {                                             \
                thispathfile = findVarInFrame(frame, thispathfileSymbol);\
                if (thispathfile == R_UnboundValue)            \
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(thispathfileSymbol)));\
                if (TYPEOF(thispathfile) != PROMSXP)           \
                    error("invalid '%s', is not a promise; should never happen, please report!", EncodeChar(PRINTNAME(thispathfileSymbol)));\
                if (promise_must_be_forced) {                  \
                    if (PRVALUE(thispathfile) == R_UnboundValue)\
                        error("invalid '%s', this promise should have already been forced", EncodeChar(PRINTNAME(thispathfileSymbol)));\
                    returnthis = PRVALUE(thispathfile);        \
                }                                              \
                else {                                         \
                    if (PRVALUE(thispathfile) == R_UnboundValue) {\
                        if (original)                          \
                            returnthis = thispathofile;        \
                        else {                                 \
                            if (PRSEEN(thispathfile)) {        \
                                if (PRSEEN(thispathfile) == 1) {}\
                                else SET_PRSEEN(thispathfile, 0);\
                            }                                  \
                            returnthis = eval(thispathfile, R_EmptyEnv);\
                        }                                      \
                    }                                          \
                    else returnthis = PRVALUE(thispathfile);   \
                }                                              \
            }                                                  \
            if (verbose) Rprintf("Source: call to function %s\n", fun_name);\
            UNPROTECT(nprotect + nprotect_loop);               \
            return returnthis


            returnfile(
                /* int character_only         = */ FALSE,
                /* int file_only              = */ FALSE,
                /* SEXP which                 = */ which,
                /* int promise_must_be_forced = */ FALSE,
                /* const char *fun_name       = */
                "source"
            );
        }


        else if (identical(function, sys_source)) {
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                ofile = findVarInFrame(frame, fileSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(fileSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    if (PRSEEN(ofile) == 1) {
                        /* if ofile is a promise already under evaluation */
                        UNPROTECT(nprotect_loop);
                        continue;
                    }
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                checkfile(
                    /* SEXP call                  = */ sys_call(which, rho),
                    /* SEXP sym                   = */ fileSymbol,
                    /* SEXP ofile                 = */ ofile,
                    /* SEXP frame                 = */ frame,
                    /* int forcepromise           = */ FALSE,
                    /* int assign_returnvalue     = */ FALSE,
                    /* int maybe_chdir            = */ TRUE,
                    /* SEXP getowd                = */ findVarInFrame(frame, owdSymbol),
                    /* int hasowd                 = */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* int character_only         = */ TRUE,
                    /* int conv2utf8              = */ FALSE,
                    /* int allow_blank_string     = */ FALSE,
                    /* int allow_clipboard        = */ FALSE,
                    /* int allow_stdin            = */ FALSE,
                    /* int allow_url              = */ FALSE,
                    /* int allow_file_uri         = */ FALSE,
                    /* int allow_unz              = */ FALSE,
                    /* int allow_pipe             = */ FALSE,
                    /* int allow_terminal         = */ FALSE,
                    /* int allow_textConnection   = */ FALSE,
                    /* int allow_rawConnection    = */ FALSE,
                    /* int allow_sockconn         = */ FALSE,
                    /* int allow_servsockconn     = */ FALSE,
                    /* int allow_customConnection = */ FALSE,
                    /* int ignore_blank_string    = */ FALSE,
                    /* int ignore_clipboard       = */ FALSE,
                    /* int ignore_stdin           = */ FALSE,
                    /* int ignore_url             = */ FALSE,
                    /* int ignore_file_uri        = */ FALSE
                )
            }
            returnfile(
                /* int character_only         = */ TRUE,
                /* int file_only              = */ TRUE,
                /* SEXP which                 = */ which,
                /* int promise_must_be_forced = */ FALSE,
                /* const char *fun_name       = */
                "sys.source"
            );
        }


        else if (has_tools_rstudio && identical(function, debugSource)) {
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                ofile = findVarInFrame(frame, fileNameSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(fileNameSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    if (PRSEEN(ofile) == 1) {
                        /* if ofile is a promise already under evaluation */
                        UNPROTECT(nprotect_loop);
                        continue;
                    }
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                checkfile(
                    /* SEXP call                  = */ sys_call(which, rho),
                    /* SEXP sym                   = */ fileNameSymbol,
                    /* SEXP ofile                 = */ ofile,
                    /* SEXP frame                 = */ frame,
                    /* int forcepromise           = */ FALSE,
                    /* int assign_returnvalue     = */ FALSE,
                    /* int maybe_chdir            = */ FALSE,
                    /* SEXP getowd                = */ NULL,
                    /* int hasowd                 = */ FALSE,
                    /* int character_only         = */ TRUE,
                    /* int conv2utf8              = */ TRUE,
                    /* int allow_blank_string     = */ TRUE,
                    /* int allow_clipboard        = */ TRUE,
                    /* int allow_stdin            = */ TRUE,
                    /* int allow_url              = */ TRUE,
                    /* int allow_file_uri         = */ TRUE,
                    /* int allow_unz              = */ FALSE,
                    /* int allow_pipe             = */ FALSE,
                    /* int allow_terminal         = */ FALSE,
                    /* int allow_textConnection   = */ FALSE,
                    /* int allow_rawConnection    = */ FALSE,
                    /* int allow_sockconn         = */ FALSE,
                    /* int allow_servsockconn     = */ FALSE,
                    /* int allow_customConnection = */ FALSE,
                    /* int ignore_blank_string    = */ FALSE,
                    /* int ignore_clipboard       = */ FALSE,
                    /* int ignore_stdin           = */ FALSE,
                    /* int ignore_url             = */ FALSE,
                    /* int ignore_file_uri        = */ FALSE
                )
            }
            returnfile(
                /* int character_only         = */ TRUE,
                /* int file_only              = */ FALSE,
                /* SEXP which                 = */ which,
                /* int promise_must_be_forced = */ FALSE,
                /* const char *fun_name       = */
                "debugSource in RStudio"
            );
        }


        else if (testthat_loaded && identical(function, source_file)) {
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                ofile = findVarInFrame(frame, pathSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(pathSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    if (PRSEEN(ofile) == 1) {
                        /* if ofile is a promise already under evaluation */
                        UNPROTECT(nprotect_loop);
                        continue;
                    }
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                int ignore_all = asLogical(eval(lang1(testthat_uses_brioSymbol), mynamespace));
                checkfile(
                    /* SEXP call                  = */ sys_call(which, rho),
                    /* SEXP sym                   = */ pathSymbol,
                    /* SEXP ofile                 = */ ofile,
                    /* SEXP frame                 = */ frame,
                    /* int forcepromise           = */ FALSE,
                    /* int assign_returnvalue     = */ FALSE,
                    /* int maybe_chdir            = */ TRUE,
                    /* SEXP getowd                = */ findVarInFrame(frame, old_dirSymbol),
                    /* int hasowd                 = */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* int character_only         = */ TRUE,
                    /* int conv2utf8              = */ FALSE,
                    /* int allow_blank_string     = */ FALSE,
                    /* int allow_clipboard        = */ FALSE,
                    /* int allow_stdin            = */ FALSE,
                    /* int allow_url              = */ FALSE,
                    /* int allow_file_uri         = */ FALSE,
                    /* int allow_unz              = */ FALSE,
                    /* int allow_pipe             = */ FALSE,
                    /* int allow_terminal         = */ FALSE,
                    /* int allow_textConnection   = */ FALSE,
                    /* int allow_rawConnection    = */ FALSE,
                    /* int allow_sockconn         = */ FALSE,
                    /* int allow_servsockconn     = */ FALSE,
                    /* int allow_customConnection = */ FALSE,
                    /* int ignore_blank_string    = */ ignore_all,
                    /* int ignore_clipboard       = */ ignore_all,
                    /* int ignore_stdin           = */ ignore_all,
                    /* int ignore_url             = */ ignore_all,
                    /* int ignore_file_uri        = */ ignore_all
                )
            }
            returnfile(
                /* int character_only         = */ TRUE,
                /* int file_only              = */ TRUE,
                /* SEXP which                 = */ which,
                /* int promise_must_be_forced = */ FALSE,
                /* const char *fun_name       = */
                "source_file from package testthat"
            );
        }


        else if (knitr_loaded && identical(function, knit)) {
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                if (findVarInFrame(frame, oenvirSymbol) == R_UnboundValue) {
                    UNPROTECT(nprotect_loop);
                    continue;
                }
                /* missing(input) */
                SEXP expr = lang2(missingSymbol, inputSymbol);
                PROTECT(expr);
                int missing_input = asLogical(eval(expr, frame));
                UNPROTECT(1);  /* expr */
                if (missing_input) {
                    assign_null(frame);
                    UNPROTECT(nprotect_loop);
                    continue;
                }
                ofile = getInFrame(inputSymbol, frame, FALSE);
                checkfile(
                    /* SEXP call                  = */ sys_call(which, rho),
                    /* SEXP sym                   = */ inputSymbol,
                    /* SEXP ofile                 = */ ofile,
                    /* SEXP frame                 = */ frame,
                    /* int forcepromise           = */ FALSE,
                    /* int assign_returnvalue     = */ FALSE,
                    /* int maybe_chdir            = */ TRUE,
                    /* SEXP getowd                = */ eval(lang1(knitr_output_dirSymbol), mynamespace),
                    /* int hasowd                 = */ ((owd) != R_NilValue),
                    /* int character_only         = */ FALSE,
                    /* int conv2utf8              = */ FALSE,
                    /* int allow_blank_string     = */ FALSE,
                    /* int allow_clipboard        = */ TRUE,
                    /* int allow_stdin            = */ TRUE,
                    /* int allow_url              = */ TRUE,
                    /* int allow_file_uri         = */ TRUE,
                    /* int allow_unz              = */ TRUE,
                    /* int allow_pipe             = */ TRUE,
                    /* int allow_terminal         = */ TRUE,
                    /* int allow_textConnection   = */ TRUE,
                    /* int allow_rawConnection    = */ TRUE,
                    /* int allow_sockconn         = */ TRUE,
                    /* int allow_servsockconn     = */ TRUE,
                    /* int allow_customConnection = */ TRUE,
                    /* int ignore_blank_string    = */ FALSE,
                    /* int ignore_clipboard       = */ FALSE,
                    /* int ignore_stdin           = */ FALSE,
                    /* int ignore_url             = */ FALSE,
                    /* int ignore_file_uri        = */ FALSE
                )
            }
            returnfile(
                /* int character_only         = */ FALSE,
                /* int file_only              = */ FALSE,
                /* SEXP which                 = */ which,
                /* int promise_must_be_forced = */ FALSE,
                /* const char *fun_name       = */
                "knit from package knitr"
            );
        }


        else if (identical(function, wrap_source)) {
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                UNPROTECT(nprotect_loop);
                continue;
            }
            returnfile(
                /* int character_only         = */ FALSE,
                /* int file_only              = */ FALSE,
                /* SEXP which                 = */ getInFrame(thispathnSymbol, frame, FALSE),
                /* int promise_must_be_forced = */ TRUE,
                /* const char *fun_name       = */
                "wrap.source from package this.path"
            );
        }


        else if (box_loaded && identical(function, load_from_source)) {
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                /* info$source_path */
                SEXP expr = lang3(
                    R_DollarSymbol,
                    install("info"),
                    install("source_path")
                );
                PROTECT(expr);
                SEXP ofile = eval(expr, frame);
                UNPROTECT(1);  /* expr */
                PROTECT(ofile);
                checkfile(
                    /* SEXP call                  = */ sys_call(which, rho),
                    /* SEXP sym                   = */ install("info$source_path"),
                    /* SEXP ofile                 = */ ofile,
                    /* SEXP frame                 = */ frame,
                    /* int forcepromise           = */ TRUE,
                    /* int assign_returnvalue     = */ FALSE,
                    /* int maybe_chdir            = */ FALSE,
                    /* SEXP getowd                = */ NULL,
                    /* int hasowd                 = */ FALSE,
                    /* int character_only         = */ TRUE,
                    /* int conv2utf8              = */ FALSE,
                    /* int allow_blank_string     = */ FALSE,
                    /* int allow_clipboard        = */ FALSE,
                    /* int allow_stdin            = */ FALSE,
                    /* int allow_url              = */ FALSE,
                    /* int allow_file_uri         = */ FALSE,
                    /* int allow_unz              = */ FALSE,
                    /* int allow_pipe             = */ FALSE,
                    /* int allow_terminal         = */ FALSE,
                    /* int allow_textConnection   = */ FALSE,
                    /* int allow_rawConnection    = */ FALSE,
                    /* int allow_sockconn         = */ FALSE,
                    /* int allow_servsockconn     = */ FALSE,
                    /* int allow_customConnection = */ FALSE,
                    /* int ignore_blank_string    = */ FALSE,
                    /* int ignore_clipboard       = */ FALSE,
                    /* int ignore_stdin           = */ FALSE,
                    /* int ignore_url             = */ FALSE,
                    /* int ignore_file_uri        = */ FALSE
                )
                UNPROTECT(1);  /* ofile */
            }
            returnfile(
                /* int character_only         = */ TRUE,
                /* int file_only              = */ TRUE,
                /* SEXP which                 = */ which,
                /* int promise_must_be_forced = */ TRUE,
                /* const char *fun_name       = */
                "load_from_source from package box"
            );
        }


        else if ((insidesourcewashere = findVarInFrame(frame, insidesourcewashereSymbol)) != R_UnboundValue) {
            if (insidesourcewashere == R_MissingArg) {
                UNPROTECT(nprotect_loop);
                continue;
            }
            SEXP thispathn = getInFrame(thispathnSymbol, frame, FALSE);
            /* this could happen with eval() or similar */
            if (iwhich[0] != asInteger(thispathn)) {
                UNPROTECT(nprotect_loop);
                continue;
            }
            returnfile(
                /* int character_only         = */ FALSE,
                /* int file_only              = */ FALSE,
                /* SEXP which                 = */ thispathn,
                /* int promise_must_be_forced = */ TRUE,
                /* const char *fun_name       = */ CHAR(insidesourcewashere)
            );
        }


        UNPROTECT(nprotect_loop);
    }


    UNPROTECT(nprotect);
    toplevel;


#undef toplevel
#undef returnfile
}


SEXP do_inittoolsrstudio do_formals
{
    do_start("inittoolsrstudio", -1);


    Rboolean skipCheck = FALSE;
    int nargs = length(args);
    if (nargs == 0) {
    }
    else if (nargs == 1) {
        skipCheck = asLogical(CAR(args));
    }
    else errorcall(call, wrong_nargs_to_External(nargs, "C_inittoolsrstudio", "0 or 1"));
    return ScalarLogical(init_tools_rstudio(skipCheck));
}


SEXP do_thispathrgui do_formals
{
    do_start("thispathrgui", 6);


    SEXP wintitle, untitled, r_editor;
    Rboolean verbose, original, for_msg;


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


    verbose = asLogical(CAR(args)); args = CDR(args);
    if (verbose == NA_LOGICAL)
        errorcall(call, _("invalid '%s' value"), "verbose");


    original = asLogical(CAR(args)); args = CDR(args);
    if (verbose == NA_LOGICAL)
        errorcall(call, _("invalid '%s' value"), "original");


    for_msg = asLogical(CAR(args)); args = CDR(args);
    if (for_msg == NA_LOGICAL)
        errorcall(call, _("invalid '%s' value"), "for.msg");


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
                if (for_msg) return mkString(_RGui("Untitled"));
                error("%s%s",
                    this_path_used_in_an_inappropriate_fashion,
                    (active) ? "* active document in Rgui does not exist" :
                               "* source document in Rgui does not exist");
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
                    SEXP expr = allocList(2);                  \
                    PROTECT(expr);                             \
                    SET_TYPEOF(expr, LANGSXP);                 \
                    SETCAR(expr, _normalizePathSymbol);        \
                    SETCADR(expr, ScalarString((charsxp)));    \
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


    if (active) error("no windows in Rgui; should never happen, please report!");
    if (for_msg) return ScalarString(NA_STRING);


    char msg[256];
    snprintf(msg, 256, "%s%s",
        this_path_used_in_an_inappropriate_fashion,
        "* R is being run from Rgui with no documents open");
    SEXP cond = thisPathNotExistsError(msg, PROTECT(getCurrentCall(rho)));
    PROTECT(cond);
    stop(cond);
    UNPROTECT(2);
    return R_NilValue;  /* should not be reached */
}
