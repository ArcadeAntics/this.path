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


#define errorbody                                              \
    args = CDR(args);                                          \
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1 ||      \
        STRING_ELT(CAR(args), 0) == NA_STRING)                 \
    {                                                          \
        errorcall(call, "invalid first argument");             \
    }                                                          \
    const char *msg = translateChar(STRING_ELT(CAR(args), 0)); \
    args = CDR(args);                                          \
    SEXP call2 = CAR(args);                                    \
    ENSURE_NAMEDMAX(call2)


SEXP do_thispathunrecognizedconnectionclasserror(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    SEXP call2 = CAR(args);
    ENSURE_NAMEDMAX(call2);
    args = CDR(args);
    Rconnection Rcon = R_GetConnection(CAR(args));
    return thisPathUnrecognizedConnectionClassError(call2, Rcon);
}


SEXP do_thispathunrecognizedmannererror(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    op = CADR(args);
    ENSURE_NAMEDMAX(op);
    return thisPathUnrecognizedMannerError(op);
}


SEXP do_thispathnotimplementederror(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    errorbody;
    return thisPathNotImplementedError(msg, call2);
}


SEXP do_thispathnotexistserror(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    errorbody;
    return thisPathNotExistsError(msg, call2);
}


SEXP do_thispathinzipfileerror(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    SEXP call2 = CAR(args);
    args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1 ||
        STRING_ELT(CAR(args), 0) == NA_STRING)
    {
        errorcall(call, "invalid second argument");
    }
    SEXP description = STRING_ELT(CAR(args), 0);
    return thisPathInZipFileError(call2, description);
}


SEXP do_thispathinaquaerror(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return thisPathInAQUAError(lazy_duplicate(CADR(args)));
}


#undef errorbody


SEXP do_isclipboard(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP file = CADR(args);
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


SEXP do_thispath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP returnthis = NULL;


    args = CDR(args);


    int verbose  = asFlag(CAR(args), "verbose");
    int original = asLogical(CADR(args));
    int for_msg  = asFlag(CADDR(args), "for.msg");
    int N        = asInteger(CADDDR(args));
    if (N == NA_INTEGER || N < 0)
        error(_("invalid '%s' argument"), "N");
    int get_frame_number = asFlag(CAD4R(args), "get.frame.number");


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
        returnthis = eval(expr, rho);                          \
        UNPROTECT(1);                                          \
        return returnthis
        /* exclude the ; on purpose */


        toplevel;
    }


    SEXP thispathofile,
         thispathfile,
         thispathformsg,
         thispatherror;


    int nprotect = 0;


    int testthat_loaded, knitr_loaded;


    SEXP source = PRVALUE(findVarInFrame(R_BaseEnv, sourceSymbol));
    SEXP sys_source = PRVALUE(findVarInFrame(R_BaseEnv, sys_sourceSymbol));
    SEXP tools_rstudio;
    SEXP debugSource, source_file, knit, wrap_source;


    if (gui_rstudio == -1) {
        SEXP tmp = findVarInFrame(ENCLOS(rho), gui_rstudioSymbol);
        if (tmp == R_UnboundValue)
            error(_("object '%s' not found"), "gui.rstudio");
        if (TYPEOF(tmp) != PROMSXP)
            error("invalid '%s', is not a promise", "gui.rstudio");
        if (PRVALUE(tmp) == R_UnboundValue)
            tmp = eval(tmp, R_EmptyEnv);
        else
            tmp = PRVALUE(tmp);
        gui_rstudio = LOGICAL_ELT(tmp, 0);
    }
    if (gui_rstudio) {
        tools_rstudio = as_environment_char("tools:rstudio");
        debugSource = findVarInFrame(tools_rstudio, debugSourceSymbol);
        if (debugSource == R_UnboundValue)
            error(_("object '%s' not found"), "debugSource");
        if (TYPEOF(debugSource) == PROMSXP) {
            if (PRVALUE(debugSource) == R_UnboundValue)
                debugSource = eval(debugSource, rho);
            else
                debugSource = PRVALUE(debugSource);
        }
        if (TYPEOF(debugSource) != CLOSXP)
            error(_("object '%s' of mode '%s' was not found"), "debugSource", "function");
    }
    else debugSource = R_NilValue;


    testthat_loaded = (findVarInFrame(R_NamespaceRegistry, testthatSymbol) != R_UnboundValue);
    if (testthat_loaded)
        source_file = R_getNSValue(R_NilValue, testthatSymbol, source_fileSymbol, TRUE);
    else
        source_file = R_NilValue;


    knitr_loaded = (findVarInFrame(R_NamespaceRegistry, knitrSymbol) != R_UnboundValue);
    if (knitr_loaded)
        knit = R_getNSValue(R_NilValue, knitrSymbol, knitSymbol, TRUE);
    else
        knit = R_NilValue;


    wrap_source = R_getNSValue(R_NilValue, this_pathSymbol, wrap_sourceSymbol, FALSE);


/* doesn't work because sys.function() duplicates its return value */
// #define identical(x, y) ((x) == (y))


/* num.eq = TRUE                 num_as_bits = FALSE      0
   single.NA = TRUE              NA_as_bits = FALSE       0
   attrib.as.set = TRUE          attr_by_order = FALSE    0
   ignore.bytecode = TRUE        use_bytecode = FALSE     0
   ignore.environment = FALSE    use_cloenv = TRUE        16
   ignore.srcref = TRUE          use_srcref = FALSE       0
   extptr.as.ref = FALSE         extptr_as_ref = FALSE    0
 */
// #define identical(x, y) R_compute_identical((x), (y), 16)


/* num.eq = FALSE                num_as_bits = TRUE       1
   single.NA = FALSE             NA_as_bits = TRUE        2
   attrib.as.set = FALSE         attr_by_order = TRUE     4
   ignore.bytecode = FALSE       use_bytecode = TRUE      8
   ignore.environment = FALSE    use_cloenv = TRUE        16
   ignore.srcref = FALSE         use_srcref = TRUE        32
   extptr.as.ref = TRUE          extptr_as_ref = TRUE     64
 */
#define identical(x, y) R_compute_identical((x), (y), 127)


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
        if (identical(function, source)) {
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                ofile = findVarInFrame(frame, ofileSymbol);
                if (ofile == R_UnboundValue) {
                    UNPROTECT(1);
                    continue;
                }
                if (TYPEOF(ofile) == PROMSXP) {
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                checkfile(
                    /* SEXP sym           = */ ofileSymbol,
                    /* SEXP ofile         = */ ofile,
                    /* SEXP frame         = */ frame,
                    /* int character_only = */ FALSE,
                    /* int file_only      = */ FALSE,
                    /* SEXP rho           = */ rho  ,
                    /* int forcepromise   = */ FALSE,
                    /* SEXP call          = */ sys_call(which, rho),
                    /* int maybe_chdir    = */ TRUE ,
                    /* SEXP getowd        = */ findVarInFrame(frame, owdSymbol),
                    /* int hasowd         = */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* int do_enc2utf8    = */ FALSE,
                    /* int normalize      = */ FALSE
                )
            }


#define returnfile(character_only, file_only, maybe_decrement, \
    promise_must_be_forced, fun_name)                          \
            thispathofile = findVarInFrame(frame, thispathofileSymbol);\
            /* don't check right away that this is missing, in case another */\
            /* error must be thrown or the user requests the frame number   */\
            if (!file_only) {                                  \
                /* if file_only is TRUE, thispathofile cannot be NULL */\
                if (thispathofile == R_NilValue) {             \
                    UNPROTECT(1);                              \
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
                        UNPROTECT(nprotect + 1);               \
                        return thispathformsg;                 \
                    }                                          \
                    else if (get_frame_number) {               \
                        UNPROTECT(nprotect + 1);               \
                        if (findVarInFrame(frame, thispathassocwfileSymbol) == R_UnboundValue)\
                            return ScalarInteger(NA_INTEGER);  \
                        if (maybe_decrement) {                 \
                            if (iwhich[0] >= 2) {              \
                                iwhich[0]--;                   \
                            }                                  \
                        }                                      \
                        return which;                          \
                    }                                          \
                    else {                                     \
                        thispatherror = duplicate(thispatherror);\
                        PROTECT(thispatherror);                \
                        SET_VECTOR_ELT(thispatherror, 1, getCurrentCall(rho));\
                        stop(thispatherror);                   \
                        /* should not reach here */            \
                        UNPROTECT(1);                          \
                        UNPROTECT(nprotect + 1);               \
                        return R_NilValue;                     \
                    }                                          \
                }                                              \
            }                                                  \
            if (get_frame_number) {                            \
                UNPROTECT(nprotect + 1);                       \
                if (maybe_decrement) {                         \
                    if (iwhich[0] >= 2) {                      \
                        iwhich[0]--;                           \
                    }                                          \
                }                                              \
                return which;                                  \
            }                                                  \
            if (thispathofile == R_UnboundValue)               \
                error(_("object '%s' not found"), EncodeChar(PRINTNAME(thispathofileSymbol)));\
            if (for_msg) {                                     \
                if (original == TRUE) {                        \
                    UNPROTECT(nprotect + 1);                   \
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
                    UNPROTECT(nprotect + 1);                   \
                    return PRVALUE(thispathfile);              \
                }                                              \
                else {                                         \
                    UNPROTECT(nprotect + 1);                   \
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
                    if (PRVALUE(thispathfile) == R_UnboundValue)\
                        returnthis = (original ? thispathofile : eval(thispathfile, R_EmptyEnv));\
                    else                                       \
                        returnthis = PRVALUE(thispathfile);    \
                }                                              \
            }                                                  \
            if (verbose) Rprintf("Source: call to function %s\n", fun_name);\
            UNPROTECT(nprotect + 1);                           \
            return returnthis


            returnfile(
                /* int character_only         = */ FALSE,
                /* int file_only              = */ FALSE,
                /* int maybe_decrement        = */ FALSE,
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
                        UNPROTECT(1);
                        continue;
                    }
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                checkfile(
                    /* SEXP sym           = */ fileSymbol,
                    /* SEXP ofile         = */ ofile,
                    /* SEXP frame         = */ frame,
                    /* int character_only = */ TRUE ,
                    /* int file_only      = */ TRUE ,
                    /* SEXP rho           = */ rho  ,
                    /* int forcepromise   = */ FALSE,
                    /* SEXP call          = */ sys_call(which, rho),
                    /* int maybe_chdir    = */ TRUE ,
                    /* SEXP getowd        = */ findVarInFrame(frame, owdSymbol),
                    /* int hasowd         = */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* int do_enc2utf8    = */ FALSE,
                    /* int normalize      = */ FALSE
                )
            }
            returnfile(
                /* int character_only         = */ TRUE ,
                /* int file_only              = */ TRUE ,
                /* int maybe_decrement        = */ FALSE,
                /* int promise_must_be_forced = */ FALSE,
                /* const char *fun_name       = */
                "sys.source"
            );
        }


        else if (gui_rstudio && identical(function, debugSource)) {
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                ofile = findVarInFrame(frame, fileNameSymbol);
                if (ofile == R_UnboundValue)
                    error(_("object '%s' not found"), EncodeChar(PRINTNAME(fileNameSymbol)));
                if (TYPEOF(ofile) == PROMSXP) {
                    if (PRSEEN(ofile) == 1) {
                        /* if ofile is a promise already under evaluation */
                        UNPROTECT(1);
                        continue;
                    }
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                checkfile(
                    /* SEXP sym           = */ fileNameSymbol,
                    /* SEXP ofile         = */ ofile,
                    /* SEXP frame         = */ frame,
                    /* int character_only = */ TRUE ,
                    /* int file_only      = */ FALSE,
                    /* SEXP rho           = */ rho  ,
                    /* int forcepromise   = */ FALSE,
                    /* SEXP call          = */ sys_call(which, rho),
                    /* int maybe_chdir    = */ FALSE,
                    /* SEXP getowd        = */ NULL ,
                    /* int hasowd         = */ FALSE,
                    /* int do_enc2utf8    = */ TRUE,
                    /* int normalize      = */ FALSE
                )
            }
            returnfile(
                /* int character_only         = */ TRUE ,
                /* int file_only              = */ FALSE,
                /* int maybe_decrement        = */ FALSE,
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
                        UNPROTECT(1);
                        continue;
                    }
                    if (PRVALUE(ofile) == R_UnboundValue)
                        ofile = eval(ofile, R_EmptyEnv);
                    else
                        ofile = PRVALUE(ofile);
                }
                checkfile(
                    /* SEXP sym           = */ pathSymbol,
                    /* SEXP ofile         = */ ofile,
                    /* SEXP frame         = */ frame,
                    /* int character_only = */ TRUE ,
                    /* int file_only      = */ TRUE ,
                    /* SEXP rho           = */ rho  ,
                    /* int forcepromise   = */ FALSE,
                    /* SEXP call          = */ sys_call(which, rho),
                    /* int maybe_chdir    = */ TRUE ,
                    /* SEXP getowd        = */ findVarInFrame(frame, old_dirSymbol),
                    /* int hasowd         = */ ((owd) != R_UnboundValue && (owd) != R_NilValue),
                    /* int do_enc2utf8    = */ FALSE,
                    /* int normalize      = */ asLogical(eval(lang1(testthat_uses_brioSymbol), rho))
                )
            }
            returnfile(
                /* int character_only         = */ TRUE ,
                /* int file_only              = */ TRUE ,
                /* int maybe_decrement        = */ FALSE,
                /* int promise_must_be_forced = */ FALSE,
                /* const char *fun_name       = */
                "source_file in package testthat"
            );
        }


        else if (knitr_loaded && identical(function, knit)) {
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                if (findVarInFrame(frame, install("oenvir")) == R_UnboundValue) {
                    UNPROTECT(1);
                    continue;
                }
                SEXP expr = lang2(missingSymbol, inputSymbol);
                PROTECT(expr);
                int missing_input = asLogical(eval(expr, frame));
                UNPROTECT(1);
                if (missing_input) {
                    assign_null(frame);
                    assign_done(frame);
                    UNPROTECT(1);
                    continue;
                }
                ofile = getInFrame(inputSymbol, frame, FALSE);
                checkfile(
                    /* SEXP sym           = */ inputSymbol,
                    /* SEXP ofile         = */ ofile,
                    /* SEXP frame         = */ frame,
                    /* int character_only = */ FALSE,
                    /* int file_only      = */ FALSE,
                    /* SEXP rho           = */ rho  ,
                    /* int forcepromise   = */ FALSE,
                    /* SEXP call          = */ sys_call(which, rho),
                    /* int maybe_chdir    = */ TRUE ,
                    /* SEXP getowd        = */ eval(lang1(knitr_output_dirSymbol), rho),
                    /* int hasowd         = */ ((owd) != R_NilValue),
                    /* int do_enc2utf8    = */ FALSE,
                    /* int normalize      = */ FALSE
                )
            }
            returnfile(
                /* int character_only         = */ FALSE,
                /* int file_only              = */ FALSE,
                /* int maybe_decrement        = */ FALSE,
                /* int promise_must_be_forced = */ FALSE,
                /* const char *fun_name       = */
                "knit in package knitr"
            );
        }


        else if (identical(function, wrap_source)) {
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                UNPROTECT(1);
                continue;
            }
            returnfile(
                /* int character_only         = */ FALSE,
                /* int file_only              = */ FALSE,
                /* int maybe_decrement        = */ TRUE ,
                /* int promise_must_be_forced = */ TRUE ,
                /* const char *fun_name       = */
                "wrap.source in package this.path"
            );
        }


        else if (findVarInFrame(frame, insidesourcewashereSymbol) != R_UnboundValue) {
            returnfile(
                /* int character_only         = */ FALSE,
                /* int file_only              = */ FALSE,
                /* int maybe_decrement        = */ TRUE ,
                /* int promise_must_be_forced = */ TRUE ,
                /* const char *fun_name       = */
                "inside.source in package this.path"
            );
        }


        UNPROTECT(1);
    }


    UNPROTECT(nprotect);
    toplevel;


#undef toplevel
#undef identical
#undef returnfile
}
