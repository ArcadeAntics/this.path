#include "thispathdefn.h"
#include "drivewidth.h"


#if R_version_less_than(3, 0, 0)
#define XLENGTH LENGTH
#endif


#define errorbody                                              \
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1 ||      \
        STRING_ELT(CAR(args), 0) == NA_STRING)                 \
    {                                                          \
        errorcall(call, _("invalid first argument"));          \
    }                                                          \
    const char *msg = translateChar(STRING_ELT(CAR(args), 0)); args = CDR(args);\
    SEXP call2 = CAR(args); args = CDR(args);                  \
    ENSURE_NAMEDMAX(call2)


SEXP do_thispathunrecognizedconnectionclasserror do_formals
{
    do_start_no_call_op_rho("thispathunrecognizedconnectionclasserror", 2);


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


SEXP do_thispathunrecognizedmannererror do_formals
{
    do_start_no_call_op_rho("thispathunrecognizedmannererror", 1);


    ENSURE_NAMEDMAX(CAR(args));
    return thisPathUnrecognizedMannerError(CAR(args));
}


SEXP do_thispathnotimplementederror do_formals
{
    do_start_no_op_rho("thispathnotimplementederror", 2);


    errorbody;
    return thisPathNotImplementedError(msg, call2);
}


SEXP do_thispathnotexistserror do_formals
{
    do_start_no_op_rho("thispathnotexistserror", 2);


    errorbody;
    return thisPathNotExistsError(msg, call2);
}


SEXP do_thispathinzipfileerror do_formals
{
    do_start_no_op_rho("thispathinzipfileerror", 2);


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
    do_start_no_call_op_rho("thispathinaquaerror", 1);


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


SEXP _thispath(Rboolean verbose         , Rboolean original        ,
               Rboolean for_msg         , Rboolean get_frame_number,
               Rboolean local           , Rboolean contents        ,
               int N                    , SEXP rho                 )
{
    if (verbose == NA_LOGICAL)
        error(_("invalid '%s' value"), "verbose");
    /* original is allowed to be NA */
    if (for_msg == NA_LOGICAL)
        error(_("invalid '%s' value"), "for.msg");
    if (get_frame_number == NA_LOGICAL)
        error(_("invalid '%s' value"), "get.frame.number");
    if (local == NA_LOGICAL)
        error(_("invalid '%s' value"), "local");
    if (contents == NA_LOGICAL)
        error(_("invalid '%s' value"), "contents");


    if (N == NA_INTEGER || N < 0)
        error(_("invalid '%s' argument"), "N");


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
    }


#undef four
#undef three
#undef two
#undef one


    SEXP returnthis = NULL;
    SEXP returnvalue;  /* checkfile() creates a variable 'returnvalue' that is
                          used in insidesource() (see ./src/wrapsource.c).
                          not used elsewhere but must be declared */


    if (local) {
        const char *name = "this.path(local = TRUE)";


        if (N <= 0) error("%s cannot be used within the global environment", name);
        SEXP frame = eval(lang1(parent_frameSymbol), rho);
        PROTECT(frame);


        /* ensure 'inside.source()' isn't evaluated in an invalid environment */
        if (frame == R_GlobalEnv)
            error("%s cannot be used within the global environment", name);
        else if (frame == R_BaseEnv)
            error("%s cannot be used within the base environment", name);
        else if (frame == R_EmptyEnv)
            error("%s cannot be used within the empty environment", name);
        else if (R_IsPackageEnv(frame))
            error("%s cannot be used within a package environment", name);
        else if (R_IsNamespaceEnv(frame))
            error("%s cannot be used within a namespace environment", name);


        if (findVarInFrame(frame, insidesourcewashereSymbol) == R_UnboundValue)
            error("%s cannot be called within this environment", name);
        if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue)
            error("%s cannot be called within this environment", name);


        UNPROTECT(1);  /* frame */


        SEXP function = eval(PROTECT(lang2(sys_functionSymbol, ScalarInteger(N))), rho);
        if (TYPEOF(function) != CLOSXP)
            error("%s cannot be used within a '%s', possible errors with eval?",
                  name, type2char(TYPEOF(function)));
        UNPROTECT(1);
    }


    // Rprintf("\nverbose: %s\noriginal: %s\nfor.msg: %s\nget.frame.number: %s\nlocal: %s\ncontents: %s\nN: %d\n",
    //         (verbose          == NA_LOGICAL) ? "NA" : (verbose          ? "TRUE" : "FALSE"),
    //         (original         == NA_LOGICAL) ? "NA" : (original         ? "TRUE" : "FALSE"),
    //         (for_msg          == NA_LOGICAL) ? "NA" : (for_msg          ? "TRUE" : "FALSE"),
    //         (get_frame_number == NA_LOGICAL) ? "NA" : (get_frame_number ? "TRUE" : "FALSE"),
    //         (local            == NA_LOGICAL) ? "NA" : (local            ? "TRUE" : "FALSE"),
    //         (contents         == NA_LOGICAL) ? "NA" : (contents         ? "TRUE" : "FALSE"),
    //         N);


    if (N <= 0) {


#define toplevel                                               \
        if (get_frame_number) return ScalarInteger(0);         \
        SEXP expr;                                             \
        if (contents)                                          \
            expr = lang5(_this_path_toplevelSymbol, ScalarLogical(verbose), ScalarLogical(original), ScalarLogical(for_msg), ScalarLogical(contents));\
        else if (for_msg)                                      \
            expr = lang4(_this_path_toplevelSymbol, ScalarLogical(verbose), ScalarLogical(original), ScalarLogical(for_msg));\
        else if (original)                                     \
            expr = lang3(_this_path_toplevelSymbol, ScalarLogical(verbose), ScalarLogical(original));\
        else if (verbose)                                      \
            expr = lang2(_this_path_toplevelSymbol, ScalarLogical(verbose));\
        else                                                   \
            expr = lang1(_this_path_toplevelSymbol);           \
        PROTECT(expr);                                         \
        returnthis = eval(expr, mynamespace);                  \
        UNPROTECT(1);                                          \
        return returnthis


        toplevel;
    }


    SEXP thispathofile      , thispathfile       , thispathformsg     ,
         thispatherror      , insidesourcewashere;


    int nprotect = 0;


    init_tools_rstudio(FALSE);


    SEXP source      = getInFrame(sourceSymbol    , R_BaseEnv, FALSE),
         sys_source  = getInFrame(sys_sourceSymbol, R_BaseEnv, FALSE),
         debugSource = get_debugSource,
         wrap_source = getInFrame(wrap_sourceSymbol, mynamespace, FALSE);


    SEXP ns;
    Rboolean testthat_loaded, knitr_loaded, box_loaded      , compiler_loaded;
    SEXP     source_file    , knit        , load_from_source, loadcmp        ;


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
    int *iwhich = INTEGER(which);
    SEXP getframe = lang2(getInFrame(sys_frameSymbol, R_BaseEnv, FALSE), which);
    PROTECT(getframe); nprotect++;
    SEXP getfunction = lang2(getInFrame(sys_functionSymbol, R_BaseEnv, FALSE), which);
    PROTECT(getfunction); nprotect++;


    SEXP frame, function;
    SEXP ofile;


    int to = ((local) ? N : 1);


    for (iwhich[0] = N; iwhich[0] >= to; iwhich[0]--) {
        frame = eval(getframe, rho);
        // PROTECT(frame);
        function = eval(getfunction, rho);
        PROTECT(function);
/* the number of objects protected in each iteration (that must be unprotected
   before moving to the next iteration) */
#define nprotect_loop 1


        if (identical(function, source)) {
            if (local)
                error("%s cannot be called within %s()",
                      "this.path(local = TRUE)", EncodeChar(PRINTNAME(sourceSymbol)));
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
                        if (contents) {                        \
                            UNPROTECT(nprotect + nprotect_loop);\
                            return ScalarString(NA_STRING);    \
                        }                                      \
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
                /* character_only         */ FALSE,
                /* file_only              */ FALSE,
                /* which                  */ which,
                /* promise_must_be_forced */ FALSE,
                /* fun_name               */ "source"
            );
        }


        else if (identical(function, sys_source)) {
            if (local)
                error("%s cannot be called within %s()",
                      "this.path(local = TRUE)", EncodeChar(PRINTNAME(sys_sourceSymbol)));
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
                /* promise_must_be_forced */ FALSE,
                /* fun_name               */ "sys.source"
            );
        }


        else if (has_tools_rstudio && identical(function, debugSource)) {
            if (local)
                error("%s cannot be called within %s() in RStudio",
                      "this.path(local = TRUE)", EncodeChar(PRINTNAME(debugSourceSymbol)));
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
                /* promise_must_be_forced */ FALSE,
                /* fun_name               */ "debugSource in RStudio"
            );
        }


        else if (testthat_loaded && identical(function, source_file)) {
            if (local)
                error("%s cannot be called within %s() from package %s",
                      "this.path(local = TRUE)", EncodeChar(PRINTNAME(source_fileSymbol)), EncodeChar(PRINTNAME(testthatSymbol)));
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
                /* promise_must_be_forced */ FALSE,
                /* fun_name               */ "source_file from package testthat"
            );
        }


        else if (knitr_loaded && identical(function, knit)) {
            if (local)
                error("%s cannot be called within %s() from package %s",
                      "this.path(local = TRUE)", EncodeChar(PRINTNAME(knitSymbol)), EncodeChar(PRINTNAME(knitrSymbol)));
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
                    /* call                   */ sys_call(which, rho),
                    /* sym                    */ inputSymbol,
                    /* ofile                  */ ofile,
                    /* frame                  */ frame,
                    /* check_not_directory    */ FALSE,
                    /* forcepromise           */ FALSE,
                    /* assign_returnvalue     */ FALSE,
                    /* maybe_chdir            */ TRUE,
                    /* getowd                 */ eval(lang1(knitr_output_dirSymbol), mynamespace),
                    /* hasowd                 */ ((owd) != R_NilValue),
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
                /* promise_must_be_forced */ FALSE,
                /* fun_name               */ "knit from package knitr"
            );
        }


        else if (identical(function, wrap_source)) {
            if (local)
                error("%s cannot be called within %s() from package %s",
                      "this.path(local = TRUE)", EncodeChar(PRINTNAME(wrap_sourceSymbol)), "this.path");
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                UNPROTECT(nprotect_loop);
                continue;
            }
            returnfile(
                /* character_only         */ FALSE,
                /* file_only              */ FALSE,
                /* which                  */ getInFrame(thispathnSymbol, frame, FALSE),
                /* promise_must_be_forced */ TRUE,
                /* fun_name               */ "wrap.source from package this.path"
            );
        }


        else if (box_loaded && identical(function, load_from_source)) {
            if (local)
                error("%s cannot be called within %s() from package %s",
                      "this.path(local = TRUE)", EncodeChar(PRINTNAME(load_from_sourceSymbol)), EncodeChar(PRINTNAME(boxSymbol)));
            if (findVarInFrame(frame, thispathdoneSymbol) == R_UnboundValue) {
                /* info$source_path */
                SEXP expr = lang3(R_DollarSymbol, infoSymbol, source_pathSymbol);
                PROTECT(expr);
                SEXP ofile = eval(expr, frame);
                UNPROTECT(1);  /* expr */
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
                /* promise_must_be_forced */ TRUE,
                /* fun_name               */ "load_from_source from package box"
            );
        }


        else if (compiler_loaded && identical(function, loadcmp)) {
            /* much the same as sys.source() */
            if (local)
                error("%s cannot be called within %s() from package %s",
                      "this.path(local = TRUE)", EncodeChar(PRINTNAME(loadcmpSymbol)), EncodeChar(PRINTNAME(compilerSymbol)));
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
                /* promise_must_be_forced */ FALSE,
                /* fun_name               */ "loadcmp from package compiler"
            );
        }


        else if ((insidesourcewashere = findVarInFrame(frame, insidesourcewashereSymbol)) != R_UnboundValue) {
            // Rprintf("\nhere\n");
            if (insidesourcewashere == R_MissingArg) {
                UNPROTECT(nprotect_loop);
                continue;
            }
            SEXP thispathn = findVarInFrame(frame, thispathnSymbol);
            if (TYPEOF(thispathn) != INTSXP || LENGTH(thispathn) != 1)
                error(_("invalid '%s' value"), EncodeChar(PRINTNAME(thispathnSymbol)));
            /* this could happen with eval() or similar */
            if (iwhich[0] != INTEGER(thispathn)[0]) {
                UNPROTECT(nprotect_loop);
                continue;
            }
            returnfile(
                /* character_only         */ FALSE,
                /* file_only              */ FALSE,
                /* which                  */ thispathn,
                /* promise_must_be_forced */ TRUE,
                /* fun_name               */ CHAR(insidesourcewashere)
            );
        }


        UNPROTECT(nprotect_loop);
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


SEXP thispath(Rboolean verbose         , Rboolean original        ,
              Rboolean for_msg         , Rboolean get_frame_number,
              Rboolean local           , Rboolean contents        ,
              int N                    , SEXP rho                 )
{
    SEXP value = _thispath(verbose         , original        ,
                           for_msg         , get_frame_number,
                           local           , contents        ,
                           N               , rho             );
    if (!contents)
        return value;
    if (TYPEOF(value) == VECSXP) {
        if (XLENGTH(value) == 1) {
            SEXP names = getAttrib(value, R_NamesSymbol);
            if (TYPEOF(names) == STRSXP && XLENGTH(names) == 1 && STRING_ELT(names, 0) == mkChar("contents")) {
                value = VECTOR_ELT(value, 0);
                if (TYPEOF(value) != STRSXP)
                    error("internal error; invalid '%s' value", "_thispath()");
            }
        }
        return value;
    }
    if (TYPEOF(value) == STRSXP) {
        if (XLENGTH(value) != 1)
            error("internal error; invalid '%s' value", "_thispath()");
        if (STRING_ELT(value, 0) == NA_STRING)
            return R_NilValue;
        PROTECT(value);
        SEXP expr = lang2(getContentsSymbol, value);
        PROTECT(expr);
        value = eval(expr, mynamespace);
        UNPROTECT(2);
        return value;
    }
    error("internal error; invalid '%s' value", "_thispath()");
    return R_NilValue;
}


SEXP do_thispath do_formals
{
    do_start_no_op("thispath", -1);


    Rboolean verbose         , original        , for_msg         ,
             get_frame_number, local           , contents        ;
    int N;


    switch (length(args)) {
    case 0:
        verbose          = FALSE;
        original         = FALSE;
        for_msg          = FALSE;
        get_frame_number = FALSE;
        local            = FALSE;
        contents         = FALSE;
        N                = asInteger(eval(lang1(getInFrame(sys_nframeSymbol, R_BaseEnv, FALSE)), rho)) - 1;
        break;
    case 1:
        verbose          = asLogical(CAR(args)); args = CDR(args);
        original         = FALSE;
        for_msg          = FALSE;
        get_frame_number = FALSE;
        local            = FALSE;
        contents         = FALSE;
        N                = asInteger(eval(lang1(getInFrame(sys_nframeSymbol, R_BaseEnv, FALSE)), rho)) - 1;
        break;
    case 5:
        verbose          = asLogical(CAR(args)); args = CDR(args);
        original         = asLogical(CAR(args)); args = CDR(args);
        for_msg          = asLogical(CAR(args)); args = CDR(args);
        get_frame_number = FALSE;
        local            = asLogical(CAR(args)); args = CDR(args);
        contents         = asLogical(CAR(args)); args = CDR(args);
        if (local)
            N = get_sys_parent(1, rho);
        else
            N = asInteger(eval(lang1(getInFrame(sys_nframeSymbol, R_BaseEnv, FALSE)), rho)) - 1;
        break;
    default:
        errorcall(call, wrong_nargs_to_External(length(args), "C_thispath", "0, 1, or 5"));
        return R_NilValue;
    }


    return thispath(verbose         , original        , for_msg         ,
                    get_frame_number, local           , contents        ,
                    N               , rho             );
}


SEXP do_getframenumber do_formals
{
    do_start_no_call_op("getframenumber", 0);


    Rboolean verbose         , original        , for_msg         ,
             get_frame_number, local           , contents        ;
    int N;


    verbose          = FALSE;
    original         = FALSE;
    for_msg          = FALSE;
    get_frame_number = TRUE;
    contents         = FALSE;
    local            = FALSE;
    N                = asInteger(eval(lang1(getInFrame(sys_nframeSymbol, R_BaseEnv, FALSE)), rho)) - 1;


    return thispath(verbose         , original        , for_msg         ,
                    get_frame_number, local           , contents        ,
                    N               , rho             );
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
        errorcall(call, wrong_nargs_to_External(length(args), "C_inittoolsrstudio", "0 or 1"));
    }
    return ScalarLogical(init_tools_rstudio(skipCheck));
}


SEXP do_thispathrgui do_formals
{
    do_start_no_op("thispathrgui", 7);


    SEXP wintitle, untitled, r_editor;
    Rboolean verbose , original, for_msg , contents;


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


    contents = asLogical(CAR(args)); args = CDR(args);
    if (contents == NA_LOGICAL)
        errorcall(call, _("invalid '%s' value"), "contents");


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
