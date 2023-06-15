#include "thispathdefn.h"


#define R_THIS_PATH_INITIALIZE_SYMBOLS
#include "symbols.h"
#undef R_THIS_PATH_INITIALIZE_SYMBOLS


SEXP mynamespace = NULL,
     promiseenv  = NULL;


SEXP expr_commandArgs                               = NULL,
#if defined(R_THIS_PATH_HAVE_invisibleSymbol)
     expr_invisible                                 = NULL,
#endif
     expr_parent_frame                              = NULL,
     expr_sys_call                                  = NULL,
     expr_sys_nframe                                = NULL,
     expr_sys_parents                               = NULL,
     expr_missing_file                              = NULL,
     expr_missing_input                             = NULL,
     expr_missing_ofile                             = NULL,
     expr_info_dollar_source_path                   = NULL,
     expr_delayedAssign_x                           = NULL,
     expr_knitr_output_dir                          = NULL,
     expr_testthat_source_file_uses_brio_read_lines = NULL,
     expr__sys_path_toplevel                        = NULL,
     expr_getOption_topLevelEnvironment             = NULL,
     expr__toplevel_context_number                  = NULL;


LibExtern Rboolean mbcslocale;
// LibExtern Rboolean utf8locale;
// LibExtern Rboolean latin1locale;


SEXP do_mbcslocale do_formals
{
    do_start_no_call_op_rho("mbcslocale", 0);
    return ScalarLogical(mbcslocale);
}


// SEXP do_utf8locale do_formals
// {
//     do_start("utf8locale", 0);
//     return ScalarLogical(utf8locale);
// }


// SEXP do_latin1locale do_formals
// {
//     do_start("latin1locale", 0);
//     return ScalarLogical(latin1locale);
// }


#if R_version_at_least(4, 2, 0)
LibExtern int R_MB_CUR_MAX;
#endif
SEXP do_R_MB_CUR_MAX do_formals
{
    do_start_no_call_op_rho("R_MB_CUR_MAX", 0);
#if R_version_at_least(4, 2, 0)
    return ScalarInteger(R_MB_CUR_MAX);
#else
    return ScalarInteger(MB_CUR_MAX);
#endif
}


SEXP do_onLoad do_formals
{
    do_start_no_call_op_rho("onLoad", 2);


#define R_THIS_PATH_DEFINE_SYMBOLS
#include "symbols.h"
#undef R_THIS_PATH_DEFINE_SYMBOLS


    /* these arguments are passed from .onLoad() */
    // SEXP libname = CAR(args);  // warning: unused variable 'libname'
    SEXP pkgname = CADR(args);


#if R_version_at_least(3, 2, 0)
    SEXP _packageName = installChar(STRING_ELT(pkgname, 0));
#else
    SEXP _packageName = install(CHAR(STRING_ELT(pkgname, 0)));
#endif


    /* get my namespace from the namespace registry */
    mynamespace = findVarInFrame(R_NamespaceRegistry, _packageName);
    if (TYPEOF(mynamespace) != ENVSXP)
        error(_("not an environment"));
    R_PreserveObject(mynamespace);


#if R_version_at_least(4, 1, 0)
    promiseenv = R_NewEnv(/* enclos */ R_EmptyEnv, /* hash */ TRUE, /* size */ 1);
#else
    {
        SEXP expr = LCONS(install("new.env"),
                          CONS(/* hash */ R_TrueValue,
                               CONS(/* parent */ R_EmptyEnv,
                                    CONS(/* size */ ScalarInteger(1), R_NilValue))));
        PROTECT(expr);
        promiseenv = eval(expr, R_BaseEnv);
        UNPROTECT(1);
    }
#endif
    PROTECT(promiseenv);
    INCREMENT_NAMED_defineVar(xSymbol, R_NilValue, promiseenv);
    R_LockEnvironment(promiseenv, FALSE);
    R_PreserveObject(promiseenv);
    UNPROTECT(1);


#define LockCLOENV(symbol, bindings)                           \
    do {                                                       \
        SEXP sym = (symbol);                                   \
        SEXP tmp = getFromMyNS(sym);                           \
        if (TYPEOF(tmp) != CLOSXP)                             \
            error(_("object '%s' of mode '%s' was not found"), EncodeChar(sym), "function");\
        R_LockEnvironment(CLOENV(tmp), (bindings));            \
    } while (0)


    /* rprojroot.R */
    LockCLOENV(install(".find.root"), TRUE);
    LockCLOENV(install(".proj"), FALSE);
    /* ./R/thispath.R */
    LockCLOENV(install(".shFILE"), TRUE);
    LockCLOENV(_sys_path_toplevelSymbol, TRUE);
    /* ./R/zzz.R */
    // LockCLOENV(install("eval.with.message"), FALSE);


    /* force the promise 'initwd' */
    getFromMyNS(install("initwd"));


    /* save HAVE_AQUA, PATH_MAX, and NAMEDMAX in my namespace */
#if defined(HAVE_AQUA)
    INCREMENT_NAMED_defineVar(install(".HAVE_AQUA"), R_TrueValue, mynamespace);
#else
    INCREMENT_NAMED_defineVar(install(".HAVE_AQUA"), R_FalseValue, mynamespace);
#endif


    INCREMENT_NAMED_defineVar(install(".PATH_MAX"), PROTECT(ScalarInteger(PATH_MAX)), mynamespace);
    UNPROTECT(1);


#if R_version_less_than(3, 0, 0)
    INCREMENT_NAMED_defineVar(install(".NAMEDMAX"), PROTECT(ScalarInteger(NA_INTEGER)), mynamespace);
#else
    INCREMENT_NAMED_defineVar(install(".NAMEDMAX"), PROTECT(ScalarInteger(NAMEDMAX)), mynamespace);
#endif
    UNPROTECT(1);


#define convertclosure2activebinding(symbol)                   \
    do {                                                       \
        SEXP sym = (symbol);                                   \
        SEXP fun = getFromMyNS(sym);                           \
        if (TYPEOF(fun) != CLOSXP)                             \
            error(_("object '%s' of mode '%s' was not found"), EncodeChar(sym), "function");\
        R_removeVarFromFrame(sym, mynamespace);                \
        R_MakeActiveBinding(sym, fun, mynamespace);            \
    } while (0)


    convertclosure2activebinding(install(".mbcslocale"));
    convertclosure2activebinding(install(".utf8locale"));
    convertclosure2activebinding(install(".latin1locale"));
    convertclosure2activebinding(install(".R_MB_CUR_MAX"));
    // convertclosure2activebinding(install("FILE"));
    // convertclosure2activebinding(install("LINE"));


    SEXP value = allocVector(VECSXP, 13);
    PROTECT(value);
    SEXP names = allocVector(STRSXP, 13);
    setAttrib(value, R_NamesSymbol, names);


    int i = 0;


    SET_STRING_ELT(names, i, mkChar("AIX"));
#if defined(_AIX)
    /* IBM AIX. ------------------------------------------------- */
    SET_VECTOR_ELT(value, i++, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#endif


//     SET_STRING_ELT(names, i, mkChar("BSD"));
// #if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
// #include <sys/param.h>
// #if defined(BSD)
//     /* BSD (DragonFly BSD, FreeBSD, OpenBSD, NetBSD). ----------- */
//     SET_VECTOR_ELT(value, i++, R_TrueValue);
// #else
//     SET_VECTOR_ELT(value, i++, R_FalseValue);
// #endif
// #else
//     SET_VECTOR_ELT(value, i++, R_FalseValue);
// #endif


    SET_STRING_ELT(names, i, mkChar("HPUX"));
#if defined(__hpux)
    /* Hewlett-Packard HP-UX. ----------------------------------- */
    SET_VECTOR_ELT(value, i++, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#endif


    SET_STRING_ELT(names, i, mkChar("linux"));
#if defined(__linux__)
    /* Linux. --------------------------------------------------- */
    SET_VECTOR_ELT(value, i++, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#endif


    SET_STRING_ELT(names, i + 0, mkChar("darwin"));
    SET_STRING_ELT(names, i + 1, mkChar("iOS.simulator"));
    SET_STRING_ELT(names, i + 2, mkChar("iOS"));
    SET_STRING_ELT(names, i + 3, mkChar("macOS"));
#if defined(__APPLE__) && defined(__MACH__)
    /* Apple OSX and iOS (Darwin). ------------------------------ */
    SET_VECTOR_ELT(value, i++, R_TrueValue);
#include <TargetConditionals.h>
#if TARGET_IPHONE_SIMULATOR == 1
    /* iOS in Xcode simulator */
    SET_VECTOR_ELT(value, i++, R_TrueValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#elif TARGET_OS_IPHONE == 1
    /* iOS on iPhone, iPad, etc. */
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_TrueValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#elif TARGET_OS_MAC == 1
    /* OSX */
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#endif
#else
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#endif


    SET_STRING_ELT(names, i, mkChar("solaris"));
#if defined(__sun) && defined(__SVR4)
    /* Solaris. ------------------------------------------------- */
    SET_VECTOR_ELT(value, i++, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#endif


    SET_STRING_ELT(names, i, mkChar("cygwin"));
#if defined(__CYGWIN__) && !defined(_WIN32)
    /* Cygwin POSIX under Microsoft Windows. -------------------- */
    SET_VECTOR_ELT(value, i++, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#endif


    SET_STRING_ELT(names, i + 0, mkChar("windows"));
    SET_STRING_ELT(names, i + 1, mkChar("win64"));
    SET_STRING_ELT(names, i + 2, mkChar("win32"));
#if defined(_WIN64)
    /* Microsoft Windows (64-bit). ------------------------------ */
    SET_VECTOR_ELT(value, i++, R_TrueValue);
    SET_VECTOR_ELT(value, i++, R_TrueValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#elif defined(_WIN32)
    /* Microsoft Windows (32-bit). ------------------------------ */
    SET_VECTOR_ELT(value, i++, R_TrueValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#endif


//     SET_STRING_ELT(names, i + 0, mkChar("UNIX"));
//     SET_STRING_ELT(names, i + 1, mkChar("POSIX"));
// #if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
//     /* UNIX-style OS. ------------------------------------------- */
//     SET_VECTOR_ELT(value, i++, R_TrueValue);
// // #include <unistd.h>
// #if defined(_POSIX_VERSION)
//     /* POSIX compliant */
//     SET_VECTOR_ELT(value, i++, R_TrueValue);
// #else
//     SET_VECTOR_ELT(value, i++, R_FalseValue);
// #endif
// #else
//     SET_VECTOR_ELT(value, i++, R_FalseValue);
//     SET_VECTOR_ELT(value, i++, R_FalseValue);
// #endif


    SET_STRING_ELT(names, i, mkChar("UNIX"));
#if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
    /* UNIX-style OS. ------------------------------------------- */
    SET_VECTOR_ELT(value, i++, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i++, R_FalseValue);
#endif


    MARK_NOT_MUTABLE_defineVar(install("OS.type"), value, mynamespace);
    UNPROTECT(1);  /* value */


    expr_commandArgs = LCONS(getFromBase(commandArgsSymbol), R_NilValue);
    PROTECT(expr_commandArgs);
    if (!isFunction(CAR(expr_commandArgs)))
        error(_("object '%s' of mode '%s' was not found"),
              EncodeChar(PRINTNAME(commandArgsSymbol)), "function");
    R_PreserveObject(expr_commandArgs);
    UNPROTECT(1);


#if defined(R_THIS_PATH_HAVE_invisibleSymbol)
    expr_invisible = LCONS(getFromBase(invisibleSymbol), R_NilValue);
    PROTECT(expr_invisible);
    if (!isFunction(CAR(expr_invisible)))
        error(_("object '%s' of mode '%s' was not found"),
              EncodeChar(PRINTNAME(invisibleSymbol)), "function");
    R_PreserveObject(expr_invisible);
    UNPROTECT(1);
#endif


    expr_parent_frame = LCONS(getFromBase(parent_frameSymbol), R_NilValue);
    PROTECT(expr_parent_frame);
    if (!isFunction(CAR(expr_parent_frame)))
        error(_("object '%s' of mode '%s' was not found"),
              EncodeChar(PRINTNAME(parent_frameSymbol)), "function");
    R_PreserveObject(expr_parent_frame);
    UNPROTECT(1);


    expr_sys_call = LCONS(getFromBase(sys_callSymbol), R_NilValue);
    PROTECT(expr_sys_call);
    if (!isFunction(CAR(expr_sys_call)))
        error(_("object '%s' of mode '%s' was not found"),
              EncodeChar(PRINTNAME(sys_callSymbol)), "function");
    R_PreserveObject(expr_sys_call);
    UNPROTECT(1);


    expr_sys_nframe = LCONS(getFromBase(sys_nframeSymbol), R_NilValue);
    PROTECT(expr_sys_nframe);
    if (!isFunction(CAR(expr_sys_nframe)))
        error(_("object '%s' of mode '%s' was not found"),
              EncodeChar(PRINTNAME(sys_nframeSymbol)), "function");
    R_PreserveObject(expr_sys_nframe);
    UNPROTECT(1);


    expr_sys_parents = LCONS(getFromBase(sys_parentsSymbol), R_NilValue);
    PROTECT(expr_sys_parents);
    if (!isFunction(CAR(expr_sys_parents)))
        error(_("object '%s' of mode '%s' was not found"),
              EncodeChar(PRINTNAME(sys_parentsSymbol)), "function");
    R_PreserveObject(expr_sys_parents);
    UNPROTECT(1);


    {
        SEXP tmp;
        PROTECT(tmp = getFromBase(missingSymbol));
        expr_missing_file = LCONS(tmp, CONS(fileSymbol, R_NilValue));
        PROTECT(expr_missing_file);
        if (!isFunction(CAR(expr_missing_file)))
            error(_("object '%s' of mode '%s' was not found"),
                  EncodeChar(PRINTNAME(missingSymbol)), "function");
        R_PreserveObject(expr_missing_file);
        UNPROTECT(2);
    }


    {
        SEXP tmp;
        PROTECT(tmp = getFromBase(missingSymbol));
        expr_missing_input = LCONS(tmp, CONS(inputSymbol, R_NilValue));
        PROTECT(expr_missing_input);
        if (!isFunction(CAR(expr_missing_input)))
            error(_("object '%s' of mode '%s' was not found"),
                  EncodeChar(PRINTNAME(missingSymbol)), "function");
        R_PreserveObject(expr_missing_input);
        UNPROTECT(2);
    }


    {
        SEXP tmp;
        PROTECT(tmp = getFromBase(missingSymbol));
        expr_missing_ofile = LCONS(tmp, CONS(ofileSymbol, R_NilValue));
        PROTECT(expr_missing_ofile);
        if (!isFunction(CAR(expr_missing_ofile)))
            error(_("object '%s' of mode '%s' was not found"),
                  EncodeChar(PRINTNAME(missingSymbol)), "function");
        R_PreserveObject(expr_missing_ofile);
        UNPROTECT(2);
    }


    {
        SEXP tmp;
        PROTECT(tmp = getFromBase(R_DollarSymbol));
        expr_info_dollar_source_path = LCONS(tmp,
                                             CONS(infoSymbol,
                                                  CONS(source_pathSymbol, R_NilValue)));
        PROTECT(expr_info_dollar_source_path);
        if (!isFunction(CAR(expr_info_dollar_source_path)))
            error(_("object '%s' of mode '%s' was not found"),
                  EncodeChar(PRINTNAME(R_DollarSymbol)), "function");
        R_PreserveObject(expr_info_dollar_source_path);
        UNPROTECT(2);
    }


    {
        SEXP tmp, tmp2;
        PROTECT(tmp = getFromBase(delayedAssignSymbol));
        PROTECT(tmp2 = ScalarString(PRINTNAME(xSymbol)));
        expr_delayedAssign_x = LCONS(tmp,
                                     CONS(/* x */ tmp2,
                                          CONS(/* value */ R_NilValue,
                                               CONS(/* eval.env */ R_EmptyEnv,
                                                    CONS(/* assign.env */ promiseenv, R_NilValue)))));
        PROTECT(expr_delayedAssign_x);
        if (!isFunction(CAR(expr_delayedAssign_x)))
            error(_("object '%s' of mode '%s' was not found"),
                  EncodeChar(PRINTNAME(delayedAssignSymbol)), "function");
        R_PreserveObject(expr_delayedAssign_x);
        UNPROTECT(3);
    }


    expr_knitr_output_dir = allocList(2);
    PROTECT(expr_knitr_output_dir);
    SET_TYPEOF(expr_knitr_output_dir, LANGSXP);
    {
        SEXP tmp;
        SETCAR (expr_knitr_output_dir, tmp = allocList(3)); SET_TYPEOF(tmp, LANGSXP);
        SETCADR(expr_knitr_output_dir, mkString("output.dir"));
        {
            SEXP tmp2;
            SETCAR  (tmp, getFromBase(R_Bracket2Symbol));
            SETCADR (tmp, tmp2 = allocList(3)); SET_TYPEOF(tmp2, LANGSXP);
            SETCADDR(tmp, mkString("get"));
            {
                SETCAR  (tmp2, getFromBase(R_DoubleColonSymbol));
                SETCADR (tmp2, knitrSymbol);
                SETCADDR(tmp2, install("opts_knit"));
            }
        }
    }
    R_PreserveObject(expr_knitr_output_dir);
    UNPROTECT(1);


    expr_testthat_source_file_uses_brio_read_lines = allocList(3);
    PROTECT(expr_testthat_source_file_uses_brio_read_lines);
    SET_TYPEOF(expr_testthat_source_file_uses_brio_read_lines, LANGSXP);
    {
        SEXP tmp;
        SETCAR  (expr_testthat_source_file_uses_brio_read_lines, getFromBase(install(">=")));
        SETCADR (expr_testthat_source_file_uses_brio_read_lines, tmp = allocList(2)); SET_TYPEOF(tmp, LANGSXP);
        SETCADDR(expr_testthat_source_file_uses_brio_read_lines, mkString("3.1.2"));
        {
            SEXP tmp2;
            SETCAR (tmp, getFromBase(install("as.numeric_version")));
            SETCADR(tmp, tmp2 = allocList(2)); SET_TYPEOF(tmp2, LANGSXP);
            {
                SETCAR (tmp2, getFromBase(install("getNamespaceVersion")));
                SETCADR(tmp2, ScalarString(PRINTNAME(testthatSymbol)));
            }
        }
    }
    R_PreserveObject(expr_testthat_source_file_uses_brio_read_lines);
    UNPROTECT(1);


    expr__sys_path_toplevel = LCONS(_sys_path_toplevelSymbol, R_NilValue);
    PROTECT(expr__sys_path_toplevel);
    R_PreserveObject(expr__sys_path_toplevel);
    UNPROTECT(1);


    {
        SEXP tmp;
        PROTECT(tmp = getFromBase(install("getOption")));
        expr_getOption_topLevelEnvironment = LCONS(tmp, CONS(mkString("topLevelEnvironment"), R_NilValue));
        PROTECT(expr_getOption_topLevelEnvironment);
        if (!isFunction(CAR(expr_getOption_topLevelEnvironment)))
            error(_("object '%s' of mode '%s' was not found"), "getOption", "function");
        R_PreserveObject(expr_getOption_topLevelEnvironment);
        UNPROTECT(2);
    }


    expr__toplevel_context_number = LCONS(getFromMyNS(install(".toplevel.context.number")), R_NilValue);
    PROTECT(expr__toplevel_context_number);
    if (!isFunction(CAR(expr__toplevel_context_number)))
        error(_("object '%s' of mode '%s' was not found"), ".toplevel.context.number", "function");
    R_PreserveObject(expr__toplevel_context_number);
    UNPROTECT(1);


    return R_NilValue;
}


SEXP do_onUnload do_formals
{
    do_start_no_call_op_rho("onUnload", 1);


    // SEXP libpath = CAR(args);


    R_ReleaseObject(mynamespace);
    R_ReleaseObject(promiseenv);


    R_ReleaseObject(expr_commandArgs                              );
#if defined(R_THIS_PATH_HAVE_invisibleSymbol)
    R_ReleaseObject(expr_invisible                                );
#endif
    R_ReleaseObject(expr_parent_frame                             );
    R_ReleaseObject(expr_sys_call                                 );
    R_ReleaseObject(expr_sys_nframe                               );
    R_ReleaseObject(expr_sys_parents                              );
    R_ReleaseObject(expr_missing_file                             );
    R_ReleaseObject(expr_missing_input                            );
    R_ReleaseObject(expr_missing_input                            );

    R_ReleaseObject(expr_info_dollar_source_path                  );
    R_ReleaseObject(expr_delayedAssign_x                          );
    R_ReleaseObject(expr_knitr_output_dir                         );
    R_ReleaseObject(expr_testthat_source_file_uses_brio_read_lines);
    R_ReleaseObject(expr__sys_path_toplevel                       );
    R_ReleaseObject(expr_getOption_topLevelEnvironment            );
    R_ReleaseObject(expr__toplevel_context_number                 );


    return R_NilValue;
}
