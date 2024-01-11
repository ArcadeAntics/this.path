#include "thispathdefn.h"


#define R_THIS_PATH_INITIALIZE_SYMBOLS
#include "symbols.h"


SEXP mynamespace = NULL,
     DocumentContextClass = NULL,
     ThisPathInAQUAErrorClass                      = NULL,
     ThisPathInZipFileErrorClass                   = NULL,
     ThisPathNotExistsErrorClass                   = NULL,
     ThisPathNotFoundErrorClass                    = NULL,
     ThisPathNotImplementedErrorClass              = NULL,
     ThisPathUnrecognizedConnectionClassErrorClass = NULL,
     ThisPathUnrecognizedMannerErrorClass          = NULL,
     last_condition = NULL,
     _custom_gui_path_character_environment = NULL,
     _custom_gui_path_function_environment  = NULL;


#ifdef R_THIS_PATH_NEED_BLANKSCALARSTRING
SEXP R_BlankScalarString = NULL;
#endif


SEXP expr_commandArgs                               = NULL,
     expr_invisible                                 = NULL,
     expr_parent_frame                              = NULL,
     expr_sys_call                                  = NULL,
     expr_sys_call_which                            = NULL,
     expr_sys_function_which                        = NULL,
     eval_op                                        = NULL,
     expr_sys_nframe                                = NULL,
     expr_sys_parents                               = NULL,
     expr_missing_file                              = NULL,
     expr_missing_input                             = NULL,
     expr_missing_ofile                             = NULL,
     expr_info_dollar_source_path                   = NULL,
     expr_knitr_output_dir                          = NULL,
     expr_testthat_source_file_uses_brio_read_lines = NULL,
     expr_getOption_topLevelEnvironment             = NULL,
     expr__toplevel_nframe                          = NULL,
     expr__isMethodsDispatchOn                      = NULL,
     expr_UseMethod_lengths                         = NULL;


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


SEXP _packageName;


SEXP do_onLoad do_formals
{
    do_start_no_call_op_rho("onLoad", 2);


    static int been_here_before = 0;
    if (been_here_before)
        error("cannot call 'onLoad' more than once (wtf are you doing\?\?\?)");
    been_here_before = 1;


#define R_THIS_PATH_DEFINE_SYMBOLS
#include "symbols.h"


    /* these arguments are passed from .onLoad() */
    // SEXP libname = CAR(args);  // warning: unused variable 'libname'
    SEXP pkgname = CADR(args);


#if R_version_at_least(3, 2, 0)
    _packageName = installChar(STRING_ELT(pkgname, 0));
#else
    _packageName = install(CHAR(STRING_ELT(pkgname, 0)));
#endif


#ifdef R_VERSION
    {
        SEXP expr = LCONS(install("getRversion"), R_NilValue);
        PROTECT(expr);
        SEXP v = eval(expr, R_BaseEnv);
        PROTECT(v);
        if (IS_SCALAR(v, VECSXP)) {
            v = VECTOR_ELT(v, 0);
            if (TYPEOF(v) == INTSXP && LENGTH(v) == 3) {
                int *iv = INTEGER(v);
                if (iv[0] == atoi(R_MAJOR) &&
                    iv[1] == atoi(R_MINOR));
                else warningcall_immediate(R_NilValue,
                    "package '%s' was built under R version %s.%s\n but is being loaded in R %d.%d.%d",
                    CHAR(PRINTNAME(_packageName)), R_MAJOR, R_MINOR, iv[0], iv[1], iv[2]);
            }
        }
        UNPROTECT(2);
    }
#endif


    /* get my namespace from the namespace registry */
    mynamespace = findVarInFrame(R_NamespaceRegistry, _packageName);
    if (TYPEOF(mynamespace) != ENVSXP)
        error(_("not an environment"));
    R_PreserveObject(mynamespace);


    INCREMENT_NAMED_defineVar(install(".mynamespace"), mynamespace, mynamespace);


#define make_STRSXP_from_char_array(var, ...)                  \
    do {                                                       \
        const char *Class[] = __VA_ARGS__;                     \
        int nClass = 0;                                        \
        while (Class[nClass]) ++nClass;                        \
        var = allocVector(STRSXP, nClass);                     \
        R_PreserveObject(var);                                 \
        for (int i = 0; i < nClass; i++)                       \
            SET_STRING_ELT(var, i, mkChar(Class[i]));          \
        MARK_NOT_MUTABLE(var);                                 \
    } while (0)


    make_STRSXP_from_char_array(
        DocumentContextClass,
        { "ThisPathDocumentContext", "environment", NULL }
    );


/* this code is written this way on purpose, do not reformat */
#define NotImplementedErrorClass_string                        \
    "NotImplementedError"
#define ThisPathInAQUAErrorClass_string                        \
    "ThisPathInAQUAError"
#define ThisPathInZipFileErrorClass_string                     \
    "ThisPathInZipFileError"
#define ThisPathNotExistsErrorClass_string                     \
    "ThisPathNotExistsError"
#define ThisPathNotFoundErrorClass_string                      \
    "ThisPathNotFoundError"
#define ThisPathNotImplementedErrorClass_string                \
    "ThisPathNotImplementedError"
#define ThisPathUnrecognizedConnectionClassErrorClass_string   \
    "ThisPathUnrecognizedConnectionClassError"
#define ThisPathUnrecognizedMannerErrorClass_string            \
    "ThisPathUnrecognizedMannerError"


/* new names of the error classes along with the old names */
#define NotImplementedErrorClass_strings                       \
    NotImplementedErrorClass_string,                           \
    "notImplementedError"
#define ThisPathInAQUAErrorClass_strings                       \
    ThisPathInAQUAErrorClass_string,                           \
    "this.path::thisPathInAQUAError"
#define ThisPathInZipFileErrorClass_strings                    \
    ThisPathInZipFileErrorClass_string,                        \
    "this.path::thisPathInZipFileError"
#define ThisPathNotExistsErrorClass_strings                    \
    ThisPathNotExistsErrorClass_string,                        \
    "thisPathNotExistsError",                                  \
    "this.path::thisPathNotExistsError",                       \
    "this.path::thisPathNotExistError",                        \
    "this.path_this.path_not_exists_error"
#define ThisPathNotFoundErrorClass_strings                     \
    ThisPathNotFoundErrorClass_string,                         \
    "thisPathNotFoundError",                                   \
    "this.path::thisPathNotFoundError"
#define ThisPathNotImplementedErrorClass_strings               \
    ThisPathNotImplementedErrorClass_string,                   \
    "this.path::thisPathNotImplementedError",                  \
    "this.path_this.path_unimplemented_error"
#define ThisPathUnrecognizedConnectionClassErrorClass_strings  \
    ThisPathUnrecognizedConnectionClassErrorClass_string,      \
    "this.path::thisPathUnrecognizedConnectionClassError"
#define ThisPathUnrecognizedMannerErrorClass_strings           \
    ThisPathUnrecognizedMannerErrorClass_string,               \
    "this.path::thisPathUnrecognizedMannerError"


#define ErrorClass_strings                                     \
    "error", "condition", NULL


    make_STRSXP_from_char_array(
        ThisPathInAQUAErrorClass,
        {
            ThisPathInAQUAErrorClass_strings,
            ThisPathNotFoundErrorClass_strings,
            ThisPathNotImplementedErrorClass_strings,
            NotImplementedErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathInZipFileErrorClass,
        {
            ThisPathInZipFileErrorClass_strings,
            ThisPathNotFoundErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathNotExistsErrorClass,
        {
            ThisPathNotExistsErrorClass_strings,
            ThisPathNotFoundErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathNotFoundErrorClass,
        {
            ThisPathNotFoundErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathNotImplementedErrorClass,
        {
            ThisPathNotImplementedErrorClass_strings,
            NotImplementedErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathUnrecognizedConnectionClassErrorClass,
        {
            ThisPathUnrecognizedConnectionClassErrorClass_strings,
            ThisPathNotFoundErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathUnrecognizedMannerErrorClass,
        {
            ThisPathUnrecognizedMannerErrorClass_strings,
            ThisPathNotFoundErrorClass_strings,
            ErrorClass_strings
        }
    );


    /* it might seem more intuitive to say
     * last_condition = R_NilValue;
     *
     * but that means every time last_condition gets updated,
     * we must release the old SEXP and preserve the new one
     *
     * this is preferable because we only preserve and release one object
     */
    last_condition = CONS(R_NilValue, R_NilValue);
    R_PreserveObject(last_condition);


    _custom_gui_path_character_environment =
        R_NewEnv(/* enclos */ mynamespace, /* hash */ TRUE, /* size */ 10);
    R_PreserveObject(_custom_gui_path_character_environment);
    defineVar(guinameSymbol, R_MissingArg, _custom_gui_path_character_environment);
    {
        SEXP na = ScalarString(NA_STRING);
        PROTECT(na);
        ENSURE_NAMEDMAX(na);
        defineVar(ofileSymbol, makeEVPROMISE(na, na), _custom_gui_path_character_environment);
        R_LockBinding(ofileSymbol, _custom_gui_path_character_environment);
        UNPROTECT(1);
    }
    defineVar(fileSymbol, makePROMISE(
        LCONS(_normalizeNotDirectorySymbol, CONS(ofileSymbol, R_NilValue)),
        _custom_gui_path_character_environment
    ), _custom_gui_path_character_environment);
    R_LockBinding(fileSymbol, _custom_gui_path_character_environment);
    defineVar(_getContentsSymbol, R_NilValue, _custom_gui_path_character_environment);
    R_LockEnvironment(_custom_gui_path_character_environment, FALSE);


    _custom_gui_path_function_environment =
        R_NewEnv(/* enclos */ R_EmptyEnv, /* hash */ TRUE, /* size */ 2);
    R_PreserveObject(_custom_gui_path_function_environment);
    defineVar(_custom_gui_path_functionSymbol, R_NilValue, _custom_gui_path_function_environment);
    R_LockEnvironment(_custom_gui_path_function_environment, FALSE);


#ifdef R_THIS_PATH_NEED_BLANKSCALARSTRING
    R_BlankScalarString = ScalarString(R_BlankString);
    R_PreserveObject(R_BlankScalarString);
#endif


#define LockCLOENV(symbol, bindings)                           \
    do {                                                       \
        SEXP sym = (symbol);                                   \
        SEXP tmp = getFromMyNS(sym);                           \
        if (TYPEOF(tmp) != CLOSXP)                             \
            error(_("object '%s' of mode '%s' was not found"), EncodeChar(sym), "function");\
        R_LockEnvironment(CLOENV(tmp), (bindings));            \
    } while (0)


    /* rprojroot.R */
    LockCLOENV(install(".find_root"), TRUE);
    LockCLOENV(install(".proj"), FALSE);
    /* startup.R */
    LockCLOENV(_site_fileSymbol, TRUE);
    LockCLOENV(install(".in_site_file"), FALSE);
    LockCLOENV(_init_fileSymbol, TRUE);
    /* thispath.R */
    LockCLOENV(_shFILESymbol, TRUE);
    LockCLOENV(_jupyter_pathSymbol, TRUE);
    LockCLOENV(install(".emacs_path"), TRUE);
    /* zzz.R */
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
    UNPROTECT(1);
#else
    INCREMENT_NAMED_defineVar(install(".NAMEDMAX"), PROTECT(ScalarInteger(NAMEDMAX)), mynamespace);
    UNPROTECT(1);
#endif


#define convertclosure2activebinding(symbol)                   \
    do {                                                       \
        SEXP sym = (symbol);                                   \
        SEXP fun = getFromMyNS(sym);                           \
        if (TYPEOF(fun) != CLOSXP)                             \
            error(_("object '%s' of mode '%s' was not found"), EncodeChar(sym), "function");\
        R_removeVarFromFrame(sym, mynamespace);                \
        R_MakeActiveBinding(sym, fun, mynamespace);            \
    } while (0)


    /* ./R/ns-hooks.R */
    convertclosure2activebinding(install(".mbcslocale"));
    convertclosure2activebinding(install(".utf8locale"));
    convertclosure2activebinding(install(".latin1locale"));
    convertclosure2activebinding(install(".R_MB_CUR_MAX"));
    /* ./R/startup.R */
    convertclosure2activebinding(install(".in_site_file"));
    /* ./R/trycatch.R */
    convertclosure2activebinding(install("last.condition"));


    SEXP value = allocVector(VECSXP, 13);
    MARK_NOT_MUTABLE_defineVar(install("OS.type"), value, mynamespace);
    SEXP names = allocVector(STRSXP, 13);
    setAttrib(value, R_NamesSymbol, names);


    int i = -1;


    SET_STRING_ELT(names, ++i, mkChar("AIX"));
#if defined(_AIX)
    /* IBM AIX. ------------------------------------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


//     SET_STRING_ELT(names, ++i, mkChar("BSD"));
// #if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
// #include <sys/param.h>
// #if defined(BSD)
//     /* BSD (DragonFly BSD, FreeBSD, OpenBSD, NetBSD). ----------- */
//     SET_VECTOR_ELT(value, i, R_TrueValue);
// #else
//     SET_VECTOR_ELT(value, i, R_FalseValue);
// #endif
// #else
//     SET_VECTOR_ELT(value, i, R_FalseValue);
// #endif


    SET_STRING_ELT(names, ++i, mkChar("HPUX"));
#if defined(__hpux)
    /* Hewlett-Packard HP-UX. ----------------------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


    SET_STRING_ELT(names, ++i, mkChar("linux"));
#if defined(__linux__)
    /* Linux. --------------------------------------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


    SET_STRING_ELT(names, i + 1, mkChar("darwin"));
    SET_STRING_ELT(names, i + 2, mkChar("iOS.simulator"));
    SET_STRING_ELT(names, i + 3, mkChar("iOS"));
    SET_STRING_ELT(names, i + 4, mkChar("macOS"));
#if defined(__APPLE__) && defined(__MACH__)
    /* Apple OSX and iOS (Darwin). ------------------------------ */
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
#include <TargetConditionals.h>
#if TARGET_IPHONE_SIMULATOR == 1
    /* iOS in Xcode simulator */
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#elif TARGET_OS_IPHONE == 1
    /* iOS on iPhone, iPad, etc. */
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#elif TARGET_OS_MAC == 1
    /* OSX */
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#endif
#else /* #if defined(__APPLE__) && defined(__MACH__) */
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#endif


    SET_STRING_ELT(names, ++i, mkChar("solaris"));
#if defined(__sun) && defined(__SVR4)
    /* Solaris. ------------------------------------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


    SET_STRING_ELT(names, ++i, mkChar("cygwin"));
#if defined(__CYGWIN__) && !defined(_WIN32)
    /* Cygwin POSIX under Microsoft Windows. -------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


    SET_STRING_ELT(names, i + 1, mkChar("windows"));
    SET_STRING_ELT(names, i + 2, mkChar("win64"));
    SET_STRING_ELT(names, i + 3, mkChar("win32"));
#if defined(_WIN64)
    /* Microsoft Windows (64-bit). ------------------------------ */
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#elif defined(_WIN32)
    /* Microsoft Windows (32-bit). ------------------------------ */
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#endif


//     SET_STRING_ELT(names, i + 1, mkChar("UNIX"));
//     SET_STRING_ELT(names, i + 2, mkChar("POSIX"));
// #if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
//     /* UNIX-style OS. ------------------------------------------- */
//     SET_VECTOR_ELT(value, ++i, R_TrueValue);
// // #include <unistd.h>
// #if defined(_POSIX_VERSION)
//     /* POSIX compliant */
//     SET_VECTOR_ELT(value, ++i, R_TrueValue);
// #else
//     SET_VECTOR_ELT(value, ++i, R_FalseValue);
// #endif
// #else
//     SET_VECTOR_ELT(value, ++i, R_FalseValue);
//     SET_VECTOR_ELT(value, ++i, R_FalseValue);
// #endif


    SET_STRING_ELT(names, ++i, mkChar("UNIX"));
#if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
    /* UNIX-style OS. ------------------------------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


    expr_commandArgs = LCONS(getFromBase(commandArgsSymbol), R_NilValue);
    R_PreserveObject(expr_commandArgs);
    if (!isFunction(CAR(expr_commandArgs)))
        error(_("object '%s' of mode '%s' was not found"),
              CHAR(PRINTNAME(commandArgsSymbol)), "function");


    expr_invisible = LCONS(getFromBase(invisibleSymbol), R_NilValue);
    R_PreserveObject(expr_invisible);
    if (!isFunction(CAR(expr_invisible)))
        error(_("object '%s' of mode '%s' was not found"),
              CHAR(PRINTNAME(invisibleSymbol)), "function");


    expr_parent_frame = LCONS(getFromBase(parent_frameSymbol), R_NilValue);
    R_PreserveObject(expr_parent_frame);
    if (!isFunction(CAR(expr_parent_frame)))
        error(_("object '%s' of mode '%s' was not found"),
              CHAR(PRINTNAME(parent_frameSymbol)), "function");


    expr_sys_call = LCONS(getFromBase(sys_callSymbol), R_NilValue);
    R_PreserveObject(expr_sys_call);
    if (!isFunction(CAR(expr_sys_call)))
        error(_("object '%s' of mode '%s' was not found"),
              CHAR(PRINTNAME(sys_callSymbol)), "function");


    expr_sys_call_which = LCONS(CAR(expr_sys_call), CONS(ScalarInteger(0), R_NilValue));
    R_PreserveObject(expr_sys_call_which);


    {
        expr_sys_function_which = LCONS(getFromBase(sys_functionSymbol), CDR(expr_sys_call_which));
        R_PreserveObject(expr_sys_function_which);
    }


    eval_op = INTERNAL(R_EvalSymbol);
    if (TYPEOF(eval_op) != BUILTINSXP)
        error(_("object '%s' of mode '%s' was not found"),
              CHAR(PRINTNAME(R_EvalSymbol)), "builtin");


    expr_sys_nframe = LCONS(getFromBase(sys_nframeSymbol), R_NilValue);
    R_PreserveObject(expr_sys_nframe);
    if (!isFunction(CAR(expr_sys_nframe)))
        error(_("object '%s' of mode '%s' was not found"),
              CHAR(PRINTNAME(sys_nframeSymbol)), "function");


    expr_sys_parents = LCONS(getFromBase(sys_parentsSymbol), R_NilValue);
    R_PreserveObject(expr_sys_parents);
    if (!isFunction(CAR(expr_sys_parents)))
        error(_("object '%s' of mode '%s' was not found"),
              CHAR(PRINTNAME(sys_parentsSymbol)), "function");


    {
        SEXP tmp;
        PROTECT(tmp = getFromBase(missingSymbol));
        expr_missing_file = LCONS(tmp, CONS(fileSymbol, R_NilValue));
        R_PreserveObject(expr_missing_file);
        UNPROTECT(1);
        if (!isFunction(CAR(expr_missing_file)))
            error(_("object '%s' of mode '%s' was not found"),
                  CHAR(PRINTNAME(missingSymbol)), "function");
    }


    {
        SEXP tmp;
        PROTECT(tmp = getFromBase(missingSymbol));
        expr_missing_input = LCONS(tmp, CONS(inputSymbol, R_NilValue));
        R_PreserveObject(expr_missing_input);
        UNPROTECT(1);
        if (!isFunction(CAR(expr_missing_input)))
            error(_("object '%s' of mode '%s' was not found"),
                  CHAR(PRINTNAME(missingSymbol)), "function");
    }


    {
        SEXP tmp;
        PROTECT(tmp = getFromBase(missingSymbol));
        expr_missing_ofile = LCONS(tmp, CONS(ofileSymbol, R_NilValue));
        R_PreserveObject(expr_missing_ofile);
        UNPROTECT(1);
        if (!isFunction(CAR(expr_missing_ofile)))
            error(_("object '%s' of mode '%s' was not found"),
                  CHAR(PRINTNAME(missingSymbol)), "function");
    }


    {
        SEXP tmp;
        PROTECT(tmp = getFromBase(R_DollarSymbol));
        expr_info_dollar_source_path = LCONS(tmp,
                                             CONS(infoSymbol,
                                                  CONS(source_pathSymbol, R_NilValue)));
        R_PreserveObject(expr_info_dollar_source_path);
        UNPROTECT(1);
        if (!isFunction(CAR(expr_info_dollar_source_path)))
            error(_("object '%s' of mode '%s' was not found"),
                  CHAR(PRINTNAME(R_DollarSymbol)), "function");
    }


    expr_knitr_output_dir = allocList(2);
    R_PreserveObject(expr_knitr_output_dir);
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


    expr_testthat_source_file_uses_brio_read_lines = allocList(3);
    R_PreserveObject(expr_testthat_source_file_uses_brio_read_lines);
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


    {
        SEXP tmp;
        PROTECT(tmp = getFromBase(install("getOption")));
        expr_getOption_topLevelEnvironment = LCONS(tmp, CONS(mkString("topLevelEnvironment"), R_NilValue));
        R_PreserveObject(expr_getOption_topLevelEnvironment);
        UNPROTECT(1);
        if (!isFunction(CAR(expr_getOption_topLevelEnvironment)))
            error(_("object '%s' of mode '%s' was not found"), "getOption", "function");
    }


    expr__toplevel_nframe = LCONS(getFromMyNS(install(".toplevel.nframe")), R_NilValue);
    R_PreserveObject(expr__toplevel_nframe);
    if (!isFunction(CAR(expr__toplevel_nframe)))
        error(_("object '%s' of mode '%s' was not found"), ".toplevel.nframe", "function");


    expr__isMethodsDispatchOn = LCONS(getFromBase(_isMethodsDispatchOnSymbol), R_NilValue);
    R_PreserveObject(expr__isMethodsDispatchOn);
    if (!isFunction(CAR(expr__isMethodsDispatchOn)))
        error(_("object '%s' of mode '%s' was not found"),
            CHAR(PRINTNAME(_isMethodsDispatchOnSymbol)), "function");


#if R_version_less_than(3, 2, 0)
    expr_UseMethod_lengths = LCONS(UseMethodSymbol, CONS(mkString("lengths"), R_NilValue));
    R_PreserveObject(expr_UseMethod_lengths);
#endif


    {
        /* if package:utils is loaded, call '.fix_utils' */
        if (!ISUNBOUND(findVarInFrame(R_NamespaceRegistry, utilsSymbol))) {
            SEXP expr = LCONS(install(".fix_utils"), R_NilValue);
            PROTECT(expr);
            eval(expr, mynamespace);
            UNPROTECT(1);
        }


        /* for when package:utils is loaded (or possibly unloaded then reloaded), set as a hook */
        SEXP expr = LCONS(install(".maybe_setHook_packageEvent_utils_fix_utils"), R_NilValue);
        PROTECT(expr);
        eval(expr, mynamespace);
        UNPROTECT(1);
    }


    {
        /* if package:plumber is loaded, call '.fix_plumber_parseUTF8' */
        if (!ISUNBOUND(findVarInFrame(R_NamespaceRegistry, plumberSymbol))) {
            SEXP expr = LCONS(install(".fix_plumber_parseUTF8"), R_NilValue);
            PROTECT(expr);
            eval(expr, mynamespace);
            UNPROTECT(1);
        }


        /* for when package:plumber is loaded (or possibly unloaded then reloaded), set as a hook */
        SEXP expr = LCONS(install(".maybe_setHook_packageEvent_plumber_fix_plumber_parseUTF8"), R_NilValue);
        PROTECT(expr);
        eval(expr, mynamespace);
        UNPROTECT(1);
    }


    return R_NilValue;
}


SEXP do_onUnload do_formals
{
    do_start_no_call_op("onUnload", 1);


    SEXP libpath = CAR(args);


#define maybe_release(var) if ((var)) R_ReleaseObject((var))


    maybe_release(mynamespace);
    maybe_release(DocumentContextClass);
    maybe_release(ThisPathInAQUAErrorClass);
    maybe_release(ThisPathInZipFileErrorClass);
    maybe_release(ThisPathNotExistsErrorClass);
    maybe_release(ThisPathNotFoundErrorClass);
    maybe_release(ThisPathNotImplementedErrorClass);
    maybe_release(ThisPathUnrecognizedConnectionClassErrorClass);
    maybe_release(ThisPathUnrecognizedMannerErrorClass);
    maybe_release(last_condition);
    maybe_release(_custom_gui_path_character_environment);
    maybe_release(_custom_gui_path_function_environment);


#ifdef R_THIS_PATH_NEED_BLANKSCALARSTRING
    maybe_release(R_BlankScalarString);
#endif


    maybe_release(expr_commandArgs);
    maybe_release(expr_invisible);
    maybe_release(expr_parent_frame);
    maybe_release(expr_sys_call);
    maybe_release(expr_sys_call_which);
    maybe_release(expr_sys_function_which);
    maybe_release(expr_sys_nframe);
    maybe_release(expr_sys_parents);
    maybe_release(expr_missing_file);
    maybe_release(expr_missing_input);
    maybe_release(expr_missing_input);
    maybe_release(expr_info_dollar_source_path);
    maybe_release(expr_knitr_output_dir);
    maybe_release(expr_testthat_source_file_uses_brio_read_lines);
    maybe_release(expr_getOption_topLevelEnvironment);
    maybe_release(expr__toplevel_nframe);
    maybe_release(expr__isMethodsDispatchOn);
    maybe_release(expr_UseMethod_lengths);


    {
        SEXP expr;
        PROTECT_INDEX indx;
        PROTECT_WITH_INDEX(expr = CONS(libpath, R_NilValue), &indx);
        REPROTECT(expr = CONS(ScalarString(PRINTNAME(_packageName)), expr), indx);
        REPROTECT(expr = LCONS(getFromBase(install("library.dynam.unload")), expr), indx);
        REPROTECT(expr = CONS(expr, R_NilValue), indx);
        REPROTECT(expr = LCONS(getFromBase(on_exitSymbol), expr), indx);
        eval(expr, rho);
        UNPROTECT(1);
    }


    return R_NilValue;
}
