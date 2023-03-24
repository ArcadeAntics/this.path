#include "thispathdefn.h"


#define R_THIS_PATH_INITIALIZE_SYMBOLS
#include "symbols.h"
#undef R_THIS_PATH_INITIALIZE_SYMBOLS


SEXP mynamespace = NULL;


// LibExtern Rboolean utf8locale;
LibExtern Rboolean mbcslocale;
// LibExtern Rboolean latin1locale;


// SEXP do_utf8locale do_formals
// {
//     do_start("utf8locale", 0);
//     return ScalarLogical(utf8locale);
// }


SEXP do_mbcslocale do_formals
{
    do_start("mbcslocale", 0);
    return ScalarLogical(mbcslocale);
}


// SEXP do_latin1locale do_formals
// {
//     do_start("latin1locale", 0);
//     return ScalarLogical(latin1locale);
// }


#if R_version_at_least(4, 2, 0)
LibExtern int R_MB_CUR_MAX;
SEXP do_R_MB_CUR_MAX do_formals
{
    do_start("R_MB_CUR_MAX", 0);
    return ScalarInteger(R_MB_CUR_MAX);
}
#else
SEXP do_R_MB_CUR_MAX do_formals
{
    do_start("R_MB_CUR_MAX", 0);
    return ScalarInteger(MB_CUR_MAX);
}
#endif


SEXP do_onload do_formals
{
    do_start("onload", 2);


    /* these arguments are passed from .onLoad() */
    SEXP libname = CAR(args),
         pkgname = CADR(args);


#if R_version_at_least(3, 2, 0)
    SEXP _packageName = installChar(STRING_ELT(pkgname, 0));
#else
    SEXP _packageName = install(CHAR(STRING_ELT(pkgname, 0)));
#endif


    /* get my namespace from the namespace registry */
    mynamespace = findVarInFrame(R_NamespaceRegistry, _packageName);
    R_PreserveObject(mynamespace);


#define LockCLOENV(symbol, bindings)                           \
    do {                                                       \
        SEXP sym = (symbol);                                   \
        SEXP tmp = getInFrame(sym, mynamespace, FALSE);        \
        if (TYPEOF(tmp) != CLOSXP)                             \
            error(_("object '%s' of mode '%s' was not found"), EncodeChar(sym), "function");\
        R_LockEnvironment(CLOENV(tmp), (bindings));            \
    } while (0)


    /* get the function .shFILE and lock its environment and bindings */
    LockCLOENV(install(".shFILE"), TRUE);


    /* get the function .this.proj and lock its environment */
    LockCLOENV(install(".this.proj"), FALSE);


    /* get the function find_root and lock its environment and bindings */
    LockCLOENV(install("find_root"), TRUE);


    /* get the function .this.path.toplevel and lock its environment and bindings */
    LockCLOENV(install(".this.path.toplevel"), TRUE);


    // /* get the function eval.with.message and lock its environment */
    // LockCLOENV(install("eval.with.message"), FALSE);


    /* force the promise initwd */
    getInFrame(install("initwd"), mynamespace, FALSE);


    /* define a variable and increase its reference counter */
#define defineVarInc(symbol, value, rho)                       \
    do {                                                       \
        SEXP val = (value);                                    \
        PROTECT(val);                                          \
        defineVar((symbol), val, (rho));                       \
        INCREMENT_NAMED(val);                                  \
        UNPROTECT(1);  /* val */                               \
    } while (0)


    /* save libname and pkgname in the namespace */
    defineVarInc(install("libname"), libname, mynamespace);
    defineVarInc(install("pkgname"), pkgname, mynamespace);


    /* find and save libpath in the namespace */
    /* building the call getNamespaceInfo(pkgname, "path") and evaluating */
    SEXP expr = allocList(3);
    PROTECT(expr);
    SET_TYPEOF(expr, LANGSXP);
    SETCAR  (expr, install("getNamespaceInfo"));
    SETCADR (expr, install("pkgname"));
    SETCADDR(expr, mkString("path"));
    defineVarInc(install("libpath"), eval(expr, rho), mynamespace);
    UNPROTECT(1);  /* expr */


#define R_THIS_PATH_DEFINE_SYMBOLS
#include "symbols.h"
#undef R_THIS_PATH_DEFINE_SYMBOLS


    /* save HAVE_AQUA and PATH_MAX in my namespace */
#if defined(HAVE_AQUA)
    defineVarInc(install("HAVE_AQUA"), ScalarLogical(TRUE), mynamespace);
#else
    defineVarInc(install("HAVE_AQUA"), ScalarLogical(FALSE), mynamespace);
#endif


    defineVarInc(install("PATH_MAX"), ScalarInteger(PATH_MAX), mynamespace);


#if R_version_less_than(3, 0, 0)
    defineVarInc(install("NAMEDMAX"), ScalarInteger(NA_INTEGER), mynamespace);
#else
    defineVarInc(install("NAMEDMAX"), ScalarInteger(NAMEDMAX), mynamespace);
#endif


#define convertclosure2activebinding(symbol)                   \
    do {                                                       \
        SEXP sym = (symbol);                                   \
        SEXP fun = getInFrame(sym, mynamespace, FALSE);        \
        if (TYPEOF(fun) != CLOSXP)                             \
            error(_("object '%s' of mode '%s' was not found"), EncodeChar(sym), "function");\
        R_removeVarFromFrame(sym, mynamespace);                \
        R_MakeActiveBinding(sym, fun, mynamespace);            \
    } while (0)


    // convertclosure2activebinding(install("utf8locale"));
    convertclosure2activebinding(install("mbcslocale"));
    // convertclosure2activebinding(install("latin1locale"));
    convertclosure2activebinding(install("R_MB_CUR_MAX"));
    convertclosure2activebinding(install("utf8"));


    SEXP value = allocVector(VECSXP, 13);
    PROTECT(value);
    SEXP names = allocVector(STRSXP, 13);
    setAttrib(value, R_NamesSymbol, names);


    int i = 0;


    SET_STRING_ELT(names, i, mkChar("AIX"));
#if defined(_AIX)
    /* IBM AIX. ------------------------------------------------- */
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
#else
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#endif


//     SET_STRING_ELT(names, i, mkChar("BSD"));
// #if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
// #include <sys/param.h>
// #if defined(BSD)
//     /* BSD (DragonFly BSD, FreeBSD, OpenBSD, NetBSD). ----------- */
//     SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
// #else
//     SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
// #endif
// #else
//     SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
// #endif


    SET_STRING_ELT(names, i, mkChar("HPUX"));
#if defined(__hpux)
    /* Hewlett-Packard HP-UX. ----------------------------------- */
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
#else
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#endif


    SET_STRING_ELT(names, i, mkChar("linux"));
#if defined(__linux__)
    /* Linux. --------------------------------------------------- */
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
#else
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#endif


    SET_STRING_ELT(names, i + 0, mkChar("darwin"));
    SET_STRING_ELT(names, i + 1, mkChar("iOS.simulator"));
    SET_STRING_ELT(names, i + 2, mkChar("iOS"));
    SET_STRING_ELT(names, i + 3, mkChar("macOS"));
#if defined(__APPLE__) && defined(__MACH__)
    /* Apple OSX and iOS (Darwin). ------------------------------ */
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
#include <TargetConditionals.h>
#if TARGET_IPHONE_SIMULATOR == 1
    /* iOS in Xcode simulator */
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#elif TARGET_OS_IPHONE == 1
    /* iOS on iPhone, iPad, etc. */
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#elif TARGET_OS_MAC == 1
    /* OSX */
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
#else
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#endif
#else
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#endif


    SET_STRING_ELT(names, i, mkChar("solaris"));
#if defined(__sun) && defined(__SVR4)
    /* Solaris. ------------------------------------------------- */
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
#else
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#endif


    SET_STRING_ELT(names, i, mkChar("cygwin"));
#if defined(__CYGWIN__) && !defined(_WIN32)
    /* Cygwin POSIX under Microsoft Windows. -------------------- */
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
#else
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#endif


    SET_STRING_ELT(names, i + 0, mkChar("windows"));
    SET_STRING_ELT(names, i + 1, mkChar("win64"));
    SET_STRING_ELT(names, i + 2, mkChar("win32"));
#if defined(_WIN64)
    /* Microsoft Windows (64-bit). ------------------------------ */
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#elif defined(_WIN32)
    /* Microsoft Windows (32-bit). ------------------------------ */
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
#else
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#endif


//     SET_STRING_ELT(names, i + 0, mkChar("UNIX"));
//     SET_STRING_ELT(names, i + 1, mkChar("POSIX"));
// #if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
//     /* UNIX-style OS. ------------------------------------------- */
//     SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
// // #include <unistd.h>
// #if defined(_POSIX_VERSION)
//     /* POSIX compliant */
//     SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
// #else
//     SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
// #endif
// #else
//     SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
//     SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
// #endif


    SET_STRING_ELT(names, i, mkChar("UNIX"));
#if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
    /* UNIX-style OS. ------------------------------------------- */
    SET_VECTOR_ELT(value, i++, ScalarLogical(TRUE));
#else
    SET_VECTOR_ELT(value, i++, ScalarLogical(FALSE));
#endif


    MARK_NOT_MUTABLE(value);
    defineVar(install("OS.type"), value, mynamespace);
    UNPROTECT(1);  /* value */


    return R_NilValue;
}


SEXP do_onunload do_formals
{
    do_start("onunload", 1);


    // SEXP libpath = CAR(args);


    R_ReleaseObject(mynamespace);


    return R_NilValue;
}
