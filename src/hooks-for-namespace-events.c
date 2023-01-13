#include <R.h>
#include <Rinternals.h>
#include "translations.h"


SEXP
    thispathofileSymbol           = NULL,
    thispathfileSymbol            = NULL,
    thispathformsgSymbol          = NULL,
    thispatherrorSymbol           = NULL,
    thispathassocwfileSymbol      = NULL,
    thispathdoneSymbol            = NULL,
    insidesourcewashereSymbol     = NULL,
    thispathnSymbol               = NULL,
    _normalizePathSymbol          = NULL,
    _normalizeAgainstSymbol       = NULL,
    stopSymbol                    = NULL,
    delayedAssignSymbol           = NULL,
    normalizePathSymbol           = NULL,
    winslashSymbol                = NULL,
    mustWorkSymbol                = NULL,
    normalizeURL_1Symbol          = NULL,
    sourceSymbol                  = NULL,
    sys_sourceSymbol              = NULL,
    gui_rstudioSymbol             = NULL,
    init_tools_rstudioSymbol      = NULL,
    tools_rstudioSymbol           = NULL,
    _rs_api_getActiveDocumentContextSymbol = NULL,
    _rs_api_getSourceEditorContextSymbol = NULL,
    debugSourceSymbol             = NULL,
    testthatSymbol                = NULL,
    source_fileSymbol             = NULL,
    testthat_uses_brioSymbol      = NULL,
    knitr_output_dirSymbol        = NULL,
    knitrSymbol                   = NULL,
    knitSymbol                    = NULL,
    wrap_sourceSymbol             = NULL,
    sys_callSymbol                = NULL,
    sys_frameSymbol               = NULL,
    sys_functionSymbol            = NULL,
    sys_nframeSymbol              = NULL,
    sys_parentSymbol              = NULL,
    sys_parentsSymbol             = NULL,
    ofileSymbol                   = NULL,
    owdSymbol                     = NULL,
    old_dirSymbol                 = NULL,
    fileSymbol                    = NULL,
    fileNameSymbol                = NULL,
    pathSymbol                    = NULL,
    inputSymbol                   = NULL,
    missingSymbol                 = NULL,
    returnSymbol                  = NULL,
    this_path_toplevelSymbol      = NULL,
    encodeStringSymbol            = NULL,
    na_encodeSymbol               = NULL,
    exprSymbol                    = NULL,
    on_exitSymbol                 = NULL,
    External2Symbol               = NULL,
    C_setprseen2Symbol            = NULL,
    thispathtempSymbol            = NULL,
    parent_frameSymbol            = NULL,
    invisibleSymbol               = NULL,
    getConnectionSymbol           = NULL,
    as_environmentSymbol          = NULL,
    oenvirSymbol                  = NULL,
    withArgsSymbol                = NULL,
    thispathhelperSymbol          = NULL,
    GetConnectionSymbol           = NULL,
    GetUnderlyingConnectionSymbol = NULL,
    summary_connectionSymbol      = NULL,
    require_this_path_helperSymbol= NULL,
    _asArgsSymbol                 = NULL,
    commandArgsSymbol             = NULL,
    maybe_in_shellSymbol          = NULL,
    _packageName                  = NULL;


SEXP mynamespace = NULL;


extern SEXP getInFrame(SEXP sym, SEXP env, int unbound_ok);


SEXP do_onload(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* these arguments are passed from .onLoad() */
    SEXP libname = CADR(args),
         pkgname = CADDR(args);


    _packageName = installChar(STRING_ELT(pkgname, 0));


    /* get my namespace from the namespace registry */
    mynamespace = findVarInFrame(R_NamespaceRegistry, _packageName);
    R_PreserveObject(mynamespace);


    /* get the function .shFILE and lock its environment and bindings */
    SEXP _shFILE = getInFrame(install(".shFILE"), mynamespace, FALSE);
    if (TYPEOF(_shFILE) != CLOSXP)
        error(_("object '%s' of mode '%s' was not found"), ".shFILE", "function");
    R_LockEnvironment(CLOENV(_shFILE), TRUE);


    /* get the function .this.proj and lock its environment */
    SEXP _this_proj = getInFrame(install(".this.proj"), mynamespace, FALSE);
    if (TYPEOF(_this_proj) != CLOSXP)
        error(_("object '%s' of mode '%s' was not found"), ".this.proj", "function");
    R_LockEnvironment(CLOENV(_this_proj), FALSE);


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
    /* building the call getNamespaceInfo(pkgname, "path") and evaluating in the base environment */
    SEXP expr = allocList(3);
    PROTECT(expr);
    SET_TYPEOF(expr, LANGSXP);
    SETCAR  (expr, install("getNamespaceInfo"));
    SETCADR (expr, pkgname);
    SETCADDR(expr, mkString("path"));
    defineVarInc(install("libpath"), eval(expr, R_BaseEnv), mynamespace);
    UNPROTECT(1);  /* expr */


    /* code is written this way on purpose, do not reformat */
#define thispathofileChar                                      \
    "._this.path::ofile_."
#define thispathfileChar                                       \
    "._this.path::file_."
#define thispathformsgChar                                     \
    "._this.path::for msg_."
#define thispatherrorChar                                      \
    "._this.path::error_."
#define thispathassocwfileChar                                 \
    "._this.path::associated with file_."
#define thispathdoneChar                                       \
    "._this.path::done_."
#define insidesourcewashereChar                                \
    "._this.path::inside.source() was here_."
#define thispathnChar                                          \
    "._this.path::n_."
    thispathofileSymbol           = install(thispathofileChar);
    thispathfileSymbol            = install(thispathfileChar);
    thispathformsgSymbol          = install(thispathformsgChar);
    thispatherrorSymbol           = install(thispatherrorChar);
    thispathassocwfileSymbol      = install(thispathassocwfileChar);
    thispathdoneSymbol            = install(thispathdoneChar);
    insidesourcewashereSymbol     = install(insidesourcewashereChar);
    thispathnSymbol               = install(thispathnChar);
    _normalizePathSymbol          = install(".normalizePath");
    _normalizeAgainstSymbol       = install(".normalizeAgainst");
    stopSymbol                    = install("stop");
    delayedAssignSymbol           = install("delayedAssign");
    normalizePathSymbol           = install("normalizePath");
    winslashSymbol                = install("winslash");
    mustWorkSymbol                = install("mustWork");
    normalizeURL_1Symbol          = install("normalizeURL.1");
    sourceSymbol                  = install("source");
    sys_sourceSymbol              = install("sys.source");
    gui_rstudioSymbol             = install("gui.rstudio");
    init_tools_rstudioSymbol      = install("init.tools:rstudio");
    tools_rstudioSymbol           = install("tools:rstudio");
    _rs_api_getActiveDocumentContextSymbol = install(".rs.api.getActiveDocumentContext");
    _rs_api_getSourceEditorContextSymbol = install(".rs.api.getSourceEditorContext");
    debugSourceSymbol             = install("debugSource");
    testthatSymbol                = install("testthat");
    source_fileSymbol             = install("source_file");
    testthat_uses_brioSymbol      = install("testthat.uses.brio");
    knitr_output_dirSymbol        = install("knitr.output.dir");
    knitrSymbol                   = install("knitr");
    knitSymbol                    = install("knit");
    wrap_sourceSymbol             = install("wrap.source");
    sys_callSymbol                = install("sys.call");
    sys_frameSymbol               = install("sys.frame");
    sys_functionSymbol            = install("sys.function");
    sys_nframeSymbol              = install("sys.nframe");
    sys_parentSymbol              = install("sys.parent");
    sys_parentsSymbol             = install("sys.parents");
    ofileSymbol                   = install("ofile");
    owdSymbol                     = install("owd");
    old_dirSymbol                 = install("old_dir");
    fileSymbol                    = install("file");
    fileNameSymbol                = install("fileName");
    pathSymbol                    = install("path");
    inputSymbol                   = install("input");
    missingSymbol                 = install("missing");
    returnSymbol                  = install("return");
    this_path_toplevelSymbol      = install(".this.path.toplevel");
    encodeStringSymbol            = install("encodeString");
    na_encodeSymbol               = install("na.encode");
    exprSymbol                    = install("expr");
    on_exitSymbol                 = install("on.exit");
    External2Symbol               = install(".External2");
    C_setprseen2Symbol            = install("C_setprseen2");
    thispathtempSymbol            = install("._this.path::temp_.");
    parent_frameSymbol            = install("parent.frame");
    invisibleSymbol               = install("invisible");
    getConnectionSymbol           = install("getConnection");
    as_environmentSymbol          = install("as.environment");
    oenvirSymbol                  = install("oenvir");
    withArgsSymbol                = install("withArgs");
    thispathhelperSymbol          = install("this.path.helper");
    GetConnectionSymbol           = install("GetConnection");
    GetUnderlyingConnectionSymbol = install("GetUnderlyingConnection");
    summary_connectionSymbol      = install("summary.connection");
    require_this_path_helperSymbol= install("require.this.path.helper");
    _asArgsSymbol                 = install(".asArgs");
    commandArgsSymbol             = install("commandArgs");
    maybe_in_shellSymbol          = install("maybe.in.shell");


#include "requirethispathhelper.h"


    requirethispathhelper;


    /* save HAVE_AQUA and PATH_MAX in my namespace */
#if defined(HAVE_AQUA)
    defineVarInc(install("HAVE_AQUA"), ScalarLogical(TRUE ), mynamespace);
#else
    defineVarInc(install("HAVE_AQUA"), ScalarLogical(FALSE), mynamespace);
#endif


    defineVarInc(install("PATH_MAX"), ScalarInteger(PATH_MAX), mynamespace);


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


SEXP do_onunload(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    // SEXP libpath = CADR(args);


    R_ReleaseObject(mynamespace);


    return R_NilValue;
}
