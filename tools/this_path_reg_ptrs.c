#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>          /* need 'R_VERSION' and 'R_Version' */
#include <R_ext/Rdynload.h>    /* need 'R_RegisterCCallable' */
#include <R_ext/Visibility.h>  /* need 'attribute_visible' */


#if defined(ENABLE_NLS)
#include <libintl.h>
#define _(String) dgettext ("R", String)
#else
#define _(String) (String)
#endif


/* file R_ext/Connections.h was added in R 3.0.0 */
#if defined(R_VERSION) && R_VERSION >= R_Version(3,0,0)
    #include <R_ext/Connections.h>
    #if !defined(R_CONNECTIONS_VERSION)
    #elif R_CONNECTIONS_VERSION == 1
        #define R_CONNECTIONS_VERSION_1
        /* R_GetConnection was added in R 3.3.0 */
        #if defined(R_VERSION) && R_VERSION >= R_Version(3,3,0)
            extern Rconnection R_GetConnection(SEXP sConn);
        #else
            /* create our own version of R_GetConnection */
            extern Rconnection getConnection(int n);
            Rconnection R_GetConnection(SEXP sConn) {
                if (!Rf_inherits(sConn, "connection")) Rf_error(_("invalid connection"));
                return getConnection(Rf_asInteger(sConn));
            }
        #endif
    #endif
#endif


/* R_Visible was made visible in R 3.0.0 */
#if defined(R_VERSION) && R_VERSION >= R_Version(3,0,0)
extern Rboolean R_Visible;
#define HAVE_SET_R_VISIBLE
void set_R_Visible(Rboolean x)
{
    R_Visible = x;
}
#endif


LibExtern Rboolean utf8locale;
LibExtern Rboolean latin1locale;
Rboolean get_utf8locale(void)
{
    return utf8locale;
}
Rboolean get_latin1locale(void)
{
    return latin1locale;
}


attribute_visible
void R_init_this_path_reg_ptrs(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
#if defined(R_VERSION) && R_VERSION >= R_Version(3,0,0)
    R_forceSymbols(dll, TRUE);
#endif


#if defined(R_CONNECTIONS_VERSION_1)
    R_RegisterCCallable("this_path_reg_ptrs", "R_GetConnection", (DL_FUNC) R_GetConnection);
#endif


#if defined(HAVE_SET_R_VISIBLE)
    R_RegisterCCallable("this_path_reg_ptrs", "set_R_Visible", (DL_FUNC) set_R_Visible);
#endif


    R_RegisterCCallable("this_path_reg_ptrs", "get_utf8locale", (DL_FUNC) get_utf8locale);
    R_RegisterCCallable("this_path_reg_ptrs", "get_latin1locale", (DL_FUNC) get_latin1locale);


#if defined(R_VERSION) && R_VERSION >= R_Version(4,5,0)
    R_RegisterCCallable("this_path_reg_ptrs", "PRCODE", (DL_FUNC) PRCODE);
    R_RegisterCCallable("this_path_reg_ptrs", "PRENV", (DL_FUNC) PRENV);
    R_RegisterCCallable("this_path_reg_ptrs", "R_PromiseExpr", (DL_FUNC) R_PromiseExpr);
    R_RegisterCCallable("this_path_reg_ptrs", "PRVALUE", (DL_FUNC) PRVALUE);
    R_RegisterCCallable("this_path_reg_ptrs", "SET_PRCODE", (DL_FUNC) SET_PRCODE);
    R_RegisterCCallable("this_path_reg_ptrs", "SET_PRENV", (DL_FUNC) SET_PRENV);
    R_RegisterCCallable("this_path_reg_ptrs", "SET_PRVALUE", (DL_FUNC) SET_PRVALUE);
#endif
}
