#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>          /* need 'R_VERSION' and 'R_Version' */
#include <R_ext/Rdynload.h>    /* need 'R_RegisterCCallable' */
#include <R_ext/Visibility.h>  /* need 'attribute_visible' */


#if defined(R_VERSION) && R_VERSION >= R_Version(3,0,0)
extern Rboolean R_Visible;
#define HAVE_SET_R_VISIBLE
void set_R_Visible(Rboolean x)
{
    R_Visible = x;
}
#endif


attribute_visible
void R_init_this_path_reg_ptrs(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
#if defined(R_VERSION) && R_VERSION >= R_Version(3,0,0)
    R_forceSymbols(dll, TRUE);
#endif


#if defined(R_VERSION) && R_VERSION >= R_Version(3,3,0)
    #include <R_ext/Connections.h>
    #if !defined(R_CONNECTIONS_VERSION)
    #elif R_CONNECTIONS_VERSION == 1
        extern Rconnection R_GetConnection(SEXP sConn);
        R_RegisterCCallable("this_path_reg_ptrs", "R_GetConnection", (DL_FUNC) R_GetConnection);
    #endif
#endif


#if defined(HAVE_SET_R_VISIBLE)
    R_RegisterCCallable("this_path_reg_ptrs", "set_R_Visible", (DL_FUNC) set_R_Visible);
#endif
}
