#ifndef R_THISPATH_DEVEL_H
#define R_THISPATH_DEVEL_H


#include "rversiondefines.h" /* need definition of R_version_at_least */


/* handle R_THIS_PATH_DEVEL */


#if R_version_at_least(3,0,0)
    #include <Rinternals.h> /* need definition of SEXP */
    #include <R_ext/Connections.h>
    #if !defined(R_CONNECTIONS_VERSION)
    #elif R_CONNECTIONS_VERSION == 1
        #define R_CONNECTIONS_VERSION_1
        typedef struct gzconn {
            Rconnection con;
            /* there are other components to an 'Rgzconn', but only 'con' is needed */
        } *Rgzconn;
        extern Rconnection (*ptr_R_GetConnection)(SEXP sConn);
        #if defined(R_THIS_PATH_DEVEL)
            #if R_version_at_least(3,3,0)
                extern Rconnection R_GetConnection(SEXP sConn);
            #else
                extern Rconnection getConnection(int n);
            #endif
        #endif
    #endif
#endif


#if R_version_less_than(3,0,0)
    /* updating R_Visible has no effect since we are using .External */
    #define set_R_Visible(v) do {} while (0)
    #include <Rinternals.h> /* need definition of defineVar, ScalarLogical */
    #include "symbols.h"    /* need definition of _this_path_valueSymbol, _this_path_visibleSymbol */
    #define set_this_path_value(v) { defineVar(_this_path_valueSymbol, (v), rho); }
    #define set_this_path_visible(v) { (v) ? 0xDEADBEEF : defineVar(_this_path_visibleSymbol, ScalarLogical(0), rho); }
#elif defined(R_THIS_PATH_DEVEL)
    #include <R_ext/Boolean.h> /* need definition of Rboolean, TRUE, FALSE */
    extern Rboolean R_Visible;
    #define set_R_Visible(v) { R_Visible = ((v) ? TRUE : FALSE); }
#else
    #define HAVE_SET_R_VISIBLE
    extern void (*ptr_set_R_Visible)(Rboolean);
    #include <Rinternals.h> /* need definition of eval, R_NilValue, R_EmptyEnv */
    #include "ns-hooks.h"   /* need definition of expr_invisible */
    #define set_R_Visible(v) {                                 \
        (v) ? ((ptr_set_R_Visible) ? ptr_set_R_Visible(TRUE) : eval(R_NilValue, R_EmptyEnv)) :\
              ((ptr_set_R_Visible) ? ptr_set_R_Visible(FALSE) : eval(expr_invisible, R_EmptyEnv));\
    }
#endif


#endif
