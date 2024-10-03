#ifndef R_THISPATH_DEVEL_H
#define R_THISPATH_DEVEL_H


#define R_NO_REMAP
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
    #define set_R_Visible(v) do { } while (0)
    #include <Rinternals.h> /* need definition of Rf_defineVar, Rf_ScalarLogical */
    #include "symbols.h"    /* need definition of _this_path_valueSymbol, _this_path_visibleSymbol */
    #define set_this_path_value(v) { Rf_defineVar(_this_path_valueSymbol, (v), rho); }
    #define set_this_path_visible(v) { (v) ? 0xDEADBEEF : Rf_defineVar(_this_path_visibleSymbol, Rf_ScalarLogical(0), rho); }
#elif defined(R_THIS_PATH_DEVEL)
    #include <R_ext/Boolean.h> /* need definition of Rboolean, TRUE, FALSE */
    extern Rboolean R_Visible;
    #define set_R_Visible(v) { R_Visible = ((v) ? TRUE : FALSE); }
#else
    #define HAVE_SET_R_VISIBLE
    extern void (*ptr_set_R_Visible)(Rboolean);
    #include <Rinternals.h> /* need definition of Rf_eval, R_NilValue, R_EmptyEnv */
    #include "ns-hooks.h"   /* need definition of expr_invisible */
    #define set_R_Visible(v) {                                 \
        (v) ? ((ptr_set_R_Visible) ? (ptr_set_R_Visible(TRUE), TRUE) : (Rf_eval(R_NilValue, R_EmptyEnv), TRUE)) :\
              ((ptr_set_R_Visible) ? (ptr_set_R_Visible(FALSE), FALSE) : (Rf_eval(expr_invisible, R_EmptyEnv), FALSE));\
    }
#endif


#if defined(R_THIS_PATH_DEVEL) || R_version_less_than(4,5,0)
    #define ptr_PRCODE PRCODE
    #define ptr_PRENV PRENV
    #define ptr_R_PromiseExpr R_PromiseExpr
    #define ptr_PRVALUE PRVALUE
    extern void SET_PRCODE (SEXP x, SEXP v);
    extern void SET_PRENV  (SEXP x, SEXP v);
    extern void SET_PRVALUE(SEXP x, SEXP v);
    #define ptr_SET_PRCODE SET_PRCODE
    #define ptr_SET_PRENV SET_PRENV
    #define ptr_SET_PRVALUE SET_PRVALUE
#else
    #define NEED_R_4_5_0_FUNCTIONS
    #include <Rinternals.h> /* need SEXP */
    extern SEXP (*ptr_PRCODE)(SEXP x);
    extern SEXP (*ptr_PRENV)(SEXP x);
    extern SEXP (*ptr_R_PromiseExpr)(SEXP x);
    extern SEXP (*ptr_PRVALUE)(SEXP x);
    extern void (*ptr_SET_PRCODE)(SEXP x, SEXP v);
    extern void (*ptr_SET_PRENV)(SEXP x, SEXP v);
    extern void (*ptr_SET_PRVALUE)(SEXP x, SEXP v);
#endif


#if defined(R_THIS_PATH_DEVEL)
    #include <R_ext/Boolean.h>
    LibExtern Rboolean utf8locale;
    #define my_utf8locale utf8locale
#else
    #define HAVE_GET_UTF8LOCALE
    extern Rboolean (*ptr_get_utf8locale)(void);
    #define my_utf8locale ( ptr_get_utf8locale() )
#endif


#endif
