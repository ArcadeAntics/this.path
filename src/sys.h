#ifndef R_THISPATH_SYS_H
#define R_THISPATH_SYS_H


#define R_NO_REMAP
#include <Rinternals.h>
#include "ns-hooks.h"


extern SEXP sys_call(SEXP which, SEXP rho);
#define getCurrentCall(rho) ( Rf_eval(expr_sys_call, (rho)) )
extern int sys_parent(int n, SEXP rho);


extern SEXP sys_srcref(int k, SEXP rho);
extern SEXP sys_srcfile(int k, SEXP rho);


#endif
