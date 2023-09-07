#ifndef R_THISPATH_PROMISES_H
#define R_THISPATH_PROMISES_H


#include <Rinternals.h>  /* need definition of SEXP */


extern SEXP makePROMISE(SEXP expr, SEXP env);
extern SEXP makeEVPROMISE(SEXP expr, SEXP value);


#endif
