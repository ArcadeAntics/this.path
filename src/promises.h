#ifndef R_THIS_PATH_PROMISES
#define R_THIS_PATH_PROMISES


#include <Rinternals.h>  /* need definition of SEXP */


extern SEXP makePROMISE(SEXP expr, SEXP env);
extern SEXP makeEVPROMISE(SEXP expr, SEXP value);


#endif
