/*
this.path : Get Executing Script's Path
Copyright (C) 2023-2024   Iris Simmons
 */


#ifndef R_THISPATH_PROMISES_H
#define R_THISPATH_PROMISES_H


#define R_NO_REMAP
#include <Rinternals.h>  /* need definition of SEXP */


extern SEXP makePROMISE(SEXP expr, SEXP env);
extern SEXP makeEVPROMISE(SEXP expr, SEXP value);


#endif
