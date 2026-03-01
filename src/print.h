/*
this.path : Get Executing Script's Path
Copyright (C) 2023-2024   Iris Simmons
 */


#ifndef R_THISPATH_PRINT_H
#define R_THISPATH_PRINT_H


#define R_NO_REMAP
#include <Rinternals.h>       /* need definition of SEXP */


extern void my_PrintValueEnv(SEXP s, SEXP env);


#endif
