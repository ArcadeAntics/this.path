/*
this.path : Get Executing Script's Path
Copyright (C) 2026   Iris Simmons
 */


#ifndef R_THIS_PATH_BINDING_H
#define R_THIS_PATH_BINDING_H


#define R_NO_REMAP
#include <Rinternals.h>
#include <Rversion.h>


typedef struct {
    SEXP env;
    SEXP sym;
    SEXP value;
#if defined(R_VERSION) && R_VERSION >= R_Version(4,6,0)
    R_BindingType_t type;
#endif
} @R_PACKAGE_LIB@_binding_t;


#endif
