/*
this.path : Get Executing Script's Path
Copyright (C) 2023-2025   Iris Simmons
 */


#ifndef R_THISPATH_NSHOOKS_H
#define R_THISPATH_NSHOOKS_H


#define R_NO_REMAP
#include <Rinternals.h>  /* need 'SEXP' */


extern SEXP mynamespace,
            DocumentContextClass,
            ThisPathInAQUAErrorClass                     ,
            ThisPathInZipFileErrorClass                  ,
            ThisPathNotExistsErrorClass                  ,
            ThisPathNotFoundErrorClass                   ,
            ThisPathNotImplementedErrorClass             ,
            ThisPathUnrecognizedConnectionClassErrorClass,
            ThisPathUnrecognizedMannerErrorClass         ,
            last_condition,
            _custom_gui_path_env;


extern int _isMethodsDispatchOn(void);
extern int _toplevel_nframe(void);
extern SEXP commandArgs(void);
extern SEXP getwd(void);
extern SEXP info_dollar_source_path(SEXP rho);
extern void invisible(void);
extern SEXP knitr_output_dir(void);
extern int missing_file(SEXP rho);
extern int missing_input(SEXP rho);
extern int missing_ofile(SEXP rho);
extern SEXP parent_frame(SEXP rho);
extern int sys_nframe(SEXP rho);
extern SEXP sys_parents(SEXP rho);
extern int testthat_source_file_uses_brio_read_lines(void);
extern SEXP UseMethod_lengths(SEXP rho);
extern SEXP expr_sys_call                                 ,
            expr_sys_call_which                           ,
            expr_sys_function_which                       ,
            eval_op                                       ;


#endif /* R_THISPATH_NSHOOKS_H */
