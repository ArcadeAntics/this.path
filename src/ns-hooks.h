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
            _custom_gui_path_character_environment,
            _custom_gui_path_function_environment ;


extern SEXP expr_commandArgs                              ,
            expr_invisible                                ,
            expr_parent_frame                             ,
            expr_sys_call                                 ,
            expr_sys_call_which                           ,
            expr_sys_function_which                       ,
            eval_op                                       ,
            expr_sys_nframe                               ,
            expr_sys_parents                              ,
            expr_missing_file                             ,
            expr_missing_input                            ,
            expr_missing_ofile                            ,
            expr_info_dollar_source_path                  ,
            expr_knitr_output_dir                         ,
            expr_testthat_source_file_uses_brio_read_lines,
            expr_getOption_topLevelEnvironment            ,
            expr__toplevel_nframe                         ,
            expr__isMethodsDispatchOn                     ,
            expr_UseMethod_lengths                        ;


#endif /* R_THISPATH_NSHOOKS_H */
