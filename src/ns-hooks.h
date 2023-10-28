#ifndef R_THISPATH_NSHOOKS_H
#define R_THISPATH_NSHOOKS_H


#include <Rinternals.h>       /* need definition of SEXP */


extern SEXP mynamespace         ,
            DocumentContextClass,
            last_condition      ,
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
            expr__toplevel_context_number                 ,
            expr__isMethodsDispatchOn                     ,
            expr_UseMethod_lengths                        ;


#endif /* R_THISPATH_NSHOOKS_H */
