/*
My C code is stylized much the same as R's C code. See

https://github.com/wch/r-source/blob/trunk/src/main/names.c#L51

Each function must:
  * be called from R by .External2(.)
  * have a name that starts with "do_"
  * return a SEXP
  * have an argument list (SEXP call, SEXP op, SEXP args, SEXP rho)

call is the LANGSXP that called the C code, like .External2(.C_funname, ...)

op is the operator that called the C code, always .External2

args is the argument list provided to .External2(.), including .C_funname

rho is the environment in which .External2(.) was evaluated, useful for
accessing and assigning variables





for R < 3.0.0, .External2 does not exist, so we must use .External
here are the alternate rules. Each function must:
  * be called from R by .External(.)
  * have a name that starts with "do_"
  * return a SEXP
  * have an argument list (SEXP args)

the first element of 'args' is '.NAME', as usual. The next 3 elements must be
the 'call', 'op', and 'rho' that would have been provided if .External2() was
available. for example:

 .External2(.C_sys.path, verbose)

becomes:

 .External(.C_sys.path,
     quote(.External(.C_sys.path, verbose)),
     .External,
     environment(),
     verbose)
 */


#ifndef R_THISPATH_THISPATH_H
#define R_THISPATH_THISPATH_H


#include <Rinternals.h>         /* need definition of SEXP */
#include "backports.h"          /* need definition of do_formals */
#include "rversiondefines.h"    /* need definition of R_version_less_than */


/* aquarootscript.c */


extern SEXP do_aquarootscript do_formals;


/* backports.c */


#if R_version_less_than(3, 1, 0)
extern SEXP do_anyNA                 do_formals;
extern SEXP do_anyNA_data_frame      do_formals;
extern SEXP do_anyNA_numeric_version do_formals;
extern SEXP do_anyNA_default         do_formals;
#endif


#if R_version_less_than(3, 2, 0)
extern SEXP do_dir_exists            do_formals;
extern SEXP do_lengths               do_formals;
extern SEXP do_lengths_default       do_formals;
extern SEXP do_isRegisteredNamespace do_formals;
#endif


#if R_version_less_than(3, 3, 0)
extern SEXP do_strrep     do_formals;
extern SEXP do_startsWith do_formals;
extern SEXP do_endsWith   do_formals;
#endif


#if R_version_less_than(3, 5, 0)
extern SEXP do_dotslength do_formals;
#endif


/* basename2.c */


extern SEXP do_windows_basename2 do_formals;
extern SEXP do_unix_basename2    do_formals;
extern SEXP do_basename2         do_formals;

extern SEXP do_windows_dirname2 do_formals;
extern SEXP do_unix_dirname2    do_formals;
extern SEXP do_dirname2         do_formals;


/* ext.c */


extern SEXP do_windows_splitext do_formals;
extern SEXP do_unix_splitext    do_formals;
extern SEXP do_splitext         do_formals;

extern SEXP do_windows_removeext do_formals;
extern SEXP do_unix_removeext    do_formals;
extern SEXP do_removeext         do_formals;

extern SEXP do_windows_ext do_formals;
extern SEXP do_unix_ext    do_formals;
extern SEXP do_ext         do_formals;

extern SEXP do_windows_extgets do_formals;
extern SEXP do_unix_extgets    do_formals;
extern SEXP do_extgets         do_formals;


/* isabspath.c */


extern SEXP do_windows_is_abs_path do_formals;
extern SEXP do_unix_is_abs_path    do_formals;
extern SEXP do_is_abs_path         do_formals;


/* ns-hooks.c */


extern SEXP do_mbcslocale   do_formals;
// extern SEXP do_utf8locale   do_formals;
// extern SEXP do_latin1locale do_formals;
extern SEXP do_R_MB_CUR_MAX do_formals;

extern SEXP do_onLoad         do_formals;
extern SEXP do_onUnload       do_formals;


/* pathjoin.c */


extern SEXP do_windows_path_join do_formals;
extern SEXP do_unix_path_join    do_formals;
extern SEXP do_path_join        do_formals;


/* pathsplit.c */


extern SEXP do_windows_path_split do_formals;
extern SEXP do_unix_path_split    do_formals;
extern SEXP do_path_split         do_formals;

extern SEXP do_windows_path_split_1 do_formals;
extern SEXP do_unix_path_split_1    do_formals;
extern SEXP do_path_split_1         do_formals;

extern SEXP do_windows_path_unsplit do_formals;
extern SEXP do_unix_path_unsplit    do_formals;
extern SEXP do_path_unsplit         do_formals;


/* print.c */


extern SEXP do_PrintValueEnv                 do_formals;
extern SEXP do_print_ThisPathDocumentContext do_formals;


/* progargs.c */


extern SEXP do_asArgs do_formals;


/* promises.c */


extern SEXP do_is_unevaluated_promise do_formals;
extern SEXP do_promise_is_unevaluated do_formals;
extern SEXP do_PRVALUE_no_warn        do_formals;
extern SEXP do_PRINFO                 do_formals;
extern SEXP do_mkPROMISE              do_formals;
extern SEXP do_mkEVPROMISE            do_formals;
extern SEXP do_unlockEnvironment      do_formals;


/* rprojroot.c */


extern SEXP do_reset_proj do_formals;


/* shfile.c */


extern SEXP do_init_file do_formals;
extern SEXP do_site_file do_formals;
extern SEXP do_shFILE    do_formals;
extern SEXP do_shINFO    do_formals;


/* thispath.c */


extern SEXP do_thisPathUnrecognizedConnectionClassError do_formals;
extern SEXP do_thisPathUnrecognizedMannerError          do_formals;
extern SEXP do_thisPathNotImplementedError              do_formals;
extern SEXP do_thisPathNotExistsError                   do_formals;
extern SEXP do_thisPathInZipFileError                   do_formals;
extern SEXP do_thisPathInAQUAError                      do_formals;

extern SEXP do_is_clipboard         do_formals;
extern SEXP do_init_tools_rstudio   do_formals;
extern SEXP do_sys_path_jupyter     do_formals;
extern SEXP do_set_sys_path_jupyter do_formals;
extern SEXP do_sys_path_rgui        do_formals;
extern SEXP do_sys_path             do_formals;
extern SEXP do_getframenumber       do_formals;
extern SEXP do_env_path             do_formals;
extern SEXP do_sys_srcref           do_formals;
extern SEXP do_src_path             do_formals;
extern SEXP do_src_LINENO           do_formals;
extern SEXP do_this_path            do_formals;
extern SEXP do_istrue               do_formals;
extern SEXP do_isfalse              do_formals;
extern SEXP do_asInteger            do_formals;
extern SEXP do_asIntegerGE0         do_formals;


/* thispathdefn.c */


extern SEXP do_get_dyn do_formals;


/* trycatch.c */


extern SEXP do_last_condition do_formals;
extern SEXP do_tryCatch2      do_formals;
extern SEXP do_tryCatch3      do_formals;


/* wrapsource.c */


extern SEXP do_SET_PRSEEN_2          do_formals;
extern SEXP do_wrap_source           do_formals;
extern SEXP do_set_sys_path          do_formals;
extern SEXP do_unset_sys_path        do_formals;
extern SEXP do_set_env_path          do_formals;
extern SEXP do_set_src_path          do_formals;
extern SEXP do_set_src_path_function do_formals;


#endif  /* R_THISPATH_THISPATH_H */
