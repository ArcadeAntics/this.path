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

 .External2(.C_syspath, verbose)

becomes:

 .External(.C_syspath,
     quote(.External(.C_syspath, verbose)),
     .External,
     environment(),
     verbose)
 */


#ifndef THIS_PATH_H
#define THIS_PATH_H


#include <Rinternals.h>         /* need definition of SEXP */
#include "thispathbackports.h"  /* need definition of do_formals */
#include "rversiondefines.h"    /* need definition of R_version_less_than */


/* aquarootscript.c */


extern SEXP do_aquarootscript do_formals;


/* backports.c */


#if R_version_less_than(3, 5, 0)
extern SEXP do_dotslength do_formals;
#endif


#if R_version_less_than(3, 3, 0)
extern SEXP do_strrep     do_formals;
extern SEXP do_startsWith do_formals;
extern SEXP do_endsWith   do_formals;
#endif


#if R_version_less_than(3, 2, 0)
extern SEXP do_direxists do_formals;
extern SEXP do_lengths   do_formals;
extern SEXP do_isRegisteredNamespace do_formals;
#endif


#if R_version_less_than(3, 1, 0)
extern SEXP do_anyNA do_formals;
#endif


/* basename2.c */


extern SEXP do_windowsbasename2 do_formals;
extern SEXP do_unixbasename2    do_formals;
extern SEXP do_basename2        do_formals;

extern SEXP do_windowsdirname2 do_formals;
extern SEXP do_unixdirname2    do_formals;
extern SEXP do_dirname2        do_formals;


/* ext.c */


extern SEXP do_windowssplitext do_formals;
extern SEXP do_unixsplitext    do_formals;
extern SEXP do_splitext        do_formals;

extern SEXP do_windowsremoveext do_formals;
extern SEXP do_unixremoveext    do_formals;
extern SEXP do_removeext        do_formals;

extern SEXP do_windowsext do_formals;
extern SEXP do_unixext    do_formals;
extern SEXP do_ext        do_formals;

extern SEXP do_windowsextgets do_formals;
extern SEXP do_unixextgets    do_formals;
extern SEXP do_extgets        do_formals;


/* isabspath.c */


extern SEXP do_windowsisabspath do_formals;
extern SEXP do_unixisabspath    do_formals;
extern SEXP do_isabspath        do_formals;


/* ns-hooks.c */


extern SEXP do_mbcslocale   do_formals;
// extern SEXP do_utf8locale   do_formals;
// extern SEXP do_latin1locale do_formals;
extern SEXP do_R_MB_CUR_MAX do_formals;

extern SEXP do_onLoad   do_formals;
extern SEXP do_onUnload do_formals;


/* pathjoin.c */


extern SEXP do_windowspathjoin do_formals;
extern SEXP do_unixpathjoin    do_formals;
extern SEXP do_pathjoin        do_formals;


/* pathsplit.c */


extern SEXP do_windowspathsplit do_formals;
extern SEXP do_unixpathsplit    do_formals;
extern SEXP do_pathsplit        do_formals;

extern SEXP do_windowspathsplit1 do_formals;
extern SEXP do_unixpathsplit1    do_formals;
extern SEXP do_pathsplit1        do_formals;

extern SEXP do_windowspathunsplit do_formals;
extern SEXP do_unixpathunsplit    do_formals;
extern SEXP do_pathunsplit        do_formals;


/* progargs.c */


extern SEXP do_asArgs do_formals;


/* promises.c */


extern SEXP do_isunevaluatedpromise     do_formals;
extern SEXP do_promiseisunevaluated     do_formals;
extern SEXP do_getpromisewithoutwarning do_formals;
extern SEXP do_PRINFO                   do_formals;
extern SEXP do_setsyspathjupyter        do_formals;
extern SEXP do_mkPROMISE                do_formals;
extern SEXP do_mkEVPROMISE              do_formals;
extern SEXP do_unlockEnvironment        do_formals;


/* rprojroot.c */


extern SEXP do_resetproj do_formals;


/* shfile.c */


extern SEXP do_shFILE do_formals;
extern SEXP do_shINFO do_formals;


/* thispath.c */


extern SEXP do_thisPathUnrecognizedConnectionClassError do_formals;
extern SEXP do_thisPathUnrecognizedMannerError          do_formals;
extern SEXP do_thisPathNotImplementedError              do_formals;
extern SEXP do_thisPathNotExistsError                   do_formals;
extern SEXP do_thisPathInZipFileError                   do_formals;
extern SEXP do_thisPathInAQUAError                      do_formals;

extern SEXP do_isclipboard      do_formals;
extern SEXP do_inittoolsrstudio do_formals;
extern SEXP do_syspathjupyter   do_formals;
extern SEXP do_syspathrgui      do_formals;
extern SEXP do_syspath          do_formals;
extern SEXP do_getframenumber   do_formals;
extern SEXP do_envpath          do_formals;
extern SEXP do_srcpath          do_formals;
extern SEXP do_srclineno        do_formals;
extern SEXP do_thispath         do_formals;
extern SEXP do_istrue           do_formals;
extern SEXP do_isfalse          do_formals;
extern SEXP do_asInteger        do_formals;


/* wrapsource.c */


extern SEXP do_setprseen2   do_formals;
extern SEXP do_wrapsource   do_formals;
extern SEXP do_setsyspath   do_formals;
extern SEXP do_unsetsyspath do_formals;


#endif  /* #ifndef THIS_PATH_H */
