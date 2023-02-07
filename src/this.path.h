/*
My C code is stylized much the same as R's C code. See

https://github.com/wch/r-source/blob/trunk/src/main/names.c#L51

Each function must:
  * be called from R by .External2(.)
  * have a name that starts with "do_"
  * return a SEXP
  * have an argument list (SEXP call, SEXP op, SEXP args, SEXP rho)

call is the LANGSXP that called the C code, like .External2(C_funname, ...)

op is the operator that called the C code, always .External2

args is the argument list provided to .External2(.), including C_funname

rho is the environment in which .External2(.) was evaluated, useful for
accessing and assigning variables
 */


#ifndef THIS_PATH_H
#define THIS_PATH_H


#include <Rinternals.h>


/* aquarootscript.c */


extern SEXP do_aquarootscript(SEXP call, SEXP op, SEXP args, SEXP rho);


/* args.c */


extern SEXP do_asargs(SEXP call, SEXP op, SEXP args, SEXP rho);


/* basename2.c */


extern SEXP do_windowsbasename2(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixbasename2   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_basename2       (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_windowsdirname2(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixdirname2   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_dirname2       (SEXP call, SEXP op, SEXP args, SEXP rho);


/* ext.c */


extern SEXP do_windowssplitext(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixsplitext   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_splitext       (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_windowsremoveext(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixremoveext   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_removeext       (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_windowsext(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixext   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_ext       (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_windowsextgets(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixextgets   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_extgets       (SEXP call, SEXP op, SEXP args, SEXP rho);


/* hooks-for-namespace-events.c */


extern SEXP do_onload  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_onunload(SEXP call, SEXP op, SEXP args, SEXP rho);


/* isabspath.c */


extern SEXP do_windowsisabspath(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixisabspath   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_isabspath       (SEXP call, SEXP op, SEXP args, SEXP rho);


/* pathjoin.c */


extern SEXP do_windowspathjoin(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixpathjoin   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pathjoin       (SEXP call, SEXP op, SEXP args, SEXP rho);


/* pathsplit.c */


extern SEXP do_windowspathsplit(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixpathsplit   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pathsplit       (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_windowspathsplit1(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixpathsplit1   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pathsplit1       (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_windowspathunsplit(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixpathunsplit   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pathunsplit       (SEXP call, SEXP op, SEXP args, SEXP rho);


/* promises.c */


extern SEXP do_isunevaluatedpromise    (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_promiseisunevaluated    (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_getpromisewithoutwarning(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_prinfo                  (SEXP call, SEXP op, SEXP args, SEXP rho);


/* shfile.c */


extern SEXP do_shfile(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_shinfo(SEXP call, SEXP op, SEXP args, SEXP rho);


/* thispath.c */


extern SEXP do_thispathunrecognizedconnectionclasserror(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispathunrecognizedmannererror         (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispathnotimplementederror             (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispathnotexistserror                  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispathinzipfileerror                  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispathinaquaerror                     (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_isclipboard     (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispath        (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_inittoolsrstudio(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispathrgui    (SEXP call, SEXP op, SEXP args, SEXP rho);


/* wrapsource.c */


extern SEXP do_setprseen2   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_wrapsource   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_insidesource (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_setthispath  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unsetthispath(SEXP call, SEXP op, SEXP args, SEXP rho);


#endif  /* #ifndef THIS_PATH_H */
