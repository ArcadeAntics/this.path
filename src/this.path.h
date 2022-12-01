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


extern SEXP do_onload(SEXP call, SEXP op, SEXP args, SEXP rho);


/* isabspath.c */


extern SEXP do_windowsisabspath(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixisabspath   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_isabspath       (SEXP call, SEXP op, SEXP args, SEXP rho);


/* pathjoin.c */


extern SEXP do_windowspathjoin(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixpathjoin   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pathjoin       (SEXP call, SEXP op, SEXP args, SEXP rho);


/* promises.c */


extern SEXP do_isunevaluatedpromise    (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_getpromisewithoutwarning(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_prinfo                  (SEXP call, SEXP op, SEXP args, SEXP rho);


/* shfile.c */


extern SEXP do_shfile(SEXP call, SEXP op, SEXP args, SEXP rho);


/* thispath.c */


extern SEXP do_thispathunrecognizedconnectionclasserror(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispathunrecognizedmannererror         (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispathnotimplementederror             (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispathnotexistserror                  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispathinzipfileerror                  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispathinaquaerror                     (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_isclipboard(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_thispath   (SEXP call, SEXP op, SEXP args, SEXP rho);


/* wrapsource.c */


extern SEXP do_setprseen2  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_wrapsource  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_insidesource(SEXP call, SEXP op, SEXP args, SEXP rho);


#endif  /* #ifndef THIS_PATH_H */
