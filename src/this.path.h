#include <Rinternals.h>


/* basename2.c */


extern SEXP do_windowsbasename2(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixbasename2   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_basename2       (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_windowsdirname2(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixdirname2   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_dirname2       (SEXP call, SEXP op, SEXP args, SEXP rho);


/* extension.c */


extern SEXP do_windowssplitext(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixsplitext   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_splitext       (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_windowsextension(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixextension   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_extension       (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_windowsremoveext(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixremoveext   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_removeext       (SEXP call, SEXP op, SEXP args, SEXP rho);


/* pathjoin.c */


extern SEXP do_windowspathjoin(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixpathjoin   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pathjoin       (SEXP call, SEXP op, SEXP args, SEXP rho);
