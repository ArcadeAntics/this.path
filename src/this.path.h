#include <Rinternals.h>


/* basename2.c */


extern SEXP do_windowsbasename2(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixbasename2   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_basename2       (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_windowsdirname2(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixdirname2   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_dirname2       (SEXP call, SEXP op, SEXP args, SEXP rho);


/* pathjoin.c */


extern SEXP do_windowspathjoin(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixpathjoin   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pathjoin       (SEXP call, SEXP op, SEXP args, SEXP rho);
