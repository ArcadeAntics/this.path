#include <Rinternals.h>


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


/* pathjoin.c */


extern SEXP do_windowspathjoin(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_unixpathjoin   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pathjoin       (SEXP call, SEXP op, SEXP args, SEXP rho);


/* thispath.c */


extern SEXP do_isunevaluatedpromise(SEXP call, SEXP op, SEXP args, SEXP rho);
