#ifndef R_THIS_PATH_BACKPORTS
#define R_THIS_PATH_BACKPORTS


#include "Rversiondefines.h"


#if R_version_at_least(3, 0, 0)
#define do_formals (SEXP call, SEXP op, SEXP args, SEXP rho)
#define do_start(name, numParameters) args = CDR(args)
#else
#define do_formals (SEXP args)
#define do_start(name, numParameters)                          \
    args = CDR(args);                                          \
    if (length(args) < 3)                                      \
        error(_("in .External(), 'call', 'op', and 'rho' must be provided"));\
    if ((numParameters) > -1) {                                \
        int nargs = length(args) - 3;                          \
        if ((numParameters) != nargs)                          \
            errorcall(CAR(args),                               \
                _("Incorrect number of arguments (%d), expecting %d for '%s'"),\
                nargs, (numParameters), (name));               \
    }                                                          \
    SEXP call = CAR(args); args = CDR(args);                   \
    SEXP op   = CAR(args); args = CDR(args);                   \
    SEXP rho  = CAR(args); args = CDR(args)
#endif


#endif
