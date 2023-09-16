#ifndef R_THISPATH_BACKPORTS_H
#define R_THISPATH_BACKPORTS_H


#include <Rinternals.h>       /* need definition of SEXP */
#include "rversiondefines.h"  /* need definition of R_version_less_than */


extern SEXP lazy_duplicate(SEXP s);
extern SEXP shallow_duplicate(SEXP s);

extern SEXP R_lsInternal3(SEXP env, Rboolean all, Rboolean sorted);
extern SEXP topenv(SEXP target, SEXP envir);

extern SEXP R_shallow_duplicate_attr(SEXP x);
extern SEXP installTrChar(SEXP x);

extern void R_removeVarFromFrame(SEXP name, SEXP env);

extern SEXP R_NewEnv(SEXP enclos, int hash, int size);
extern int IS_ASCII(SEXP x);

extern Rboolean R_existsVarInFrame(SEXP rho, SEXP symbol);


#if R_version_less_than(3, 0, 0)
#define XLENGTH LENGTH
#define xlength length
#define R_xlen_t R_len_t
#endif


#if R_version_less_than(3, 0, 0)
#define NAMEDMAX 2
#define NO_REFERENCES(x) (NAMED(x) == 0)
#define MAYBE_REFERENCED(x) (! NO_REFERENCES(x))
#define MARK_NOT_MUTABLE(x) SET_NAMED(x, NAMEDMAX)
#endif


#if R_version_at_least(3, 0, 0)
#define do_formals (SEXP call, SEXP op, SEXP args, SEXP rho)
#define do_start(name, numParameters) args = CDR(args)
#define do_start_no_call(name, numParameters) do_start(name, numParameters)
#define do_start_no_op(name, numParameters) do_start(name, numParameters)
#define do_start_no_rho(name, numParameters) do_start(name, numParameters)
#define do_start_no_call_op(name, numParameters) do_start(name, numParameters)
#define do_start_no_call_rho(name, numParameters) do_start(name, numParameters)
#define do_start_no_op_rho(name, numParameters) do_start(name, numParameters)
#define do_start_no_call_op_rho(name, numParameters) do_start(name, numParameters)
#else
#define do_formals (SEXP args)
#define _do_start(name, numParameters)                         \
    args = CDR(args);                                          \
    if (length(args) < 3)                                      \
        error(_("in .External(), 'call', 'op', and 'rho' must be provided"));\
    if ((numParameters) > -1) {                                \
        int nargs = length(args) - 3;                          \
        if ((numParameters) != nargs)                          \
            errorcall(CAR(args),                               \
                _("Incorrect number of arguments (%d), expecting %d for '%s'"),\
                nargs, (numParameters), (name));               \
    }
#define do_start(name, numParameters)                          \
    _do_start(name, numParameters);                            \
    SEXP call = CAR(args); args = CDR(args);                   \
    SEXP op   = CAR(args); args = CDR(args);                   \
    SEXP rho  = CAR(args); args = CDR(args)
#define do_start_no_call(name, numParameters)                  \
    _do_start(name, numParameters);                            \
                           args = CDR(args);                   \
    SEXP op   = CAR(args); args = CDR(args);                   \
    SEXP rho  = CAR(args); args = CDR(args)
#define do_start_no_op(name, numParameters)                    \
    _do_start(name, numParameters);                            \
    SEXP call = CAR(args); args = CDR(args);                   \
                           args = CDR(args);                   \
    SEXP rho  = CAR(args); args = CDR(args)
#define do_start_no_rho(name, numParameters)                   \
    _do_start(name, numParameters);                            \
    SEXP call = CAR(args); args = CDR(args);                   \
    SEXP op   = CAR(args); args = CDR(args);                   \
                           args = CDR(args)
#define do_start_no_call_op(name, numParameters)               \
    _do_start(name, numParameters);                            \
                           args = CDR(args);                   \
                           args = CDR(args);                   \
    SEXP rho  = CAR(args); args = CDR(args)
#define do_start_no_call_rho(name, numParameters)              \
    _do_start(name, numParameters);                            \
                           args = CDR(args);                   \
    SEXP op   = CAR(args); args = CDR(args);                   \
                           args = CDR(args)
#define do_start_no_op_rho(name, numParameters)                \
    _do_start(name, numParameters);                            \
    SEXP call = CAR(args); args = CDR(args);                   \
                           args = CDR(args);                   \
                           args = CDR(args)
#define do_start_no_call_op_rho(name, numParameters)           \
    _do_start(name, numParameters);                            \
                           args = CDR(args);                   \
                           args = CDR(args);                   \
                           args = CDR(args)
#endif


#if R_version_less_than(3, 1, 0)
#define INCREMENT_NAMED(x) do {                                \
    SEXP _x_ = (x);                                            \
    if (NAMED(_x_) != NAMEDMAX)                                \
        SET_NAMED(_x_, NAMED(_x_) + 1);                        \
} while (0)
#endif


#if R_version_at_least(3, 1, 0)
LibExtern SEXP R_TrueValue;
LibExtern SEXP R_FalseValue;
LibExtern SEXP R_LogicalNAValue;
#else
#define R_TrueValue ScalarLogical(TRUE)
#define R_FalseValue ScalarLogical(FALSE)
#define R_LogicalNAValue ScalarLogical(NA_LOGICAL)
#endif


#if R_version_less_than(3, 1, 0)
extern SEXP lazy_duplicate(SEXP s);
extern SEXP shallow_duplicate(SEXP s);
extern int IS_SCALAR(SEXP x, int type);
#endif


#if R_version_less_than(3, 2, 0)
#define R_THIS_PATH_NEED_BLANKSCALARSTRING
extern SEXP R_BlankScalarString;
#endif


#if R_version_less_than(3, 4, 0)
#define R_CurrentExpression R_NilValue
#endif


#if R_version_less_than(3, 5, 0)
#define ENSURE_NAMEDMAX(_x_) SET_NAMED((_x_), NAMEDMAX)
#else
extern void (ENSURE_NAMEDMAX)(SEXP x);
#endif


#if R_version_less_than(4, 1, 0)
#define IS_UTF8(x) (getCharCE((x)) == CE_UTF8)
#else
extern int IS_UTF8(SEXP x);
#endif


#endif /* R_THISPATH_BACKPORTS_H */
