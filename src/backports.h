#ifndef R_THISPATH_BACKPORTS_H
#define R_THISPATH_BACKPORTS_H


#define R_NO_REMAP
#include <Rinternals.h>       /* need definition of SEXP */
#include "devel.h"
#include "rversiondefines.h"  /* need definition of R_version_less_than */


extern SEXP Rf_lazy_duplicate(SEXP s);
extern SEXP Rf_shallow_duplicate(SEXP s);

extern SEXP R_lsInternal3(SEXP env, Rboolean all, Rboolean sorted);
extern SEXP Rf_topenv(SEXP target, SEXP envir);

#if R_version_less_than(3,6,0) || (!defined(R_THIS_PATH_DEVEL) && R_version_at_least(4,5,0))
#define R_shallow_duplicate_attr(x) Rf_shallow_duplicate(x)
#else
extern SEXP R_shallow_duplicate_attr(SEXP x);
#endif
extern SEXP Rf_installTrChar(SEXP x);

extern void R_removeVarFromFrame(SEXP name, SEXP env);

extern SEXP R_NewEnv(SEXP enclos, int hash, int size);
#if R_version_less_than(4,1,0) || (!defined(R_THIS_PATH_DEVEL) && R_version_at_least(4,5,0))
#define IS_ASCII my_IS_ASCII
#endif
extern int IS_ASCII(SEXP x);

extern Rboolean R_existsVarInFrame(SEXP rho, SEXP symbol);

#if R_version_less_than(3,5,0) || (!defined(R_THIS_PATH_DEVEL) && R_version_at_least(4,5,0))
#define ddfind my_ddfind
#endif
extern SEXP ddfind(int i, SEXP rho);


#if R_version_less_than(3,0,0)
#define XLENGTH LENGTH
#define Rf_xlength Rf_length
#define R_xlen_t R_len_t
#define R_XLEN_T_MAX R_LEN_T_MAX
#define asXLength asLength
#endif


#if R_version_less_than(3,0,0)
#define NAMEDMAX 2
#define NO_REFERENCES(x) (NAMED(x) == 0)
#define MAYBE_REFERENCED(x) (! NO_REFERENCES(x))
#define MARK_NOT_MUTABLE(x) SET_NAMED(x, NAMEDMAX)
#endif


#if R_version_at_least(3,0,0)
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
#include "translations.h"
#define do_formals (SEXP args)
#define _do_start(name, numParameters)                         \
    args = CDR(args);                                          \
    if (Rf_length(args) < 3)                                   \
        Rf_error(_("in .External(), 'call', 'op', and 'rho' must be provided"));\
    if ((numParameters) > -1) {                                \
        int nargs = Rf_length(args) - 3;                       \
        if ((numParameters) != nargs)                          \
            Rf_errorcall(CAR(args),                            \
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


#if R_version_less_than(3,1,0)
#define INCREMENT_NAMED(x) do {                                \
    SEXP _x_ = (x);                                            \
    if (NAMED(_x_) != NAMEDMAX)                                \
        SET_NAMED(_x_, NAMED(_x_) + 1);                        \
} while (0)
#endif


#if R_version_at_least(3,1,0)
LibExtern SEXP R_TrueValue;
LibExtern SEXP R_FalseValue;
LibExtern SEXP R_LogicalNAValue;
#else
#define R_TrueValue Rf_ScalarLogical(TRUE)
#define R_FalseValue Rf_ScalarLogical(FALSE)
#define R_LogicalNAValue Rf_ScalarLogical(NA_LOGICAL)
#endif


#if R_version_less_than(3,1,0)
extern SEXP Rf_lazy_duplicate(SEXP s);
extern SEXP Rf_shallow_duplicate(SEXP s);
#endif
#if R_version_less_than(3,1,0) || R_version_at_least(4,5,0)
#define R_THIS_PATH_NEED_IS_SCALAR
extern int IS_SCALAR(SEXP x, int type);
#endif


#if R_version_less_than(3,2,0)
#define R_THIS_PATH_NEED_BLANKSCALARSTRING
extern SEXP R_BlankScalarString;
#endif


#if R_version_less_than(3,2,0)
#define Rf_installChar(x) Rf_install(R_CHAR((x)))
#endif


#if R_version_less_than(3,4,0)
#define R_CurrentExpression NULL
#define my_errorcall(call, ...) do {                           \
        SEXP call2 = (call);                                   \
        if (call2 == R_CurrentExpression)                      \
            Rf_error(__VA_ARGS__);                             \
        else                                                   \
            Rf_errorcall(call2, __VA_ARGS__);                  \
    } while (0)
#else
#define my_errorcall Rf_errorcall
#endif


#if R_version_less_than(3,5,0)
#define ENSURE_NAMEDMAX(_x_) SET_NAMED((_x_), NAMEDMAX)
#elif defined(R_THIS_PATH_DEVEL) || R_version_less_than(4,5,0)
extern void (ENSURE_NAMEDMAX)(SEXP x);
#else
#define ENSURE_NAMEDMAX(_x_) do { } while (0)
#endif


#if R_version_less_than(4,1,0) || (!defined(R_THIS_PATH_DEVEL) && R_version_at_least(4,5,0))
#define IS_UTF8(x) (Rf_getCharCE((x)) == CE_UTF8)
#else
extern int IS_UTF8(SEXP x);
#endif


#if R_version_less_than(4,1,0) || R_version_at_least(4,4,0) || !defined(R_THIS_PATH_DEVEL)
#define IS_LATIN1(x) (Rf_getCharCE((x)) == CE_LATIN1)
#else
extern int IS_LATIN1(SEXP x);
#endif


#define ENC_KNOWN(x) (IS_LATIN1((x)) || IS_UTF8((x)))


#if R_version_less_than(4,5,0)
extern SEXP Rf_allocLang(int n);
extern SEXP R_mkClosure(SEXP formals, SEXP body, SEXP rho);
#endif


#if !defined(R_THIS_PATH_DEVEL) && R_version_at_least(4,5,0)
#define Rf_isValidStringF my_Rf_isValidStringF
extern Rboolean Rf_isValidStringF(SEXP);
#endif


#endif /* R_THISPATH_BACKPORTS_H */
