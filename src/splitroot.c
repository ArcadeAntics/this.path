/*
this.path : Get Executing Script's Path
Copyright (C) 2026   Iris Simmons
 */


#include "thispathdefn.h"


static R_INLINE
SEXP _do_splitroot(int windows, SEXP args)
{
    SEXP path = CAR(args);
    if (TYPEOF(path) != STRSXP)
        Rf_error(_("a character vector argument expected"));


    R_xlen_t n = Rf_xlength(path);
    if (n > INT_MAX) Rf_error(_("invalid 'ncol' value (too large or NA)"));
    SEXP value = Rf_allocMatrix(STRSXP, 3, (int) n);
    Rf_protect(value);
    SEXP dimnames = Rf_allocVector(VECSXP, 2);
    Rf_setAttrib(value, R_DimNamesSymbol, dimnames);
    SEXP rownames = Rf_allocVector(STRSXP, 3);
    SET_VECTOR_ELT(dimnames, 0, rownames);
    SET_STRING_ELT(rownames, 0, Rf_mkChar("drive"));
    SET_STRING_ELT(rownames, 1, Rf_mkChar("root"));
    SET_STRING_ELT(rownames, 2, Rf_mkChar("path"));


    if (n <= 0) { Rf_unprotect(1); return value; }


    for (R_xlen_t i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(path, i);
        if (cs == NA_STRING) {
            SET_STRING_ELT(value, 3 * i    , NA_STRING);
            SET_STRING_ELT(value, 3 * i + 1, NA_STRING);
            SET_STRING_ELT(value, 3 * i + 2, NA_STRING);
            continue;
        }
        if (cs == R_BlankString) continue;


        const char *s = R_CHAR(cs);
        cetype_t enc = Rf_getCharCE(cs);


        int drivewidth = _drive_width_no_tilde(windows, s);
        SET_STRING_ELT(value, 3 * i, Rf_mkCharLenCE(s, drivewidth, enc));
        s += drivewidth;


        if (windows) {
            if (*s == '/' || *s == '\\')
                SET_STRING_ELT(value, 3 * i + 1, Rf_mkCharLenCE(s++, 1, enc));
        } else {
            if (*s == '/')
                SET_STRING_ELT(value, 3 * i + 1, Rf_mkCharLenCE(s++, 1, enc));
        }


        SET_STRING_ELT(value, 3 * i + 2, Rf_mkCharCE(s, enc));
    }


    Rf_unprotect(1);
    return value;
}


SEXP do_splitroot_windows do_formals
{
    do_start_no_call_op_rho("splitroot_windows", 1);
    return _do_splitroot(TRUE, args);
}


SEXP do_splitroot_unix do_formals
{
    do_start_no_call_op_rho("splitroot_unix", 1);
    return _do_splitroot(FALSE, args);
}


SEXP do_splitroot do_formals
{
    do_start_no_call_op_rho("splitroot", 1);
#if defined(_WIN32)
    return _do_splitroot(TRUE, args);
#else
    return _do_splitroot(FALSE, args);
#endif
}


static R_INLINE
SEXP _do_splitdrive(int windows, SEXP args)
{
    SEXP path = CAR(args);
    if (TYPEOF(path) != STRSXP)
        Rf_error(_("a character vector argument expected"));


    R_xlen_t n = Rf_xlength(path);
    if (n > INT_MAX) Rf_error(_("invalid 'ncol' value (too large or NA)"));
    SEXP value = Rf_allocMatrix(STRSXP, 2, (int) n);
    Rf_protect(value);
    SEXP dimnames = Rf_allocVector(VECSXP, 2);
    Rf_setAttrib(value, R_DimNamesSymbol, dimnames);
    SEXP rownames = Rf_allocVector(STRSXP, 2);
    SET_VECTOR_ELT(dimnames, 0, rownames);
    SET_STRING_ELT(rownames, 0, Rf_mkChar("drive"));
    SET_STRING_ELT(rownames, 1, Rf_mkChar("path"));


    if (n <= 0) { Rf_unprotect(1); return value; }


    for (R_xlen_t i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(path, i);
        if (cs == NA_STRING) {
            SET_STRING_ELT(value, 2 * i    , NA_STRING);
            SET_STRING_ELT(value, 2 * i + 1, NA_STRING);
            continue;
        }
        if (cs == R_BlankString) continue;


        const char *s = R_CHAR(cs);
        cetype_t enc = Rf_getCharCE(cs);


        int drivewidth = _drive_width_no_tilde(windows, s);
        SET_STRING_ELT(value, 2 * i, Rf_mkCharLenCE(s, drivewidth, enc));
        s += drivewidth;


        SET_STRING_ELT(value, 2 * i + 1, Rf_mkCharCE(s, enc));
    }


    Rf_unprotect(1);
    return value;
}


SEXP do_splitdrive_windows do_formals
{
    do_start_no_call_op_rho("splitdrive_windows", 1);
    return _do_splitdrive(TRUE, args);
}


SEXP do_splitdrive_unix do_formals
{
    do_start_no_call_op_rho("splitdrive_unix", 1);
    return _do_splitdrive(FALSE, args);
}


SEXP do_splitdrive do_formals
{
    do_start_no_call_op_rho("splitdrive", 1);
#if defined(_WIN32)
    return _do_splitdrive(TRUE, args);
#else
    return _do_splitdrive(FALSE, args);
#endif
}
