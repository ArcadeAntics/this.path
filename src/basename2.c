/*
this.path : Get Executing Script's Path
Copyright (C) 2022-2025   Iris Simmons
 */


#include "thispathdefn.h"





static R_INLINE
SEXP basename2(int windows, SEXP args)
{
    int nprotect = 0;


    SEXP path = CAR(args);
    if (TYPEOF(path) != STRSXP)
        Rf_error(_("a character vector argument expected"));


    const char *s;
    char *p, *last_char, *slash;
    R_xlen_t n; int nchar, drivewidth;


    SEXP value = Rf_allocVector(STRSXP, n = Rf_xlength(path));
    Rf_protect(value); nprotect++;
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(path, i);
        if (cs == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
            continue;
        }


        s = R_CHAR(cs);
        nchar = strlen(s);
        if (nchar == 0) {
            /* don't bother assigning an empty string, should already be empty
            SET_STRING_ELT(value, i, R_BlankString); */
            continue;
        }


        drivewidth = _drive_width_no_tilde(windows, s);
        nchar -= drivewidth;  /* number of characters in the pathspec */
        if (nchar == 0) {
            /* don't bother assigning an empty string, should already be empty
            SET_STRING_ELT(value, i, R_BlankString); */
            continue;
        }
        s += drivewidth;


        /* allocate a buffer to hold the basename
         * add 1 for the trailing '\0'
         */
        char _buf[nchar + 1];
        p = _buf;
        strcpy(p, s);


        /* point to the last character of 'p' */
        last_char = p + (nchar - 1);


        /* remove trailing path separators */
        if (windows) {
            while (last_char >= p && (*last_char == '/' || *last_char == '\\')) {
                *(last_char--) = '\0';
            }
        } else {
            while (last_char >= p && *last_char == '/') {
                *(last_char--) = '\0';
            }
        }


        /* if the pathspec was comprised solely of path separators
         * then the basename is non-existent, return empty string
         */
        if (last_char < p) {
            /* don't bother assigning an empty string, should already be empty
            SET_STRING_ELT(value, i, R_BlankString); */
            continue;
        }


        if (windows) {
            /* find the last slash and backslash */
                  slash  = strrchr(p, '/');
            char *bslash = strrchr(p, '\\');


            if (slash) {  /* slash was found */
                if (bslash) {  /* backslash was also found */
                    if (slash < bslash)  /* slash found before backslash */
                        p = bslash + 1;
                    else p = slash + 1;  /* backslash found before slash */
                }
                else p = slash + 1;  /* backslash was not found */
            }
            else {  /* slash was not found */
                if (bslash)  /* backslash was found */
                    p = bslash + 1;
                else; /* pathspec contains no path separators so do nothing */
            }
        } else {
            /* find the last slash */
            slash = strrchr(p, '/');
            if (slash) p = slash + 1;
        }


        SET_STRING_ELT(value, i, Rf_mkCharCE(p, Rf_getCharCE(cs)));
    }


    Rf_unprotect(nprotect);
    return value;
}


SEXP do_windows_basename2 do_formals
{
    do_start_no_call_op_rho("windows_basename2", 1);
    return basename2(TRUE, args);
}


SEXP do_unix_basename2 do_formals
{
    do_start_no_call_op_rho("unix_basename2", 1);
    return basename2(FALSE, args);
}


SEXP do_basename2 do_formals
{
    do_start_no_call_op_rho("basename2", 1);
#if defined(_WIN32)
    return basename2(TRUE, args);
#else
    return basename2(FALSE, args);
#endif
}


/* it's not documented in the R function dirname2() or in man/dirname2.Rd
   but dirname2() actually accepts 1 or 2 arguments

   the first argument is always the 'path' argument
   the second argument (optional) is the number of additional times to
   calculate dirname2(). for example:

   .External2(.C_dirname2, path, 2)

   will calculate the dirname() once, then calculate it 2 more times
   afterward

   this saves miniscule time (because you don't have to setup a for loop
   at the R level) but it was easy enough to do so why not?
 */


static R_INLINE
SEXP dirname2(SEXP call, int windows, const char *name, SEXP args)
{
    int nprotect = 0;


    SEXP path;
    R_xlen_t times;
    switch (Rf_length(args)) {
    case 1:
        path = CAR(args);
        if (TYPEOF(path) != STRSXP)
            Rf_error(_("a character vector argument expected"));
        times = 0;
        break;
    case 2:
        path = CAR(args);
        if (TYPEOF(path) != STRSXP)
            Rf_error(_("a character vector argument expected"));
        times = asXLength(CADR(args));
        if (times < 0)
            Rf_errorcall(call, "invalid second argument, must be coercible to non-negative integer");
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), name, "1 or 2"));
        return R_NilValue;
    }
    SEXP cs;


    const char *s;
    char *p, *last_char, *slash, *pathspec;
    R_xlen_t n; int nchar, drivewidth, skip;


    SEXP value = Rf_allocVector(STRSXP, n = Rf_xlength(path));
    Rf_protect(value); nprotect++;
    for (R_xlen_t i = 0; i < n; i++) {
        cs = STRING_ELT(path, i);
        if (cs == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
            continue;
        }


        s = R_CHAR(cs);
        nchar = strlen(s);
        if (nchar == 0) {
            continue;
        }


        drivewidth = _drive_width_no_tilde(windows, s);
        if (drivewidth == nchar) {  /* pathspec is 0 bytes long */
            if (windows && drivewidth == 2) {
                char buf[4];
                buf[0] = s[0];
                buf[1] = s[1];
                buf[2] = '.';
                buf[3] = '\0';
                SET_STRING_ELT(value, i, Rf_mkCharCE(buf, Rf_getCharCE(cs)));
            }
            else {
                SET_STRING_ELT(value, i, Rf_mkCharCE(s, Rf_getCharCE(cs)));
            }
            continue;
        }


        char buf[nchar + 1];  /* allocate a buffer to hold the dirname */
        p = buf;
        strcpy(p, s);


        pathspec = p + drivewidth;  /* point to the start of the pathspec */


        /* point to the last character of p */
        last_char = p + (nchar - 1);


        skip = 0;
        for (R_xlen_t j = times; j >= 0; j--) {


            /* remove the trailing path separators
               we remove the trailing separators a little different than
               do_basename2()
               in this case, we only want to remove trailing slashes
               only if there is a non-slash before those trailing slashes */
            if (windows) {
                for (; last_char >= pathspec; last_char--) {
                    if (*last_char == '/' || *last_char == '\\');
                    else {
                        *(last_char + 1) = '\0';
                        break;
                    }
                }
            }
            else {
                for (; last_char >= pathspec; last_char--) {
                    if (*last_char == '/');
                    else {
                        *(last_char + 1) = '\0';
                        break;
                    }
                }
            }


            /* if the pathspec was comprised solely of path separators */
            /* then the dirname is just the whole path                 */
            if (last_char < pathspec) {
                SET_STRING_ELT(value, i, Rf_mkCharCE(p, Rf_getCharCE(cs)));
                skip = 1;
                break;
            }


            /* find the last path separator */
            if (windows) {
                      slash     = strrchr(pathspec, '/');
                char *bslash = strrchr(pathspec, '\\');


                if (slash) {  /* slash was found */
                    if (bslash) {  /* backslash was also found */
                        if (slash < bslash)  /* slash found before backslash */
                            last_char = bslash;
                        else last_char = slash;  /* backslash found before slash */
                    }
                    else last_char = slash;  /* backslash was not found */
                }
                else {  /* slash was not found */
                    if (bslash)  /* backslash was found */
                        last_char = bslash;
                    else {


                        /* for a drive with a pathspec without a path separator
                         * e.g. d:file
                         */
                        if (drivewidth) {
                            *pathspec = '.';
                            *(pathspec + 1) = '\0';
                            SET_STRING_ELT(value, i, Rf_mkCharCE(p, Rf_getCharCE(cs)));
                        }
                        else {
                            SET_STRING_ELT(value, i, Rf_mkChar("."));
                        }
                        skip = 1;
                        break;
                    }
                }
            }
            else {
                slash = strrchr(pathspec, '/');


                if (slash)  /* slash was found */
                    last_char = slash;
                else {  /* pathspec has no path separators */
                    SET_STRING_ELT(value, i, Rf_mkChar("."));
                    skip = 1;
                    break;
                }
            }


            /* remove the basename */
            *(last_char + 1) = '\0';


            /* last_char should already point to the last character of p */
        }
        if (skip) continue;


        /* remove the trailing path separators */
        if (windows) {
            for (; last_char >= pathspec; last_char--) {
                if (*last_char == '/' || *last_char == '\\');
                else {
                    *(last_char + 1) = '\0';
                    break;
                }
            }
        }
        else {
            for (; last_char >= pathspec; last_char--) {
                if (*last_char == '/');
                else {
                    *(last_char + 1) = '\0';
                    break;
                }
            }
        }


        SET_STRING_ELT(value, i, Rf_mkCharCE(p, Rf_getCharCE(cs)));
    }


    Rf_unprotect(nprotect);
    return value;
}


SEXP do_windows_dirname2 do_formals
{
    do_start_no_op_rho("windows_dirname2", -1);
    return dirname2(call, TRUE, ".C_windows_dirname2", args);
}


SEXP do_unix_dirname2 do_formals
{
    do_start_no_op_rho("unix_dirname2", -1);
    return dirname2(call, FALSE, ".C_unix_dirname2", args);
}


SEXP do_dirname2 do_formals
{
    do_start_no_op_rho("dirname2", -1);
#if defined(_WIN32)
    return dirname2(call, TRUE, ".C_dirname2", args);
#else
    return dirname2(call, FALSE, ".C_dirname2", args);
#endif
}
