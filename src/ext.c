#include <R.h>
#include <Rinternals.h>


// #define debug


extern int get_drive_width(const char *s, int nchar);
extern int get_drive_width_unix(const char *s, int nchar);





SEXP do_windowssplitext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP path = CADR(args);
    Rboolean compression = asLogical(CADDR(args));
    if (compression == NA_LOGICAL)
        error("invalid 'compression' value");
    const char *ptr;
    char *buf, *pathspec, *last_char, *slash, *backslash, *basename, *dot, *compression_dot, *tmp;
    int n, i, nchar, drivewidth, nchar_basename, found_non_dot;


    if (TYPEOF(path) != STRSXP)
        error("a character vector argument expected");
    SEXP value = PROTECT(allocMatrix(STRSXP, 2, n = LENGTH(path))); nprotect++;
    SEXP dimnames = allocVector(VECSXP, 2);
    setAttrib(value, R_DimNamesSymbol, dimnames);
    SEXP rownames = allocVector(STRSXP, 2);
    SET_VECTOR_ELT(dimnames, 0, rownames);
    SET_STRING_ELT(rownames, 0, mkChar("root"));
    SET_STRING_ELT(rownames, 1, mkChar("ext"));
#ifdef debug
    Rprintf("in C_splitext\n\n");
#endif
    for (i = 0; i < n; i++) {
#ifdef debug
        Rprintf("string %d: ", i + 1);
#endif
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, 2 * i    , NA_STRING);
            SET_STRING_ELT(value, 2 * i + 1, NA_STRING);
#ifdef debug
            Rprintf("NA\n");
            Rprintf("assigning: NA\n");
            Rprintf("assigning: NA\n\n");
#endif
            continue;
        }
        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
        nchar = strlen(ptr);
#ifdef debug
        Rprintf("%s\n", ptr);
        Rprintf("%d bytes long\n", nchar);
#endif
        if (nchar == 0) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, 2 * i, mkChar(""));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("assigning: \n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }
        drivewidth = get_drive_width(ptr, nchar);
#ifdef debug
        Rprintf("drivespec is %d bytes long\n", drivewidth);
#endif
        if (nchar == drivewidth) {
            /* don't bother assigning an empty string, should already be empty */
            SET_STRING_ELT(value, 2 * i, mkCharCE(ptr, CE_UTF8));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("pathspec is 0 bytes long\n");
            Rprintf("assigning: %s\n", ptr);
            Rprintf("assigning: \n\n");
#endif
            continue;
        }
        /* allocate a buffer to hold the basename
         * I made this mistake before, don't forget to add 1 for the trailing '\0'
         */
        char _buf[nchar + 1];
        buf = _buf;
        strcpy(buf, ptr);
#ifdef debug
        Rprintf("made a buffer %d bytes long\n", nchar);
        Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
        if (nchar != strlen(ptr)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr));
#endif
        pathspec = buf + drivewidth;


        /* point to the last character of the buf */
        last_char = buf + (nchar - 1);
#ifdef debug
        Rprintf("made variable last_char pointing to last byte\n");
        Rprintf("last_char = %s", last_char); Rprintf("\n");
        Rprintf("made variable pathspec pointing to start of pathspec\n");
        Rprintf("pathspec                       : %s", pathspec); Rprintf("\n");
#endif
        /* remove the trailing / and \ */
        while (last_char >= pathspec &&
            (
                *last_char == '/' ||
                *last_char == '\\'
            ))
        {
            *(last_char--) = '\0';
        }
#ifdef debug
        Rprintf("after removing trailing slashes: %s", buf); Rprintf("\n");
#endif


        /* if the path specification was only comprised of / and \
         * then the basename is non-existent, return empty string */
        if (last_char < pathspec) {
            /* don't bother assigning an empty string, should already be empty */
            SET_STRING_ELT(value, 2 * i, mkCharCE(ptr, CE_UTF8));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("pathspec was /\n");
            Rprintf("assigning: %s\n", ptr);
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        /* find the last slash/backslash */
        slash     = strrchr(pathspec, '/');
        backslash = strrchr(pathspec, '\\');


        if (slash) {  /* slash was found */
            if (backslash) {  /* backslash was also found */
                if (slash < backslash)  /* slash found before backslash */
                    basename = backslash + 1;
                else basename = slash + 1;  /* backslash found before slash */
            }
            else basename = slash + 1;  /* backslash was not found */
        }
        else {  /* slash was not found */
            if (backslash)  /* backslash was found */
                basename = backslash + 1;
            else basename = pathspec;
        }


        nchar_basename = strlen(basename);
        if (nchar_basename < 3) {
            /* don't bother assigning an empty string, should already be empty */
            SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("basename contains less than 3 characters\n");
            Rprintf("assigning: %s\n", buf);
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        if (compression) {
            compression_dot = NULL;
            if (nchar_basename >= 6 &&
                *(last_char - 2) == '.' &&
                *(last_char - 1) == 'g' &&
                *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            else if (nchar_basename >= 7 &&
                     *(last_char - 3) == '.' &&
                     *(last_char - 2) == 'b' &&
                     *(last_char - 1) == 'z' &&
                     *last_char       == '2')
            {
                compression_dot = last_char - 3;
            }
            else if (nchar_basename >= 6 &&
                     *(last_char - 2) == '.' &&
                     *(last_char - 1) == 'x' &&
                     *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            if (compression_dot) {
                *compression_dot = '\0';
                dot = strrchr(basename, '.');
                *compression_dot = '.';


                if (dot && dot != (compression_dot - 1) && dot != basename) {
                    /* there must be a non-dot character before the last dot */
                    found_non_dot = 0;
                    for (tmp = dot; tmp >= basename; tmp--) {
                        if (*tmp != '.') {
                            found_non_dot = 1;
                            break;
                        }
                    }
                    if (found_non_dot) {
                        SET_STRING_ELT(value, 2 * i + 1, mkCharCE(dot, CE_UTF8));
                        *dot = '\0';
                        SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));
#ifdef debug
                        Rprintf("\n");
#endif
                        continue;
                    }
                }
                dot = compression_dot;
            }
            else dot = strrchr(basename, '.');
        }
        else dot = strrchr(basename, '.');


        /* if we could not find a dot or dot is the first/last character */
        if (!dot || dot == last_char || dot == basename) {
            /* don't bother assigning an empty string, should already be empty */
            SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("basename does not contain a dot, or dot is first/last character\n");
            Rprintf("assigning: %s\n", buf);
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        /* there must be a non-dot character before the last dot */
        found_non_dot = 0;
        for (tmp = dot; tmp >= basename; tmp--) {
            if (*tmp != '.') {
                found_non_dot = 1;
                break;
            }
        }
        if (!found_non_dot) {
            /* don't bother assigning an empty string, should already be empty */
            SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("basename contains only leading dots\n");
            Rprintf("assigning: %s\n", buf);
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        SET_STRING_ELT(value, 2 * i + 1, mkCharCE(dot, CE_UTF8));
        *dot = '\0';
        SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));
#ifdef debug
        Rprintf("\n");
#endif
    }
#ifdef debug
    Rprintf("\n");
#endif


    UNPROTECT(nprotect);
    return value;
}


SEXP do_unixsplitext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP path = CADR(args);
    Rboolean compression = asLogical(CADDR(args));
    if (compression == NA_LOGICAL)
        error("invalid 'compression' value");
    const char *ptr;
    char *buf, *pathspec, *last_char, *slash, *basename, *dot, *compression_dot, *tmp;
    int n, i, nchar, drivewidth, nchar_basename, found_non_dot;


    if (TYPEOF(path) != STRSXP)
        error("a character vector argument expected");
    SEXP value = PROTECT(allocMatrix(STRSXP, 2, n = LENGTH(path))); nprotect++;
    SEXP dimnames = allocVector(VECSXP, 2);
    setAttrib(value, R_DimNamesSymbol, dimnames);
    SEXP rownames = allocVector(STRSXP, 2);
    SET_VECTOR_ELT(dimnames, 0, rownames);
    SET_STRING_ELT(rownames, 0, mkChar("root"));
    SET_STRING_ELT(rownames, 1, mkChar("ext"));
#ifdef debug
    Rprintf("in C_splitext\n\n");
#endif
    for (i = 0; i < n; i++) {
#ifdef debug
        Rprintf("string %d: ", i + 1);
#endif
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, 2 * i    , NA_STRING);
            SET_STRING_ELT(value, 2 * i + 1, NA_STRING);
#ifdef debug
            Rprintf("NA\n");
            Rprintf("assigning: NA\n");
            Rprintf("assigning: NA\n\n");
#endif
            continue;
        }
        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
        nchar = strlen(ptr);
#ifdef debug
        Rprintf("%s\n", ptr);
        Rprintf("%d bytes long\n", nchar);
#endif
        if (nchar == 0) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, 2 * i, mkChar(""));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("assigning: \n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }
        drivewidth = get_drive_width_unix(ptr, nchar);
#ifdef debug
        Rprintf("drivespec is %d bytes long\n", drivewidth);
#endif
        if (nchar == drivewidth) {
            /* don't bother assigning an empty string, should already be empty */
            SET_STRING_ELT(value, 2 * i, mkCharCE(ptr, CE_UTF8));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("pathspec is 0 bytes long\n");
            Rprintf("assigning: %s\n", ptr);
            Rprintf("assigning: \n\n");
#endif
            continue;
        }
        /* allocate a buffer to hold the basename
         * I made this mistake before, don't forget to add 1 for the trailing '\0'
         */
        char _buf[nchar + 1];
        buf = _buf;
        strcpy(buf, ptr);
#ifdef debug
        Rprintf("made a buffer %d bytes long\n", nchar);
        Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
        if (nchar != strlen(ptr)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr));
#endif
        pathspec = buf + drivewidth;


        /* point to the last character of the buf */
        last_char = buf + (nchar - 1);
#ifdef debug
        Rprintf("made variable last_char pointing to last byte\n");
        Rprintf("last_char = %s", last_char); Rprintf("\n");
        Rprintf("made variable pathspec pointing to start of pathspec\n");
        Rprintf("pathspec                       : %s", pathspec); Rprintf("\n");
#endif
        /* remove trailing / */
        while (last_char >= pathspec && *last_char == '/') {
            *(last_char--) = '\0';
        }
#ifdef debug
        Rprintf("after removing trailing slashes: %s", buf); Rprintf("\n");
#endif


        /* if the path specification was only comprised of /
         * then the basename is non-existent, return empty string */
        if (last_char < pathspec) {
            /* don't bother assigning an empty string, should already be empty */
            SET_STRING_ELT(value, 2 * i, mkCharCE(ptr, CE_UTF8));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("pathspec was /\n");
            Rprintf("assigning: %s\n", ptr);
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        /* find the last slash */
        slash = strrchr(pathspec, '/');
        if (slash) basename = slash + 1;  /* slash was found */
        else       basename = pathspec;  /* slash was not found */


        nchar_basename = strlen(basename);
        if (nchar_basename < 3) {
            /* don't bother assigning an empty string, should already be empty */
            SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("basename contains less than 3 characters\n");
            Rprintf("assigning: %s\n", buf);
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        if (compression) {
            compression_dot = NULL;
            if (nchar_basename >= 6 &&
                *(last_char - 2) == '.' &&
                *(last_char - 1) == 'g' &&
                *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            else if (nchar_basename >= 7 &&
                     *(last_char - 3) == '.' &&
                     *(last_char - 2) == 'b' &&
                     *(last_char - 1) == 'z' &&
                     *last_char       == '2')
            {
                compression_dot = last_char - 3;
            }
            else if (nchar_basename >= 6 &&
                     *(last_char - 2) == '.' &&
                     *(last_char - 1) == 'x' &&
                     *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            if (compression_dot) {
                *compression_dot = '\0';
                dot = strrchr(basename, '.');
                *compression_dot = '.';


                if (dot && dot != (compression_dot - 1) && dot != basename) {
                    /* there must be a non-dot character before the last dot */
                    found_non_dot = 0;
                    for (tmp = dot; tmp >= basename; tmp--) {
                        if (*tmp != '.') {
                            found_non_dot = 1;
                            break;
                        }
                    }
                    if (found_non_dot) {
                        SET_STRING_ELT(value, 2 * i + 1, mkCharCE(dot, CE_UTF8));
                        *dot = '\0';
                        SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));
#ifdef debug
                        Rprintf("\n");
#endif
                        continue;
                    }
                }
                dot = compression_dot;
            }
            else dot = strrchr(basename, '.');
        }
        else dot = strrchr(basename, '.');


        /* if we could not find a dot or dot is the first/last character */
        if (!dot || dot == last_char || dot == basename) {
            /* don't bother assigning an empty string, should already be empty */
            SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("basename does not contain a dot, or dot is first/last character\n");
            Rprintf("assigning: %s\n", buf);
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        /* there must be a non-dot character before the last dot */
        found_non_dot = 0;
        for (tmp = dot; tmp >= basename; tmp--) {
            if (*tmp != '.') {
                found_non_dot = 1;
                break;
            }
        }
        if (!found_non_dot) {
            /* don't bother assigning an empty string, should already be empty */
            SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));
            // SET_STRING_ELT(value, 2 * i + 1, mkChar(""));
#ifdef debug
            Rprintf("basename contains only leading dots\n");
            Rprintf("assigning: %s\n", buf);
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        SET_STRING_ELT(value, 2 * i + 1, mkCharCE(dot, CE_UTF8));
        *dot = '\0';
        SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));
#ifdef debug
        Rprintf("\n");
#endif
    }
#ifdef debug
    Rprintf("\n");
#endif


    UNPROTECT(nprotect);
    return value;
}


SEXP do_splitext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int windows = asLogical(eval(install("os.windows"), rho));
    if (windows == NA_LOGICAL)
        error("invalid 'os.windows'; should never happen, please report!");
    if (windows)
        return do_windowssplitext(call, op, args, rho);
    else return do_unixsplitext(call, op, args, rho);
}





SEXP do_windowsremoveext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP path = CADR(args);
    Rboolean compression = asLogical(CADDR(args));
    if (compression == NA_LOGICAL)
        error("invalid 'compression' value");
    const char *ptr;
    char *buf, *pathspec, *last_char, *slash, *backslash, *basename, *dot, *compression_dot, *tmp;
    int n, i, nchar, drivewidth, nchar_basename, found_non_dot;


    if (TYPEOF(path) != STRSXP)
        error("a character vector argument expected");
    SEXP value = PROTECT(allocVector(STRSXP, n = LENGTH(path))); nprotect++;
#ifdef debug
    Rprintf("in C_removeext\n\n");
#endif
    for (i = 0; i < n; i++) {
#ifdef debug
        Rprintf("string %d: ", i + 1);
#endif
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
#ifdef debug
            Rprintf("NA\n");
            Rprintf("assigning: NA\n\n");
#endif
            continue;
        }
        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
        nchar = strlen(ptr);
#ifdef debug
        Rprintf("%s\n", ptr);
        Rprintf("%d bytes long\n", nchar);
#endif
        if (nchar == 0) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("assigning: \n\n");
#endif
            continue;
        }
        drivewidth = get_drive_width(ptr, nchar);
#ifdef debug
        Rprintf("drivespec is %d bytes long\n", drivewidth);
#endif
        if (nchar == drivewidth) {
            SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));
#ifdef debug
            Rprintf("pathspec is 0 bytes long\n");
            Rprintf("assigning: %s\n\n", ptr);
#endif
            continue;
        }
        /* allocate a buffer to hold the basename
         * I made this mistake before, don't forget to add 1 for the trailing '\0'
         */
        char _buf[nchar + 1];
        buf = _buf;
        strcpy(buf, ptr);
#ifdef debug
        Rprintf("made a buffer %d bytes long\n", nchar);
        Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
        if (nchar != strlen(ptr)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr));
#endif
        pathspec = buf + drivewidth;


        /* point to the last character of the buf */
        last_char = buf + (nchar - 1);
#ifdef debug
        Rprintf("made variable last_char pointing to last byte\n");
        Rprintf("last_char = %s", last_char); Rprintf("\n");
        Rprintf("made variable pathspec pointing to start of pathspec\n");
        Rprintf("pathspec                       : %s", pathspec); Rprintf("\n");
#endif
        /* remove the trailing / and \ */
        while (last_char >= pathspec &&
            (
                *last_char == '/' ||
                *last_char == '\\'
            ))
        {
            *(last_char--) = '\0';
        }
#ifdef debug
        Rprintf("after removing trailing slashes: %s", buf); Rprintf("\n");
#endif


        /* if the path specification was only comprised of / and \
         * then the basename is non-existent, return empty string */
        if (last_char < pathspec) {
            SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));
#ifdef debug
            Rprintf("pathspec was /\n");
            Rprintf("assigning: %s\n\n", ptr);
#endif
            continue;
        }


        /* find the last slash/backslash */
        slash     = strrchr(pathspec, '/');
        backslash = strrchr(pathspec, '\\');


        if (slash) {  /* slash was found */
            if (backslash) {  /* backslash was also found */
                if (slash < backslash)  /* slash found before backslash */
                    basename = backslash + 1;
                else basename = slash + 1;  /* backslash found before slash */
            }
            else basename = slash + 1;  /* backslash was not found */
        }
        else {  /* slash was not found */
            if (backslash)  /* backslash was found */
                basename = backslash + 1;
            else basename = pathspec;
        }


        nchar_basename = strlen(basename);
        if (nchar_basename < 3) {
            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
            Rprintf("basename contains less than 3 characters\n");
            Rprintf("assigning: %s\n\n", buf);
#endif
            continue;
        }


        if (compression) {
            compression_dot = NULL;
            if (nchar_basename >= 6 &&
                *(last_char - 2) == '.' &&
                *(last_char - 1) == 'g' &&
                *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            else if (nchar_basename >= 7 &&
                     *(last_char - 3) == '.' &&
                     *(last_char - 2) == 'b' &&
                     *(last_char - 1) == 'z' &&
                     *last_char       == '2')
            {
                compression_dot = last_char - 3;
            }
            else if (nchar_basename >= 6 &&
                     *(last_char - 2) == '.' &&
                     *(last_char - 1) == 'x' &&
                     *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            if (compression_dot) {
                *compression_dot = '\0';
                dot = strrchr(basename, '.');
                *compression_dot = '.';


                if (dot && dot != (compression_dot - 1) && dot != basename) {
                    /* there must be a non-dot character before the last dot */
                    found_non_dot = 0;
                    for (tmp = dot; tmp >= basename; tmp--) {
                        if (*tmp != '.') {
                            found_non_dot = 1;
                            break;
                        }
                    }
                    if (found_non_dot) {
                        *dot = '\0';
                        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
                        Rprintf("\n");
#endif
                        continue;
                    }
                }
                dot = compression_dot;
            }
            else dot = strrchr(basename, '.');
        }
        else dot = strrchr(basename, '.');


        /* if we could not find a dot or dot is the first/last character */
        if (!dot || dot == last_char || dot == basename) {
            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
            Rprintf("basename does not contain a dot, or dot is first/last character\n");
            Rprintf("assigning: %s\n\n", buf);
#endif
            continue;
        }


        /* there must be a non-dot character before the last dot */
        found_non_dot = 0;
        for (tmp = dot; tmp >= basename; tmp--) {
            if (*tmp != '.') {
                found_non_dot = 1;
                break;
            }
        }
        if (!found_non_dot) {
            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
            Rprintf("basename contains only leading dots\n");
            Rprintf("assigning: %s\n\n", buf);
#endif
            continue;
        }


        *dot = '\0';
        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
        Rprintf("\n");
#endif
    }
#ifdef debug
    Rprintf("\n");
#endif


    UNPROTECT(nprotect);
    return value;
}


SEXP do_unixremoveext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP path = CADR(args);
    Rboolean compression = asLogical(CADDR(args));
    if (compression == NA_LOGICAL)
        error("invalid 'compression' value");
    const char *ptr;
    char *buf, *pathspec, *last_char, *slash, *basename, *dot, *compression_dot, *tmp;
    int n, i, nchar, drivewidth, nchar_basename, found_non_dot;


    if (TYPEOF(path) != STRSXP)
        error("a character vector argument expected");
    SEXP value = PROTECT(allocVector(STRSXP, n = LENGTH(path))); nprotect++;
#ifdef debug
    Rprintf("in C_removeext\n\n");
#endif
    for (i = 0; i < n; i++) {
#ifdef debug
        Rprintf("string %d: ", i + 1);
#endif
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
#ifdef debug
            Rprintf("NA\n");
            Rprintf("assigning: NA\n\n");
#endif
            continue;
        }
        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
        nchar = strlen(ptr);
#ifdef debug
        Rprintf("%s\n", ptr);
        Rprintf("%d bytes long\n", nchar);
#endif
        if (nchar == 0) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("assigning: \n\n");
#endif
            continue;
        }
        drivewidth = get_drive_width_unix(ptr, nchar);
#ifdef debug
        Rprintf("drivespec is %d bytes long\n", drivewidth);
#endif
        if (nchar == drivewidth) {
            SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));
#ifdef debug
            Rprintf("pathspec is 0 bytes long\n");
            Rprintf("assigning: %s\n\n", ptr);
#endif
            continue;
        }
        /* allocate a buffer to hold the basename
         * I made this mistake before, don't forget to add 1 for the trailing '\0'
         */
        char _buf[nchar + 1];
        buf = _buf;
        strcpy(buf, ptr);
#ifdef debug
        Rprintf("made a buffer %d bytes long\n", nchar);
        Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
        if (nchar != strlen(ptr)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr));
#endif
        pathspec = buf + drivewidth;


        /* point to the last character of the buf */
        last_char = buf + (nchar - 1);
#ifdef debug
        Rprintf("made variable last_char pointing to last byte\n");
        Rprintf("last_char = %s", last_char); Rprintf("\n");
        Rprintf("made variable pathspec pointing to start of pathspec\n");
        Rprintf("pathspec                       : %s", pathspec); Rprintf("\n");
#endif
        /* remove trailing / */
        while (last_char >= pathspec && *last_char == '/') {
            *(last_char--) = '\0';
        }
#ifdef debug
        Rprintf("after removing trailing slashes: %s", buf); Rprintf("\n");
#endif


        /* if the path specification was only comprised of /
         * then the basename is non-existent, return empty string */
        if (last_char < pathspec) {
            SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));
#ifdef debug
            Rprintf("pathspec was /\n");
            Rprintf("assigning: %s\n\n", ptr);
#endif
            continue;
        }


        /* find the last slash */
        slash = strrchr(pathspec, '/');
        if (slash) basename = slash + 1;  /* slash was found */
        else       basename = pathspec;  /* slash was not found */


        nchar_basename = strlen(basename);
        if (nchar_basename < 3) {
            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
            Rprintf("basename contains less than 3 characters\n");
            Rprintf("assigning: %s\n\n", buf);
#endif
            continue;
        }


        if (compression) {
            compression_dot = NULL;
            if (nchar_basename >= 6 &&
                *(last_char - 2) == '.' &&
                *(last_char - 1) == 'g' &&
                *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            else if (nchar_basename >= 7 &&
                     *(last_char - 3) == '.' &&
                     *(last_char - 2) == 'b' &&
                     *(last_char - 1) == 'z' &&
                     *last_char       == '2')
            {
                compression_dot = last_char - 3;
            }
            else if (nchar_basename >= 6 &&
                     *(last_char - 2) == '.' &&
                     *(last_char - 1) == 'x' &&
                     *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            if (compression_dot) {
                *compression_dot = '\0';
                dot = strrchr(basename, '.');
                *compression_dot = '.';


                if (dot && dot != (compression_dot - 1) && dot != basename) {
                    /* there must be a non-dot character before the last dot */
                    found_non_dot = 0;
                    for (tmp = dot; tmp >= basename; tmp--) {
                        if (*tmp != '.') {
                            found_non_dot = 1;
                            break;
                        }
                    }
                    if (found_non_dot) {
                        *dot = '\0';
                        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
                        Rprintf("\n");
#endif
                        continue;
                    }
                }
                dot = compression_dot;
            }
            else dot = strrchr(basename, '.');
        }
        else dot = strrchr(basename, '.');


        /* if we could not find a dot or dot is the first/last character */
        if (!dot || dot == last_char || dot == basename) {
            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
            Rprintf("basename does not contain a dot, or dot is first/last character\n");
            Rprintf("assigning: %s\n\n", buf);
#endif
            continue;
        }


        /* there must be a non-dot character before the last dot */
        found_non_dot = 0;
        for (tmp = dot; tmp >= basename; tmp--) {
            if (*tmp != '.') {
                found_non_dot = 1;
                break;
            }
        }
        if (!found_non_dot) {
            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
            Rprintf("basename contains only leading dots\n");
            Rprintf("assigning: %s\n\n", buf);
#endif
            continue;
        }


        *dot = '\0';
        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
        Rprintf("\n");
#endif
    }
#ifdef debug
    Rprintf("\n");
#endif


    UNPROTECT(nprotect);
    return value;
}


SEXP do_removeext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int windows = asLogical(eval(install("os.windows"), rho));
    if (windows == NA_LOGICAL)
        error("invalid 'os.windows'; should never happen, please report!");
    if (windows)
        return do_windowsremoveext(call, op, args, rho);
    else return do_unixremoveext(call, op, args, rho);
}





SEXP do_windowsext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP path = CADR(args);
    Rboolean compression = asLogical(CADDR(args));
    if (compression == NA_LOGICAL)
        error("invalid 'compression' value");
    const char *ptr;
    char *buf, *pathspec, *last_char, *slash, *backslash, *basename, *dot, *compression_dot, *tmp;
    int n, i, nchar, drivewidth, nchar_basename, found_non_dot;


    if (TYPEOF(path) != STRSXP)
        error("a character vector argument expected");
    SEXP value = PROTECT(allocVector(STRSXP, n = LENGTH(path))); nprotect++;
#ifdef debug
    Rprintf("in C_ext\n\n");
#endif
    for (i = 0; i < n; i++) {
#ifdef debug
        Rprintf("string %d: ", i + 1);
#endif
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
#ifdef debug
            Rprintf("NA\n");
            Rprintf("assigning: NA\n\n");
#endif
            continue;
        }
        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
        nchar = strlen(ptr);
#ifdef debug
        Rprintf("%s\n", ptr);
        Rprintf("%d bytes long\n", nchar);
#endif
        if (nchar == 0) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("assigning: \n\n");
#endif
            continue;
        }
        drivewidth = get_drive_width(ptr, nchar);
#ifdef debug
        Rprintf("drivespec is %d bytes long\n", drivewidth);
#endif
        if (nchar == drivewidth) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("pathspec is 0 bytes long\n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }
        /* allocate a buffer to hold the basename
         * I made this mistake before, don't forget to add 1 for the trailing '\0'
         */
        char _buf[nchar + 1];
        buf = _buf;
        strcpy(buf, ptr);
#ifdef debug
        Rprintf("made a buffer %d bytes long\n", nchar);
        Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
        if (nchar != strlen(ptr)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr));
#endif
        pathspec = buf + drivewidth;


        /* point to the last character of the buf */
        last_char = buf + (nchar - 1);
#ifdef debug
        Rprintf("made variable last_char pointing to last byte\n");
        Rprintf("last_char = %s", last_char); Rprintf("\n");
        Rprintf("made variable pathspec pointing to start of pathspec\n");
        Rprintf("pathspec                       : %s", pathspec); Rprintf("\n");
#endif
        /* remove the trailing / and \ */
        while (last_char >= pathspec &&
            (
                *last_char == '/' ||
                *last_char == '\\'
            ))
        {
            *(last_char--) = '\0';
        }
#ifdef debug
        Rprintf("after removing trailing slashes: %s", buf); Rprintf("\n");
#endif


        /* if the path specification was only comprised of / and \
         * then the basename is non-existent, return empty string */
        if (last_char < pathspec) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("pathspec was /\n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        /* find the last slash/backslash */
        slash     = strrchr(pathspec, '/');
        backslash = strrchr(pathspec, '\\');


        if (slash) {  /* slash was found */
            if (backslash) {  /* backslash was also found */
                if (slash < backslash)  /* slash found before backslash */
                    basename = backslash + 1;
                else basename = slash + 1;  /* backslash found before slash */
            }
            else basename = slash + 1;  /* backslash was not found */
        }
        else {  /* slash was not found */
            if (backslash)  /* backslash was found */
                basename = backslash + 1;
            else basename = pathspec;
        }


        nchar_basename = strlen(basename);
        if (nchar_basename < 3) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("basename contains less than 3 characters\n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        if (compression) {
            compression_dot = NULL;
            if (nchar_basename >= 6 &&
                *(last_char - 2) == '.' &&
                *(last_char - 1) == 'g' &&
                *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            else if (nchar_basename >= 7 &&
                     *(last_char - 3) == '.' &&
                     *(last_char - 2) == 'b' &&
                     *(last_char - 1) == 'z' &&
                     *last_char       == '2')
            {
                compression_dot = last_char - 3;
            }
            else if (nchar_basename >= 6 &&
                     *(last_char - 2) == '.' &&
                     *(last_char - 1) == 'x' &&
                     *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            if (compression_dot) {
                *compression_dot = '\0';
                dot = strrchr(basename, '.');
                *compression_dot = '.';


                if (dot && dot != (compression_dot - 1) && dot != basename) {
                    /* there must be a non-dot character before the last dot */
                    found_non_dot = 0;
                    for (tmp = dot; tmp >= basename; tmp--) {
                        if (*tmp != '.') {
                            found_non_dot = 1;
                            break;
                        }
                    }
                    if (found_non_dot) {
                        SET_STRING_ELT(value, i, mkCharCE(dot, CE_UTF8));
#ifdef debug
                        Rprintf("\n");
#endif
                        continue;
                    }
                }
                dot = compression_dot;
            }
            else dot = strrchr(basename, '.');
        }
        else dot = strrchr(basename, '.');


        /* if we could not find a dot or dot is the first/last character */
        if (!dot || dot == last_char || dot == basename) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("basename does not contain a dot, or dot is first/last character\n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        /* there must be a non-dot character before the last dot */
        found_non_dot = 0;
        for (tmp = dot; tmp >= basename; tmp--) {
            if (*tmp != '.') {
                found_non_dot = 1;
                break;
            }
        }
        if (!found_non_dot) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("basename contains only leading dots\n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        SET_STRING_ELT(value, i, mkCharCE(dot, CE_UTF8));
#ifdef debug
        Rprintf("\n");
#endif
    }
#ifdef debug
    Rprintf("\n");
#endif


    UNPROTECT(nprotect);
    return value;
}


SEXP do_unixext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP path = CADR(args);
    Rboolean compression = asLogical(CADDR(args));
    if (compression == NA_LOGICAL)
        error("invalid 'compression' value");
    const char *ptr;
    char *buf, *pathspec, *last_char, *slash, *basename, *dot, *compression_dot, *tmp;
    int n, i, nchar, drivewidth, nchar_basename, found_non_dot;


    if (TYPEOF(path) != STRSXP)
        error("a character vector argument expected");
    SEXP value = PROTECT(allocVector(STRSXP, n = LENGTH(path))); nprotect++;
#ifdef debug
    Rprintf("in C_ext\n\n");
#endif
    for (i = 0; i < n; i++) {
#ifdef debug
        Rprintf("string %d: ", i + 1);
#endif
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
#ifdef debug
            Rprintf("NA\n");
            Rprintf("assigning: NA\n\n");
#endif
            continue;
        }
        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
        nchar = strlen(ptr);
#ifdef debug
        Rprintf("%s\n", ptr);
        Rprintf("%d bytes long\n", nchar);
#endif
        if (nchar == 0) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("assigning: \n\n");
#endif
            continue;
        }
        drivewidth = get_drive_width_unix(ptr, nchar);
#ifdef debug
        Rprintf("drivespec is %d bytes long\n", drivewidth);
#endif
        if (nchar == drivewidth) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("pathspec is 0 bytes long\n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }
        /* allocate a buffer to hold the basename
         * I made this mistake before, don't forget to add 1 for the trailing '\0'
         */
        char _buf[nchar + 1];
        buf = _buf;
        strcpy(buf, ptr);
#ifdef debug
        Rprintf("made a buffer %d bytes long\n", nchar);
        Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
        if (nchar != strlen(ptr)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr));
#endif
        pathspec = buf + drivewidth;


        /* point to the last character of the buf */
        last_char = buf + (nchar - 1);
#ifdef debug
        Rprintf("made variable last_char pointing to last byte\n");
        Rprintf("last_char = %s", last_char); Rprintf("\n");
        Rprintf("made variable pathspec pointing to start of pathspec\n");
        Rprintf("pathspec                       : %s", pathspec); Rprintf("\n");
#endif
        /* remove trailing / */
        while (last_char >= pathspec && *last_char == '/') {
            *(last_char--) = '\0';
        }
#ifdef debug
        Rprintf("after removing trailing slashes: %s", buf); Rprintf("\n");
#endif


        /* if the path specification was only comprised of /
         * then the basename is non-existent, return empty string */
        if (last_char < pathspec) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("pathspec was /\n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        /* find the last slash */
        slash = strrchr(pathspec, '/');
        if (slash) basename = slash + 1;  /* slash was found */
        else       basename = pathspec;  /* slash was not found */


        nchar_basename = strlen(basename);
        if (nchar_basename < 3) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("basename contains less than 3 characters\n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        if (compression) {
            compression_dot = NULL;
            if (nchar_basename >= 6 &&
                *(last_char - 2) == '.' &&
                *(last_char - 1) == 'g' &&
                *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            else if (nchar_basename >= 7 &&
                     *(last_char - 3) == '.' &&
                     *(last_char - 2) == 'b' &&
                     *(last_char - 1) == 'z' &&
                     *last_char       == '2')
            {
                compression_dot = last_char - 3;
            }
            else if (nchar_basename >= 6 &&
                     *(last_char - 2) == '.' &&
                     *(last_char - 1) == 'x' &&
                     *last_char       == 'z')
            {
                compression_dot = last_char - 2;
            }
            if (compression_dot) {
                *compression_dot = '\0';
                dot = strrchr(basename, '.');
                *compression_dot = '.';


                if (dot && dot != (compression_dot - 1) && dot != basename) {
                    /* there must be a non-dot character before the last dot */
                    found_non_dot = 0;
                    for (tmp = dot; tmp >= basename; tmp--) {
                        if (*tmp != '.') {
                            found_non_dot = 1;
                            break;
                        }
                    }
                    if (found_non_dot) {
                        SET_STRING_ELT(value, i, mkCharCE(dot, CE_UTF8));
#ifdef debug
                        Rprintf("\n");
#endif
                        continue;
                    }
                }
                dot = compression_dot;
            }
            else dot = strrchr(basename, '.');
        }
        else dot = strrchr(basename, '.');


        /* if we could not find a dot or dot is the first/last character */
        if (!dot || dot == last_char || dot == basename) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("basename does not contain a dot, or dot is first/last character\n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        /* there must be a non-dot character before the last dot */
        found_non_dot = 0;
        for (tmp = dot; tmp >= basename; tmp--) {
            if (*tmp != '.') {
                found_non_dot = 1;
                break;
            }
        }
        if (!found_non_dot) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
            Rprintf("basename contains only leading dots\n");
            Rprintf("assigning: \n\n");
#endif
            continue;
        }


        SET_STRING_ELT(value, i, mkCharCE(dot, CE_UTF8));
#ifdef debug
        Rprintf("\n");
#endif
    }
#ifdef debug
    Rprintf("\n");
#endif


    UNPROTECT(nprotect);
    return value;
}


SEXP do_ext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int windows = asLogical(eval(install("os.windows"), rho));
    if (windows == NA_LOGICAL)
        error("invalid 'os.windows'; should never happen, please report!");
    if (windows)
        return do_windowsext(call, op, args, rho);
    else return do_unixext(call, op, args, rho);
}
