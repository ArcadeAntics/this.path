#include "thispathdefn.h"





static R_INLINE
SEXP path_split(int windows, int length1, SEXP args)
{
    SEXP path = CAR(args);
    if (TYPEOF(path) != STRSXP)
        Rf_error(_("a character vector argument expected"));


    int n = LENGTH(path);
    if (length1 && n != 1)
        Rf_error(_("'%s' must be a character string"), "path");


    SEXP value = Rf_allocVector(VECSXP, n);
    Rf_protect(value);
    for (int i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(path, i);
        if (cs == NA_STRING) {
            SET_VECTOR_ELT(value, i, Rf_ScalarString(NA_STRING));
            continue;
        }
        if (cs == R_BlankString) {
            SET_VECTOR_ELT(value, i, Rf_allocVector(STRSXP, 0));
            continue;
        }
        const char *str = R_CHAR(cs);
        cetype_t enc = Rf_getCharCE(cs);
        int nchar = (int) strlen(str);
        int nchar_scheme = is_url(str);
        if (nchar_scheme) {
            const char *p = strchr(str + nchar_scheme, '/');
            if (p == NULL) {
                SET_VECTOR_ELT(value, i, Rf_ScalarString(Rf_mkCharLenCE(str, nchar, enc)));
            } else {
                int nchar_prefix = p - str;
                const char *end = str + nchar;
                int nstrings = 1;
                while (p && p < end) {
                    while (*p == '/') p++;
                    if (p >= end) break;
                    nstrings++;
                    p = strchr(p, '/');
                }
                SEXP value0 = Rf_allocVector(STRSXP, nstrings);
                SET_VECTOR_ELT(value, i, value0);
                /* the prefix should INCLUDE the slash immediately afterward */
                SET_STRING_ELT(value0, 0, Rf_mkCharLenCE(str, nchar_prefix + 1, enc));
                p = str + nchar_prefix;
                for (int j = 1; j < nstrings; j++) {
                    while (*p == '/') p++;
                    if (p >= end) break;
                    const char *slash = strchr(p, '/');
                    if (slash == NULL) {
                        SET_STRING_ELT(value0, j, Rf_mkCharCE(p, enc));
                        break;
                    }
                    SET_STRING_ELT(value0, j, Rf_mkCharLenCE(p, slash - p, enc));
                    p = slash;
                }
            }
        }
        else {
            int drivewidth = _drive_width(windows, str);
            const char *p = str + drivewidth;
            const char *end = str + nchar;
            int nstrings = 0;
            if (windows) {
                while (p && p < end) {
                    while (*p == '/' || *p == '\\') p++;
                    if (p >= end) break;
                    nstrings++;
                    const char *slash = strchr(p, '/');
                    const char *bslash = strchr(p, '\\');
                    if (slash) {
                        if (bslash) {
                            p = (slash < bslash) ? slash : bslash;
                        } else {
                            p = slash;
                        }
                    } else {
                        if (bslash) {
                            p = bslash;
                        } else {
                            p = NULL;
                        }
                    }
                }
                if (drivewidth || *str == '/' || *str == '\\') nstrings++;
                SEXP value0 = Rf_allocVector(STRSXP, nstrings);
                SET_VECTOR_ELT(value, i, value0);
                int j = 0;
                if (drivewidth == 2) {
                    p = str + drivewidth;
                    if (*p == '/' || *p == '\\') {
                        char buf[3];
                        strncpy(buf, str, 2);
                        buf[2] = '/';
                        SET_STRING_ELT(value0, j++, Rf_mkCharLenCE(buf, 3, enc));
                    } else {
                        SET_STRING_ELT(value0, j++, Rf_mkCharLenCE(str, 2, enc));
                    }
                }
                else if (drivewidth == 1) {
                    SET_STRING_ELT(value0, j++, Rf_mkChar("~"));
                }
                else if (drivewidth) {
                    char _buf[drivewidth + 2];
                    char *buf = _buf;
                    *(buf++) = '/';
                    *(buf++) = '/';
                    p = str + 2;
                    const char *slash = strchr(p, '/');
                    const char *bslash = strchr(p, '\\');
                    if (slash) {
                        if (bslash) {
                            if (slash >= bslash) slash = bslash;
                        }
                    } else {
                        if (bslash) {
                            slash = bslash;
                        } else {
                            Rf_error("something went wrong");
                        }
                    }
                    int nchar = slash - p;
                    strncpy(buf, p, nchar);
                    buf += nchar;
                    *(buf++) = '/';
                    p = slash + 1;
                    while (*p == '/' || *p == '\\') p++;
                    nchar = str + drivewidth - p;
                    strncpy(buf, p, nchar);
                    buf += nchar;
                    p += nchar;
                    if (*p == '/' || *p == '\\') {
                        *(buf++) = '/';
                    }
                    *buf = '\0';
                    SET_STRING_ELT(value0, j++, Rf_mkCharCE(_buf, enc));
                }
                else if (*str == '/' || *str == '\\') {
                    SET_STRING_ELT(value0, j++, Rf_mkChar("/"));
                }
                p = str + drivewidth;
                for (; j < nstrings; j++) {
                    while (*p == '/' || *p == '\\') p++;
                    if (p >= end) break;
                    const char *slash = strchr(p, '/');
                    const char *bslash = strchr(p, '\\');
                    if (slash) {
                        if (bslash) {
                            if (slash >= bslash) slash = bslash;
                        }
                    } else {
                        if (bslash) {
                            slash = bslash;
                        } else {
                            SET_STRING_ELT(value0, j, Rf_mkCharCE(p, enc));
                            break;
                        }
                    }
                    SET_STRING_ELT(value0, j, Rf_mkCharLenCE(p, slash - p, enc));
                    p = slash;
                }
            }
            else {
                while (p && p < end) {
                    while (*p == '/') p++;
                    if (p >= end) break;
                    nstrings++;
                    p = strchr(p, '/');
                }
                if (drivewidth || *str == '/') nstrings++;
                SEXP value0 = Rf_allocVector(STRSXP, nstrings);
                SET_VECTOR_ELT(value, i, value0);
                int j = 0;
                if (drivewidth) {
                    char _buf[drivewidth + 2];
                    char *buf = _buf;
                    *(buf++) = '/';
                    *(buf++) = '/';
                    p = str + 2;
                    const char *slash = strchr(p, '/');
                    if (slash);
                    else Rf_error("something went wrong");
                    int nchar = slash - p;
                    strncpy(buf, p, nchar);
                    buf += nchar;
                    *(buf++) = '/';
                    p = slash + 1;
                    while (*p == '/') p++;
                    nchar = str + drivewidth - p;
                    strncpy(buf, p, nchar);
                    buf += nchar;
                    p += nchar;
                    if (*p == '/') {
                        *(buf++) = '/';
                    }
                    *buf = '\0';
                    SET_STRING_ELT(value0, j++, Rf_mkCharCE(_buf, enc));
                }
                else if (*str == '/') {
                    SET_STRING_ELT(value0, j++, Rf_mkChar("/"));
                }
                p = str + drivewidth;
                for (; j < nstrings; j++) {
                    while (*p == '/') p++;
                    if (p >= end) break;
                    const char *slash = strchr(p, '/');
                    if (slash);
                    else {
                        SET_STRING_ELT(value0, j, Rf_mkCharCE(p, enc));
                        break;
                    }
                    SET_STRING_ELT(value0, j, Rf_mkCharLenCE(p, slash - p, enc));
                    p = slash;
                }
            }
        }
    }


    if (length1) value = VECTOR_ELT(value, 0);
    Rf_unprotect(1);
    return value;
}


SEXP do_windows_path_split do_formals
{
    do_start_no_call_op_rho("windows_path_split", 1);
    return path_split(TRUE, FALSE, args);
}


SEXP do_unix_path_split do_formals
{
    do_start_no_call_op_rho("unix_path_split", 1);
    return path_split(FALSE, FALSE, args);
}


SEXP do_path_split do_formals
{
    do_start_no_call_op_rho("path_split", 1);
#if defined(_WIN32)
    return path_split(TRUE, FALSE, args);
#else
    return path_split(FALSE, FALSE, args);
#endif
}


SEXP do_windows_path_split_1 do_formals
{
    do_start_no_call_op_rho("windows_path_split_1", 1);
    return path_split(TRUE, TRUE, args);
}


SEXP do_unix_path_split_1 do_formals
{
    do_start_no_call_op_rho("unix_path_split_1", 1);
    return path_split(FALSE, TRUE, args);
}


SEXP do_path_split_1 do_formals
{
    do_start_no_call_op_rho("path_split_1", 1);
#if defined(_WIN32)
    return path_split(TRUE, TRUE, args);
#else
    return path_split(FALSE, TRUE, args);
#endif
}


static R_INLINE
SEXP path_unsplit(int windows, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP dots = Rf_findVarInFrame(rho, R_DotsSymbol);
    Rf_protect(dots); nprotect++;
    if (dots == R_UnboundValue)
        Rf_error(_("'...' used in an incorrect context"));


    int dots_length = ((TYPEOF(dots) == DOTSXP) ? Rf_length(dots) : 0);
    if (dots_length <= 0) {
        Rf_unprotect(nprotect);
        return Rf_allocVector(STRSXP, 0);
    }


    SEXP x;
    int n;
    if (dots_length == 1) {
        x = Rf_eval(CAR(dots), rho);
        if (Rf_isPairList(x)) {
            Rf_protect(x); nprotect++;
            n = Rf_length(x);
            /* verify that each element is a character vector */
            SEXP xptr = x;
            for (int i = 0; i < n; i++, xptr = CDR(xptr)) {
                if (TYPEOF(CAR(xptr)) != STRSXP)
                    Rf_error("invalid first argument, elements must be character vectors");
            }
        }
        else if (Rf_isVectorList(x)) {
            Rf_protect(x); nprotect++;
            n = LENGTH(x);
            /* verify that each element is a character vector */
            for (int i = 0; i < n; i++) {
                if (TYPEOF(VECTOR_ELT(x, i)) != STRSXP)
                    Rf_error("invalid first argument, elements must be character vectors");
            }
        }
        else if (TYPEOF(x) == STRSXP) {
            x = Rf_list1(x);
            Rf_protect(x); nprotect++;
            n = 1;
        }
        else {
            UNIMPLEMENTED_TYPE("path.unsplit", x);
            Rf_unprotect(nprotect);
            return R_NilValue;
        }
    } else {
        n = dots_length;
        x = Rf_allocVector(VECSXP, n);
        Rf_protect(x); nprotect++;
        SEXP d = dots;
        for (int i = 0; i < n; i++, d = CDR(d)) {
            SET_VECTOR_ELT(x, i, Rf_eval(CAR(d), rho));
            if (TYPEOF(VECTOR_ELT(x, i)) != STRSXP)
                UNIMPLEMENTED_TYPE("path.unsplit", VECTOR_ELT(x, i));
        }
    }


    Rboolean ispairlist = Rf_isPairList(x);
    SEXP value = Rf_allocVector(STRSXP, n);
    Rf_protect(value); nprotect++;


    Rboolean allLatin1, anyLatin1, use_UTF8, use_Bytes;


    for (int i = 0; i < n; i++) {
        SEXP path;
        if (ispairlist) {
            path = CAR(x);
            x = CDR(x);
        }
        else {
            path = VECTOR_ELT(x, i);
        }


        int nstrings = LENGTH(path);
        if (nstrings <= 0) {
            continue;
        }
        if (nstrings == 1) {
            SET_STRING_ELT(value, i, STRING_ELT(path, 0));
            continue;
        }


        anyLatin1 = FALSE; allLatin1 = TRUE; use_UTF8 = FALSE; use_Bytes = FALSE;
        SEXP cs;


        for (int j = 0; j < nstrings; j++) {
            cs = STRING_ELT(path, j);
            if (IS_BYTES(cs)) { use_Bytes = TRUE; break; }
            if (!use_UTF8) {
                if (IS_UTF8(cs)) use_UTF8 = TRUE;
                else {
                    allLatin1 = allLatin1 && (IS_ASCII(cs) || IS_LATIN1(cs));
                    anyLatin1 = anyLatin1 || IS_LATIN1(cs);
                }
            }
        }


        int pwidth = 0;
        for (int j = 0; j < nstrings; j++) {
            cs = STRING_ELT(path, j);
            if (use_Bytes)
                pwidth += (int) strlen(R_CHAR(cs));
            else if (use_UTF8)
                pwidth += (int) strlen(Rf_translateCharUTF8(cs));
            else if (anyLatin1 && allLatin1)
                pwidth += (int) strlen(R_CHAR(cs));
            else
                pwidth += (int) strlen(Rf_translateChar(cs));
        }
        /* a slash between each string plus a nul-terminating byte */
        pwidth += (int) nstrings;


        char _buf[pwidth];
        char *buf = _buf;


        cs = STRING_ELT(path, 0);
        const char *str;
        if (use_Bytes)
            str = R_CHAR(cs);
        else if (use_UTF8)
            str = Rf_translateCharUTF8(cs);
        else if (anyLatin1 && allLatin1)
            str = R_CHAR(cs);
        else
            str = Rf_translateChar(cs);
        int nchar = (int) strlen(str);


        if (windows) {
            /* if the last character is a slash, copy as is */
            if (*(str + nchar - 1) == '/' ||
                *(str + nchar - 1) == '\\')
            {
                memcpy(buf, str, nchar);
                buf += nchar;
            }
            /* if the string is a "d:" or similar, copy as is */
            else if (nchar == 2 && *str <= 0x7f && *(str + 1) == ':') {
                memcpy(buf, str, nchar);
                buf += nchar;
            }
            /* otherwise, add trailing slash */
            else {
                memcpy(buf, str, nchar);
                buf += nchar;
                *buf = '/';
                buf++;
            }
        } else {
            if (*(str + nchar - 1) == '/') {
                memcpy(buf, str, nchar);
                buf += nchar;
            }
            else {
                memcpy(buf, str, nchar);
                buf += nchar;
                *buf = '/';
                buf++;
            }
        }


        for (int j = 1; j < nstrings; j++) {
            cs = STRING_ELT(path, j);
            if (use_Bytes)
                str = R_CHAR(cs);
            else if (use_UTF8)
                str = Rf_translateCharUTF8(cs);
            else if (anyLatin1 && allLatin1)
                str = R_CHAR(cs);
            else
                str = Rf_translateChar(cs);
            int nchar = (int) strlen(str);


            memcpy(buf, str, nchar);
            buf += nchar;
            if (j < nstrings - 1) {
                *buf = '/';
                buf++;
            }
        }


        *buf = '\0';
        cetype_t enc = CE_NATIVE;
        if (use_Bytes) enc = CE_BYTES;
        else if (use_UTF8) enc = CE_UTF8;
        else if (anyLatin1 && allLatin1) enc = CE_LATIN1;
        SET_STRING_ELT(value, i, Rf_mkCharCE(_buf, enc));
    }


    Rf_unprotect(nprotect);
    return value;
}


SEXP do_windows_path_unsplit do_formals
{
    do_start_no_call_op("windows_path_unsplit", 0);
    return path_unsplit(TRUE, args, rho);
}


SEXP do_unix_path_unsplit do_formals
{
    do_start_no_call_op("unix_path_unsplit", 0);
    return path_unsplit(FALSE, args, rho);
}


SEXP do_path_unsplit do_formals
{
    do_start_no_call_op("path_unsplit", 0);
#if defined(_WIN32)
    return path_unsplit(TRUE, args, rho);
#else
    return path_unsplit(FALSE, args, rho);
#endif
}
