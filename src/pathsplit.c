/*
this.path : Get Executing Script's Path
Copyright (C) 2023-2026   Iris Simmons
 */


#include "thispathdefn.h"





static R_INLINE
SEXP path_split(int windows, int length1, SEXP args)
{
    SEXP path = CAR(args);
    if (TYPEOF(path) != STRSXP)
        Rf_error(_("a character vector argument expected"));


    R_xlen_t n = XLENGTH(path);
    if (length1 && n != 1)
        Rf_error(_("'%s' must be a character string"), "path");


    SEXP value = Rf_allocVector(VECSXP, n);
    Rf_protect(value);
    for (R_xlen_t i = 0; i < n; i++) {
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
        int nchar_scheme = is_url(str);
        if (nchar_scheme) {
            const char *p = strchr(str + nchar_scheme, '/');
            if (p == NULL) {
                SET_VECTOR_ELT(value, i, Rf_ScalarString(cs));
            } else {
                int nchar_prefix = p - str;
                int nstrings = 1;
                do {
                    while (*p == '/') p++;
                    if (*p == '\0') break;
                    nstrings++;
                    p = strchr(p, '/');
                } while (p);
                SEXP value0 = Rf_allocVector(STRSXP, nstrings);
                SET_VECTOR_ELT(value, i, value0);
                /* the prefix should INCLUDE the slash immediately afterward */
                SET_STRING_ELT(value0, 0, Rf_mkCharLenCE(str, nchar_prefix + 1, enc));
                p = str + nchar_prefix + 1;
                for (int j = 1; j < nstrings; j++) {
                    while (*p == '/') p++;
                    if (*p == '\0') break;
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
            int nstrings = 0;
            if (windows) {
                if (drivewidth || *str == '/' || *str == '\\') nstrings++;
                while (*p) {
                    while (*p == '/' || *p == '\\') p++;
                    if (*p == '\0') break;
                    nstrings++;
                    while (*p && *p != '/' && *p != '\\') p++;
                }
                SEXP value0 = Rf_allocVector(STRSXP, nstrings);
                SET_VECTOR_ELT(value, i, value0);
                int j = 0;
                if (drivewidth == 2) {
                    p = str + drivewidth;
                    if (*p == '/' || *p == '\\') {
                        char buf[3];
                        memcpy(buf, str, 2);
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
                    *buf++ = '\\';
                    *buf++ = '\\';
                    p = str + 2;
                    const char *end = str + drivewidth;
                    while (p < end) {
                        if (*p == '/' || *p == '\\') {
                            *buf++ = '/'; p++;
                            while (p < end && (*p == '/' || *p == '\\')) p++;
                        }
                        else *buf++ = *p++;
                    }
                    if (*end == '/' || *end == '\\')
                        *buf++ = '/';
                    *buf = '\0';
                    SET_STRING_ELT(value0, j++, Rf_mkCharCE(_buf, enc));
                }
                else if (*str == '/' || *str == '\\') {
                    SET_STRING_ELT(value0, j++, Rf_mkChar("/"));
                }
                p = str + drivewidth;
                for (; j < nstrings; j++) {
                    while (*p == '/' || *p == '\\') p++;
                    if (*p == '\0') break;
                    const char *slash = p;
                    while (*slash && *slash != '/' && *slash != '\\') slash++;
                    if (*slash == '\0') {
                        SET_STRING_ELT(value0, j, Rf_mkCharCE(p, enc));
                        break;
                    }
                    SET_STRING_ELT(value0, j, Rf_mkCharLenCE(p, slash - p, enc));
                    p = slash;
                }
            }
            else {
                if (drivewidth || *str == '/') nstrings++;
                while (p) {
                    while (*p == '/') p++;
                    if (*p == '\0') break;
                    nstrings++;
                    p = strchr(p, '/');
                }
                SEXP value0 = Rf_allocVector(STRSXP, nstrings);
                SET_VECTOR_ELT(value, i, value0);
                int j = 0;
                if (drivewidth) {
                    char _buf[drivewidth + 2];
                    char *buf = _buf;
                    *buf++ = '/';
                    *buf++ = '/';
                    p = str + 2;
                    const char *end = str + drivewidth;
                    while (p < end) {
                        if (*p == '/') {
                            *buf++ = *p++;
                            while (p < end && *p == '/') p++;
                        }
                        else *buf++ = *p++;
                    }
                    if (*end == '/')
                        *buf++ = '/';
                    *buf = '\0';
                    SET_STRING_ELT(value0, j++, Rf_mkCharCE(_buf, enc));
                }
                else if (*str == '/') {
                    SET_STRING_ELT(value0, j++, Rf_mkChar("/"));
                }
                p = str + drivewidth;
                for (; j < nstrings; j++) {
                    while (*p == '/') p++;
                    if (*p == '\0') break;
                    const char *slash = strchr(p, '/');
                    if (!slash) {
                        SET_STRING_ELT(value0, j, Rf_mkCharCE(p, enc));
                        break;
                    }
                    SET_STRING_ELT(value0, j, Rf_mkCharLenCE(p, slash - p, enc));
                    p = slash;
                }
            }
        }
    }


    if (length1) {
        SEXP tmp = value;
        value = VECTOR_ELT(value, 0);
        SET_VECTOR_ELT(tmp, 0, R_NilValue);  /* decrease reference count */
    }
    Rf_unprotect(1);
    return value;
}


SEXP do_path_split_windows do_formals
{
    do_start_no_call_op_rho("path_split_windows", 1);
    return path_split(TRUE, FALSE, args);
}


SEXP do_path_split_unix do_formals
{
    do_start_no_call_op_rho("path_split_unix", 1);
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


SEXP do_path_split_1_windows do_formals
{
    do_start_no_call_op_rho("path_split_1_windows", 1);
    return path_split(TRUE, TRUE, args);
}


SEXP do_path_split_1_unix do_formals
{
    do_start_no_call_op_rho("path_split_1_unix", 1);
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


    SEXP dots = my_findValInFrame(rho, R_DotsSymbol);
    Rf_protect(dots); nprotect++;
    if (dots == my_UnboundValue)
        Rf_error(_("'...' used in an incorrect context"));


    int dots_length = length_DOTS(dots);
    if (dots_length <= 0) {
        Rf_unprotect(nprotect);
        return Rf_allocVector(STRSXP, 0);
    }


    SEXP x;
    R_xlen_t n;
    if (dots_length == 1) {
        x = CAR(dots);
        if (x == R_MissingArg) {
            char buf[15];
            snprintf(buf, 15, "..%d", 1);
            MissingArgError_c(buf, R_CurrentExpression, rho, "evalError");
        }
        else if (TYPEOF(x) == PROMSXP)
            x = Rf_eval(x, rho);
        if (Rf_isPairList(x)) {
            Rf_protect(x); nprotect++;
            n = Rf_xlength(x);
            /* verify that each element is a character vector */
            SEXP xptr = x;
            for (R_xlen_t i = 0; i < n; i++, xptr = CDR(xptr)) {
                if (TYPEOF(CAR(xptr)) != STRSXP)
                    Rf_error("invalid first argument, elements must be character vectors");
            }
        }
        else if (Rf_isVectorList(x)) {
            Rf_protect(x); nprotect++;
            n = Rf_xlength(x);
            /* verify that each element is a character vector */
            for (R_xlen_t i = 0; i < n; i++) {
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
        for (R_xlen_t i = 0; i < n; i++, d = CDR(d)) {
            SEXP xi = CAR(d);
            if (xi == R_MissingArg) {
                /* if the last argument is missing, allow it */
                if (i == dots_length - 1) { n--; continue; }
                char buf[15];
                snprintf(buf, 15, "..%d", (int) i + 1);
                MissingArgError_c(buf, R_CurrentExpression, rho, "evalError");
            }
            else if (TYPEOF(xi) == PROMSXP)
                xi = Rf_eval(xi, rho);
            SET_VECTOR_ELT(x, i, xi);
            if (TYPEOF(VECTOR_ELT(x, i)) != STRSXP)
                UNIMPLEMENTED_TYPE("path.unsplit", VECTOR_ELT(x, i));
        }
    }


    Rboolean ispairlist = Rf_isPairList(x);
    SEXP value = Rf_allocVector(STRSXP, n);
    Rf_protect(value); nprotect++;


    Rboolean allLatin1, anyLatin1, use_UTF8, use_Bytes;


    for (R_xlen_t i = 0; i < n; i++) {
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
                *buf++ = '/';
            }
        } else {
            if (*(str + nchar - 1) == '/') {
                memcpy(buf, str, nchar);
                buf += nchar;
            }
            else {
                memcpy(buf, str, nchar);
                buf += nchar;
                *buf++ = '/';
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
            if (j < nstrings - 1) *buf++ = '/';
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


SEXP do_path_unsplit_windows do_formals
{
    do_start_no_call_op("path_unsplit_windows", 0);
    return path_unsplit(TRUE, args, rho);
}


SEXP do_path_unsplit_unix do_formals
{
    do_start_no_call_op("path_unsplit_unix", 0);
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
