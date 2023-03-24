#include <R.h>
#include <Rinternals.h>


#include "drivewidth.h"
#include "thispathbackports.h"
#include "translations.h"





#define looks_like_url(_STR_, _SCHEME_, _NCHAR_SCHEME_)        \
        (                                                      \
        !strncmp((_STR_), (_SCHEME_), (_NCHAR_SCHEME_)) &&     \
        (_STR_)[(_NCHAR_SCHEME_)] != '\0'               &&     \
        (_STR_)[(_NCHAR_SCHEME_)] != '/'                       \
        )


#define if_url_do_pathsplit(_SCHEME_, _NCHAR_SCHEME_)          \
        if (looks_like_url(str, (_SCHEME_), (_NCHAR_SCHEME_))) {\
            str = translateCharUTF8(path0);                    \
            int nchar = (int) strlen(str);                     \
            cetype_t enc = CE_UTF8;                            \
            const char *p = strchr(str + (_NCHAR_SCHEME_), '/');\
            if (p == NULL) {                                   \
                SET_VECTOR_ELT(value, i, ScalarString(mkCharLenCE(str, nchar, enc)));\
            } else {                                           \
                int nchar_prefix = p - str;                    \
                const char *end = str + nchar;                 \
                int nstrings = 1;                              \
                while (p && p < end) {                         \
                    while (*p == '/') p++;                     \
                    if (p >= end) break;                       \
                    nstrings++;                                \
                    p = strchr(p, '/');                        \
                }                                              \
                SEXP value0 = allocVector(STRSXP, nstrings);   \
                SET_VECTOR_ELT(value, i, value0);              \
                /* the prefix should INCLUDE the slash immediately afterward */\
                SET_STRING_ELT(value0, 0, mkCharLenCE(str, nchar_prefix + 1, enc));\
                p = str + nchar_prefix;                        \
                for (int j = 1; j < nstrings; j++) {           \
                    while (*p == '/') p++;                     \
                    if (p >= end) break;                       \
                    const char *slash = strchr(p, '/');        \
                    if (slash == NULL) {                       \
                        SET_STRING_ELT(value0, j, mkCharCE(p, enc));\
                        break;                                 \
                    }                                          \
                    SET_STRING_ELT(value0, j, mkCharLenCE(p, slash - p, enc));\
                    p = slash;                                 \
                }                                              \
            }                                                  \
        }


#define do_pathsplit_body(windows, length1)                    \
    SEXP path = CAR(args);                                     \
    if (TYPEOF(path) != STRSXP)                                \
        error(_("a character vector argument expected"));      \
                                                               \
                                                               \
    int n = LENGTH(path);                                      \
    if ((length1) && n != 1)                                   \
        error(_("'%s' must be a character string"), "path");   \
                                                               \
                                                               \
    SEXP value = allocVector(VECSXP, n);                       \
    PROTECT(value);                                            \
    for (int i = 0; i < n; i++) {                              \
        SEXP path0 = STRING_ELT(path, i);                      \
        if (path0 == NA_STRING) {                              \
            SET_VECTOR_ELT(value, i, ScalarString(NA_STRING)); \
            continue;                                          \
        }                                                      \
        if (path0 == R_BlankString) {                          \
            SET_VECTOR_ELT(value, i, allocVector(STRSXP, 0));  \
            continue;                                          \
        }                                                      \
        const char *str = CHAR(path0);                         \
                                                               \
                                                               \
        if_url_do_pathsplit("https://", 8)                     \
        else if_url_do_pathsplit("http://", 7)                 \
        else if_url_do_pathsplit("ftp://", 6)                  \
        else if_url_do_pathsplit("ftps://", 7)                 \
        else {                                                 \
            str = translateChar(path0);                        \
            int nchar = (int) strlen(str);                     \
            cetype_t enc = CE_NATIVE;                          \
            int drivewidth = get_drive_width(windows, str, nchar);\
            const char *p = str + drivewidth;                  \
            const char *end = str + nchar;                     \
            int nstrings = 0;                                  \
            if (windows) {                                     \
                while (p && p < end) {                         \
                    while (*p == '/' || *p == '\\') p++;       \
                    if (p >= end) break;                       \
                    nstrings++;                                \
                    const char *slash = strchr(p, '/');        \
                    const char *bslash = strchr(p, '\\');      \
                    if (slash) {                               \
                        if (bslash) {                          \
                            p = (slash < bslash) ? slash : bslash;\
                        } else {                               \
                            p = slash;                         \
                        }                                      \
                    } else {                                   \
                        if (bslash) {                          \
                            p = bslash;                        \
                        } else {                               \
                            p = NULL;                          \
                        }                                      \
                    }                                          \
                }                                              \
                if (drivewidth || *str == '/' || *str == '\\') nstrings++;\
                SEXP value0 = allocVector(STRSXP, nstrings);   \
                SET_VECTOR_ELT(value, i, value0);              \
                int j = 0;                                     \
                if (drivewidth == 2) {                         \
                    p = str + drivewidth;                      \
                    if (*p == '/' || *p == '\\') {             \
                        char buf[3];                           \
                        strncpy(buf, str, 2);                  \
                        buf[2] = '/';                          \
                        SET_STRING_ELT(value0, j++, mkCharLenCE(buf, 3, enc));\
                    } else {                                   \
                        SET_STRING_ELT(value0, j++, mkCharLenCE(str, 2, enc));\
                    }                                          \
                }                                              \
                else if (drivewidth == 1) {                    \
                    SET_STRING_ELT(value0, j++, mkCharLen("~", 1));\
                }                                              \
                else if (drivewidth) {                         \
                    char _buf[drivewidth + 2];                 \
                    char *buf = _buf;                          \
                    *(buf++) = '/';                            \
                    *(buf++) = '/';                            \
                    p = str + 2;                               \
                    const char *slash = strchr(p, '/');        \
                    const char *bslash = strchr(p, '\\');      \
                    if (slash) {                               \
                        if (bslash) {                          \
                            if (slash >= bslash) slash = bslash;\
                        }                                      \
                    } else {                                   \
                        if (bslash) {                          \
                            slash = bslash;                    \
                        } else {                               \
                            error("something went wrong");     \
                        }                                      \
                    }                                          \
                    int nchar = slash - p;                     \
                    strncpy(buf, p, nchar);                    \
                    buf += nchar;                              \
                    *(buf++) = '/';                            \
                    p = slash + 1;                             \
                    while (*p == '/' || *p == '\\') p++;       \
                    nchar = str + drivewidth - p;              \
                    strncpy(buf, p, nchar);                    \
                    buf += nchar;                              \
                    p += nchar;                                \
                    if (*p == '/' || *p == '\\') {             \
                        *(buf++) = '/';                        \
                    }                                          \
                    *buf = '\0';                               \
                    SET_STRING_ELT(value0, j++, mkCharCE(_buf, enc));\
                }                                              \
                else if (*str == '/' || *str == '\\') {        \
                    SET_STRING_ELT(value0, j++, mkCharLen("/", 1));\
                }                                              \
                p = str + drivewidth;                          \
                for (; j < nstrings; j++) {                    \
                    while (*p == '/' || *p == '\\') p++;       \
                    if (p >= end) break;                       \
                    const char *slash = strchr(p, '/');        \
                    const char *bslash = strchr(p, '\\');      \
                    if (slash) {                               \
                        if (bslash) {                          \
                            if (slash >= bslash) slash = bslash;\
                        }                                      \
                    } else {                                   \
                        if (bslash) {                          \
                            slash = bslash;                    \
                        } else {                               \
                            SET_STRING_ELT(value0, j, mkCharCE(p, enc));\
                            break;                             \
                        }                                      \
                    }                                          \
                    SET_STRING_ELT(value0, j, mkCharLenCE(p, slash - p, enc));\
                    p = slash;                                 \
                }                                              \
            }                                                  \
            else {                                             \
                while (p && p < end) {                         \
                    while (*p == '/') p++;                     \
                    if (p >= end) break;                       \
                    nstrings++;                                \
                    p = strchr(p, '/');                        \
                }                                              \
                if (drivewidth || *str == '/') nstrings++;     \
                SEXP value0 = allocVector(STRSXP, nstrings);   \
                SET_VECTOR_ELT(value, i, value0);              \
                int j = 0;                                     \
                if (drivewidth) {                              \
                    char _buf[drivewidth + 2];                 \
                    char *buf = _buf;                          \
                    *(buf++) = '/';                            \
                    *(buf++) = '/';                            \
                    p = str + 2;                               \
                    const char *slash = strchr(p, '/');        \
                    if (slash) {}                              \
                    else error("something went wrong");        \
                    int nchar = slash - p;                     \
                    strncpy(buf, p, nchar);                    \
                    buf += nchar;                              \
                    *(buf++) = '/';                            \
                    p = slash + 1;                             \
                    while (*p == '/') p++;                     \
                    nchar = str + drivewidth - p;              \
                    strncpy(buf, p, nchar);                    \
                    buf += nchar;                              \
                    p += nchar;                                \
                    if (*p == '/') {                           \
                        *(buf++) = '/';                        \
                    }                                          \
                    *buf = '\0';                               \
                    SET_STRING_ELT(value0, j++, mkCharCE(_buf, enc));\
                }                                              \
                else if (*str == '/') {                        \
                    SET_STRING_ELT(value0, j++, mkCharLen("/", 1));\
                }                                              \
                p = str + drivewidth;                          \
                for (; j < nstrings; j++) {                    \
                    while (*p == '/') p++;                     \
                    if (p >= end) break;                       \
                    const char *slash = strchr(p, '/');        \
                    if (slash) {}                              \
                    else {                                     \
                        SET_STRING_ELT(value0, j, mkCharCE(p, enc));\
                        break;                                 \
                    }                                          \
                    SET_STRING_ELT(value0, j, mkCharLenCE(p, slash - p, enc));\
                    p = slash;                                 \
                }                                              \
            }                                                  \
        }                                                      \
    }                                                          \
                                                               \
                                                               \
    if ((length1)) value = VECTOR_ELT(value, 0);               \
    UNPROTECT(1);                                              \
    return value


SEXP do_windowspathsplit do_formals
{
    do_start("windowspathsplit", 1);
    do_pathsplit_body(TRUE, FALSE);
}


SEXP do_unixpathsplit do_formals
{
    do_start("unixpathsplit", 1);
    do_pathsplit_body(FALSE, FALSE);
}


SEXP do_pathsplit do_formals
{
    do_start("pathsplit", 1);
#ifdef _WIN32
    do_pathsplit_body(TRUE, FALSE);
#else
    do_pathsplit_body(FALSE, FALSE);
#endif
}


SEXP do_windowspathsplit1 do_formals
{
    do_start("windowspathsplit1", 1);
    do_pathsplit_body(TRUE, TRUE);
}


SEXP do_unixpathsplit1 do_formals
{
    do_start("unixpathsplit1", 1);
    do_pathsplit_body(FALSE, TRUE);
}


SEXP do_pathsplit1 do_formals
{
    do_start("pathsplit1", 1);
#ifdef _WIN32
    do_pathsplit_body(TRUE, TRUE);
#else
    do_pathsplit_body(FALSE, TRUE);
#endif
}


#undef do_pathsplit_body





#define do_pathunsplit_body(windows)                           \
    SEXP dots = findVarInFrame(rho, R_DotsSymbol);             \
    if (dots == R_UnboundValue)                                \
        error(_("'...' used in an incorrect context"));        \
                                                               \
                                                               \
    int dots_length = ((TYPEOF(dots) == DOTSXP) ? length(dots) : 0);\
    if (dots_length <= 0) {                                    \
        return allocVector(STRSXP, 0);                         \
    }                                                          \
                                                               \
                                                               \
    SEXP x;                                                    \
    int n;                                                     \
    if (dots_length == 1) {                                    \
        x = eval(CAR(dots), rho);                              \
        if (isPairList(x)) {                                   \
            PROTECT(x);                                        \
            n = length(x);                                     \
            /* verify that each element is a character vector */\
            SEXP xptr = x;                                     \
            for (int i = 0; i < n; i++, xptr = CDR(xptr)) {    \
                if (TYPEOF(CAR(xptr)) != STRSXP)               \
                    error("%s, elements must be character vectors", _("invalid first argument"));\
            }                                                  \
        }                                                      \
        else if (isVectorList(x)) {                            \
            PROTECT(x);                                        \
            n = LENGTH(x);                                     \
            /* verify that each element is a character vector */\
            for (int i = 0; i < n; i++) {                      \
                if (TYPEOF(VECTOR_ELT(x, i)) != STRSXP)        \
                    error("%s, elements must be character vectors", _("invalid first argument"));\
            }                                                  \
        }                                                      \
        else if (TYPEOF(x) == STRSXP) {                        \
            x = list1(x);                                      \
            PROTECT(x);                                        \
            n = 1;                                             \
        }                                                      \
        else error(_("unimplemented type '%s' in '%s'"), type2char(TYPEOF(x)), "path.unsplit");\
    } else {                                                   \
        n = dots_length;                                       \
        x = allocVector(VECSXP, n);                            \
        PROTECT(x);                                            \
        SEXP d = dots;                                         \
        for (int i = 0; i < n; i++, d = CDR(d)) {              \
            SET_VECTOR_ELT(x, i, eval(CAR(d), rho));           \
            if (TYPEOF(VECTOR_ELT(x, i)) != STRSXP)            \
                error(_("unimplemented type '%s' in '%s'"), type2char(TYPEOF(VECTOR_ELT(x, i))), "path.unsplit");\
        }                                                      \
    }                                                          \
                                                               \
                                                               \
    Rboolean ispairlist = isPairList(x);                       \
    SEXP value = allocVector(STRSXP, n);                       \
    PROTECT(value);                                            \
    for (int i = 0; i < n; i++) {                              \
        SEXP x0;                                               \
        if (ispairlist) {                                      \
            x0 = CAR(x);                                       \
            x = CDR(x);                                        \
        }                                                      \
        else {                                                 \
            x0 = VECTOR_ELT(x, i);                             \
        }                                                      \
                                                               \
                                                               \
        int nstrings = LENGTH(x0);                             \
        if (nstrings == 0) {                                   \
            continue;                                          \
        }                                                      \
        if (nstrings == 1) {                                   \
            SET_STRING_ELT(value, i, STRING_ELT(x0, 0));       \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        unsigned int pwidth = 0;                               \
        SEXP x00 = STRING_ELT(x0, 0);                          \
        const char *str = CHAR(x00);                           \
        Rboolean tr2utf8 = (looks_like_url(str, "https://", 8) ||\
                            looks_like_url(str, "http://" , 7) ||\
                            looks_like_url(str, "ftp://"  , 6) ||\
                            looks_like_url(str, "ftps://" , 7));\
        cetype_t enc = (tr2utf8 ? CE_UTF8 : CE_NATIVE);        \
                                                               \
                                                               \
        for (int j = 0; j < nstrings; j++) {                   \
            x00 = STRING_ELT(x0, j);                           \
            str = (tr2utf8 ? translateCharUTF8(x00) :          \
                             translateChar(x00));              \
            pwidth += (int) strlen(str);                       \
        }                                                      \
        /* a slash between each string plus a nul-terminating byte */\
        pwidth += nstrings;                                    \
                                                               \
                                                               \
        char _buf[pwidth];                                     \
        char *buf = _buf;                                      \
                                                               \
                                                               \
        x00 = STRING_ELT(x0, 0);                               \
        str = (tr2utf8 ? translateCharUTF8(x00) :              \
                         translateChar(x00));                  \
        int nchar = (int) strlen(str);                         \
                                                               \
                                                               \
        if (windows) {                                         \
            /* if the last character is a slash, copy as is */ \
            if (*(str + nchar - 1) == '/' ||                   \
                *(str + nchar - 1) == '\\')                    \
            {                                                  \
                memcpy(buf, str, nchar);                       \
                buf += nchar;                                  \
            }                                                  \
            /* if the string is a "d:" or similar, copy as is */\
            else if (nchar == 2 && *str <= 0x7f && *(str + 1) == ':') {\
                memcpy(buf, str, nchar);                       \
                buf += nchar;                                  \
            }                                                  \
            /* otherwise, add trailing slash */                \
            else {                                             \
                memcpy(buf, str, nchar);                       \
                buf += nchar;                                  \
                *buf = '/';                                    \
                buf++;                                         \
            }                                                  \
        } else {                                               \
            if (*(str + nchar - 1) == '/') {                   \
                memcpy(buf, str, nchar);                       \
                buf += nchar;                                  \
            }                                                  \
            else {                                             \
                memcpy(buf, str, nchar);                       \
                buf += nchar;                                  \
                *buf = '/';                                    \
                buf++;                                         \
            }                                                  \
        }                                                      \
                                                               \
                                                               \
        for (int j = 1; j < nstrings; j++) {                   \
            x00 = STRING_ELT(x0, j);                           \
            str = (tr2utf8 ? translateCharUTF8(x00) :          \
                             translateChar(x00));              \
            int nchar = (int) strlen(str);                     \
                                                               \
                                                               \
            memcpy(buf, str, nchar);                           \
            buf += nchar;                                      \
            if (j < nstrings - 1) {                            \
                *buf = '/';                                    \
                buf++;                                         \
            }                                                  \
        }                                                      \
                                                               \
                                                               \
        *buf = '\0';                                           \
        SET_STRING_ELT(value, i, mkCharCE(_buf, enc));         \
    }                                                          \
                                                               \
                                                               \
    UNPROTECT(2);                                              \
    return value


SEXP do_windowspathunsplit do_formals
{
    do_start("windowspathunsplit", 0);
    do_pathunsplit_body(TRUE);
}


SEXP do_unixpathunsplit do_formals
{
    do_start("unixpathunsplit", 0);
    do_pathunsplit_body(FALSE);
}


SEXP do_pathunsplit do_formals
{
    do_start("pathunsplit", 0);
#ifdef _WIN32
    do_pathunsplit_body(TRUE);
#else
    do_pathunsplit_body(FALSE);
#endif
}


#undef do_pathunsplit_body
