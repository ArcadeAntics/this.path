#include "thispathdefn.h"
#include "drivewidth.h"


#define debug 0





#define SPLITEXT 0
#define REMOVEEXT 1
#define EXT 2
#define EXTGETS 3
#define ext(windows, op)                                       \
    int nprotect = 0;                                          \
                                                               \
                                                               \
    if (op == EXTGETS) {                                       \
        /* ext<-(), duplicate 'path' if we need to */          \
        if (MAYBE_REFERENCED(CAR(args))) {                     \
            SETCAR(args, R_shallow_duplicate_attr(CAR(args))); \
        }                                                      \
    }                                                          \
                                                               \
                                                               \
    SEXP path = CAR(args); args = CDR(args);                   \
    if (TYPEOF(path) != STRSXP)                                \
        error(_("a character vector argument expected"));      \
                                                               \
                                                               \
    Rboolean compression = asLogical(CAR(args)); args = CDR(args);\
    if (compression == NA_LOGICAL)                             \
        error(_("invalid '%s' value"), "compression");         \
                                                               \
                                                               \
    SEXP newext;                                               \
    int length_newext;                                         \
    if (op == EXTGETS) {                                       \
        if (!isString(CAR(args))) {                            \
            if (OBJECT(CAR(args))) {                           \
                SEXP expr = allocList(2);                      \
                PROTECT(expr);                                 \
                SET_TYPEOF(expr, LANGSXP);                     \
                SETCAR(expr, findVarInFrame(R_BaseEnv, R_AsCharacterSymbol));\
                SEXP expr2;                                    \
                SETCADR(expr, expr2 = allocList(2));           \
                SET_TYPEOF(expr2, LANGSXP);                    \
                SETCAR(expr2, findVarInFrame(R_BaseEnv, R_QuoteSymbol));\
                SETCADR(expr2, CAR(args));                     \
                SETCAR(args, eval(expr, rho));                 \
                UNPROTECT(1);                                  \
            }                                                  \
            else if (isSymbol(CAR(args)))                      \
                SETCAR(args, ScalarString(PRINTNAME(CAR(args))));\
            else SETCAR(args, coerceVector(CAR(args), STRSXP));\
            if (!isString(CAR(args)))                        \
                errorcall(call, _("non-string argument to '%s'"), "C_extgets");\
        }                                                      \
                                                               \
                                                               \
        length_newext = length(CAR(args));                     \
        if (!length_newext) {                                  \
            SETCAR(args, mkString(""));                        \
            length_newext = 1;                                 \
        }                                                      \
        newext = CAR(args);                                    \
    }                                                          \
                                                               \
                                                               \
    const char *ptr;                                           \
    char *buf, *pathspec, *last_char, *slash,                  \
         *basename, *dot, *compression_dot, *tmp;              \
    int n, i, nchar, drivewidth, nchar_basename, found_non_dot;\
                                                               \
                                                               \
    SEXP value;                                                \
    n = LENGTH(path);                                          \
    if (op == SPLITEXT) {                                      \
        value = allocMatrix(STRSXP, 2, n);                     \
        PROTECT(value); nprotect++;                            \
        SEXP dimnames = allocVector(VECSXP, 2);                \
        setAttrib(value, R_DimNamesSymbol, dimnames);          \
        SEXP rownames = allocVector(STRSXP, 2);                \
        SET_VECTOR_ELT(dimnames, 0, rownames);                 \
        SET_STRING_ELT(rownames, 0, mkChar("root"));           \
        SET_STRING_ELT(rownames, 1, mkChar("ext"));            \
    }                                                          \
    else if (op == EXTGETS) {                                  \
        value = path;                                          \
    }                                                          \
    else {                                                     \
        value = allocVector(STRSXP, n);                        \
        PROTECT(value); nprotect++;                            \
    }                                                          \
                                                               \
                                                               \
    char _buf_ext[PATH_MAX + 1];                               \
                                                               \
                                                               \
    for (i = 0; i < n; i++) {                                  \
        if (debug) {                                           \
            Rprintf("\n");                                     \
            Rprintf("string %d: ", i + 1);                     \
        }                                                      \
        if (STRING_ELT(path, i) == NA_STRING) {                \
            if (debug) Rprintf("NA\n");                        \
            if (op == SPLITEXT) {                              \
                SET_STRING_ELT(value, 2 * i    , NA_STRING);   \
                SET_STRING_ELT(value, 2 * i + 1, NA_STRING);   \
                if (debug) {                                   \
                    Rprintf("assigning: NA\n");                \
                    Rprintf("assigning: NA\n");                \
                }                                              \
            }                                                  \
            else if (op == REMOVEEXT || op == EXT) {           \
                SET_STRING_ELT(value, i, NA_STRING);           \
                if (debug) {                                   \
                    Rprintf("assigning: NA\n");                \
                }                                              \
            }                                                  \
            else if (op == EXTGETS) {                          \
                if (debug) {                                   \
                    Rprintf("assigning: NA\n");                \
                }                                              \
            }                                                  \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        if (op == EXTGETS) {                                   \
            if (STRING_ELT(newext, i % length_newext) == NA_STRING) {\
                SET_STRING_ELT(value, i, NA_STRING);           \
                continue;                                      \
            }                                                  \
        }                                                      \
                                                               \
                                                               \
        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));\
        nchar = strlen(ptr);                                   \
        if (debug) {                                           \
            Rprintf("\"%s\"\n", ptr);                          \
            Rprintf("%d bytes long\n", nchar);                 \
        }                                                      \
        if (nchar == 0) {                                      \
            if (op == SPLITEXT) {                              \
                if (debug) {                                   \
                    Rprintf("assigning: \"\"\n");              \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            } else {                                           \
                if (debug) {                                   \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            }                                                  \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        drivewidth = (windows) ? get_drive_width_windows(ptr, nchar) : get_drive_width_unix(ptr, nchar);\
        if (debug) {                                           \
            Rprintf("drivespec is %d bytes long\n", drivewidth);\
        }                                                      \
        if (nchar == drivewidth) {                             \
            if (debug) Rprintf("pathspec is 0 bytes long\n");  \
            if (op == SPLITEXT) {                              \
                SET_STRING_ELT(value, 2 * i, mkCharCE(ptr, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", ptr);       \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            }                                                  \
            else if (op == REMOVEEXT) {                        \
                SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", ptr);       \
                }                                              \
            }                                                  \
            else if (op == EXT) {                              \
                if (debug) {                                   \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            }                                                  \
            else if (op == EXTGETS) {                          \
                SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", ptr);       \
                }                                              \
            }                                                  \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        const char *ptr_ext;                                   \
        const char *cext;                                      \
        if (op == EXTGETS) {                                   \
            if (i < length_newext) {                           \
                ptr_ext = translateCharUTF8(STRING_ELT(newext, i));\
                if (windows) {                                 \
                    if (!strlen(ptr_ext))                      \
                        cext = ptr_ext;                        \
                    else {                                     \
                        const char *p = ptr_ext;               \
                        int add_dot = 1;                       \
                        if (*p == '.') {                       \
                            add_dot = 0;                       \
                            p++;                               \
                            if (!strlen(p)) error("extension \".\" is invalid");\
                            if (*p == '.')  error("extension starting with \"..\" is invalid");\
                        }                                      \
                        for (; *p; p++) {                      \
                            if (*p == '/' || *p == '\\') error("extension containing / is invalid");\
                            if (*p == '.') {                   \
                                if (strlen(p) == 3 &&          \
                                    *(p + 1) == 'g' &&         \
                                    *(p + 2) == 'z')           \
                                {                              \
                                    break;                     \
                                }                              \
                                if (strlen(p) == 4 &&          \
                                    *(p + 1) == 'b' &&         \
                                    *(p + 2) == 'z' &&         \
                                    *(p + 3) == '2')           \
                                {                              \
                                    break;                     \
                                }                              \
                                if (strlen(p) == 3 &&          \
                                    *(p + 1) == 'x' &&         \
                                    *(p + 2) == 'z')           \
                                {                              \
                                    break;                     \
                                }                              \
                                error("extension containing \".\" but is not a compression extension");\
                            }                                  \
                        }                                      \
                        if (add_dot) {                         \
                            if (strlen(ptr_ext) >= PATH_MAX)   \
                                error("file extension is too long");\
                            buf = _buf_ext;                    \
                            cext = _buf_ext;                   \
                            *buf = '.';                        \
                            strcpy(buf + 1, ptr_ext);          \
                        }                                      \
                        else cext = ptr_ext;                   \
                    }                                          \
                } else {                                       \
                    if (!strlen(ptr_ext))                      \
                        cext = ptr_ext;                        \
                    else {                                     \
                        const char *p = ptr_ext;               \
                        int add_dot = 1;                       \
                        if (*p == '.') {                       \
                            add_dot = 0;                       \
                            p++;                               \
                            if (!strlen(p)) error("extension \".\" is invalid");\
                            if (*p == '.')  error("extension starting with \"..\" is invalid");\
                        }                                      \
                        for (; *p; p++) {                      \
                            if (*p == '/') error("extension containing / is invalid");\
                            if (*p == '.') {                   \
                                if (strlen(p) == 3 &&          \
                                    *(p + 1) == 'g' &&         \
                                    *(p + 2) == 'z')           \
                                {                              \
                                    break;                     \
                                }                              \
                                if (strlen(p) == 4 &&          \
                                    *(p + 1) == 'b' &&         \
                                    *(p + 2) == 'z' &&         \
                                    *(p + 3) == '2')           \
                                {                              \
                                    break;                     \
                                }                              \
                                if (strlen(p) == 3 &&          \
                                    *(p + 1) == 'x' &&         \
                                    *(p + 2) == 'z')           \
                                {                              \
                                    break;                     \
                                }                              \
                                error("extension containing \".\" but is not a compression extension");\
                            }                                  \
                        }                                      \
                        if (add_dot) {                         \
                            if (strlen(ptr_ext) >= PATH_MAX)   \
                                error("file extension is too long");\
                            buf = _buf_ext;                    \
                            cext = _buf_ext;                   \
                            *buf = '.';                        \
                            strcpy(buf + 1, ptr_ext);          \
                        }                                      \
                        else cext = ptr_ext;                   \
                    }                                          \
                }                                              \
            } else {                                           \
                ptr_ext = translateCharUTF8(STRING_ELT(newext, i % length_newext));\
                if (!strlen(ptr_ext) || *ptr_ext == '.')       \
                    cext = ptr_ext;                            \
                else {                                         \
                    char _buf[strlen(ptr_ext) + 2];            \
                    buf = _buf;                                \
                    cext = _buf;                               \
                    *buf = '.';                                \
                    strcpy(buf + 1, ptr_ext);                  \
                }                                              \
            }                                                  \
            if (debug) Rprintf("ext %d: \"%s\"\n", i + 1, cext);\
        }                                                      \
                                                               \
                                                               \
        char _buf[(op != EXTGETS) ? (nchar + 1) : (nchar + strlen(cext) + 1)];\
        buf = _buf;                                            \
        strcpy(buf, ptr);                                      \
        if (debug) {                                           \
            Rprintf("made a buffer %d bytes long\n", strlen(_buf));\
            Rprintf("copied to buffer: \"%s\"\n", ptr);        \
            if (nchar != strlen(ptr)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr));\
        }                                                      \
        pathspec = buf + drivewidth;                           \
                                                               \
                                                               \
        /* point to the last character of buf */               \
        last_char = buf + (nchar - 1);                         \
        if (debug) {                                           \
            Rprintf("made variable last_char pointing to last byte\n");\
            Rprintf("last_char = \"%s\"\n", last_char);        \
            Rprintf("made variable pathspec pointing to start of pathspec\n");\
            Rprintf("pathspec                       : \"%s\"\n", pathspec);\
        }                                                      \
                                                               \
                                                               \
        /* remove trailing path separators */                  \
        if (windows) {                                         \
            while (last_char >= pathspec && (*last_char == '/' || *last_char == '\\')) {\
                *(last_char--) = '\0';                         \
            }                                                  \
        } else {                                               \
            while (last_char >= pathspec && *last_char == '/') {\
                *(last_char--) = '\0';                         \
            }                                                  \
        }                                                      \
        if (debug) {                                           \
            Rprintf("after removing trailing slashes: \"%s\"\n", pathspec);\
        }                                                      \
                                                               \
                                                               \
        /* if the pathspec was solely comprised of path separators */\
        /* then the basename is non-existent, return empty string  */\
        if (last_char < pathspec) {                            \
            if (debug) Rprintf("pathspec is /\n");             \
            if (op == SPLITEXT) {                              \
                SET_STRING_ELT(value, 2 * i, mkCharCE(ptr, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", ptr);       \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            }                                                  \
            else if (op == REMOVEEXT) {                        \
                SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", ptr);       \
                }                                              \
            }                                                  \
            else if (op == EXT) {                              \
                if (debug) {                                   \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            }                                                  \
            else if (op == EXTGETS) {                          \
                SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", ptr);       \
                }                                              \
            }                                                  \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        /* get the basename of 'pathspec' */                   \
        if (windows) {                                         \
            /* find the last slash/backslash */                \
                  slash     = strrchr(pathspec, '/');          \
            char *backslash = strrchr(pathspec, '\\');         \
                                                               \
                                                               \
            if (slash) {  /* slash was found */                \
                if (backslash) {  /* backslash was also found */\
                    if (slash < backslash)  /* slash found before backslash */\
                        basename = backslash + 1;              \
                    else basename = slash + 1;  /* backslash found before slash */\
                }                                              \
                else basename = slash + 1;  /* backslash was not found */\
            }                                                  \
            else {  /* slash was not found */                  \
                if (backslash)  /* backslash was found */      \
                    basename = backslash + 1;                  \
                else basename = pathspec;                      \
            }                                                  \
        } else {                                               \
            slash = strrchr(pathspec, '/');   /* find the last slash */\
            if (slash) basename = slash + 1;  /* slash was found */\
            else       basename = pathspec;   /* slash was not found */\
        }                                                      \
                                                               \
                                                               \
        nchar_basename = strlen(basename);                     \
        if (nchar_basename < 3) {                              \
            if (debug) Rprintf("basename contains less than 3 characters\n");\
            if (op == SPLITEXT) {                              \
                SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", buf);       \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            }                                                  \
            else if (op == REMOVEEXT) {                        \
                SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", buf);       \
                }                                              \
            }                                                  \
            else if (op == EXT) {                              \
                if (debug) {                                   \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            }                                                  \
            else if (op == EXTGETS) {                          \
                found_non_dot = 0;                             \
                for (tmp = basename; *tmp; tmp++) {            \
                    if (*tmp != '.') {                         \
                        found_non_dot = 1;                     \
                        break;                                 \
                    }                                          \
                }                                              \
                if (found_non_dot) {                           \
                    strcpy(last_char + 1, cext);               \
                }                                              \
                SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", buf);       \
                }                                              \
            }                                                  \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        if (compression) {                                     \
            compression_dot = NULL;                            \
            if (nchar_basename >= 6 &&                         \
                *(last_char - 2) == '.' &&                     \
                *(last_char - 1) == 'g' &&                     \
                *last_char       == 'z')                       \
            {                                                  \
                compression_dot = last_char - 2;               \
            }                                                  \
            else if (nchar_basename >= 7 &&                    \
                     *(last_char - 3) == '.' &&                \
                     *(last_char - 2) == 'b' &&                \
                     *(last_char - 1) == 'z' &&                \
                     *last_char       == '2')                  \
            {                                                  \
                compression_dot = last_char - 3;               \
            }                                                  \
            else if (nchar_basename >= 6 &&                    \
                     *(last_char - 2) == '.' &&                \
                     *(last_char - 1) == 'x' &&                \
                     *last_char       == 'z')                  \
            {                                                  \
                compression_dot = last_char - 2;               \
            }                                                  \
            if (compression_dot) {                             \
                *compression_dot = '\0';                       \
                dot = strrchr(basename, '.');                  \
                *compression_dot = '.';                        \
                                                               \
                                                               \
                if (dot && dot != (compression_dot - 1) && dot != basename) {\
                    /* there must be a non-dot character before the last dot */\
                    found_non_dot = 0;                         \
                    for (tmp = dot; tmp >= basename; tmp--) {  \
                        if (*tmp != '.') {                     \
                            found_non_dot = 1;                 \
                            break;                             \
                        }                                      \
                    }                                          \
                    if (found_non_dot) {                       \
                        if (op == SPLITEXT) {                  \
                            SET_STRING_ELT(value, 2 * i + 1, mkCharCE(dot, CE_UTF8));\
                            *dot = '\0';                       \
                            SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));\
                            if (debug) {                       \
                                Rprintf("assigning: \"%s\"\n", buf);\
                                Rprintf("assigning: \"%s\"\n", CHAR(STRING_ELT(value, 2 * i + 1)));\
                            }                                  \
                        }                                      \
                        else if (op == REMOVEEXT) {            \
                            *dot = '\0';                       \
                            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));\
                            if (debug) {                       \
                                Rprintf("assigning: \"%s\"\n", buf);\
                            }                                  \
                        }                                      \
                        else if (op == EXT) {                  \
                            SET_STRING_ELT(value, i, mkCharCE(dot, CE_UTF8));\
                            if (debug) {                       \
                                Rprintf("assigning: \"%s\"\n", dot);\
                            }                                  \
                        }                                      \
                        else if (op == EXTGETS) {              \
                            strcpy(dot, cext);                 \
                            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));\
                            if (debug) {                       \
                                Rprintf("assigning: \"%s\"\n", buf);\
                            }                                  \
                        }                                      \
                        continue;                              \
                    }                                          \
                }                                              \
                dot = compression_dot;                         \
            }                                                  \
            else dot = strrchr(basename, '.');                 \
        }                                                      \
        else dot = strrchr(basename, '.');                     \
                                                               \
                                                               \
        /* if we could not find a dot or dot is the first/last character */\
        if (!dot || dot == last_char || dot == basename) {     \
            if (debug) Rprintf("basename does not contain a dot, or dot is first/last character\n");\
            if (op == SPLITEXT) {                              \
                SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", buf);       \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            }                                                  \
            else if (op == REMOVEEXT) {                        \
                SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", buf);       \
                }                                              \
            }                                                  \
            else if (op == EXT) {                              \
                if (debug) {                                   \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            }                                                  \
            else if (op == EXTGETS) {                          \
                found_non_dot = 0;                             \
                for (tmp = basename; *tmp; tmp++) {            \
                    if (*tmp != '.') {                         \
                        found_non_dot = 1;                     \
                        break;                                 \
                    }                                          \
                }                                              \
                if (found_non_dot) {                           \
                    strcpy(last_char + 1, cext);               \
                }                                              \
                SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", buf);       \
                }                                              \
            }                                                  \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        /* there must be a non-dot character before the last dot */\
        found_non_dot = 0;                                     \
        for (tmp = dot; tmp >= basename; tmp--) {              \
            if (*tmp != '.') {                                 \
                found_non_dot = 1;                             \
                break;                                         \
            }                                                  \
        }                                                      \
        if (!found_non_dot) {                                  \
            if (debug) Rprintf("basename contains only leading dots\n");\
            if (op == SPLITEXT) {                              \
                SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", buf);       \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            }                                                  \
            else if (op == REMOVEEXT) {                        \
                SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", buf);       \
                }                                              \
            }                                                  \
            else if (op == EXT) {                              \
                if (debug) {                                   \
                    Rprintf("assigning: \"\"\n");              \
                }                                              \
            }                                                  \
            else if (op == EXTGETS) {                          \
                SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("assigning: \"%s\"\n", buf);       \
                }                                              \
            }                                                  \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        if (op == SPLITEXT) {                                  \
            SET_STRING_ELT(value, 2 * i + 1, mkCharCE(dot, CE_UTF8));\
            *dot = '\0';                                       \
            SET_STRING_ELT(value, 2 * i, mkCharCE(buf, CE_UTF8));\
            if (debug) {                                       \
                Rprintf("assigning: \"%s\"\n", buf);           \
                Rprintf("assigning: \"%s\"\n", CHAR(STRING_ELT(value, 2 * i + 1)));\
            }                                                  \
        }                                                      \
        else if (op == REMOVEEXT) {                            \
            *dot = '\0';                                       \
            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));  \
            if (debug) {                                       \
                Rprintf("assigning: \"%s\"\n", buf);           \
            }                                                  \
        }                                                      \
        else if (op == EXT) {                                  \
            SET_STRING_ELT(value, i, mkCharCE(dot, CE_UTF8));  \
            if (debug) {                                       \
                Rprintf("assigning: \"%s\"\n", dot);           \
            }                                                  \
        }                                                      \
        else if (op == EXTGETS) {                              \
            strcpy(dot, cext);                                 \
            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));  \
            if (debug) {                                       \
                Rprintf("assigning: \"%s\"\n", buf);           \
            }                                                  \
        }                                                      \
    }                                                          \
                                                               \
                                                               \
    if (debug) Rprintf("\n");                                  \
                                                               \
                                                               \
    UNPROTECT(nprotect);                                       \
    return value





SEXP do_windowssplitext do_formals
{
    do_start("windowssplitext", 2);
    if (debug) Rprintf("in do_windowssplitext\n\n");
    ext(1, SPLITEXT);
}


SEXP do_unixsplitext do_formals
{
    do_start("unixsplitext", 2);
    if (debug) Rprintf("in do_unixsplitext\n\n");
    ext(0, SPLITEXT);
}


SEXP do_splitext do_formals
{
    do_start("splitext", 2);
    if (debug) Rprintf("in do_splitext\n\n");
#ifdef _WIN32
    ext(1, SPLITEXT);
#else
    ext(0, SPLITEXT);
#endif
}





SEXP do_windowsremoveext do_formals
{
    do_start("windowsremoveext", 2);
    if (debug) Rprintf("in do_windowsremoveext\n\n");
    ext(1, REMOVEEXT);
}


SEXP do_unixremoveext do_formals
{
    do_start("unixremoveext", 2);
    if (debug) Rprintf("in do_unixremoveext\n\n");
    ext(0, REMOVEEXT);
}


SEXP do_removeext do_formals
{
    do_start("removeext", 2);
    if (debug) Rprintf("in do_removeext\n\n");
#ifdef _WIN32
    ext(1, REMOVEEXT);
#else
    ext(0, REMOVEEXT);
#endif
}





SEXP do_windowsext do_formals
{
    do_start("windowsext", 2);
    if (debug) Rprintf("in do_windowsext\n\n");
    ext(1, EXT);
}


SEXP do_unixext do_formals
{
    do_start("unixext", 2);
    if (debug) Rprintf("in do_unixext\n\n");
    ext(0, EXT);
}


SEXP do_ext do_formals
{
    do_start("ext", 2);
    if (debug) Rprintf("in do_ext\n\n");
#ifdef _WIN32
    ext(1, EXT);
#else
    ext(0, EXT);
#endif
}





SEXP do_windowsextgets do_formals
{
    do_start("windowsextgets", 3);
    if (debug) Rprintf("in do_windowsextgets\n\n");
    ext(1, EXTGETS);
}


SEXP do_unixextgets do_formals
{
    do_start("unixextgets", 3);
    if (debug) Rprintf("in do_unixextgets\n\n");
    ext(0, EXTGETS);
}


SEXP do_extgets do_formals
{
    do_start("extgets", 3);
    if (debug) Rprintf("in do_extgets\n\n");
#ifdef _WIN32
    ext(1, EXTGETS);
#else
    ext(0, EXTGETS);
#endif
}
