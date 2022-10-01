#include <R.h>
#include <Rinternals.h>


#define debug 0


extern int get_drive_width(const char *s, int nchar);
extern int get_drive_width_unix(const char *s, int nchar);





#define SPLITEXT 0
#define REMOVEEXT 1
#define EXT 2
#define ext(windows, op)                                       \
{                                                              \
    if (debug) {                                               \
        if (op == SPLITEXT) {                                  \
            Rprintf((windows) ? "in do_windowssplitext\n\n" : "in do_unixsplitext\n\n");\
        }                                                      \
        else if (op == REMOVEEXT) {                            \
            Rprintf((windows) ? "in do_windowsremoveext\n\n" : "in do_unixremoveext\n\n");\
        }                                                      \
        else if (op == EXT) {                                  \
            Rprintf((windows) ? "in do_windowsext\n\n" : "in do_unixext\n\n");\
        }                                                      \
    }                                                          \
                                                               \
                                                               \
    int nprotect = 0;                                          \
                                                               \
                                                               \
    SEXP path = CADR(args);                                    \
    if (TYPEOF(path) != STRSXP)                                \
        error("a character vector argument expected");         \
                                                               \
                                                               \
    Rboolean compression = asLogical(CADDR(args));             \
    if (compression == NA_LOGICAL)                             \
        error("invalid 'compression' value");                  \
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
    } else {                                                   \
        value = allocVector(STRSXP, n);                        \
        PROTECT(value); nprotect++;                            \
    }                                                          \
                                                               \
                                                               \
    for (i = 0; i < n; i++) {                                  \
        if (debug) {                                           \
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
            } else {                                           \
                SET_STRING_ELT(value, i, NA_STRING);           \
                if (debug) {                                   \
                    Rprintf("assigning: NA\n");                \
                }                                              \
            }                                                  \
            if (debug) Rprintf("\n");                          \
            continue;                                          \
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
            if (debug) Rprintf("\n");                          \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        drivewidth = (windows) ? get_drive_width(ptr, nchar) : get_drive_width_unix(ptr, nchar);\
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
            if (debug) Rprintf("\n");                          \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        char _buf[nchar + 1];                                  \
        buf = _buf;                                            \
        strcpy(buf, ptr);                                      \
        if (debug) {                                           \
            Rprintf("made a buffer %d bytes long\n", nchar);   \
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
            Rprintf("after removing trailing slashes: \"%s\"\n", buf);\
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
            if (debug) Rprintf("\n");                          \
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
            if (debug) Rprintf("\n");                          \
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
                        if (debug) Rprintf("\n");              \
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
            if (debug) Rprintf("\n");                          \
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
            if (debug) Rprintf("\n");                          \
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
        if (debug) Rprintf("\n");                              \
    }                                                          \
                                                               \
                                                               \
    if (debug) Rprintf("\n");                                  \
                                                               \
                                                               \
    UNPROTECT(nprotect);                                       \
    return value;                                              \
}





SEXP do_windowssplitext(SEXP call, SEXP op, SEXP args, SEXP rho) ext(1, SPLITEXT)


SEXP do_unixsplitext(SEXP call, SEXP op, SEXP args, SEXP rho) ext(0, SPLITEXT)


SEXP do_splitext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (debug) {
        Rprintf("in do_splitext\n\n");
    }
    int windows = asLogical(eval(install("os.windows"), rho));
    if (windows == NA_LOGICAL)
        error("invalid 'os.windows'; should never happen, please report!");
    if (windows)
        return do_windowssplitext(call, op, args, rho);
    else return do_unixsplitext(call, op, args, rho);
}





SEXP do_windowsremoveext(SEXP call, SEXP op, SEXP args, SEXP rho) ext(1, REMOVEEXT)


SEXP do_unixremoveext(SEXP call, SEXP op, SEXP args, SEXP rho) ext(0, REMOVEEXT)


SEXP do_removeext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (debug) {
        Rprintf("in do_removeext\n\n");
    }
    int windows = asLogical(eval(install("os.windows"), rho));
    if (windows == NA_LOGICAL)
        error("invalid 'os.windows'; should never happen, please report!");
    if (windows)
        return do_windowsremoveext(call, op, args, rho);
    else return do_unixremoveext(call, op, args, rho);
}





SEXP do_windowsext(SEXP call, SEXP op, SEXP args, SEXP rho) ext(1, EXT)


SEXP do_unixext(SEXP call, SEXP op, SEXP args, SEXP rho) ext(0, EXT)


SEXP do_ext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (debug) {
        Rprintf("in do_ext\n\n");
    }
    int windows = asLogical(eval(install("os.windows"), rho));
    if (windows == NA_LOGICAL)
        error("invalid 'os.windows'; should never happen, please report!");
    if (windows)
        return do_windowsext(call, op, args, rho);
    else return do_unixext(call, op, args, rho);
}
