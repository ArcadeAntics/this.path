#include <R.h>
#include <Rinternals.h>


#define debug 0


extern int get_drive_width(const char *s, int nchar);
extern int get_drive_width_unix(const char *s, int nchar);





#define basename2(windows)                                     \
{                                                              \
    if (debug) {                                               \
        Rprintf((windows) ? "in do_windowsbasename2\n\n" : "in do_unixbasename2\n\n");\
    }                                                          \
                                                               \
                                                               \
    int nprotect = 0;                                          \
                                                               \
                                                               \
    SEXP path = CADR(args);                                    \
    const char *ptr;                                           \
    char *buf, *last_char, *slash;                             \
    int n, i, nchar, drivewidth;                               \
                                                               \
                                                               \
    if (TYPEOF(path) != STRSXP)                                \
        error("a character vector argument expected");         \
    SEXP value = PROTECT(allocVector(STRSXP, n = LENGTH(path))); nprotect++;\
    for (i = 0; i < n; i++) {                                  \
        if (debug) {                                           \
            Rprintf("string %d: ", i + 1);                     \
        }                                                      \
        if (STRING_ELT(path, i) == NA_STRING) {                \
            SET_STRING_ELT(value, i, NA_STRING);               \
            if (debug) {                                       \
                Rprintf("NA\n");                               \
                Rprintf("assigning: NA\n\n");                  \
            }                                                  \
            continue;                                          \
        }                                                      \
        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));\
        nchar = strlen(ptr);                                   \
        if (debug) {                                           \
            Rprintf("\"%s\"\n", ptr);                          \
            Rprintf("%d bytes long\n", nchar);                 \
        }                                                      \
        if (nchar == 0) {                                      \
            /* don't bother assigning an empty string, should already be empty */\
            /* SET_STRING_ELT(value, i, mkChar("")); */        \
            if (debug) {                                       \
                Rprintf("assigning: \"\"\n\n");                \
            }                                                  \
            continue;                                          \
        }                                                      \
        drivewidth = (windows) ? get_drive_width(ptr, nchar) : get_drive_width_unix(ptr, nchar);\
        if (debug) {                                           \
            Rprintf("drivespec is %d bytes long\n", drivewidth);\
        }                                                      \
        nchar -= drivewidth;  /* number of characters EXCLUDING the drive */\
        if (nchar == 0) {                                      \
            /* don't bother assigning an empty string, should already be empty */\
            /* SET_STRING_ELT(value, i, mkChar("")); */        \
            if (debug) {                                       \
                Rprintf("pathspec is 0 bytes long\n");         \
                Rprintf("assigning: \"\"\n\n");                \
            }                                                  \
            continue;                                          \
        }                                                      \
        ptr += drivewidth;                                     \
                                                               \
                                                               \
        /* allocate a buffer to hold the basename */           \
        /* I made this mistake before, don't forget to add 1 for the trailing '\0' */\
        char _buf[nchar + 1];                                  \
        buf = _buf;                                            \
        strcpy(buf, ptr);                                      \
        if (debug) {                                           \
            Rprintf("made a buffer %d bytes long\n", nchar);   \
            Rprintf("copied to buffer: \"%s\"\n", ptr);        \
            if (nchar != strlen(ptr)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr));\
        }                                                      \
                                                               \
                                                               \
        /* point to the last character of the buf */           \
        last_char = buf + (nchar - 1);                         \
        if (debug) {                                           \
            Rprintf("made variable last_char pointing to last byte\n");\
            Rprintf("last_char = \"%s\"\n", last_char);        \
        }                                                      \
                                                               \
                                                               \
        /* remove trailing path separators */                  \
        if (windows) {                                         \
            while (last_char >= buf && (*last_char == '/' || *last_char == '\\')) {\
                *(last_char--) = '\0';                         \
            }                                                  \
        } else {                                               \
            while (last_char >= buf && *last_char == '/') {    \
                *(last_char--) = '\0';                         \
            }                                                  \
        }                                                      \
        if (debug) {                                           \
            Rprintf("after removing trailing slashes: \"%s\"\n", buf);\
        }                                                      \
                                                               \
                                                               \
        /* if the pathspec was comprised solely of path separators */\
        /* then the basename is non-existent, return empty string */\
        if (last_char < buf) {                                 \
            /* don't bother assigning an empty string, should already be empty */\
            /* SET_STRING_ELT(value, i, mkChar("")); */        \
            if (debug) {                                       \
                Rprintf("pathspec is /\n");                    \
                Rprintf("assigning: \"\"\n\n");                \
            }                                                  \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        if (windows) {                                         \
            /* find the last slash and backslash */            \
                  slash     = strrchr(buf, '/');               \
            char *backslash = strrchr(buf, '\\');              \
                                                               \
                                                               \
            if (slash) {  /* slash was found */                \
                if (backslash) {  /* backslash was also found */\
                    if (slash < backslash)  /* slash found before backslash */\
                        buf = backslash + 1;                   \
                    else buf = slash + 1;  /* backslash found before slash */\
                }                                              \
                else buf = slash + 1;  /* backslash was not found */\
            }                                                  \
            else {  /* slash was not found */                  \
                if (backslash)  /* backslash was found */      \
                    buf = backslash + 1;                       \
                else { /* pathspec contains no path separators so do nothing */ }\
            }                                                  \
        } else {                                               \
            /* find the last slash */                          \
            slash = strrchr(buf, '/');                         \
            if (slash) buf = slash + 1;                        \
        }                                                      \
                                                               \
                                                               \
        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));      \
        if (debug) {                                           \
            Rprintf("assigning: \"%s\"\n\n", buf);             \
        }                                                      \
    }                                                          \
                                                               \
                                                               \
    if (debug) {                                               \
        Rprintf("\n");                                         \
    }                                                          \
                                                               \
                                                               \
    UNPROTECT(nprotect);                                       \
    return value;                                              \
}


#define dirname2(windows)                                      \
{                                                              \
    if (debug) {                                               \
        Rprintf((windows) ? "in do_windowsdirname2\n\n" : "in do_unixdirname2\n\n");\
    }                                                          \
                                                               \
                                                               \
    int nprotect = 0;                                          \
                                                               \
                                                               \
    SEXP path;                                                 \
    int times;                                                 \
                                                               \
                                                               \
    /* it's not documented in the R function dirname2() or in man/dirname2.Rd */\
    /* but dirname2() actually accepts 1 or 2 arguments                       */\
    /*                                                                        */\
    /* the first argument is always the 'path' argument                       */\
    /* the second argument (optional) is the number of additional times to    */\
    /* calculate dirname2(). for example:                                     */\
    /*                                                                        */\
    /* .External2(C_dirname2, path, 2)                                        */\
    /*                                                                        */\
    /* will calculate the dirname() once, then calculate it 2 more times      */\
    /* afterward                                                              */\
    /*                                                                        */\
    /* this saves miniscule time (because you don't have to setup a for loop  */\
    /* at the R level) but it was easy enough to do so why not?               */\
    int nargs = length(args) - 1;                              \
    if (nargs == 1) {                                          \
        path = CADR(args);                                     \
        if (TYPEOF(path) != STRSXP)                            \
            error("a character vector argument expected");     \
        times = 0;                                             \
    }                                                          \
    else if (nargs == 2) {                                     \
        path = CADR(args);                                     \
        if (TYPEOF(path) != STRSXP)                            \
            error("a character vector argument expected");     \
        times = asInteger(CADDR(args));                        \
        if (times == NA_INTEGER || times < 0)                  \
            errorcall(call, "invalid second argument, must be coercible to non-negative integer");\
    }                                                          \
    else errorcall(call, "%d arguments passed to .External(%s) which requires 1 or 2", nargs, (windows) ? "C_windowsdirname2" : "C_unixdirname2");\
                                                               \
                                                               \
    const char *ptr;                                           \
    char *buf, *last_char, *slash, *pathspec;                  \
    int n, i, j, nchar, drivewidth, skip;                      \
                                                               \
                                                               \
    SEXP value = PROTECT(allocVector(STRSXP, n = LENGTH(path))); nprotect++;\
    for (i = 0; i < n; i++) {                                  \
        if (debug) {                                           \
            Rprintf("string %d: ", i + 1);                     \
        }                                                      \
        if (STRING_ELT(path, i) == NA_STRING) {                \
            SET_STRING_ELT(value, i, NA_STRING);               \
            if (debug) {                                       \
                Rprintf("NA\n");                               \
                Rprintf("assigning: NA\n\n");                  \
            }                                                  \
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
            if (debug) {                                       \
                Rprintf("assigning: \"\"\n\n");                \
            }                                                  \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        drivewidth = (windows) ? get_drive_width(ptr, nchar) : get_drive_width_unix(ptr, nchar);\
        if (debug) {                                           \
            Rprintf("drivespec is %d bytes long\n", drivewidth);\
        }                                                      \
        if (drivewidth == nchar) {                             \
            SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));  \
            if (debug) {                                       \
                Rprintf("pathspec is 0 bytes long\n");         \
                Rprintf("assigning: \"%s\"\n\n", ptr);         \
            }                                                  \
            continue;                                          \
        }                                                      \
                                                               \
                                                               \
        char _buf[nchar + 1];  /* allocate a buffer to hold the dirname */\
        buf = _buf;                                            \
        strcpy(buf, ptr);                                      \
        if (debug) {                                           \
            Rprintf("made a buffer %d bytes long\n", nchar);   \
            Rprintf("copied to buffer: \"%s\"\n", ptr);        \
            if (nchar != strlen(ptr)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr));\
        }                                                      \
                                                               \
                                                               \
        pathspec = buf + drivewidth;  /* point to the start of the pathspec */\
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
        skip = 0;                                              \
        for (j = -1; j < times; j++) {                         \
                                                               \
                                                               \
            /* remove the trailing path separators                        */\
            /* we remove the trailing separators a little different than  */\
            /* do_basename2()                                             */\
            /* in this case, we only want to remove trailing slashes      */\
            /* only if there is a non-slash before those trailing slashes */\
            if (windows) {                                     \
                for (; last_char >= pathspec; last_char--) {   \
                    if (*last_char == '/' || *last_char == '\\') {}\
                    else {                                     \
                        *(last_char + 1) = '\0';               \
                        break;                                 \
                    }                                          \
                }                                              \
            }                                                  \
            else {                                             \
                for (; last_char >= pathspec; last_char--) {   \
                    if (*last_char == '/') {}                  \
                    else {                                     \
                        *(last_char + 1) = '\0';               \
                        break;                                 \
                    }                                          \
                }                                              \
            }                                                  \
                                                               \
                                                               \
            /* if the pathspec was comprised solely of path separators */\
            /* then the dirname is just the whole path                 */\
            if (last_char < pathspec) {                        \
                SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));\
                if (debug) {                                   \
                    Rprintf("pathspec is /\n");                \
                    Rprintf("assigning: \"%s\"\n\n", buf);     \
                }                                              \
                skip = 1;                                      \
                break;                                         \
            }                                                  \
            if (debug) {                                       \
                Rprintf("after removing trailing slashes: \"%s\"\n", pathspec);\
            }                                                  \
                                                               \
                                                               \
            /* find the last path separator */                 \
            if (windows) {                                     \
                      slash     = strrchr(pathspec, '/');      \
                char *backslash = strrchr(pathspec, '\\');     \
                                                               \
                                                               \
                if (slash) {  /* slash was found */            \
                    if (backslash) {  /* backslash was also found */\
                        if (slash < backslash)  /* slash found before backslash */\
                            last_char = backslash;             \
                        else last_char = slash;  /* backslash found before slash */\
                    }                                          \
                    else last_char = slash;  /* backslash was not found */\
                }                                              \
                else {  /* slash was not found */              \
                    if (backslash)  /* backslash was found */  \
                        last_char = backslash;                 \
                    else {                                     \
                                                               \
                                                               \
                        /* for a drive with a pathspec without a path separator */\
                        /* e.g. d:test                                          */\
                        if (drivewidth) {                      \
                            *pathspec = '\0';                  \
                            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));\
                            if (debug) {                       \
                                Rprintf("path is of the form d:file\n");\
                                Rprintf("assigning: \"%s\"\n\n", buf);\
                            }                                  \
                        }                                      \
                        else {                                 \
                            SET_STRING_ELT(value, i, mkChar("."));\
                            if (debug) {                       \
                                Rprintf("pathspec has no path separators\n");\
                                Rprintf("assigning: \".\"\n\n");\
                            }                                  \
                        }                                      \
                        skip = 1;                              \
                        break;                                 \
                    }                                          \
                }                                              \
            }                                                  \
            else {                                             \
                slash = strrchr(pathspec, '/');                \
                                                               \
                                                               \
                if (slash)  /* slash was found */              \
                    last_char = slash;                         \
                else {                                         \
                    SET_STRING_ELT(value, i, mkChar("."));     \
                    if (debug) {                               \
                        Rprintf("pathspec has no path separators\n");\
                        Rprintf("assigning: \".\"\n\n");       \
                    }                                          \
                    skip = 1;                                  \
                    break;                                     \
                }                                              \
            }                                                  \
                                                               \
                                                               \
            /* remove the basename */                          \
            *(last_char + 1) = '\0';                           \
            if (debug) {                                       \
                Rprintf("after removing basename        : \"%s\"\n", pathspec);\
            }                                                  \
                                                               \
                                                               \
            /* last_char should already point to the last character of buf */\
        }                                                      \
        if (skip) continue;                                    \
                                                               \
                                                               \
        /* remove the trailing path separators */              \
        if (windows) {                                         \
            for (; last_char >= pathspec; last_char--) {       \
               if (*last_char == '/' || *last_char == '\\') {} \
                else {                                         \
                    *(last_char + 1) = '\0';                   \
                    break;                                     \
                }                                              \
            }                                                  \
        }                                                      \
        else {                                                 \
            for (; last_char >= pathspec; last_char--) {       \
                if (*last_char == '/') {}                      \
                else {                                         \
                    *(last_char + 1) = '\0';                   \
                    break;                                     \
                }                                              \
            }                                                  \
        }                                                      \
        if (debug) {                                           \
            if (last_char >= pathspec) {                       \
                Rprintf("after removing trailing slashes: \"%s\"\n", pathspec);\
            }                                                  \
        }                                                      \
                                                               \
                                                               \
        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));      \
        if (debug) {                                           \
            Rprintf("assigning: \"%s\"\n\n", buf);             \
        }                                                      \
    }                                                          \
                                                               \
                                                               \
    if (debug) {                                               \
        Rprintf("\n");                                         \
    }                                                          \
                                                               \
                                                               \
    UNPROTECT(nprotect);                                       \
    return value;                                              \
}





SEXP do_windowsbasename2(SEXP call, SEXP op, SEXP args, SEXP rho) basename2(1)


SEXP do_unixbasename2(SEXP call, SEXP op, SEXP args, SEXP rho) basename2(0)


SEXP do_basename2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (debug) {
        Rprintf("in do_basename2\n\n");
    }
    int windows = asLogical(eval(install("os.windows"), rho));
    if (windows == NA_LOGICAL)
        error("invalid 'os.windows'; should never happen, please report!");
    if (windows)
        return do_windowsbasename2(call, op, args, rho);
    else return do_unixbasename2(call, op, args, rho);
}





SEXP do_windowsdirname2(SEXP call, SEXP op, SEXP args, SEXP rho) dirname2(1)


SEXP do_unixdirname2(SEXP call, SEXP op, SEXP args, SEXP rho) dirname2(0)


SEXP do_dirname2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (debug) {
        Rprintf("in do_dirname2\n\n");
    }
    int windows = asLogical(eval(install("os.windows"), rho));
    if (windows == NA_LOGICAL)
        error("invalid 'os.windows'; should never happen, please report!");
    if (windows)
        return do_windowsdirname2(call, op, args, rho);
    else return do_unixdirname2(call, op, args, rho);
}
