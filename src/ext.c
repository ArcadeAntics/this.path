#include "thispathdefn.h"





typedef enum {EXTOP_REMOVEEXT = 1,
              EXTOP_EXT          ,
              EXTOP_SPLITEXT     ,
              EXTOP_EXTGETS      } EXTOP;


static R_INLINE
SEXP ext(SEXP call, EXTOP op, int windows, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP path = CAR(args); args = CDR(args);
    if (TYPEOF(path) != STRSXP)
        Rf_error(_("a character vector argument expected"));


    Rboolean compression = Rf_asLogical(CAR(args)); args = CDR(args);
    if (compression == NA_LOGICAL)
        Rf_error(_("invalid '%s' value"), "compression");


    SEXP newext = NULL;
    int length_newext = -1;
    if (op == EXTOP_EXTGETS) {
        if (!Rf_isString(CAR(args))) {
            if (OBJECT(CAR(args))) {
                /* as.character(quote(CAR(args))) */
                SEXP expr;
                PROTECT_INDEX indx;
                R_ProtectWithIndex(expr = Rf_cons(CAR(args), R_NilValue), &indx);
                if (needQuote(CAR(args))) {
                    R_Reprotect(expr = Rf_lcons(getFromBase(R_QuoteSymbol), expr), indx);
                    R_Reprotect(expr = Rf_cons(expr, R_NilValue), indx);
                }
                R_Reprotect(expr = Rf_lcons(getFromBase(R_AsCharacterSymbol), expr), indx);
                SETCAR(args, Rf_eval(expr, rho));
                Rf_unprotect(1);
            }
            else if (Rf_isSymbol(CAR(args)))
                SETCAR(args, Rf_ScalarString(PRINTNAME(CAR(args))));
            else SETCAR(args, Rf_coerceVector(CAR(args), STRSXP));
            if (!Rf_isString(CAR(args)))
                Rf_errorcall(call, _("non-string argument to '%s'"), ".C_ext<-");
        }


        length_newext = Rf_length(CAR(args));
        if (!length_newext) {
            SETCAR(args, R_BlankScalarString);
            length_newext = 1;
        }
        newext = CAR(args);
    }


    const char *ptr;
    char *buf, *pathspec, *last_char, *slash,
         *basename, *dot, *compression_dot, *tmp;
    int n, i, nchar, drivewidth, nchar_basename, found_non_dot;


    SEXP value;
    n = LENGTH(path);
    switch (op) {
    case EXTOP_SPLITEXT:
        value = Rf_allocMatrix(STRSXP, 2, n);
        Rf_protect(value); nprotect++;
        SEXP dimnames = Rf_allocVector(VECSXP, 2);
        Rf_setAttrib(value, R_DimNamesSymbol, dimnames);
        SEXP rownames = Rf_allocVector(STRSXP, 2);
        SET_VECTOR_ELT(dimnames, 0, rownames);
        SET_STRING_ELT(rownames, 0, Rf_mkChar("root"));
        SET_STRING_ELT(rownames, 1, Rf_mkChar("ext"));
        break;
    case EXTOP_EXTGETS:
        value = R_shallow_duplicate_attr(path);
        Rf_protect(value); nprotect++;
        break;
    default:
        value = Rf_allocVector(STRSXP, n);
        Rf_protect(value); nprotect++;
    }


    char _buf_ext[PATH_MAX + 1];


    for (i = 0; i < n; i++) {
        if (STRING_ELT(path, i) == NA_STRING) {
            switch (op) {
            case EXTOP_REMOVEEXT:
            case EXTOP_EXT:
                SET_STRING_ELT(value, i, NA_STRING);
                break;
            case EXTOP_SPLITEXT:
                SET_STRING_ELT(value, 2 * i    , NA_STRING);
                SET_STRING_ELT(value, 2 * i + 1, NA_STRING);
                break;
            case EXTOP_EXTGETS:
                /* already is NA
                SET_STRING_ELT(value, i, NA_STRING); */
                break;
            }
            continue;
        }


        if (op == EXTOP_EXTGETS) {
            if (STRING_ELT(newext, i % length_newext) == NA_STRING) {
                SET_STRING_ELT(value, i, NA_STRING);
                continue;
            }
        }


        ptr = R_ExpandFileName(Rf_translateCharUTF8(STRING_ELT(path, i)));
        nchar = strlen(ptr);
        if (nchar == 0) {
            continue;
        }


        drivewidth = _drive_width(windows, ptr, nchar);
        if (nchar == drivewidth) {  /* pathspec is 0 bytes long */
            switch (op) {
            case EXTOP_REMOVEEXT:
                SET_STRING_ELT(value, i, Rf_mkCharCE(ptr, CE_UTF8));
                break;
            case EXTOP_EXT:
                // SET_STRING_ELT(value, i, R_BlankString);
                break;
            case EXTOP_SPLITEXT:
                SET_STRING_ELT(value, 2 * i, Rf_mkCharCE(ptr, CE_UTF8));
                // SET_STRING_ELT(value, 2 * i + 1, R_BlankString);
                break;
            case EXTOP_EXTGETS:
                SET_STRING_ELT(value, i, Rf_mkCharCE(ptr, CE_UTF8));
                break;
            }
            continue;
        }


        const char *ptr_ext;
        const char *cext = NULL;
        if (op == EXTOP_EXTGETS) {
            if (i < length_newext) {
                ptr_ext = Rf_translateCharUTF8(STRING_ELT(newext, i));
                if (windows) {
                    if (!strlen(ptr_ext))
                        cext = ptr_ext;
                    else {
                        const char *p = ptr_ext;
                        int add_dot = 1;
                        if (*p == '.') {
                            add_dot = 0;
                            p++;
                            if (!strlen(p)) Rf_error("extension \".\" is invalid");
                            if (*p == '.')  Rf_error("extension starting with \"..\" is invalid");
                        }
                        for (; *p; p++) {
                            if (*p == '/' || *p == '\\') Rf_error("extension containing / is invalid");
                            if (*p == '.') {
                                if (strlen(p) == 3 &&
                                    *(p + 1) == 'g' &&
                                    *(p + 2) == 'z')
                                {
                                    break;
                                }
                                if (strlen(p) == 4 &&
                                    *(p + 1) == 'b' &&
                                    *(p + 2) == 'z' &&
                                    *(p + 3) == '2')
                                {
                                    break;
                                }
                                if (strlen(p) == 3 &&
                                    *(p + 1) == 'x' &&
                                    *(p + 2) == 'z')
                                {
                                    break;
                                }
                                Rf_error("extension containing \".\" but is not a compression extension");
                            }
                        }
                        if (add_dot) {
                            if (strlen(ptr_ext) >= PATH_MAX)
                                Rf_error("file extension is too long");
                            buf = _buf_ext;
                            cext = _buf_ext;
                            *buf = '.';
                            strcpy(buf + 1, ptr_ext);
                        }
                        else cext = ptr_ext;
                    }
                } else {
                    if (!strlen(ptr_ext))
                        cext = ptr_ext;
                    else {
                        const char *p = ptr_ext;
                        int add_dot = 1;
                        if (*p == '.') {
                            add_dot = 0;
                            p++;
                            if (!strlen(p)) Rf_error("extension \".\" is invalid");
                            if (*p == '.')  Rf_error("extension starting with \"..\" is invalid");
                        }
                        for (; *p; p++) {
                            if (*p == '/') Rf_error("extension containing / is invalid");
                            if (*p == '.') {
                                if (strlen(p) == 3 &&
                                    *(p + 1) == 'g' &&
                                    *(p + 2) == 'z')
                                {
                                    break;
                                }
                                if (strlen(p) == 4 &&
                                    *(p + 1) == 'b' &&
                                    *(p + 2) == 'z' &&
                                    *(p + 3) == '2')
                                {
                                    break;
                                }
                                if (strlen(p) == 3 &&
                                    *(p + 1) == 'x' &&
                                    *(p + 2) == 'z')
                                {
                                    break;
                                }
                                Rf_error("extension containing \".\" but is not a compression extension");
                            }
                        }
                        if (add_dot) {
                            if (strlen(ptr_ext) >= PATH_MAX)
                                Rf_error("file extension is too long");
                            buf = _buf_ext;
                            cext = _buf_ext;
                            *buf = '.';
                            strcpy(buf + 1, ptr_ext);
                        }
                        else cext = ptr_ext;
                    }
                }
            } else {
                ptr_ext = Rf_translateCharUTF8(STRING_ELT(newext, i % length_newext));
                if (!strlen(ptr_ext) || *ptr_ext == '.')
                    cext = ptr_ext;
                else {
                    char _buf[strlen(ptr_ext) + 2];
                    buf = _buf;
                    cext = _buf;
                    *buf = '.';
                    strcpy(buf + 1, ptr_ext);
                }
            }
        }


        char _buf[(op != EXTOP_EXTGETS) ? (nchar + 1) : (nchar + strlen(cext) + 1)];
        buf = _buf;
        strcpy(buf, ptr);
        pathspec = buf + drivewidth;


        /* point to the last character of buf */
        last_char = buf + (nchar - 1);


        /* remove trailing path separators */
        if (windows) {
            while (last_char >= pathspec && (*last_char == '/' || *last_char == '\\')) {
                *(last_char--) = '\0';
            }
        } else {
            while (last_char >= pathspec && *last_char == '/') {
                *(last_char--) = '\0';
            }
        }


        /* if the pathspec was solely comprised of path separators
         * then the basename is non-existent, return empty string
         */
        if (last_char < pathspec) {
            switch (op) {
            case EXTOP_REMOVEEXT:
                SET_STRING_ELT(value, i, Rf_mkCharCE(ptr, CE_UTF8));
                break;
            case EXTOP_EXT:
                // SET_STRING_ELT(value, i, R_BlankString);
                break;
            case EXTOP_SPLITEXT:
                SET_STRING_ELT(value, 2 * i, Rf_mkCharCE(ptr, CE_UTF8));
                // SET_STRING_ELT(value, 2 * i + 1, R_BlankString);
                break;
            case EXTOP_EXTGETS:
                SET_STRING_ELT(value, i, Rf_mkCharCE(ptr, CE_UTF8));
                break;
            }
            continue;
        }


        /* get the basename of 'pathspec' */
        if (windows) {
            /* find the last slash/backslash */
                  slash     = strrchr(pathspec, '/');
            char *backslash = strrchr(pathspec, '\\');


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
        } else {
            slash = strrchr(pathspec, '/');   /* find the last slash */
            if (slash) basename = slash + 1;  /* slash was found */
            else       basename = pathspec;   /* slash was not found */
        }


        nchar_basename = strlen(basename);
        if (nchar_basename < 3) {
            switch (op) {
            case EXTOP_REMOVEEXT:
                SET_STRING_ELT(value, i, Rf_mkCharCE(buf, CE_UTF8));
                break;
            case EXTOP_EXT:
                // SET_STRING_ELT(value, i, R_BlankString);
                break;
            case EXTOP_SPLITEXT:
                SET_STRING_ELT(value, 2 * i, Rf_mkCharCE(buf, CE_UTF8));
                // SET_STRING_ELT(value, 2 * i + 1, R_BlankString);
                break;
            case EXTOP_EXTGETS:
                found_non_dot = 0;
                for (tmp = basename; *tmp; tmp++) {
                    if (*tmp != '.') {
                        found_non_dot = 1;
                        break;
                    }
                }
                if (found_non_dot) {
                    strcpy(last_char + 1, cext);
                }
                SET_STRING_ELT(value, i, Rf_mkCharCE(buf, CE_UTF8));
                break;
            }
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
                        switch (op) {
                        case EXTOP_EXT:
                            SET_STRING_ELT(value, i, Rf_mkCharCE(dot, CE_UTF8));
                            break;
                        case EXTOP_REMOVEEXT:
                            *dot = '\0';
                            SET_STRING_ELT(value, i, Rf_mkCharCE(buf, CE_UTF8));
                            break;
                        case EXTOP_SPLITEXT:
                            SET_STRING_ELT(value, 2 * i + 1, Rf_mkCharCE(dot, CE_UTF8));
                            *dot = '\0';
                            SET_STRING_ELT(value, 2 * i, Rf_mkCharCE(buf, CE_UTF8));
                            break;
                        case EXTOP_EXTGETS:
                            strcpy(dot, cext);
                            SET_STRING_ELT(value, i, Rf_mkCharCE(buf, CE_UTF8));
                            break;
                        }
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
            switch (op) {
            case EXTOP_REMOVEEXT:
                SET_STRING_ELT(value, i, Rf_mkCharCE(buf, CE_UTF8));
                break;
            case EXTOP_EXT:
                // SET_STRING_ELT(value, i, R_BlankString);
                break;
            case EXTOP_SPLITEXT:
                SET_STRING_ELT(value, 2 * i, Rf_mkCharCE(buf, CE_UTF8));
                // SET_STRING_ELT(value, 2 * i + 1, R_BlankString);
                break;
            case EXTOP_EXTGETS:
                found_non_dot = 0;
                for (tmp = basename; *tmp; tmp++) {
                    if (*tmp != '.') {
                        found_non_dot = 1;
                        break;
                    }
                }
                if (found_non_dot) {
                    strcpy(last_char + 1, cext);
                }
                SET_STRING_ELT(value, i, Rf_mkCharCE(buf, CE_UTF8));
                break;
            }
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
        if (!found_non_dot) {  /* basename contains only leading dots */
            switch (op) {
            case EXTOP_REMOVEEXT:
                SET_STRING_ELT(value, i, Rf_mkCharCE(buf, CE_UTF8));
                break;
            case EXTOP_EXT:
                // SET_STRING_ELT(value, i, R_BlankString);
                break;
            case EXTOP_SPLITEXT:
                SET_STRING_ELT(value, 2 * i, Rf_mkCharCE(buf, CE_UTF8));
                // SET_STRING_ELT(value, 2 * i + 1, R_BlankString);
                break;
            case EXTOP_EXTGETS:
                SET_STRING_ELT(value, i, Rf_mkCharCE(buf, CE_UTF8));
                break;
            }
            continue;
        }


        switch (op) {
        case EXTOP_EXT:
            SET_STRING_ELT(value, i, Rf_mkCharCE(dot, CE_UTF8));
            break;
        case EXTOP_REMOVEEXT:
            *dot = '\0';
            SET_STRING_ELT(value, i, Rf_mkCharCE(buf, CE_UTF8));
            break;
        case EXTOP_SPLITEXT:
            SET_STRING_ELT(value, 2 * i + 1, Rf_mkCharCE(dot, CE_UTF8));
            *dot = '\0';
            SET_STRING_ELT(value, 2 * i, Rf_mkCharCE(buf, CE_UTF8));
            break;
        case EXTOP_EXTGETS:
            strcpy(dot, cext);
            SET_STRING_ELT(value, i, Rf_mkCharCE(buf, CE_UTF8));
            break;
        }
    }


    Rf_unprotect(nprotect);
    return value;
}


SEXP do_windows_splitext do_formals
{
    do_start_no_op("windows_splitext", 2);
    return ext(call, EXTOP_SPLITEXT, TRUE, args, rho);
}


SEXP do_unix_splitext do_formals
{
    do_start_no_op("unix_splitext", 2);
    return ext(call, EXTOP_SPLITEXT, FALSE, args, rho);
}


SEXP do_splitext do_formals
{
    do_start_no_op("splitext", 2);
#if defined(_WIN32)
    return ext(call, EXTOP_SPLITEXT, TRUE, args, rho);
#else
    return ext(call, EXTOP_SPLITEXT, FALSE, args, rho);
#endif
}


SEXP do_windows_removeext do_formals
{
    do_start_no_op("windows_removeext", 2);
    return ext(call, EXTOP_REMOVEEXT, TRUE, args, rho);
}


SEXP do_unix_removeext do_formals
{
    do_start_no_op("unix_removeext", 2);
    return ext(call, EXTOP_REMOVEEXT, FALSE, args, rho);
}


SEXP do_removeext do_formals
{
    do_start_no_op("removeext", 2);
#if defined(_WIN32)
    return ext(call, EXTOP_REMOVEEXT, TRUE, args, rho);
#else
    return ext(call, EXTOP_REMOVEEXT, FALSE, args, rho);
#endif
}


SEXP do_windows_ext do_formals
{
    do_start_no_op("windows_ext", 2);
    return ext(call, EXTOP_EXT, TRUE, args, rho);
}


SEXP do_unix_ext do_formals
{
    do_start_no_op("unix_ext", 2);
    return ext(call, EXTOP_EXT, FALSE, args, rho);
}


SEXP do_ext do_formals
{
    do_start_no_op("ext", 2);
#if defined(_WIN32)
    return ext(call, EXTOP_EXT, TRUE, args, rho);
#else
    return ext(call, EXTOP_EXT, FALSE, args, rho);
#endif
}


SEXP do_windows_extgets do_formals
{
    do_start_no_op("windows_ext<-", 3);
    return ext(call, EXTOP_EXTGETS, TRUE, args, rho);
}


SEXP do_unix_extgets do_formals
{
    do_start_no_op("unix_ext<-", 3);
    return ext(call, EXTOP_EXTGETS, FALSE, args, rho);
}


SEXP do_extgets do_formals
{
    do_start_no_op("ext<-", 3);
#if defined(_WIN32)
    return ext(call, EXTOP_EXTGETS, TRUE, args, rho);
#else
    return ext(call, EXTOP_EXTGETS, FALSE, args, rho);
#endif
}
