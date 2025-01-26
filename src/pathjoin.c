#include "thispathdefn.h"
#include <ctype.h>  /* includes toupper() */


#define debug 0


#define NEED_TRAILING_SLASH_MASK   (1<<1)
#define USE_MASK                   (1<<2)
#define NEED_TRAILING_SLASH(x)     ((x) & NEED_TRAILING_SLASH_MASK)
#define SET_NEED_TRAILING_SLASH(x) ((x) |= NEED_TRAILING_SLASH_MASK)
#define IS_USE(x)                  ((x) & USE_MASK)
#define SET_USE(x)                 ((x) |= USE_MASK)
typedef unsigned char info_t;





static R_INLINE
void windows_path_join(SEXP x, int x_length, int commonLength, SEXP value)
{
    int i, j;


        /*  largest index that contains a drivespec */
    int drive_indx,
        /*  largest index that contains an absolute pathspec */
        abs_path_indx,
        /* smallest index that contains a non-empty pathspec and is >= abs_path_indx */
        non_empty_path_spec_indx;


    /* width of the path and of the drivespec */
    int pwidth, drivewidth;


    const char *str;
    info_t *info;  /* which strings need a slash afterward and which are used */
    info = (info_t *) R_alloc(x_length, sizeof(info_t));


    int len, nchar;


    Rboolean is_latin1locale = my_latin1locale,
             is_utf8locale   = my_utf8locale  ;


    Rboolean allKnown, anyKnown, use_UTF8, use_Bytes;


    SEXP cs;


    for (j = 0; j < commonLength; j++) {


        drive_indx = -1;
        abs_path_indx = -1;
        non_empty_path_spec_indx = -1;


        drivewidth = 0;


        memset(info, 0, x_length * sizeof(info_t));


        anyKnown = FALSE; allKnown = TRUE; use_UTF8 = FALSE; use_Bytes = FALSE;


        /* find the desired encoding
         * and the required indexes
         *
         * we're going to work backwards here
         */
        for (i = x_length - 1; i >= 0; i--) {
            len = LENGTH(VECTOR_ELT(x, i));
            cs = STRING_ELT(VECTOR_ELT(x, i), j % len);
            str = R_CHAR(cs);
            nchar = (int) strlen(str);
            if (!nchar)
                continue;


            /* look for a drivespec */
            drivewidth = drive_width_windows(str);


            if (abs_path_indx == -1 || drivewidth) {
                /* mark this string as used
                 * and make a note of its encoding
                 */
                SET_USE(info[i]);
                if (!use_Bytes) {
                    if (IS_BYTES(cs)) { use_Bytes = TRUE; use_UTF8 = FALSE; }
                    else if (!use_UTF8) {
                        if (IS_UTF8(cs)) use_UTF8 = TRUE;
                    }
                }
            }


            /* if we have not already found an absolute pathspec,
             * then look for one
             */
            if (abs_path_indx == -1) {


                /* non-empty pathspec */
                if (drivewidth < nchar) {


                    /* if the start of the pathspec is slash or backslash
                     * then record this index as the last absolute path
                     */
                    if (str[drivewidth] == '/' || str[drivewidth] == '\\')
                        abs_path_indx = i;


                    /* record this index as the first non-empty pathspec */
                    non_empty_path_spec_indx = i;
                }
            }


            /* non-empty drivespec
             * record this index as the last index containing a drive
             */
            if (drivewidth) {
                drive_indx = i;
                break;
            }
        }


        /* if we have not found an absolute path
         * and our drivespec is a drive
         *
         * then we will want to go back slightly further
         * in case there is more to add to the path
         *
         * this is because drives are not always absolute
         *
         * for example:
         *
         * path.join("D:p1", "p2", "d:p3", "p4", "p5")
         *
         * all of these are perfectly legitimate relative paths
         *
         * but combining them into "d:p3/p4/p5" would be incorrect
         * since we should also be adding the portions before
         * (since "d:" is not an absolute location and its pathspec is not
         * absolute either)
         *
         * we expect the results "d:p1/p2/p3/p4/p5"
         */
        if (abs_path_indx == -1 && drivewidth == 2) {


            /* use a bunch of 'maybe' variables
             * these may be used, depending on how the drives look
             */


            int maybe_abs_path_indx = abs_path_indx,
                maybe_non_empty_path_spec_indx = non_empty_path_spec_indx;


            int maybe_drivewidth = 0;
            const char *maybe_str;
            int maybe_len, maybe_nchar;


            const char drive_letter = toupper(str[0]);


            Rboolean maybe_use_UTF8 = use_UTF8,
                     maybe_use_Bytes = use_Bytes;


            SEXP maybe_cs;


            for (i--; i >= 0; i--) {
                maybe_len = LENGTH(VECTOR_ELT(x, i));
                maybe_cs = STRING_ELT(VECTOR_ELT(x, i), j % maybe_len);
                maybe_str = R_CHAR(maybe_cs);
                maybe_nchar = (int) strlen(maybe_str);
                if (!maybe_nchar)
                    continue;


                /* look for a drivespec */
                maybe_drivewidth = drive_width_windows(maybe_str);


                if (maybe_drivewidth) {
                    /* if the string has a drivespec, ensure it matches the
                     * previous drivespec, break otherwise
                     */
                    if (maybe_drivewidth != 2 ||
                        toupper(maybe_str[0]) != drive_letter)
                    {
                        break;
                    }
                }


                /* if we have not already found an absolute pathspec,
                 * then look for one
                 */
                if (maybe_abs_path_indx == -1) {


                    /* non-empty pathspec */
                    if (maybe_drivewidth < maybe_nchar) {


                        /* mark this string as used
                         * and make a note of its encoding
                         */
                        SET_USE(info[i]);
                        if (!maybe_use_Bytes) {
                            if (IS_BYTES(maybe_cs)) { maybe_use_Bytes = TRUE; maybe_use_UTF8 = FALSE; }
                            else if (!maybe_use_UTF8) {
                                if (IS_UTF8(maybe_cs)) maybe_use_UTF8 = TRUE;
                            }
                        }


                        /* if the start of the pathspec is slash or backslash
                         * then record this index as the last absolute path
                         */
                        if (maybe_str[maybe_drivewidth] == '/' ||
                            maybe_str[maybe_drivewidth] == '\\')
                        {
                            maybe_abs_path_indx = i;
                        }


                        /* record this index as the first non-empty pathspec */
                        maybe_non_empty_path_spec_indx = i;
                    }
                }


                /* non-empty drivespec that matches our previous drivespec
                 *
                 * copy the 'maybe' variables back into the original variables
                 *
                 * if we have not found an absolute path, continue looking
                 */
                if (maybe_drivewidth) {
                    non_empty_path_spec_indx = maybe_non_empty_path_spec_indx;


                    use_UTF8 = maybe_use_UTF8; use_Bytes = maybe_use_Bytes;


                    /* if we found an absolute path, then we are done */
                    if (maybe_abs_path_indx != -1) {
                        abs_path_indx = maybe_abs_path_indx;
                        break;
                    }
                }
            }
        }


        /* find the minimum of two indexes, ignoring -1 */
#define MIN_INDX(x, y)                                         \
        (((x) != -1) ? (((y) != -1) ? (((x) < (y)) ? (x) : (y)) : (x)) : (y))


        int path_min = MIN_INDX(abs_path_indx, non_empty_path_spec_indx);
        int i_min = MIN_INDX(path_min, drive_indx);


        /* if all the paths are empty */
        if (i_min == -1)
            continue;


        pwidth = 0;


        /* now that we have the desired encoding and the required indexes,
         * find the required width
         */
        for (i = x_length - 1; i >= i_min; i--) {


            /* if the string is not used, then no need to translate */
            if (!IS_USE(info[i]))
                continue;


            len = LENGTH(VECTOR_ELT(x, i));
            cs = STRING_ELT(VECTOR_ELT(x, i), j % len);
            if (use_Bytes)
                str = R_CHAR(cs);
            else if (use_UTF8)
                str = Rf_translateCharUTF8(cs);
            else
                str = Rf_translateChar(cs);
            nchar = (int) strlen(str);
            if (!nchar)
                continue;


            /* look for a drivespec */
            drivewidth = drive_width_windows(str);


            /* not already found an absolute pathspec */
            if (i >= abs_path_indx) {


                /* non-empty pathspec */
                if (drivewidth < nchar) {


                    /* if there are pathspec characters already in the buffer
                     * and this pathspec does not end with slash nor backslash
                     * then we will need to add one manually
                     *
                     * record this index as needing a trailing slash
                     */
                    if (pwidth > ((i >= drive_indx) ? 0 : 2) &&
                        str[nchar - 1] != '/' &&
                        str[nchar - 1] != '\\')
                    {
                        SET_NEED_TRAILING_SLASH(info[i]);
                        pwidth++;
                    }


                    /* add the number of chars in the pathspec
                     * to the width of the output string
                     */
                    pwidth += nchar - drivewidth;
                }


                /* empty pathspec, non-empty drivespec,
                 * if there are no characters in the buffer, do not add a trailing slash
                 * if this is a drive, do not add a trailing slash
                 * otherwise, add a trailing slash
                 */
                else if (pwidth && drivewidth != 2) {
                    SET_NEED_TRAILING_SLASH(info[i]);
                    pwidth++;
                }
            }


            /* non-empty drivespec
             * add the number of chars in the drive to the buffer
             */
            if (i == drive_indx) {
                pwidth += drivewidth;
            }
        }


        /* if all the strings were empty, return an empty string */
        if (pwidth <= 0) {
            // SET_STRING_ELT(value, j, R_BlankString);
            continue;
        }


#if debug
        Rprintf("pwidth = %d\n", pwidth);
        int check_width = 0;
        Rprintf("drive_indx               = %d\n", drive_indx);
        Rprintf("abs_path_indx            = %d\n", abs_path_indx);
        Rprintf("non_empty_path_spec_indx = %d\n", non_empty_path_spec_indx);
#endif


        char _buf[pwidth + 1];


        /* points to the current location where we will be pasting characters */
        char *buf = _buf;


        /* points to the start of the string */
        const char *cbuf = buf;


        int start_from_here = path_min;


        /* if we found a drive */
        if (drive_indx != -1) {


            i = drive_indx;
            len = LENGTH(VECTOR_ELT(x, i));
            cs = STRING_ELT(VECTOR_ELT(x, i), j % len);
            if (use_Bytes)
                str = R_CHAR(cs);
            else if (use_UTF8)
                str = Rf_translateCharUTF8(cs);
            else {
                str = Rf_translateChar(cs);
                allKnown = allKnown && (IS_ASCII(cs) || ENC_KNOWN(cs));
                anyKnown = anyKnown || ENC_KNOWN(cs);
            }
            nchar = (int) strlen(str);
            if (!nchar);


            /* if drive is at the same index as the start pos
             * copy to the buffer without the need for splitting */
            else if (start_from_here == i) {
                strcpy(buf, str);
                buf += nchar;
#if debug
                Rprintf("copied to buffer: %s", str); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", nchar);
                check_width += (int) strlen(str);
#endif
                if (NEED_TRAILING_SLASH(info[i])) {
                    strcpy(buf, "/");
                    buf++;
#if debug
                    Rprintf("copied to buffer: %s", "/"); Rprintf("\n");
                    Rprintf("shifted buffer by %d bytes\n", (int) strlen("/"));
                    check_width += (int) strlen("/");
#endif
                }
                start_from_here++;
            }


            /* drive is at some index, start pos is at some other index,
             * so copy only the drive
             */
            else {


                /* look for a drivespec */
                drivewidth = drive_width_windows(str);


                strncpy(buf, str, drivewidth);
                buf += drivewidth;
                *buf = '\0';  // copy a nul character
#if debug
                Rprintf("copied first %d bytes to buffer: %s", drivewidth, str); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", drivewidth);
                check_width += drivewidth;
#endif


                if (start_from_here > i &&
                    NEED_TRAILING_SLASH(info[i]))
                {
                    strcpy(buf, "/");
                    buf++;
#if debug
                    Rprintf("copied to buffer: %s", "/"); Rprintf("\n");
                    Rprintf("shifted buffer by %d bytes\n", (int) strlen("/"));
                    check_width += (int) strlen("/");
#endif
                }
            }
        }


        if (start_from_here != -1) {


            /* start immediately AFTER the absolute path */
            for (i = start_from_here; i < x_length; i++) {


                if (!IS_USE(info[i]))
                    continue;


                len = LENGTH(VECTOR_ELT(x, i));
                cs = STRING_ELT(VECTOR_ELT(x, i), j % len);
                if (use_Bytes)
                    str = R_CHAR(cs);
                else if (use_UTF8)
                    str = Rf_translateCharUTF8(cs);
                else {
                    str = Rf_translateChar(cs);
                    allKnown = allKnown && (IS_ASCII(cs) || ENC_KNOWN(cs));
                    anyKnown = anyKnown || ENC_KNOWN(cs);
                }
                nchar = (int) strlen(str);


                /* an empty string does not need to be copied to the buffer
                 * and will never need a trailing backslash
                 */
                if (!nchar)
                    continue;


                /* if we are before or at the drive indx,
                 * be sure to chop off the drive before pasting
                 */
                if (i <= drive_indx) {
                    drivewidth = drive_width_windows(str);
                    str += drivewidth;
                    nchar -= drivewidth;


                    if (!nchar)
                        continue;
                }


                strcpy(buf, str);
                buf += nchar;
#if debug
                Rprintf("copied to buffer: %s", str); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", nchar);
                check_width += (int) strlen(str);
#endif
                if (NEED_TRAILING_SLASH(info[i])) {
                    strcpy(buf, "/");
                    buf++;
#if debug
                    Rprintf("copied to buffer: %s", "/"); Rprintf("\n");
                    Rprintf("shifted buffer by %d bytes\n", (int) strlen("/"));
                    check_width += (int) strlen("/");
#endif
                }
            }
        }


#if debug
        if (check_width != pwidth) Rf_error("allocated string of %d bytes, allocated %d bytes instead\n", pwidth, check_width);
        if ((int) strlen(cbuf) != pwidth) Rf_error("allocated string of %d bytes, got a string of %d bytes instead\n", pwidth, (int) strlen(cbuf));
        Rprintf("\n");
#endif


        cetype_t enc = CE_NATIVE;
        if (use_Bytes) enc = CE_BYTES;
        else if (use_UTF8) enc = CE_UTF8;
        else if (anyKnown && allKnown) {
            if (is_latin1locale) enc = CE_LATIN1;
            if (is_utf8locale) enc = CE_UTF8;
        }


        /* use cbuf because we want the whole string */
        SET_STRING_ELT(value, j, Rf_mkCharCE(cbuf, enc));
    }
}


static R_INLINE
void unix_path_join(SEXP x, int x_length, int commonLength, SEXP value)
{
    int i, j;


    /* width of the path */
    int pwidth;


    const char *str;
    info_t *info;
    info = (info_t *) R_alloc(x_length, sizeof(info_t));


    int len, nchar;


    Rboolean is_latin1locale = my_latin1locale,
             is_utf8locale   = my_utf8locale  ;


    Rboolean allKnown, anyKnown, use_UTF8, use_Bytes;


    SEXP cs;


    for (j = 0; j < commonLength; j++) {


        memset(info, 0, x_length * sizeof(info_t));


        anyKnown = FALSE; allKnown = TRUE; use_UTF8 = FALSE; use_Bytes = FALSE;


        /* find the desired encoding
         * and the index of the last absolute path
         *
         * we're going to work backwards here
         */
        for (i = x_length - 1; i >= 0; i--) {
            len = LENGTH(VECTOR_ELT(x, i));
            cs = STRING_ELT(VECTOR_ELT(x, i), j % len);
            if (!use_Bytes) {
                if (IS_BYTES(cs)) { use_Bytes = TRUE; use_UTF8 = FALSE; }
                else if (!use_UTF8) {
                    if (IS_UTF8(cs)) use_UTF8 = TRUE;
                }
            }


            if (i == 0) break;


            /* test for absolute paths:
             * if it starts with /
             * if it is equal to ~
             * if it starts with ~/
             */
            str = R_CHAR(cs);
            if (*str == '/') break;  /* path starts with / */
            if (*str == '~') {
                if (*(str + 1) == '\0') break;  /* path equals ~       */
                if (*(str + 1) == '/' ) break;  /* path starts with ~/ */
            }
        }


        int i_min = (i > 0) ? i : 0;


        pwidth = 0;


        /* now that we have the desired encoding and the starting index,
         * find the required width
         */
        for (i = x_length - 1; i >= i_min; i--) {
            len = LENGTH(VECTOR_ELT(x, i));
            cs = STRING_ELT(VECTOR_ELT(x, i), j % len);
            if (use_Bytes)
                str = R_CHAR(cs);
            else if (use_UTF8)
                str = Rf_translateCharUTF8(cs);
            else
                str = Rf_translateChar(cs);
            nchar = (int) strlen(str);
            if (!nchar)
                continue;


            /* if there are characters already in the buffer
             * and this path does not end with /
             * then we will need to add one manually,
             * record this index as needing a trailing slash
             */
            if (pwidth && str[nchar - 1] != '/') {
                SET_NEED_TRAILING_SLASH(info[i]);
                pwidth++;
            }


            /* add this string to the width of the buffer */
            pwidth += nchar;
        }


        /* if all the strings were empty, return an empty string */
        if (pwidth <= 0) {
            // SET_STRING_ELT(value, j, R_BlankString);
            continue;
        }


#if debug
        Rprintf("pwidth = %d\n", pwidth);
        int check_width = 0;
#endif


        char _buf[pwidth + 1];


        /* points to the current location where we will be pasting characters */
        char *buf = _buf;


        /* points to the start of the string */
        const char *cbuf = buf;


        /* take where we exited and start from there */
        for (i = i_min; i < x_length; i++) {
            len = LENGTH(VECTOR_ELT(x, i));
            cs = STRING_ELT(VECTOR_ELT(x, i), j % len);
            if (use_Bytes)
                str = R_CHAR(cs);
            else if (use_UTF8)
                str = Rf_translateCharUTF8(cs);
            else {
                str = Rf_translateChar(cs);
                allKnown = allKnown && (IS_ASCII(cs) || ENC_KNOWN(cs));
                anyKnown = anyKnown || ENC_KNOWN(cs);
            }
            nchar = (int) strlen(str);


            /* an empty string does not need to be copied to the buffer
             * and will never need a trailing slash
             */
            if (!nchar)
                continue;


            strcpy(buf, str);
            buf += nchar;
#if debug
                Rprintf("copied to buffer: %s", str); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", nchar);
                check_width += (int) strlen(str);
#endif
            if (NEED_TRAILING_SLASH(info[i])) {
                strcpy(buf, "/");
                buf++;
#if debug
                    Rprintf("copied to buffer: %s", "/"); Rprintf("\n");
                    Rprintf("shifted buffer by %d bytes\n", (int) strlen("/"));
                    check_width += (int) strlen("/");
#endif
            }
        }


#if debug
        if (check_width != pwidth) Rf_error("allocated string of %d bytes, allocated %d bytes instead\n", pwidth, check_width);
        if ((int) strlen(cbuf) != pwidth) Rf_error("allocated string of %d bytes, got a string of %d bytes instead\n", pwidth, (int) strlen(cbuf));
        Rprintf("\n");
#endif


        cetype_t enc = CE_NATIVE;
        if (use_Bytes) enc = CE_BYTES;
        else if (use_UTF8) enc = CE_UTF8;
        else if (anyKnown && allKnown) {
            if (is_latin1locale) enc = CE_LATIN1;
            if (is_utf8locale) enc = CE_UTF8;
        }


        /* use cbuf because we want the whole string */
        SET_STRING_ELT(value, j, Rf_mkCharCE(cbuf, enc));
    }
}


static R_INLINE
SEXP path_join(SEXP call, int windows, const char *name, SEXP args, SEXP rho)
{
    int nprotect = 0;


    /* we don't pass the ... list directly because we want to
       avoid an accidental argument name match to PACKAGE
     */
    SEXP dots = Rf_findVarInFrame(rho, R_DotsSymbol);
    Rf_protect(dots); nprotect++;
    if (dots == R_UnboundValue)
        Rf_error(_("object '%s' not found"), "...");


    int dots_length = ((TYPEOF(dots) == DOTSXP) ? Rf_length(dots) : 0);


    if (dots_length == 0) {
        Rf_unprotect(nprotect);
        return Rf_allocVector(STRSXP, 0);
    }


    SEXP x = Rf_allocVector(VECSXP, dots_length);
    Rf_protect(x); nprotect++;
    int x_length = dots_length;
    int i;
    SEXP d, xi;


    /* the common length of a set of arguments is 0 if one has a length of 0 */
    /* or the maximal length when all are non-zero                           */
    int commonLength = 1;
    /* slightly slower
    for (i = 0, d = dots; d != R_NilValue; i++, d = CDR(d)) { */
    for (i = 0, d = dots; i < x_length; i++, d = CDR(d)) {


        /* evaluate each argument of 'dots' */
        xi = CAR(d);
        if (xi == R_MissingArg)
            // Rf_errorcall(call, _("argument is missing, with no default"));
            MissingArgError_c("", call, rho, "evalError");
        xi = Rf_eval(xi, rho);
        if (commonLength) {


            /* add the evaluated object to 'x' */
            SET_VECTOR_ELT(x, i, xi);


            /* coerce the object to string if needed */
            if (!Rf_isString(xi)) {
                if (OBJECT(xi)) {
                    /* as.character(quote(xi)) */
                    SEXP expr;
                    PROTECT_INDEX indx;
                    R_ProtectWithIndex(expr = Rf_cons(xi, R_NilValue), &indx);
                    if (needQuote(xi)) {
                        R_Reprotect(expr = Rf_lcons(getFromBase(R_QuoteSymbol), expr), indx);
                        R_Reprotect(expr = Rf_cons(expr, R_NilValue), indx);
                    }
                    R_Reprotect(expr = Rf_lcons(getFromBase(R_AsCharacterSymbol), expr), indx);
                    SET_VECTOR_ELT(x, i, Rf_eval(expr, rho));
                    Rf_unprotect(1);
                }
                else if (Rf_isSymbol(xi))
                    SET_VECTOR_ELT(x, i, Rf_ScalarString(PRINTNAME(xi)));
                else SET_VECTOR_ELT(x, i, Rf_coerceVector(xi, STRSXP));

                if (!Rf_isString(VECTOR_ELT(x, i)))
                    Rf_errorcall(call, "non-string argument to '%s'", name);
            }


            /* compute the length and possibly update 'commonLength'       */
            /* if the commonLength is 0, we don't need to do any more      */
            /* calculations, we know the return value is character(0), BUT */
            /* we still want to evaluate all objects in 'dots', would be a */
            /* little strange to not do that                               */
            if (commonLength) {
                int len = LENGTH(VECTOR_ELT(x, i));
                if (len == 0 || len > commonLength)
                    commonLength = len;
            }
        }
    }


    if (commonLength == 0) {
        Rf_unprotect(nprotect);
        return Rf_allocVector(STRSXP, 0);
    }


    SEXP value = Rf_allocVector(STRSXP, commonLength);
    Rf_protect(value); nprotect++;


    if (windows) {
        windows_path_join(x, x_length, commonLength, value);
    } else {
        unix_path_join(x, x_length, commonLength, value);
    }


    Rf_unprotect(nprotect);
    return value;
}


SEXP do_windows_path_join do_formals
{
    do_start_no_op("windows_path_join", 0);
    return path_join(call, TRUE, ".windows_path_join", args, rho);
}


SEXP do_unix_path_join do_formals
{
    do_start_no_op("unix_path_join", 0);
    return path_join(call, FALSE, ".unix_path_join", args, rho);
}


SEXP do_path_join do_formals
{
    do_start_no_op("path_join", 0);
#if defined(_WIN32)
    return path_join(call, TRUE, "path.join", args, rho);
#else
    return path_join(call, FALSE, "path.join", args, rho);
#endif
}
