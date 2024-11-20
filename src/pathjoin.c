#include "thispathdefn.h"
#include <ctype.h>  /* includes toupper() */


#define debug 0





static R_INLINE
void windows_path_join(SEXP x, int x_length, int commonLength, SEXP value)
{
    int i, j;


    int drive_indx,                /*  largest index of the string vector which contains a drivespec */
        abs_path_indx,             /*  largest index of the string vector which contains an absolute pathspec */
        non_empty_path_spec_indx;  /* smallest index of the string vector which contains a non-empty pathspec, but always >= abs_path_indx */


    /* width of the path and of the drivespec */
    int pwidth, drivewidth;


    const char *ptr;  /* points to the entire string from Rf_translateCharUTF8() */
    int *need_trailing_slash;  /* which strings need a slash afterward */
    need_trailing_slash = (int *) R_alloc(x_length, sizeof(int));


    int len, nchar;


    for (j = 0; j < commonLength; j++) {


        drive_indx = -1;
        abs_path_indx = -1;
        non_empty_path_spec_indx = -1;


        pwidth = 0;
        drivewidth = 0;


        ptr = "";  /* for -Wall */
        memset(need_trailing_slash, 0, x_length * sizeof(int));


        /* we're going to work backwards here
         * since we want the LAST absolute path
         */
        for (i = x_length - 1; i >= 0; i--) {
            len = LENGTH(VECTOR_ELT(x, i));
            ptr = Rf_translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % len));
            nchar = (int) strlen(ptr);
            if (!nchar)
                continue;


            /* look for a drivespec in ptr */
            drivewidth = drive_width_windows(ptr, nchar);


            /* if we have no already found an absolute pathspec,
             * then look for one
             */
            if (abs_path_indx == -1) {


                /* non-empty pathspec */
                if (drivewidth < nchar) {


                    /* if the start of the pathspec is slash or backslash
                     * then record this index as the last absolute path
                     */
                    if (ptr[drivewidth] == '/' || ptr[drivewidth] == '\\')
                        abs_path_indx = i;


                    /* record this index as the first non-empty pathspec */
                    non_empty_path_spec_indx = i;


                    /* if there are characters already in the buffer
                     * and this pathspec does not end with slash nor backslash
                     * then we will need to add one manually
                     *
                     * record this index as needing a trailing slash
                     */
                    if (pwidth &&
                        ptr[nchar - 1] != '/' &&
                        ptr[nchar - 1] != '\\')
                    {
                        need_trailing_slash[i] = 1;
                        pwidth++;
                    }


                    /* add the number of chars in the pathspec
                     * to the width of our output string
                     */
                    pwidth += nchar - drivewidth;
                }


                /* empty pathspec, non-empty drivespec,
                 * if there are no characters in the buffer, do not add a trailing slash
                 * if this is a drive, do not add a trailing slash
                 * otherwise, add a trailing slash
                 */
                else if (pwidth && drivewidth != 2) {
                    need_trailing_slash[i] = 1;
                    pwidth++;
                }
            }


            /* non-empty drivespec
             * record this index as the last index containing a drive
             * add the number of chars in the drive to the buffer
             * break immediately, will not be adding more to the path
             */
            if (drivewidth) {
                drive_indx = i;
                pwidth += drivewidth;
                break;
            }
        }


        /* if we have not found an absolute path
         * and our drivespec is a drive
         *
         * then we will want to go back slightly further
         * incase there's more to add to the path
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


            /* use a bunch of maybe variables
             * these may be used, depending on how the drives look
             */


            int maybe_abs_path_indx = -1,
                maybe_non_empty_path_spec_indx = -1;


            int maybe_pwidth = 0, maybe_drivewidth = 0;
            const char *maybe_ptr = "";
            int maybe_len, maybe_nchar;


            const char drive_letter = toupper(ptr[0]);


            for (i--; i >= 0; i--) {
                maybe_len = LENGTH(VECTOR_ELT(x, i));
                maybe_ptr = Rf_translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % maybe_len));
                maybe_nchar = (int) strlen(maybe_ptr);
                if (!maybe_nchar)
                    continue;


                /* look for a drivespec in ptr */
                maybe_drivewidth = drive_width_windows(maybe_ptr, maybe_nchar);


                if (maybe_drivewidth) {
                    if (maybe_drivewidth != 2 ||
                        toupper(maybe_ptr[0]) != drive_letter)
                    {
                        break;
                    }
                }


                /* if we have no already found an absolute pathspec,
                 * then look for one
                 */
                if (maybe_abs_path_indx == -1) {


                    /* a non-empty pathspec */
                    if (maybe_drivewidth < maybe_nchar) {


                        /* if the start of the pathspec is slash or backslash
                         * then record this index as the last absolute path
                         */
                        if (maybe_ptr[maybe_drivewidth] == '/' ||
                            maybe_ptr[maybe_drivewidth] == '\\')
                        {
                            maybe_abs_path_indx = i;
                        }


                        /* record this index as the first non-empty pathspec */
                        maybe_non_empty_path_spec_indx = i;


                        /* if there are characters already in the buffer (excluding the drive)
                         * or there are characters already in the maybe buffer
                         * and this pathspec does not end with slash nor back
                         * then we will need to add one manually
                         *
                         * record this index as needing a trailing slash
                         */
                        if ((pwidth > 2 || maybe_pwidth) &&
                            maybe_ptr[maybe_nchar - 1] != '/' &&
                            maybe_ptr[maybe_nchar - 1] != '\\')
                        {
                            need_trailing_slash[i] = 1;
                            maybe_pwidth++;
                        }


                        /* add the number of chars in the pathspec
                         * to the width of our output string
                         */
                        maybe_pwidth += maybe_nchar - maybe_drivewidth;
                    }
                    /* this statement will always be false (since drivewidth = 2) */
//
//
//                     /* empty pathspec, non-empty drivespec,
//                      * if there are no characters in the buffer, do not add a trailing slash
//                      * if this is a drive, do not add a trailing slash
//                      * otherwise, add a trailing slash
//                      */
//                     else if (pwidth && drivewidth != 2) {
//                         // Rprintf("needs a slash\n");
//                         need_trailing_slash[i] = 1;
//                         pwidth++;
//                     }
                }


                /* non-empty drivespec that matches our previous drivespec
                 * add the absolute path index and the non empty pathspec index
                 * back into our original variables
                 * also add the width to the original pwidth
                 *
                 * if we have not found an absolute path, continue looking back
                 * otherwise, we are done
                 */
                if (maybe_drivewidth) {
                    if (maybe_non_empty_path_spec_indx != -1)
                        non_empty_path_spec_indx = maybe_non_empty_path_spec_indx;


                    pwidth += maybe_pwidth; maybe_pwidth = 0;


                    /* unlike above, we are NOT adding the drive to the buffer */
                    // pwidth += drivewidth;


                    /* if we found an absolute path, then we are done */
                    if (maybe_abs_path_indx != -1) {
                        abs_path_indx = maybe_abs_path_indx;
                        break;
                    }
                }
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


        int start_from_here = abs_path_indx;
        if (start_from_here == -1)
            start_from_here = non_empty_path_spec_indx;


        /* if we found a drive */
        if (drive_indx != -1) {


            /* and the drive is at the same index as the start pos
             * copy to the buffer without the need for splitting */
            if (start_from_here == drive_indx) {
                strcpy(buf, ptr);
                buf += nchar;
#if debug
                Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", nchar);
                check_width += strlen(ptr);
#endif
                if (need_trailing_slash[drive_indx]) {
                    strcpy(buf, "/");
                    buf++;
#if debug
                    Rprintf("copied to buffer: %s", "/"); Rprintf("\n");
                    Rprintf("shifted buffer by %d bytes\n", strlen("/"));
                    check_width += strlen("/");
#endif
                }
                start_from_here++;
            }


            /* drive is at some index, start pos is at some other index,
             * so copy only the drive
             */
            else {
                strncpy(buf, ptr, drivewidth);
                buf += drivewidth;
                *buf = '\0';  // copy a nul character
#if debug
                Rprintf("copied first %d bytes to buffer: %s", drivewidth, ptr); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", drivewidth);
                check_width += drivewidth;
#endif


                if (start_from_here > drive_indx &&
                    need_trailing_slash[drive_indx])
                {
                    strcpy(buf, "/");
                    buf++;
#if debug
                    Rprintf("copied to buffer: %s", "/"); Rprintf("\n");
                    Rprintf("shifted buffer by %d bytes\n", strlen("/"));
                    check_width += strlen("/");
#endif
                }
            }
        }


        if (start_from_here != -1) {


            /* start immediately AFTER the absolute path */
            for (i = start_from_here; i < x_length; i++) {
                len = LENGTH(VECTOR_ELT(x, i));
                ptr = Rf_translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % len));
                nchar = (int) strlen(ptr);


                /* an empty string does not need to be copied to the buffer
                 * and will never need a trailing backslash
                 */
                if (!nchar)
                    continue;


                /* if we're before or at the drive indx,
                 * be sure to chop off the drive before pasting
                 */
                if (i <= drive_indx) {
                    drivewidth = drive_width_windows(ptr, nchar);
                    ptr += drivewidth;
                    nchar -= drivewidth;


                    if (!nchar)
                        continue;
                }


                strcpy(buf, ptr);
                buf += nchar;
#if debug
                Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", nchar);
                check_width += strlen(ptr);
#endif
                if (need_trailing_slash[i]) {
                    strcpy(buf, "/");
                    buf++;
#if debug
                    Rprintf("copied to buffer: %s", "/"); Rprintf("\n");
                    Rprintf("shifted buffer by %d bytes\n", strlen("/"));
                    check_width += strlen("/");
#endif
                }
            }
        }


#if debug
        if (check_width != pwidth) Rf_error("allocated string of %d bytes, allocated %d bytes instead\n", pwidth, check_width);
        if (strlen(cbuf) != pwidth) Rf_error("allocated string of %d bytes, got a string of %d bytes instead\n", pwidth, strlen(cbuf));
        Rprintf("\n");
#endif


        /* use cbuf because we want the whole string */
        SET_STRING_ELT(value, j, Rf_mkCharCE(cbuf, CE_UTF8));
    }
}


static R_INLINE
void unix_path_join(SEXP x, int x_length, int commonLength, SEXP value)
{
    int i, j;


    /* width of the path */
    int pwidth;


    const char *ptr;
    int *need_trailing_slash;
    need_trailing_slash = (int *) R_alloc(x_length, sizeof(int));


    int len, nchar;


    for (j = 0; j < commonLength; j++) {


        pwidth = 0;


        ptr = "";
        memset(need_trailing_slash, 0, x_length * sizeof(int));


        /* we're going to work backwards here
         * since we want the LAST absolute path
         */
        for (i = x_length - 1; i >= 0; i--) {
            len = LENGTH(VECTOR_ELT(x, i));
            ptr = Rf_translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % len));
            nchar = (int) strlen(ptr);
            if (!nchar)
                continue;


            /* if there are characters already in the buffer
             * and this path does not end with /
             * then we will need to add one manually,
             * record this index as needing a trailing slash
             */
            if (pwidth && ptr[nchar - 1] != '/') {
                need_trailing_slash[i] = 1;
                pwidth++;
            }


            /* add this string to the width of the buffer */
            pwidth += nchar;


            if (i == 0) break;


            /* test for absolute paths:
             * if it starts with /
             * if it is equal to ~
             * if it starts with ~/
             */
            if (*ptr == '/') break;  /* path starts with / */
            if (*ptr == '~') {
                if (nchar == 1) break;  /* path equals ~ */
                if (*(ptr + 1) == '/') break;  /* path starts with ~/ */
                if (*R_ExpandFileName(ptr) == '/') break;  /* path expands to an absolute path, e.g. ~iris/foo might expand to /home/iris/foo */
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
#endif


        char _buf[pwidth + 1];


        /* points to the current location where we will be pasting characters */
        char *buf = _buf;


        /* points to the start of the string */
        const char *cbuf = buf;


        /* take where we exited and start from there */
        if (i < 0) i = 0;
        for (; i < x_length; i++) {
            len = LENGTH(VECTOR_ELT(x, i));
            ptr = Rf_translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % len));
            nchar = (int) strlen(ptr);


            /* an empty string does not need to be copied to the buffer
             * and will never need a trailing slash
             */
            if (!nchar)
                continue;


            strcpy(buf, ptr);
            buf += nchar;
#if debug
                Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", nchar);
                check_width += strlen(ptr);
#endif
            if (need_trailing_slash[i]) {
                strcpy(buf, "/");
                buf++;
#if debug
                    Rprintf("copied to buffer: %s", "/"); Rprintf("\n");
                    Rprintf("shifted buffer by %d bytes\n", strlen("/"));
                    check_width += strlen("/");
#endif
            }
        }


#if debug
        if (check_width != pwidth) Rf_error("allocated string of %d bytes, allocated %d bytes instead\n", pwidth, check_width);
        if (strlen(cbuf) != pwidth) Rf_error("allocated string of %d bytes, got a string of %d bytes instead\n", pwidth, strlen(cbuf));
        Rprintf("\n");
#endif


        /* use cbuf because we want the whole string */
        SET_STRING_ELT(value, j, Rf_mkCharCE(cbuf, CE_UTF8));
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
    int i, j;
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


    for (i = 0; i < x_length; i++) {
        int len = LENGTH(VECTOR_ELT(x, i));
        for (j = 0; j < len; j++) {
            SEXP cs = STRING_ELT(VECTOR_ELT(x, i), j);
            if (Rf_getCharCE(cs) == CE_BYTES)
                Rf_error("strings with \"bytes\" encoding are not allowed");
        }
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
