#include <R.h>
#include <Rinternals.h>


#include <ctype.h>  /* includes tolower() */


// #define debug





// #define IS_ASCII(X) (getCharCE((X)) == CE_ANY)
// #define IS_UTF8(X)  (getCharCE((X)) == CE_UTF8)
#define IS_BYTES(X) (getCharCE((X)) == CE_BYTES)


/* it might be more helpful if we returned a pointer to the pathspec of s
 * instead of the drivespec width as an integer, but I can't be bothered to
 * change it now. maybe fix me later?
 */


int get_drive_width(const char *s, int nchar)
{
    /* there are three types of absolute paths in windows
     *
     * there are those beginning with d:/ or some other letter
     * we call these drives
     *
     * there are those beginning with //host/share
     * we call these network shares (for accessing remote data)
     *
     * and specifically for R, there are those beginning with ~
     * for functions such as dirname() and basename(), we would normally expand
     * those filenames with R_ExpandFileName(), but for path.join() we don't
     * want to modify the inputs
     *
     * unlike unix, a path beginning with / is NOT an absolute path.
     * try this for yourself:

setwd("C:/")
normalizePath("/path/to/file")

setwd("D:/")
normalizePath("/path/to/file")

     * which should do something like this:

> setwd("C:/")
> normalizePath("/path/to/file")
[1] "C:\\path\\to\\file"
>
> setwd("D:/")
> normalizePath("/path/to/file")
[1] "D:\\path\\to\\file"
>

     * this function will return the width of the drive specification of the
     * path, or 0 if no drive specification exists or is invalid
     *
     * when I say drive specification, I am referring to all three of the above
     * d:           is a drive specification
     * //host/share is a drive specification
     * ~            is a drive specification
     *
     * as a short-form, you can call it a drivespec
     *
     * the path specification of a path is the portion of the string
     * immediately following a possible drivespec
     *
     * you can call it a pathspec for short
     *
     *
     *
     * Arguments:
     *
     * s
     *
     *     the string in which we are looking for a drivespec
     *
     * nchar
     *
     *     the length of the string. this argument exists purely so that you
     *     don't have to calculate strlen(s) twice (assuming you're using nchar
     *     somewhere else in your program)
     */


    if (nchar <= 0)
        return 0;
    if (nchar >= 2 && *(s + 1) == ':')  /* s starts with d: or similar */
        return 2;
    if (*s == '~' &&      /* s starts with ~ */
        (
            nchar == 1       ||  /* s is exactly ~ */
            *(s + 1) == '/'  ||  /* s starts with ~/ */
            *(s + 1) == '\\'     /* s starts with ~\ */
        ))
    {
        return 1;
    }


    /* 5 characters is the minimum required for a network share
     * the two slashes at the start, at least one for the host name,
     * a slash between the host name and share name,
     * and at least one for the share name
     */
    if (nchar < 5)
        return 0;


    const char *ptr = s;
    if (*ptr != '/' && *ptr != '\\')  /* first character must be / or \ */
        return 0;
    ptr++;
    if (*ptr != '/' && *ptr != '\\')  /* second character must be / or \ */
        return 0;
    ptr++;


    /* third character must NOT be / or \
     * this is the start of the host name of the network share
     */
    if (*ptr == '/' || *ptr == '\\')
        return 0;


    const char *ptr_slash, *ptr_backslash;
    ptr_slash     = strchr(ptr, '/');
    ptr_backslash = strchr(ptr, '\\');
    if (ptr_slash) {  /* slash was found */
        if (ptr_backslash) {  /* backslash was also found */
            if (ptr_slash < ptr_backslash)  /* slash found before backslash */
                ptr = ptr_slash;
            else ptr = ptr_backslash;  /* backslash found before slash */
        }
        else ptr = ptr_slash;  /* backslash was not found */
    }
    else {  /* slash was not found */
        if (ptr_backslash)  /* backslash was found */
            ptr = ptr_backslash;
        else return 0;
    }
    ptr++;


    /* look for a non-slash and non-backslash character
     * this is the start of the share name of the network path
     */
    int found_share_name = 0;


    /* the condition *ptr can be also written as *ptr != '\0',
     * which is to say that ptr does NOT point to the end of the string
     * using *ptr is simply shorter
     */
    for (; *ptr; ptr++) {
        if (*ptr != '/' && *ptr != '\\') {
            found_share_name = 1;
            break;
        }
    }
    if (!found_share_name) return 0;


    /* again, look for a slash or backslash */
    ptr_slash     = strchr(ptr, '/');
    ptr_backslash = strchr(ptr, '\\');
    if (ptr_slash) {  /* slash was found */
        if (ptr_backslash) {  /* backslash was also found */
            if (ptr_slash < ptr_backslash)  /* slash found before backslash */
                return ptr_slash - s;
            else return ptr_backslash - s;  /* backslash found before slash */
        }
        else return ptr_slash - s;  /* backslash was not found */
    }
    else {  /* slash was not found */
        if (ptr_backslash)  /* backslash was found */
            return ptr_backslash - s;
        else return nchar;
    }
}


int get_drive_width_unix(const char *s, int nchar)
{
    /* similar to the above get_drive_width() but specifically for unix,
     * where a drivespec only really makes sense in terms of a network share
     */


    /* 5 characters is the minimum required for a network share
     * the two slashes at the start, at least one for the host name,
     * a slash between the host name and share name,
     * and at least one for the share name
     */
    if (nchar < 5)
        return 0;


    const char *ptr = s;
    if (*ptr != '/')  /* first character must be / */
        return 0;
    ptr++;
    if (*ptr != '/')  /* second character must be / */
        return 0;
    ptr++;


    /* third character must NOT be /
     * this is the start of the host name of the network share
     */
    if (*ptr == '/')
        return 0;


    const char *ptr_slash;
    ptr_slash = strchr(ptr, '/');
    if (ptr_slash)  /* slash was found */
        ptr = ptr_slash;
    else return 0;
    ptr++;


    /* look for a non-slash character
     * this is the start of the share name of the network share
     */
    int found_share_name = 0;


    for (; *ptr; ptr++) {
        if (*ptr != '/') {
            found_share_name = 1;
            break;
        }
    }
    if (!found_share_name) return 0;


    /* again, look for a slash */
    ptr_slash = strchr(ptr, '/');
    if (ptr_slash)  /* slash was found */
        return ptr_slash - s;
    else return nchar;
}





SEXP do_windowspathjoin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    /* we don't pass the ... list directly because we want to avoid an
     * accidental argument name match to PACKAGE
     */
    SEXP dots = findVarInFrame(rho, install("..."));
    if (dots == R_UnboundValue)
        error("could not find the ... list; should never happen, please report!");


    const int dots_length = ((TYPEOF(dots) == DOTSXP) ? length(dots) : 0);


    if (dots_length == 0) return allocVector(STRSXP, 0);


    SEXP x = PROTECT(allocVector(VECSXP, dots_length)); nprotect++;
    const int x_length = dots_length;
    int i, j;
    SEXP d, xi;


    /* the common length of a set of arguments is 0 if one has a length of 0,
     * or the maximal length when all are non-zero
     */
    int commonLength = 1;
    // for (i = 0, d = dots; d != R_NilValue; i++, d = CDR(d)) {  /* slightly slower */
    for (i = 0, d = dots; i < x_length; i++, d = CDR(d)) {


        /* evaluate each argument of 'dots' */
        xi = CAR(d);
        xi = eval(xi, rho);
        if (commonLength) {


            /* add the evaluated object to 'x' */
            SET_VECTOR_ELT(x, i, xi);


            /* coerce the object to string if needed */
            if (!isString(xi)) {
                if (OBJECT(xi)) {
                    SEXP call2 = PROTECT(lang2(findVarInFrame(R_BaseEnv, R_AsCharacterSymbol), xi));
                    SET_VECTOR_ELT(x, i, eval(call2, rho));
                    UNPROTECT(1);
                }
                else if (isSymbol(xi))
                    SET_VECTOR_ELT(x, i, ScalarString(PRINTNAME(xi)));
                else SET_VECTOR_ELT(x, i, coerceVector(xi, STRSXP));

                if (!isString(VECTOR_ELT(x, i)))
                    errorcall(call, "non-string argument to 'C_pathjoin'");
            }


            /* compute the length and possibly update 'commonLength' */
            /* if the commonLength is 0, we don't need to do any more
             * calculations, we know the return value is character(0), BUT
             * we still want to evaluate all objects in 'dots', would be a
             * little strange to not do that
             */
            if (commonLength) {
                int len = LENGTH(VECTOR_ELT(x, i));
                if (len == 0 || len > commonLength)
                    commonLength = len;
            }
        }
    }


    if (commonLength == 0) {
        UNPROTECT(nprotect);
        return allocVector(STRSXP, 0);
    }


    for (i = 0; i < x_length; i++) {
        int len = LENGTH(VECTOR_ELT(x, i));
        for (j = 0; j < len; j++) {
            SEXP cs = STRING_ELT(VECTOR_ELT(x, i), j);
            if (IS_BYTES(cs))
                error("strings with \"bytes\" encoding are not allowed");
        }
    }


    SEXP value = PROTECT(allocVector(STRSXP, commonLength)); nprotect++;


    int drive_indx,                /*  largest index of the string vector which contains a drivespec */
        abs_path_indx,             /*  largest index of the string vector which contains an absolute pathspec */
        non_empty_path_spec_indx;  /* smallest index of the string vector which contains a non-empty pathspec, but always >= abs_path_indx */


    /* width of the path and of the drivespec */
    int pwidth, drivewidth;


    const char *ptr;  /* points to the entire string from translateCharUTF8() */
    int *need_trailing_slash;  /* which strings need a slash afterward */


    int len, nchar;


    for (j = 0; j < commonLength; j++) {


        drive_indx = -1;
        abs_path_indx = -1;
        non_empty_path_spec_indx = -1;


        pwidth = 0;
        drivewidth = 0;


        ptr = "";  /* for -Wall */
        need_trailing_slash = (int *) calloc(x_length, sizeof(int));  /* which strings need a slash afterward */


        /* we're going to work backwards here
         * since we want the LAST absolute path
         */
        for (i = x_length - 1; i >= 0; i--) {
            len = LENGTH(VECTOR_ELT(x, i));
            ptr = translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % len));
            nchar = (int) strlen(ptr);
            if (!nchar)
                continue;


            /* look for a drivespec in ptr */
            drivewidth = get_drive_width(ptr, nchar);


            /* if we have no already found an absolute pathspec,
             * then look for one
             */
            if (abs_path_indx == -1) {


                /* non-empty pathspec */
                if (drivewidth < nchar) {


                    /* if the start of the pathspec is / or \
                     * then record this index as the last absolute path
                     */
                    if (ptr[drivewidth] == '/' || ptr[drivewidth] == '\\')
                        abs_path_indx = i;


                    /* record this index as the first non-empty pathspec */
                    non_empty_path_spec_indx = i;


                    /* if there are characters already in the buffer
                     * and this pathspec does not end with / or \
                     * then we will need to add one manually,
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


            const char drive_letter = tolower(ptr[0]);


            for (i--; i >= 0; i--) {
                maybe_len = LENGTH(VECTOR_ELT(x, i));
                maybe_ptr = translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % maybe_len));
                maybe_nchar = (int) strlen(maybe_ptr);
                if (!maybe_nchar)
                    continue;


                /* look for a drivespec in ptr */
                maybe_drivewidth = get_drive_width(maybe_ptr, maybe_nchar);


                if (maybe_drivewidth) {
                    if (maybe_drivewidth != 2 ||
                        tolower(maybe_ptr[0]) != drive_letter)
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


                        /* if the start of the pathspec is / or \
                         * then record this index as the last absolute path
                         */
                        if (maybe_ptr[maybe_drivewidth] == '/' ||
                            maybe_ptr[maybe_drivewidth] == '\\')
                        {
                            maybe_abs_path_indx = i;
                        }


                        /* record this index as the first non-empty pathspec */
                        maybe_non_empty_path_spec_indx = i;


                        /* if there are characters already in the buffer
                         * and this pathspec does not end with / or \ then we
                         * will need to add one manually, record this index as
                         * needing a trailing slash
                         */
                        if (pwidth &&  /* actually, do NOT use maybe_pwidth here */
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
                    /* unlike above, do not evaluate this else statement */
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
                    abs_path_indx = maybe_abs_path_indx;
                    non_empty_path_spec_indx = maybe_non_empty_path_spec_indx;


                    pwidth += maybe_pwidth; maybe_pwidth = 0;


                    /* unlike above, we are NOT adding the drive to the buffer */
                    // pwidth += drivewidth;


                    /* if we found an absolute path, then we are done */
                    if (abs_path_indx != -1)
                        break;
                }
            }
        }


        /* if all the strings were empty, return an empty string */
        if (pwidth <= 0) {
            SET_STRING_ELT(value, j, mkChar(""));
            continue;
        }


#ifdef debug
        Rprintf("pwidth = %d\n", pwidth);
        int check_width = 0;
#endif


        char _buf[pwidth + 1];


        /* points to the current location where we will be pasting characters */
        char *buf = _buf;


        /* points to the beginning of the string */
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
#ifdef debug
                Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", nchar);
                check_width += strlen(ptr);
#endif
                if (need_trailing_slash[drive_indx]) {
                    strcpy(buf, "/");
                    buf++;
#ifdef debug
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
#ifdef debug
                Rprintf("copied first %d bytes to buffer: %s", drivewidth, ptr); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", drivewidth);
                check_width += drivewidth;
#endif


                if (start_from_here > drive_indx &&
                    need_trailing_slash[drive_indx])
                {
                    strcpy(buf, "/");
                    buf++;
#ifdef debug
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
                ptr = translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % len));
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
                    drivewidth = get_drive_width(ptr, nchar);
                    ptr += drivewidth;
                    nchar -= drivewidth;


                    if (!nchar)
                        continue;
                }


                strcpy(buf, ptr);
                buf += nchar;
#ifdef debug
                Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", nchar);
                check_width += strlen(ptr);
#endif
                if (need_trailing_slash[i]) {
                    strcpy(buf, "/");
                    buf++;
#ifdef debug
                    Rprintf("copied to buffer: %s", "/"); Rprintf("\n");
                    Rprintf("shifted buffer by %d bytes\n", strlen("/"));
                    check_width += strlen("/");
#endif
                }
            }
        }


#ifdef debug
        if (check_width != pwidth) error("allocated string of %d bytes, allocated %d bytes instead\n", pwidth, check_width);
        if (strlen(cbuf) != pwidth) error("allocated string of %d bytes, got a string of %d bytes instead\n", pwidth, strlen(cbuf));
        Rprintf("\n");
#endif


        /* use cbuf because we want the whole string */
        SET_STRING_ELT(value, j, mkCharCE(cbuf, CE_UTF8));
    }


    UNPROTECT(nprotect);
    return value;
}


SEXP do_unixpathjoin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP dots = findVarInFrame(rho, install("..."));
    if (dots == R_UnboundValue)
        error("could not find the ... list; should never happen, please report!");


    int dots_length = ((TYPEOF(dots) == DOTSXP) ? length(dots) : 0);


    if (dots_length == 0) return allocVector(STRSXP, 0);


    SEXP x = PROTECT(allocVector(VECSXP, dots_length)); nprotect++;
    int x_length = dots_length;
    int i, j;
    SEXP d, xi;


    int commonLength = 1;
    for (i = 0, d = dots; d != R_NilValue; i++, d = CDR(d)) {
        xi = CAR(d);
        xi = eval(xi, rho);
        if (commonLength) {
            SET_VECTOR_ELT(x, i, xi);
            if (!isString(xi)) {
                if (OBJECT(xi)) {
                    SEXP call2 = PROTECT(lang2(findVarInFrame(R_BaseEnv, R_AsCharacterSymbol), xi));
                    SET_VECTOR_ELT(x, i, eval(call2, rho));
                    UNPROTECT(1);
                }
                else if (isSymbol(xi))
                    SET_VECTOR_ELT(x, i, ScalarString(PRINTNAME(xi)));
                else SET_VECTOR_ELT(x, i, coerceVector(xi, STRSXP));

                if (!isString(VECTOR_ELT(x, i)))
                    errorcall(call, "non-string argument to 'C_pathjoin'");
            }
            if (commonLength) {
                int len = LENGTH(VECTOR_ELT(x, i));
                if (len == 0 || len > commonLength)
                    commonLength = len;
            }
        }
    }


    if (commonLength == 0) {
        UNPROTECT(nprotect);
        return allocVector(STRSXP, 0);
    }


    for (i = 0; i < x_length; i++) {
        int len = LENGTH(VECTOR_ELT(x, i));
        for (j = 0; j < len; j++) {
            SEXP cs = STRING_ELT(VECTOR_ELT(x, i), j);
            if (IS_BYTES(cs))
                error("strings with \"bytes\" encoding are not allowed");
        }
    }


    SEXP value = PROTECT(allocVector(STRSXP, commonLength)); nprotect++;


    /* width of the path */
    int pwidth;


    const char *ptr;
    int *need_trailing_slash;


    int len, nchar;


    for (j = 0; j < commonLength; j++) {


        pwidth = 0;


        ptr = "";
        need_trailing_slash = (int *) calloc(x_length, sizeof(int));


        /* we're going to work backwards here
         * since we want the LAST absolute path
         */
        for (i = x_length - 1; i >= 0; i--) {
            len = LENGTH(VECTOR_ELT(x, i));
            ptr = translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % len));
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


            /* a path is absolute if:
             * it starts with /
             * it is equal to ~
             * it starts with ~/
             */
            if (*ptr == '/' ||
                (
                    *ptr == '~' &&      /* s starts with ~ */
                    (
                        nchar == 1 ||     /* s is exactly ~ */
                        (nchar >= 2 && *(ptr + 1) == '/')  /* s starts with ~/ */
                    )
                ))
            {
                break;  /* we will not be needing any more paths */
            }
        }


        /* if all the strings were empty, return an empty string */
        if (pwidth <= 0) {
            SET_STRING_ELT(value, j, mkChar(""));
            continue;
        }


#ifdef debug
        Rprintf("pwidth = %d\n", pwidth);
        int check_width = 0;
#endif


        char _buf[pwidth + 1];


        /* points to the current location where we will be pasting characters */
        char *buf = _buf;


        /* points to the beginning of the string */
        const char *cbuf = buf;


        /* take where we exited and start from there */
        if (i < 0) i = 0;
        for (; i < x_length; i++) {
            len = LENGTH(VECTOR_ELT(x, i));
            ptr = translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % len));
            nchar = (int) strlen(ptr);


            /* an empty string does not need to be copied to the buffer
             * and will never need a trailing backslash
             */
            if (!nchar)
                continue;


            strcpy(buf, ptr);
            buf += nchar;
#ifdef debug
                Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
                Rprintf("shifted buffer by %d bytes\n", nchar);
                check_width += strlen(ptr);
#endif
            if (need_trailing_slash[i]) {
                strcpy(buf, "/");
                buf++;
#ifdef debug
                    Rprintf("copied to buffer: %s", "/"); Rprintf("\n");
                    Rprintf("shifted buffer by %d bytes\n", strlen("/"));
                    check_width += strlen("/");
#endif
            }
        }


#ifdef debug
        if (check_width != pwidth) error("allocated string of %d bytes, allocated %d bytes instead\n", pwidth, check_width);
        if (strlen(cbuf) != pwidth) error("allocated string of %d bytes, got a string of %d bytes instead\n", pwidth, strlen(cbuf));
        Rprintf("\n");
#endif


        /* use cbuf because we want the whole string */
        SET_STRING_ELT(value, j, mkCharCE(cbuf, CE_UTF8));
    }


    UNPROTECT(nprotect);
    return value;
}


SEXP do_pathjoin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int windows = asLogical(eval(install("os.windows"), rho));
    if (windows == NA_LOGICAL)
        error("invalid 'os.windows'; should never happen, please report!");
    if (windows)
        return do_windowspathjoin(call, op, args, rho);
    else return do_unixpathjoin(call, op, args, rho);
}
