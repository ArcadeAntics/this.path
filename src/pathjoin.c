#include <R.h>
#include <Rinternals.h>


#include <ctype.h>  /* includes tolower() */





// #define IS_ASCII(X) (getCharCE((X)) == CE_ANY)
// #define IS_UTF8(X)  (getCharCE((X)) == CE_UTF8)
#define IS_BYTES(X) (getCharCE((X)) == CE_BYTES)


int get_drive_width(const char *s, int nchar)
{
    /*
     * A Windows absolute path consists of a drive specification and a path specification:
     *
     * D:/path/to/file
     *
     * D: is the drive specification
     * /path/to/file is the path specification
     *
     * specifically for R:
     *
     * ~/path/to/file
     *
     * ~ is the drive specification
     * /path/to/file is the path specification
     *
     * and UNC paths:
     *
     * //host/share/path/to/file
     *
     * //host/share is the drive specification
     * /path/to/file is the path specification
     *
     *
     *
     * return the number of characters in the drive specification
     *
     * s
     *
     *     the string in which we are looking for a drive
     *
     * nchar
     *
     *     the length of the string. this argument exists purely so that you
     *     don't have to calculate strlen(s) twice (assuming you're using nchar
     *     somewhere else in your program)
     */


    if (nchar <= 0)
        return 0;
    if (nchar >= 2 && s[1] == ':')  /* s starts with d: */
        return 2;
    if (s[0] == '~' &&      /* s starts with ~ */
        (
            nchar == 1 ||     /* s is exactly ~ */
            s[1] == '/' ||  /* s starts with ~/ */
            s[1] == '\\'    /* s starts with ~\ */
        ))
    {
        return 1;
    }


    /* 5 character is the required minimum for a UNC path
     * the two slashes at the start, at least one for the host name,
     * a slash betwenn the host name and share name,
     * and at least one for the share name
     */
    if (nchar < 5)
        return 0;


    const char *ptr = s;
    if (ptr[0] != '/' && ptr[0] != '\\')  /* first character must be / or \ */
        return 0;
    ptr++;
    if (ptr[0] != '/' && ptr[0] != '\\')  /* second character must be / or \ */
        return 0;
    ptr++;


    /* third character must NOT be / or \
     * this is the start of the host name of the UNC
     */
    if (ptr[0] == '/' || ptr[0] == '\\')
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
     * this is the start of the share name of the UNC
     */
    int found_share_name = 0;


    /* previously, the condition of the for loop was 'ptr'
     * but this doesn't work when 'ptr = "\0"' because 'ptr' is not NULL
     * but we have found the end of the string
     */
    for (; ptr[0] != '\0'; ptr++) {
        if (ptr[0] != '/' && ptr[0] != '\\') {
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
    /*
     * UNC paths:
     *
     * //host/share/path/to/file
     *
     * //host/share is the 'drive'
     * /path/to/file is the 'path specification'
     *
     *
     *
     * return the number of characters in the 'drive'
     *
     * s
     *
     *     the string in which we are looking for a drive
     *
     * nchar
     *
     *     the length of the string. this argument exists purely so that you
     *     don't have to calculate strlen(s) twice (assuming you're using nchar
     *     somewhere else in your program)
     */


    /* 5 character is the required minimum for a UNC path
     * the two slashes at the start, at least one for the host name,
     * a slash betwenn the host name and share name,
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
     * this is the start of the host name of the UNC
     */
    if (*ptr == '/')
        return 0;


    const char *ptr_slash;
    ptr_slash = strchr(ptr, '/');
    if (ptr_slash)  /* slash was found */
        ptr = ptr_slash;
    else return 0;
    ptr++;
    /* look for a non-slash and non-backslash character
     * this is the start of the share name of the UNC
     */
    int found_share_name = 0;


    /* previously, the condition of the for loop was 'ptr'
     * but this doesn't work when 'ptr = "\0"' because 'ptr' is not NULL
     * but we have found the end of the string
     */
    for (; *ptr != '\0'; ptr++) {
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


    for (j = 0; j < commonLength; j++) {
        int drive_indx = -1,  /* largest index of the string vector which contains a drive */
            abs_path_indx = -1,  /* largest index of the string vector which contains an absolute path specification */
            non_empty_path_spec_indx = -1;  /* smallest index of the string vector which contains a non-empty path specification, but always >= abs_path_indx */


        /* width of the path and of the drive */
        int pwidth = 0, drivewidth = 0;
        const char *ptr = "";  /* for -Wall */
        int *need_trailing_slash = (int *) calloc(x_length, sizeof(int));  /* which strings need a slash afterward */
        int len, nchar;


        /* we're going to work backwards here
         * since we want the LAST absolute path
         */
        for (i = x_length - 1; i >= 0; i--) {
            len = LENGTH(VECTOR_ELT(x, i));
            ptr = translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % len));
            nchar = (int) strlen(ptr);
            // Rprintf("ptr         = %s\n", ptr);
            // Rprintf("strlen(ptr) = %d\n", nchar);
            if (!nchar)
                continue;


            /* look for a drive specification in ptr */
            drivewidth = get_drive_width(ptr, nchar);


            /* if we have no already found an absolute path specification,
             * then look for one
             */
            if (abs_path_indx == -1) {


                /* a non-empty path specification */
                if (drivewidth < nchar) {


                    /* if the start of the path specification is / or \
                     * then record this index as the last absolute path
                     */
                    if (ptr[drivewidth] == '/' || ptr[drivewidth] == '\\')
                        abs_path_indx = i;


                    /* record this index as the first non-empty path specification */
                    non_empty_path_spec_indx = i;


                    /* if there are characters already in the buffer
                     * and this path specification does not end with / or \
                     * then we will need to add one manually,
                     * record this index as needing a trailing slash
                     */
                    if (pwidth &&
                        ptr[nchar - 1] != '/' &&
                        ptr[nchar - 1] != '\\')
                    {
                        // Rprintf("needs a slash\n");
                        need_trailing_slash[i] = 1;
                        pwidth++;
                    }


                    /* add the number of chars in the path specification
                     * to the width of our output string
                     */
                    pwidth += nchar - drivewidth;
                }


                /* empty path specification, non-empty drive specification,
                 * if there are no characters in the buffer, do not add a trailing slash
                 * if this is a colon-drive specification, do not add a trailing slash
                 * otherwise, add a trailing slash
                 */
                else if (pwidth && drivewidth != 2) {
                    // Rprintf("needs a slash\n");
                    need_trailing_slash[i] = 1;
                    pwidth++;
                }
            }


            /* non-empty drive specification
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


        // Rprintf("i = %d\n", i);


        /* if we have not found an absolute path
         * and our drive is a colon-drive,
         *
         * then we will want to go back slightly further
         * incase there's more to add to the path
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


                /* look for a drive specification in ptr */
                maybe_drivewidth = get_drive_width(maybe_ptr, maybe_nchar);


                if (maybe_drivewidth) {
                    if (maybe_drivewidth != 2 ||
                        tolower(maybe_ptr[0]) != drive_letter)
                    {
                        break;
                    }
                }


                /* if we have no already found an absolute path specification,
                 * then look for one
                 */
                if (maybe_abs_path_indx == -1) {


                    /* a non-empty path specification */
                    if (maybe_drivewidth < maybe_nchar) {


                        /* if the start of the path specification is / or \
                         * then record this index as the last absolute path
                         */
                        if (maybe_ptr[maybe_drivewidth] == '/' ||
                            maybe_ptr[maybe_drivewidth] == '\\')
                        {
                            maybe_abs_path_indx = i;
                        }


                        /* record this index as the first non-empty path specification */
                        maybe_non_empty_path_spec_indx = i;


                        /* if there are characters already in the buffer
                         * and this path specification does not end with / or \
                         * then we will need to add one manually,
                         * record this index as needing a trailing slash
                         */
                        if (pwidth &&  /* actually, do NOT use maybe_pwidth here */
                            maybe_ptr[maybe_nchar - 1] != '/' &&
                            maybe_ptr[maybe_nchar - 1] != '\\')
                        {
                            // Rprintf("needs a slash\n");
                            need_trailing_slash[i] = 1;
                            maybe_pwidth++;
                        }


                        /* add the number of chars in the path specification
                         * to the width of our output string
                         */
                        maybe_pwidth += maybe_nchar - maybe_drivewidth;
                    }
//
//
//                     /* empty path specification, non-empty drive specification,
//                      * if there are no characters in the buffer, do not add a trailing slash
//                      * if this is a colon-drive specification, do not add a trailing slash
//                      * otherwise, add a trailing slash
//                      */
//                     else if (pwidth && drivewidth != 2) {
//                         // Rprintf("needs a slash\n");
//                         need_trailing_slash[i] = 1;
//                         pwidth++;
//                     }
                }


                /* non-empty drive specification
                 * record this index as the last index containing a drive
                 * add the number of chars in the drive to the buffer
                 * break immediately, will not be adding more to the path
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


        /*
        for (int k = 0; k < x_length; k++) {
            Rprintf("%d ", need_trailing_slash[k]);
        }
        Rprintf("\n");
         */


        /* if all the strings were empty, return an empty string */
        if (pwidth <= 0) {
            SET_STRING_ELT(value, j, mkChar(""));
            continue;
        }


        // Rprintf("pwidth = %d\n", pwidth);


        /* add one to the width for a '\0' char */
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
                // Rprintf("copied to buffer: %s\n", ptr);
                if (need_trailing_slash[drive_indx]) {
                    strcpy(buf, "/");
                    buf++;
                    // Rprintf("copied to buffer: %s\n", "/");
                }
                start_from_here++;
            }


            /* drive is at some index, start pos is at some other index,
             * so copy only the drive
             */
            else {
                strncpy(buf, ptr, drivewidth);
                buf += drivewidth;


                if (start_from_here > drive_indx &&
                    need_trailing_slash[drive_indx])
                {
                    strcpy(buf, "/");
                    buf++;
                    // Rprintf("copied to buffer: %s\n", "/");
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
                // Rprintf("copied to buffer: %s\n", ptr);
                if (need_trailing_slash[i]) {
                    strcpy(buf, "/");
                    buf++;
                    // Rprintf("copied to buffer: %s\n", "/");
                }
            }
        }


        /* add the terminating character, shouldn't be necessary but just in case */
        // strcpy(buf, "\0");
        _buf[pwidth] = '\0';


        /* use cbuf because we want the whole string */
        SET_STRING_ELT(value, j, mkCharCE(cbuf, CE_UTF8));
        // Rprintf("\n");
    }


    UNPROTECT(nprotect);
    return value;


    // int ce = getCharCE(asChar(eval(CAR(dots), rho)));
    // if (ce == CE_NATIVE)
    //     return mkString("native.enc");
    // if (ce == CE_UTF8)
    //     return mkString("UTF-8");
    // if (ce == CE_LATIN1)
    //     return mkString("latin1");
    // if (ce == CE_BYTES)
    //     return mkString("bytes");
    // if (ce == CE_SYMBOL)
    //     return mkString("CE_SYMBOL");
    // if (ce == CE_ANY)
    //     return mkString("CE_ANY");
    // UNPROTECT(nprotect);
    // return mkString("unknown");
    // return ScalarString(mkCharCE("testing", CE_UTF8));


    // SEXP e1 = PROTECT(eval(CAR (dots), rho)); nprotect++;
    // if (!isPairList(e1))
    //     error("");
    //
    //
    // for (SEXP ptr = e1; ptr != R_NilValue; ptr = CDR(ptr)) {
    //     CAR(ptr);
    // }
    //
    //
    // UNPROTECT(nprotect);
    // return R_NilValue;


    // SEXP return_this = PROTECT(allocVector(VECSXP, 4)); nprotect++;
    // SET_VECTOR_ELT(return_this, 0, call);
    // SET_VECTOR_ELT(return_this, 1, op  );
    // SET_VECTOR_ELT(return_this, 2, args);
    // SET_VECTOR_ELT(return_this, 3, rho );
    //
    //
    // SEXP names = allocVector(STRSXP, 4);
    // setAttrib(return_this, R_NamesSymbol, names);
    // SET_STRING_ELT(names, 0, mkChar("call"));
    // SET_STRING_ELT(names, 1, mkChar("op"  ));
    // SET_STRING_ELT(names, 2, mkChar("args"));
    // SET_STRING_ELT(names, 3, mkChar("rho" ));
    //
    //
    // UNPROTECT(nprotect);
    // return return_this;
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


    for (j = 0; j < commonLength; j++) {


        /* width of the path */
        int pwidth = 0;
        const char *ptr = "";  /* for -Wall */
        int *need_trailing_slash = (int *) calloc(x_length, sizeof(int));  /* which strings need a slash afterward */
        int len, nchar;


        /* we're going to work backwards here
         * since we want the LAST absolute path
         */
        for (i = x_length - 1; i >= 0; i--) {
            len = LENGTH(VECTOR_ELT(x, i));
            ptr = translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % len));
            nchar = (int) strlen(ptr);
            // Rprintf("ptr         = %s\n", ptr);
            // Rprintf("strlen(ptr) = %d\n", nchar);
            if (!nchar)
                continue;


            /* if there are characters already in the buffer
             * and this path does not end with /
             * then we will need to add one manually,
             * record this index as needing a trailing slash
             */
            if (pwidth && ptr[nchar - 1] != '/') {
                // Rprintf("needs a slash\n");
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
            if (ptr[0] == '/' ||
                (
                    ptr[0] == '~' &&      /* s starts with ~ */
                    (
                        nchar == 1 ||     /* s is exactly ~ */
                        (nchar >= 2 && ptr[1] == '/')  /* s starts with ~/ */
                    )
                ))
            {
                break;  /* we will not be needing any more paths */
            }
        }


        // Rprintf("i = %d\n", i);


        /* if all the strings were empty, return an empty string */
        if (pwidth <= 0) {
            SET_STRING_ELT(value, j, mkChar(""));
            continue;
        }


        // Rprintf("pwidth = %d\n", pwidth);


        /* add one to the width for a '\0' char */
        char _buf[pwidth + 1];


        /* points to the current location where we will be pasting characters */
        char *buf = _buf;


        /* points to the beginning of the string */
        const char *cbuf = buf;


        /* take where we exitted and start from there */
        if (i < 0)
            i = 0;
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
            // Rprintf("copied to buffer: %s\n", ptr);
            if (need_trailing_slash[i]) {
                strcpy(buf, "/");
                buf++;
                // Rprintf("copied to buffer: %s\n", "/");
            }
        }


        /* add the terminating character, shouldn't be necessary but just in case */
        // strcpy(buf, "\0");
        _buf[pwidth] = '\0';


        /* use cbuf because we want the whole string */
        SET_STRING_ELT(value, j, mkCharCE(cbuf, CE_UTF8));
        // Rprintf("\n");
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
