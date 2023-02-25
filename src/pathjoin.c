#include "thispathdefn.h"
#include <ctype.h>  /* includes toupper() */


#define debug 0


#include "drivewidth.h"
#include "thispathbackports.h"





void windowspathjoin(SEXP x, int x_length, int commonLength, SEXP value)
{
    int i, j;


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
            drivewidth = get_drive_width_windows(ptr, nchar);


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
                maybe_ptr = translateCharUTF8(STRING_ELT(VECTOR_ELT(x, i), j % maybe_len));
                maybe_nchar = (int) strlen(maybe_ptr);
                if (!maybe_nchar)
                    continue;


                /* look for a drivespec in ptr */
                maybe_drivewidth = get_drive_width_windows(maybe_ptr, maybe_nchar);


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


                        /* if there are characters already in the buffer (excluding the drive)
                         * or there are characters already in the maybe buffer
                         * and this pathspec does not end with / or \
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
            // SET_STRING_ELT(value, j, mkChar(""));
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
                    drivewidth = get_drive_width_windows(ptr, nchar);
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
        if (check_width != pwidth) error("allocated string of %d bytes, allocated %d bytes instead\n", pwidth, check_width);
        if (strlen(cbuf) != pwidth) error("allocated string of %d bytes, got a string of %d bytes instead\n", pwidth, strlen(cbuf));
        Rprintf("\n");
#endif


        /* use cbuf because we want the whole string */
        SET_STRING_ELT(value, j, mkCharCE(cbuf, CE_UTF8));
    }
}


void unixpathjoin(SEXP x, int x_length, int commonLength, SEXP value)
{
    int i, j;


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
            // SET_STRING_ELT(value, j, mkChar(""));
            continue;
        }


#if debug
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
        if (check_width != pwidth) error("allocated string of %d bytes, allocated %d bytes instead\n", pwidth, check_width);
        if (strlen(cbuf) != pwidth) error("allocated string of %d bytes, got a string of %d bytes instead\n", pwidth, strlen(cbuf));
        Rprintf("\n");
#endif


        /* use cbuf because we want the whole string */
        SET_STRING_ELT(value, j, mkCharCE(cbuf, CE_UTF8));
    }
}


void pathjoin(SEXP x, int x_length, int commonLength, SEXP value)
{
#ifdef _WIN32
    windowspathjoin(x, x_length, commonLength, value);
#else
    unixpathjoin(x, x_length, commonLength, value);
#endif
}





#define do_pathjoin_body(name, fun)                            \
    int nprotect = 0;                                          \
                                                               \
                                                               \
    /* we don't pass the ... list directly because we want to  \
       avoid an accidental argument name match to PACKAGE      \
     */                                                        \
    SEXP dots = findVarInFrame(rho, R_DotsSymbol);             \
    if (dots == R_UnboundValue)                                \
        error("could not find the ... list; should never happen, please report!");\
                                                               \
                                                               \
    int dots_length = ((TYPEOF(dots) == DOTSXP) ? length(dots) : 0);\
                                                               \
                                                               \
    if (dots_length == 0) return allocVector(STRSXP, 0);       \
                                                               \
                                                               \
    SEXP x = allocVector(VECSXP, dots_length);                 \
    PROTECT(x); nprotect++;                                    \
    int x_length = dots_length;                                \
    int i, j;                                                  \
    SEXP d, xi;                                                \
                                                               \
                                                               \
    /* the common length of a set of arguments is 0 if one has a length of 0 */\
    /* or the maximal length when all are non-zero                           */\
    int commonLength = 1;                                      \
    /* for (i = 0, d = dots; d != R_NilValue; i++, d = CDR(d)) { *//* slightly slower */\
    for (i = 0, d = dots; i < x_length; i++, d = CDR(d)) {     \
                                                               \
                                                               \
        /* evaluate each argument of 'dots' */                 \
        xi = CAR(d);                                           \
        xi = eval(xi, rho);                                    \
        if (commonLength) {                                    \
                                                               \
                                                               \
            /* add the evaluated object to 'x' */              \
            SET_VECTOR_ELT(x, i, xi);                          \
                                                               \
                                                               \
            /* coerce the object to string if needed */        \
            if (!isString(xi)) {                               \
                if (OBJECT(xi)) {                              \
                    SEXP expr, expr2;                          \
                    expr = allocList(2);                       \
                    PROTECT(expr);                             \
                    SET_TYPEOF(expr, LANGSXP);                 \
                    SETCAR(expr, findVarInFrame(R_BaseEnv, R_AsCharacterSymbol));\
                    SETCADR(expr, expr2 = allocList(2));       \
                    SET_TYPEOF(expr2, LANGSXP);                \
                    SETCAR(expr2, findVarInFrame(R_BaseEnv, R_QuoteSymbol));\
                    SETCADR(expr2, xi);                        \
                    SET_VECTOR_ELT(x, i, eval(expr, rho));     \
                    UNPROTECT(1);                              \
                }                                              \
                else if (isSymbol(xi))                         \
                    SET_VECTOR_ELT(x, i, ScalarString(PRINTNAME(xi)));\
                else SET_VECTOR_ELT(x, i, coerceVector(xi, STRSXP));\
                                                               \
                if (!isString(VECTOR_ELT(x, i)))               \
                    errorcall(call, "non-string argument to '%s'", (name));\
            }                                                  \
                                                               \
                                                               \
            /* compute the length and possibly update 'commonLength'       */\
            /* if the commonLength is 0, we don't need to do any more      */\
            /* calculations, we know the return value is character(0), BUT */\
            /* we still want to evaluate all objects in 'dots', would be a */\
            /* little strange to not do that                               */\
            if (commonLength) {                                \
                int len = LENGTH(VECTOR_ELT(x, i));            \
                if (len == 0 || len > commonLength)            \
                    commonLength = len;                        \
            }                                                  \
        }                                                      \
    }                                                          \
                                                               \
                                                               \
    if (commonLength == 0) {                                   \
        UNPROTECT(nprotect);                                   \
        return allocVector(STRSXP, 0);                         \
    }                                                          \
                                                               \
                                                               \
    for (i = 0; i < x_length; i++) {                           \
        int len = LENGTH(VECTOR_ELT(x, i));                    \
        for (j = 0; j < len; j++) {                            \
            SEXP cs = STRING_ELT(VECTOR_ELT(x, i), j);         \
            if (getCharCE(cs) == CE_BYTES)                     \
                error("strings with \"bytes\" encoding are not allowed");\
        }                                                      \
    }                                                          \
                                                               \
                                                               \
    SEXP value = allocVector(STRSXP, commonLength);            \
    PROTECT(value); nprotect++;                                \
                                                               \
                                                               \
    fun(x, x_length, commonLength, value);                     \
                                                               \
                                                               \
    UNPROTECT(nprotect);                                       \
    return value
/* omit ; on purpose */


SEXP do_windowspathjoin do_formals
{
    do_start("windowspathjoin", 0);
    if (debug) Rprintf("in do_windowspathjoin\n\n");
    do_pathjoin_body("windows.path.join", windowspathjoin);
}


SEXP do_unixpathjoin do_formals
{
    do_start("unixpathjoin", 0);
    if (debug) Rprintf("in do_unixpathjoin\n\n");
    do_pathjoin_body("unix.path.join", unixpathjoin);
}


SEXP do_pathjoin do_formals
{
    do_start("pathjoin", 0);
    if (debug) Rprintf("in do_pathjoin\n\n");
    do_pathjoin_body("path.join", pathjoin);
}
