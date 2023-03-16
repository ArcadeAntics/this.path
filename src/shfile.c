#include "thispathdefn.h"





void Rprint(SEXP x, SEXP rho)
{
    SEXP expr = lang2(install("print"), x);
    PROTECT(expr);
    eval(expr, rho);
    UNPROTECT(1);
}


#define print_char_array(_ac_, _av_)                           \
    do {                                                       \
        SEXP tmp10 = allocVector(STRSXP, (_ac_));              \
        PROTECT(tmp10);                                        \
        for (int indx = 0; indx < (_ac_); indx++) {            \
            SET_STRING_ELT(tmp10, indx, mkChar((_av_)[indx])); \
        }                                                      \
        Rprint(tmp10, R_BaseEnv);                              \
        UNPROTECT(1);                                          \
    } while (0)


// #define debug





SEXP do_shfile do_formals
{
    /*
    do_shfile {this.path}                                        C Documentation

    Get Argument FILE Provided to R by a Shell



    Description:

    This function chooses whether to return `thispathofile` or `thispathfile`.
    It examines the promises, determining which to return based on whether
    they're evaluated.

    This is not the workhorse behind extracting FILE from a shell.
     */


    do_start("shfile", 2);


    /* see ?shFILE */
    int original = asLogical(CAR(args)); args = CDR(args);
    int for_msg  = asLogical(CAR(args)); args = CDR(args);
    if (for_msg == NA_LOGICAL)
        error(_("invalid '%s' argument"), "for.msg");


    /* if 'for.msg = TRUE', we treat 'original = FALSE' as 'original = NA' */
    if (for_msg && !original) original = NA_LOGICAL;


    if (original == NA_LOGICAL) {


#define get_and_check(var, sym)                                \
        SEXP var = findVarInFrame(ENCLOS(rho), (sym));         \
        if (var == R_UnboundValue)                             \
            error(_("object '%s' not found"), EncodeChar(PRINTNAME((sym))));\
        if (TYPEOF(var) != PROMSXP)                            \
            error("invalid '%s', must be a promise", EncodeChar(PRINTNAME((sym))))


        get_and_check(thispathfile, thispathfileSymbol);
        /* if the promise has not already been forced, just get the original */
        if (PRVALUE(thispathfile) == R_UnboundValue)
            original = TRUE;
        else
            return PRVALUE(thispathfile);
    }
    if (original) {
#define get_and_return(var, sym)                               \
        get_and_check(var, sym);                               \
        if (PRVALUE(var) == R_UnboundValue) {                  \
            /* unlike a normal promise, we DO NOT want to */   \
            /* raise a warning if var is re-evaluated     */   \
            if (PRSEEN(var)) {                                 \
                if (PRSEEN(var) == 1) {}                       \
                else SET_PRSEEN(var, 0);                       \
            }                                                  \
            return eval(var, R_EmptyEnv);                      \
        }                                                      \
        else                                                   \
            return PRVALUE(var)


        get_and_return(thispathofile, thispathofileSymbol);
    }
    else {
        get_and_return(thispathfile, thispathfileSymbol);
    }


#undef get_and_return
#undef get_and_check
}





#if defined(_WIN32)


// https://github.com/wch/r-source/blob/trunk/src/gnuwin32/system.c#L955
static void env_command_line(int *pac, const char **argv)
{
    int ac = *pac, newac = 1;
    const char **av = argv;
    Rboolean hadE = FALSE;


    while (--ac) {
        ++av;
        if (strcmp(*av, "-e") == 0) {
            hadE = TRUE;
            argv[newac++] = *av;
            continue;
        }
        if (!hadE && **av != '-' && strchr(*av, '=')) {
        }
        else argv[newac++] = *av;
        hadE = FALSE;
    }
    *pac = newac;
}


#endif


// https://github.com/wch/r-source/blob/trunk/src/main/CommandLineArgs.c#L94
void common_command_line(int *pac, const char **argv, char *enc, Rboolean *has_enc)
{
    *has_enc = FALSE;


    int ac = *pac, newac = 1;
    const char *p;
    const char **av = argv;
    Rboolean processing = TRUE;


    while (--ac) {
        if (processing && **++av == '-') {
            if (!strcmp(*av, "--version")) {
            }
            else if (!strcmp(*av, "--args")) {
                argv[newac++] = *av;
                processing = FALSE;
            }
            else if (!strcmp(*av, "--save")) {
            }
            else if (!strcmp(*av, "--no-save")) {
            }
            else if (!strcmp(*av, "--restore")) {
            }
            else if (!strcmp(*av, "--no-restore")) {
            }
            else if (!strcmp(*av, "--no-restore-data")) {
            }
            else if (!strcmp(*av, "--no-restore-history")) {
            }
            else if (!strcmp(*av, "--silent") ||
                     !strcmp(*av, "--quiet") ||
                     !strcmp(*av, "-q"))
            {
            }
            else if (!strcmp(*av, "--vanilla")) {
            }
            else if (!strcmp(*av, "--no-environ")) {
            }
            else if (!strcmp(*av, "--verbose")) {
            }
            else if (!strcmp(*av, "--no-echo") ||
                     !strcmp(*av, "--slave") ||
                     !strcmp(*av, "-s"))
            {
            }
            else if (!strcmp(*av, "--no-site-file")) {
            }
            else if (!strcmp(*av, "--no-init-file")) {
            }
            else if (!strcmp(*av, "--debug-init")) {
            }
            else if (!strncmp(*av, "--encoding", 10)) {
                *has_enc = TRUE;
                if (strlen(*av) < 12) {
                    if (ac > 1) { ac--; av++; p = *av; } else p = NULL;
                } else p = &(*av)[11];
                if (p == NULL) {
                } else {
                    strncpy(enc, p, 30);
                    enc[30] = '\0';
                }
            }
#ifdef _WIN32
            else if (!strcmp(*av, "--no-Rconsole")) {
            }
#endif
            else if (!strcmp(*av, "-save") ||
                     !strcmp(*av, "-nosave") ||
                     !strcmp(*av, "-restore") ||
                     !strcmp(*av, "-norestore") ||
                     !strcmp(*av, "-noreadline") ||
                     !strcmp(*av, "-quiet") ||
                     !strcmp(*av, "-nsize") ||
                     !strcmp(*av, "-vsize") ||
                     !strncmp(*av, "--max-nsize", 11) ||
                     !strncmp(*av, "--max-vsize", 11) ||
                     !strcmp(*av, "-V") ||
                     !strcmp(*av, "-n") ||
                     !strcmp(*av, "-v"))
            {
            }
            else if (!strncmp(*av, "--min-nsize", 11) ||
                     !strncmp(*av, "--min-vsize", 11))
            {
                if (strlen(*av) < 13) {
                    if (ac > 1) {
                        ac--;
                        av++;
                    }
                }
            }
            else if (strncmp(*av, "--max-ppsize", 12) == 0) {
                if (strlen(*av) < 14) {
                    if (ac > 1) {
                        ac--;
                        av++;
                    }
                }
            }
            else { /* unknown -option */
                argv[newac++] = *av;
            }
        }
        else {
            argv[newac++] = *av;
        }
    }
    *pac = newac;
    return;
}


#if !defined(_WIN32)


// https://github.com/wch/r-source/blob/trunk/src/unix/system.c#L163
static char *unescape_arg(char *p, const char *avp)
{
    /* Undo the escaping done in the front end */
    const char *q;
    for (q = avp; *q; q++) {
        if (*q == '~' && *(q+1) == '+' && *(q+2) == '~') {
            q += 2;
            *p++ = ' ';
        } else if (*q == '~' && *(q+1) == 'n' && *(q+2) == '~') {
            q += 2;
            *p++ = '\n';
        } else if (*q == '~' && *(q+1) == 't' && *(q+2) == '~') {
            q += 2;
            *p++ = '\t';
        } else *p++ = *q;
    }
    return p;
}


#endif


SEXP do_shinfo do_formals
{
    /*
    do_shinfo {this.path}                                        C Documentation

    Get Information About The Command Line Arguments



    Description:

    Get the command line argument FILE from -f FILE, --file=FILE and whether
    any input was provided from -e expr, -f FILE, --file=FILE.



    Details:

    The command line arguments are not processed here if we cannot be
    reasonably certain that the session was invoked without being embedded.
    This means that the GUI should be one of the standard options and the
    basename of the first command line argument must be R or Rterm.exe



    Value:

    A list with at least the following components:

    ENC

        character string; command line argument 'ENC' or NA_character_

    FILE

        character string; command line argument 'FILE' or NA_character_

    EXPR

        character string; command line argument 'EXPR' or NA_character_

    has.input

        length-one logical vector; was 'FILE' or 'EXPR' provided?
     */


    do_start("shinfo", 0);


    if (!is_maybe_unembedded_shell) {


#ifdef debug
#define debugRprint(x, rho) Rprint((x), (rho))
#else
#define debugRprint(x, rho) do {} while (0)
#endif


#define return_shinfo(_ENC_, _FILE_, _EXPR_, _HAS_INPUT_)      \
        do {                                                   \
            SEXP value = allocVector(VECSXP, 4);               \
            PROTECT(value);                                    \
            SEXP names = allocVector(STRSXP, 4);               \
            setAttrib(value, R_NamesSymbol, names);            \
            SET_STRING_ELT(names, 0, mkChar("ENC"));           \
            SET_VECTOR_ELT(value, 0, (_ENC_));                 \
            SET_STRING_ELT(names, 1, mkChar("FILE"));          \
            SET_VECTOR_ELT(value, 1, (_FILE_));                \
            SET_STRING_ELT(names, 2, mkChar("EXPR"));          \
            SET_VECTOR_ELT(value, 2, (_EXPR_));                \
            SET_STRING_ELT(names, 3, mkChar("has.input"));     \
            SET_VECTOR_ELT(value, 3, (_HAS_INPUT_));           \
            debugRprint(value, R_BaseEnv);                     \
            UNPROTECT(1);                                      \
            return value;                                      \
        } while (0)


        return_shinfo(
            ScalarString(NA_STRING),
            ScalarString(NA_STRING),
            ScalarString(NA_STRING),
            ScalarLogical(NA_LOGICAL)
        );
    }


    int ARGC; SEXP ARGV;


    SEXP expr = lang1(commandArgsSymbol);
    PROTECT(expr);
    ARGV = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    PROTECT(ARGV);
    ARGC = LENGTH(ARGV);


#ifdef debug
    Rprintf("\noriginal arguments:\n");
    Rprint(ARGV, R_BaseEnv);
#endif


    char enc[31] = "";
    Rboolean has_enc = FALSE;
    const char *FILE = NULL;
    char cmdlines[10000];
    cmdlines[0] = '\0';
    Rboolean has_input = FALSE;


    if (ARGC <= 1) {
        UNPROTECT(1);
        return_shinfo(
            ScalarString(has_enc ? mkChar(enc) : NA_STRING),
            ScalarString(FILE ? mkChar(FILE) : NA_STRING),
            ScalarString(strlen(cmdlines) ? mkChar(cmdlines) : NA_STRING),
            ScalarLogical(has_input)
        );
    }


    /* determine the number of leading arguments:
     * the arguments up to and including --args
     */
    SEXP argsChar = mkChar("--args");
    int ac = ARGC;
    for (int i = 0; i < ARGC; i++) {
        if (STRING_ELT(ARGV, i) == argsChar) {
            ac = i + 1;
            break;
        }
    }


    /* copy the arguments from the STRSXP to a *char[] */
    const char *argv[ac];
    for (int i = 0; i < ac; i++) {
        argv[i] = CHAR(STRING_ELT(ARGV, i));
    }
    UNPROTECT(1);  /* ARGV */
    const char **av = argv;


#ifdef debug
    Rprintf("\nleading arguments:\n");
    print_char_array(ac, av);
#endif


#if defined(_WIN32)


// https://github.com/wch/r-source/blob/trunk/src/gnuwin32/system.c#L1064
    Rboolean processing = TRUE;


// https://github.com/wch/r-source/blob/trunk/src/gnuwin32/system.c#L1174
    env_command_line(&ac, av);


#ifdef debug
    Rprintf("\nafter removing environment variables:\n");
    print_char_array(ac, av);
#endif


// https://github.com/wch/r-source/blob/trunk/src/gnuwin32/system.c#L1176
    common_command_line(&ac, av, enc, &has_enc);
    // Rprintf("--encoding=%s\nhas encoding: %s\n", enc, has_enc ? "TRUE" : "FALSE");


#ifdef debug
    Rprintf("\nafter removing common arguments:\n");
    print_char_array(ac, av);
#endif


// https://github.com/wch/r-source/blob/trunk/src/gnuwin32/system.c#L1179
    while (--ac) {
        if (processing && **++av == '-') {
            // if (!strcmp(*av, "--help") || !strcmp(*av, "-h")) {
            // } else if (!strcmp(*av, "--cd-to-userdocs")) {
            // } else if (!strcmp(*av, "--no-environ")) {
            // } else if (!strcmp(*av, "--ess")) {
            // } else if (!strcmp(*av, "--internet2")) {
            // } else if (!strcmp(*av, "--mdi")) {
            // } else if (!strcmp(*av, "--sdi") || !strcmp(*av, "--no-mdi")) {
            // } else if (!strcmp(*av, "--debug")) {
            // } else
            if (!strcmp(*av, "--args")) {
                break;
            } else if (!strcmp(*av, "-f")) {
                has_input = TRUE;
                ac--;
                av++;
                if (!ac) {
                    errorcall(call, "option '-f' requires a filename argument");
                }
                /* if (av != "-") */
                if (strcmp(*av, "-")) {
                    FILE = *av;
                }
            } else if (!strncmp(*av, "--file=", 7)) {
                has_input = TRUE;
                if (strcmp((*av)+7, "-")) {  /* av != "--file=-" */
                    FILE = (*av)+7;
                }
            // } else if (!strncmp(*av, "--workspace=", 12)) {
            } else if (!strcmp(*av, "-e")) {
                has_input = TRUE;
                ac--;
                av++;
                if (!ac || !strlen(*av)) {
                    error("option '-e' requires a non-empty argument");
                }
                if (strlen(cmdlines) + strlen(*av) + 2 <= 10000) {
                    strcat(cmdlines, *av);
                    strcat(cmdlines, "\n");
                }
            }
        }
    }


#else


// https://github.com/wch/r-source/blob/trunk/src/unix/system.c#L191
    int i, ioff = 1, j;
    const char **avv;


// https://github.com/wch/r-source/blob/trunk/src/unix/system.c#L346
    /* first task is to select the GUI.
       If run from the shell script, only Tk|tk|X11|x11 are allowed.
     */
    for (i = 0, avv = av; i < ac; i++, avv++) {
        if (!strcmp(*avv, "--args"))
            break;
        if (!strncmp(*avv, "--gui", 5) || !strncmp(*avv, "-g", 2)) {
            if (!strncmp(*avv, "--gui", 5) && strlen(*avv) >= 7) {
            }
            else {
                if (i + 1 < ac) {
                    avv++;
                    ioff++;
                }
            }
            /* now remove it/them */
            for (j = i; j < ac - ioff; j++)
                av[j] = av[j + ioff];
            ac -= ioff;
            break;
        }
    }


#ifdef debug
    Rprintf("\nafter removing --gui/-g argument:\n");
    print_char_array(ac, av);
#endif


// https://github.com/wch/r-source/blob/trunk/src/unix/system.c#L405
    common_command_line(&ac, av, enc, &has_enc);
    // Rprintf("--encoding=%s\nhas encoding: %s\n", enc, has_enc ? "TRUE" : "FALSE");


#ifdef debug
    Rprintf("\nafter removing common arguments:\n");
    print_char_array(ac, av);
#endif


    char path[PATH_MAX + 1];


// https://github.com/wch/r-source/blob/trunk/src/unix/system.c#L406
    while (--ac) {
        if (**++av == '-') {
            // if (!strcmp(*av, "--no-readline")) {
            // } else
            if (!strcmp(*av, "-f")) {
                has_input = TRUE;
                ac--;
                av++;
                if (!ac) {
                    errorcall(call, "option '-f' requires a filename argument");
                }
#define R_INIT_TREAT_F(_AV_)                                   \
                if (strcmp(_AV_, "-")) {                       \
                    if (strlen(_AV_) >= PATH_MAX) {            \
                        errorcall(R_NilValue, _("path given in -f/--file is too long"));\
                    }                                          \
                    char *p = path;                            \
                    p = unescape_arg(p, _AV_);                 \
                    *p = '\0';                                 \
                    FILE = path;                               \
                }
                R_INIT_TREAT_F(*av);

            } else if (!strncmp(*av, "--file=", 7)) {
                has_input = TRUE;

                R_INIT_TREAT_F((*av)+7);

            } else if (!strcmp(*av, "-e")) {
                has_input = TRUE;
                ac--;
                av++;
                if (!ac) {
                    error("option '-e' requires a non-empty argument");
                }
                if (strlen(cmdlines) + strlen(*av) + 2 <= 10000) {
                    char *p = cmdlines + strlen(cmdlines);
                    p = unescape_arg(p, *av);
                    *p++ = '\n'; *p = '\0';
                }
            } else if (!strcmp(*av, "--args")) {
                break;
            } else if (!strcmp(*av, "--interactive")) {
                break;
            } else {
#ifdef HAVE_AQUA
                // r27492: in 2003 launching from 'Finder OSX' passed this
                if (!strncmp(*av, "-psn", 4)) break;
#endif
            }
        }
    }


#endif


#ifdef debug
    set_R_Visible(1);
    Rprintf("\n");
#endif


    return_shinfo(
        ScalarString(has_enc ? mkChar(enc) : NA_STRING),
        ScalarString(FILE ? mkChar(FILE) : NA_STRING),
        ScalarString(strlen(cmdlines) ? mkChar(cmdlines) : NA_STRING),
        ScalarLogical(has_input)
    );
}
