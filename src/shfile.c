#include <R.h>
#include <Rinternals.h>


#include "thispathdefn.h"





SEXP do_shfile(SEXP call, SEXP op, SEXP args, SEXP rho)
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


    /* see ?shFILE */
    int original = asLogical(CADR(args));
    int for_msg  = asLogical(CADDR(args));
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





// https://github.com/wch/r-source/blob/173e7142252170d2525b5da854ca24cb8a4f9fc9/src/gnuwin32/system.c#L955
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


// https://github.com/wch/r-source/blob/d29fd8b7f3221aaef97f0980108a230623274442/src/main/CommandLineArgs.c#L94
void R_common_command_line(int *pac, const char **argv)
{
    int ac = *pac, newac = 1;
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
                if (strlen(*av) < 12) {
                    if (ac > 1) {
                        ac--;
                        av++;
                    }
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


#define print_char_array(ac, av)                               \
    do {                                                       \
        SEXP tmp10 = allocVector(STRSXP, ac);                  \
        PROTECT(tmp10);                                        \
        for (int indx = 0; indx < ac; indx++) {                \
            SET_STRING_ELT(tmp10, indx, mkChar(av[indx]));     \
        }                                                      \
        Rprint(tmp10, R_BaseEnv);                              \
        UNPROTECT(1);                                          \
    } while (0)


const char *windowsshinfo(int ac, const char **av, int *no_input)
{
    /*
    windowsshinfo {this.path}                                    C Documentation

    Get Information About The Command Line Arguments



    Description:

    Extract the command line argument FILE and determine if any input was
    provided (from -e expr, -f FILE, or --file=FILE).
     */
    Rboolean processing = TRUE;


// https://github.com/wch/r-source/blob/589b76ba28bb97e9d628f6e4ae1735c4e2660b3f/src/gnuwin32/system.c#L1174
    env_command_line(&ac, av);


#ifdef debug
    Rprintf("\nafter removing environment variables:\n");
    print_char_array(ac, av);
#endif


// https://github.com/wch/r-source/blob/589b76ba28bb97e9d628f6e4ae1735c4e2660b3f/src/gnuwin32/system.c#L1176
    R_common_command_line(&ac, av);


#ifdef debug
    Rprintf("\nafter removing common arguments:\n");
    print_char_array(ac, av);
#endif


    const char *FILE = NULL;
// https://github.com/wch/r-source/blob/589b76ba28bb97e9d628f6e4ae1735c4e2660b3f/src/gnuwin32/system.c#L1179
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
                *no_input = FALSE;
                ac--;
                av++;
                if (!ac) {
                    error("internal error, this should have been caught earlier");
                }
                /* if (av != "-") */
                if (strcmp(*av, "-")) {
                    FILE = *av;
                }
            } else if (!strncmp(*av, "--file=", 7)) {
                *no_input = FALSE;
                /* if (av != "-") */
                if (strcmp((*av)+7, "-")) {
                    FILE = (*av)+7;
                }
            // } else if (!strncmp(*av, "--workspace=", 12)) {
            } else if (!strcmp(*av, "-e")) {
                *no_input = FALSE;
                ac--;
                av++;
                if (!ac) {
                    error("internal error, this should have been caught earlier");
                }
            }
        }
    }


#ifdef debug
    set_R_Visible(1);
    Rprintf("\n");
#endif


    return FILE;
}


// https://github.com/wch/r-source/blob/031c225f8e928f7259eed5704218edc83b7c88b0/src/unix/system.c#L163
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


const char *unixshinfo(int ac, const char **av, int *no_input)
{
    /*
    same idea as windowsshinfo, to extract FILE and determine if any input was
    provided, but there is a different manner in which unix command line
    arguments are processed.
     */


    int i, ioff = 1, j;
    const char **avv;


// https://github.com/wch/r-source/blob/031c225f8e928f7259eed5704218edc83b7c88b0/src/unix/system.c#L346
    /*
    first task is to select the GUI.
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
    R_common_command_line(&ac, av);


#ifdef debug
    Rprintf("\nafter removing common arguments:\n");
    print_char_array(ac, av);
#endif


    const char *FILE = NULL;
// https://github.com/wch/r-source/blob/trunk/src/unix/system.c#L406
    while (--ac) {
        if (**++av == '-') {
            // if (!strcmp(*av, "--no-readline")) {
            // } else
            if (!strcmp(*av, "-f")) {
                *no_input = FALSE;
                ac--;
                av++;
#define R_INIT_TREAT_F(_AV_)                                   \
                if (strcmp(_AV_, "-")) {                       \
                    if (strlen(_AV_) >= PATH_MAX) {            \
                        errorcall(R_NilValue, _("path given in -f/--file is too long"));\
                    }                                          \
                    char path[PATH_MAX], *p = path;            \
                    p = unescape_arg(p, _AV_);                 \
                    *p = '\0';                                 \
                    FILE = path;                               \
                }
                R_INIT_TREAT_F(*av);

            } else if (!strncmp(*av, "--file=", 7)) {
                *no_input = FALSE;

                R_INIT_TREAT_F((*av)+7);

            } else if (!strcmp(*av, "-e")) {
                *no_input = FALSE;
                ac--;
                av++;
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


#ifdef debug
    set_R_Visible(1);
    Rprintf("\n");
#endif


    return FILE;
}


SEXP do_shinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
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

    FILE

        character string; command line argument FILE or NA_character_

    no.input

        length-one logical vector
     */


    if (!is_maybe_in_shell) {


#define return_shinfo(FILE, no_input)                          \
        do {                                                   \
            SEXP value = allocVector(VECSXP, 2);               \
            PROTECT(value);                                    \
            SEXP names = allocVector(STRSXP, 2);               \
            setAttrib(value, R_NamesSymbol, names);            \
            SET_STRING_ELT(names, 0, mkChar("FILE"));          \
            SET_VECTOR_ELT(value, 0, (FILE));                  \
            SET_STRING_ELT(names, 1, mkChar("no.input"));      \
            SET_VECTOR_ELT(value, 1, (no_input));              \
            UNPROTECT(1);                                      \
            return value;                                      \
        } while (0)


        return_shinfo(ScalarString(NA_STRING), ScalarLogical(NA_LOGICAL));
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


    const char *FILE = NULL;
    int no_input = TRUE;


    if (ARGC <= 1) {
        UNPROTECT(1);
        return_shinfo(ScalarString(FILE ? mkChar(FILE) : NA_STRING), ScalarLogical(no_input));
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


#ifdef _WIN32
    FILE = windowsshinfo(ac, av, &no_input);
#else
    FILE = unixshinfo(ac, av, &no_input);
#endif


    return_shinfo(ScalarString(FILE ? mkChar(FILE) : NA_STRING), ScalarLogical(no_input));
}
