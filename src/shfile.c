#include "thispathdefn.h"
#include "get_file_from_closure.h"


#if defined(_WIN32)
#define Win32 1  /* this will give us access to UImode in R_ext/RStartup.h */
#include <R_ext/RStartup.h>  /* definition of UImode */
#undef Win32
extern UImode CharacterMode;
#endif





SEXP do_site_file do_formals
{
    do_start_no_call_op_rho("site_file", 2);
#define get_file_from_closure2(sym)                            \
    (get_file_from_closure(Rf_asLogical(CAR(args)), Rf_asLogical(CADR(args)), (sym)))
    return get_file_from_closure2(_site_fileSymbol);
}


SEXP do_init_file do_formals
{
    do_start_no_call_op_rho("init_file", 2);
    return get_file_from_closure2(_init_fileSymbol);
}


SEXP do_shFILE do_formals
{
    do_start_no_call_op_rho("shFILE", 2);
    return get_file_from_closure2(_shFILESymbol);
}





#if defined(_WIN32)


// https://github.com/wch/r-source/blob/trunk/src/gnuwin32/system.c#L964
static void env_command_line(int *pac, const char **argv)
{
    int ac = *pac, newac = 1; /* Remember argv[0] is process name */
    const char **av = argv;
    Rboolean hadE = FALSE;

    /* We don't want to parse -e expressions */
    while (--ac) {
        ++av;
        if (strcmp(*av, "-e") == 0) {
            hadE = TRUE;
            argv[newac++] = *av;
            continue;
        }
        if (!hadE && **av != '-' && strchr(*av, '='))
            ;
        else
            argv[newac++] = *av;
        hadE = FALSE;
    }
    *pac = newac;
}


#endif


// https://github.com/wch/r-source/blob/trunk/src/main/CommandLineArgs.c#L94
void
common_command_line(int *pac, const char **argv,
    char *enc, Rboolean *has_enc,
    Rboolean *no_site_file,
    Rboolean *no_init_file,
    Rboolean *no_echo)
{
    int ac = *pac, newac = 1;	/* argv[0] is process name */
    const char *p, **av = argv;
    Rboolean processing = TRUE;


    while (--ac) {
        if (processing && **++av == '-') {
            if (!strcmp(*av, "--version"));
            else if (!strcmp(*av, "--args")) {
                argv[newac++] = *av;
                processing = FALSE;
            }
            else if (!strcmp(*av, "--save"));
            else if (!strcmp(*av, "--no-save"));
            else if (!strcmp(*av, "--restore"));
            else if (!strcmp(*av, "--no-restore"));
            else if (!strcmp(*av, "--no-restore-data"));
            else if (!strcmp(*av, "--no-restore-history"));
            else if (!strcmp(*av, "--silent") ||
                     !strcmp(*av, "--quiet") ||
                     !strcmp(*av, "-q"));
            else if (!strcmp(*av, "--vanilla")) {
                *no_site_file = TRUE;
                *no_init_file = TRUE;
            }
            else if (!strcmp(*av, "--no-environ"));
            else if (!strcmp(*av, "--verbose"));
#if R_version_at_least(4,0,0)
            else if (!strcmp(*av, "--no-echo") ||
                     !strcmp(*av, "--slave") ||
                     !strcmp(*av, "-s"))
#else
            else if (!strcmp(*av, "--slave") ||
                     !strcmp(*av, "-s"))
#endif
            {
                *no_echo = TRUE;
            }
            else if (!strcmp(*av, "--no-site-file")) {
                *no_site_file = TRUE;
            }
            else if (!strcmp(*av, "--no-init-file")) {
                *no_init_file = TRUE;
            }
            else if (!strcmp(*av, "--debug-init"));
            else if (!strncmp(*av, "--encoding", 10)) {
                *has_enc = TRUE;
                if (strlen(*av) < 12) {
                    if (ac > 1) { ac--; av++; p = *av; } else p = NULL;
                } else p = &(*av)[11];
                if (p == NULL);
                else {
                    strncpy(enc, p, 30);
                    enc[30] = '\0';
                }
            }
#if defined(_WIN32)
            else if (!strcmp(*av, "--no-Rconsole"));
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
                     !strcmp(*av, "-v"));
            else if (!strncmp(*av, "--min-nsize", 11) ||
                     !strncmp(*av, "--min-vsize", 11))
            {
                if (strlen(*av) < 13) {
                    if (ac > 1) { ac--; av++; }
                }
            }
            else if (strncmp(*av, "--max-ppsize", 12) == 0) {
                if (strlen(*av) < 14) {
                    if (ac > 1) { ac--; av++; }
                }
            }
#if R_version_at_least(4,4,0)
            else if (strncmp(*av, "--max-connections", 17) == 0) {
                if (strlen(*av) < 19) {
                    if (ac > 1) { ac--; av++; }
                }
            }
#endif
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
        /* replace "~+~" with space */
        if (*q == '~' && *(q+1) == '+' && *(q+2) == '~') {
            q += 2;
            *p++ = ' ';
#if R_version_at_least(3,6,0)
        /* replace "~n~" with newline */
        } else if (*q == '~' && *(q+1) == 'n' && *(q+2) == '~') {
            q += 2;
            *p++ = '\n';
#endif
#if R_version_at_least(4,1,0)
        /* replace "~t~" with tab */
        } else if (*q == '~' && *(q+1) == 't' && *(q+2) == '~') {
            q += 2;
            *p++ = '\t';
#endif
        } else *p++ = *q;
    }
    return p;
}


#endif


SEXP do_shINFO do_formals
{
    /*
    do_shINFO                 package:this.path                  C Documentation

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

    has_input

        length-one logical vector; was 'FILE' or 'EXPR' provided?
     */


    do_start_no_call_op_rho("shINFO", 0);


    if (!maybe_unembedded_shell) {


#define return_shINFO(_ENC_, _NO_SITE_FILE_, _NO_INIT_FILE_, _NO_READLINE_, _NO_ECHO_, _ESS_, _FILE_, _EXPR_, _HAS_INPUT_)\
        do {                                                   \
            int len = 9;                                       \
            SEXP value = Rf_allocVector(VECSXP, len);          \
            Rf_protect(value);                                 \
            SEXP names = Rf_allocVector(STRSXP, len);          \
            Rf_setAttrib(value, R_NamesSymbol, names);         \
            int indx = -1;                                     \
            SET_STRING_ELT(names, ++indx, Rf_mkChar("ENC"));   \
            SET_VECTOR_ELT(value,   indx, (_ENC_));            \
            SET_STRING_ELT(names, ++indx, Rf_mkChar("no_site_file"));\
            SET_VECTOR_ELT(value,   indx, (_NO_SITE_FILE_));   \
            SET_STRING_ELT(names, ++indx, Rf_mkChar("no_init_file"));\
            SET_VECTOR_ELT(value,   indx, (_NO_INIT_FILE_));   \
            SET_STRING_ELT(names, ++indx, Rf_mkChar("no_readline"));\
            SET_VECTOR_ELT(value,   indx, (_NO_READLINE_));    \
            SET_STRING_ELT(names, ++indx, Rf_mkChar("no_echo"));\
            SET_VECTOR_ELT(value,   indx, (_NO_ECHO_));        \
            SET_STRING_ELT(names, ++indx, Rf_mkChar("ess"));   \
            SET_VECTOR_ELT(value,   indx, (_ESS_));            \
            SET_STRING_ELT(names, ++indx, Rf_mkChar("FILE"));  \
            SET_VECTOR_ELT(value,   indx, (_FILE_));           \
            SET_STRING_ELT(names, ++indx, Rf_mkChar("EXPR"));  \
            SET_VECTOR_ELT(value,   indx, (_EXPR_));           \
            SET_STRING_ELT(names, ++indx, Rf_mkChar("has_input"));\
            SET_VECTOR_ELT(value,   indx, (_HAS_INPUT_));      \
            Rf_unprotect(1);                                   \
            return value;                                      \
        } while (0)


        return_shINFO(
            /* ENC          */ Rf_ScalarString(NA_STRING),
            /* no_site_file */ R_LogicalNAValue,
            /* no_init_file */ R_LogicalNAValue,
            /* no_readline  */ R_LogicalNAValue,
            /* no_echo      */ R_LogicalNAValue,
            /* ess          */ R_LogicalNAValue,
            /* FILE         */ Rf_ScalarString(NA_STRING),
            /* EXPR         */ Rf_ScalarString(NA_STRING),
            /* has_input    */ R_LogicalNAValue
        );
    }


    int ARGC; SEXP ARGV;


    ARGV = Rf_eval(expr_commandArgs, R_BaseEnv);
    Rf_protect(ARGV);
    ARGC = LENGTH(ARGV);


    char enc[31] = "";
    Rboolean has_enc = FALSE;
    Rboolean no_site_file = FALSE;
    Rboolean no_init_file = FALSE;
    Rboolean no_echo = FALSE;
    const char *FILE = NULL;
    char cmdlines[10000];
    cmdlines[0] = '\0';
    Rboolean has_input = FALSE;
    Rboolean ess, no_readline;
#if defined(_WIN32)
    ess = FALSE, no_readline = NA_LOGICAL;
#else
    ess = NA_LOGICAL, no_readline = FALSE;
#endif


    if (ARGC <= 1) {
        Rf_unprotect(1);


#define default_return_shINFO                                  \
        return_shINFO(                                         \
            Rf_ScalarString(has_enc ? Rf_mkChar(enc) : NA_STRING),\
            Rf_ScalarLogical(no_site_file),                    \
            Rf_ScalarLogical(no_init_file),                    \
            Rf_ScalarLogical(no_readline),                     \
            Rf_ScalarLogical(no_echo),                         \
            Rf_ScalarLogical(ess),                             \
            Rf_ScalarString(FILE ? Rf_mkChar(FILE) : NA_STRING),\
            Rf_ScalarString(strlen(cmdlines) ? Rf_mkChar(cmdlines) : NA_STRING),\
            Rf_ScalarLogical(has_input)                        \
        )


        default_return_shINFO;
    }


    /* determine the number of leading arguments:
     * the arguments up to and including --args
     */
    SEXP argsChar = Rf_mkChar("--args");
    Rf_protect(argsChar);
    int ac = ARGC;
    for (int i = 1; i < ARGC; i++) {
        if (STRING_ELT(ARGV, i) == argsChar) {
            ac = i + 1;
            break;
        }
    }
    Rf_unprotect(1);


    /* copy the arguments from the STRSXP to a *char[] */
    const char *argv[ac];
    for (int i = 0; i < ac; i++)
        argv[i] = R_CHAR(STRING_ELT(ARGV, i));
    Rf_unprotect(1);  /* ARGV */
    const char **av = argv;


#if defined(_WIN32)


// https://github.com/wch/r-source/blob/trunk/src/gnuwin32/system.c#L1067
    Rboolean processing = TRUE;


// https://github.com/wch/r-source/blob/trunk/src/gnuwin32/system.c#L1185
    env_command_line(&ac, av);


// https://github.com/wch/r-source/blob/trunk/src/gnuwin32/system.c#L1187
    common_command_line(&ac, av, enc, &has_enc, &no_site_file, &no_init_file, &no_echo);


// https://github.com/wch/r-source/blob/trunk/src/gnuwin32/system.c#L1190
    while (--ac) {
        if (processing && **++av == '-') {
            /*
            if (!strcmp(*av, "--help") || !strcmp(*av, "-h"));
            else if (!strcmp(*av, "--cd-to-userdocs"));
            else if (!strcmp(*av, "--no-environ"));
            else */ if (!strcmp(*av, "--ess")) {
                ess = TRUE;
            }
            /*
            else if (!strcmp(*av, "--internet2"));
            else if (!strcmp(*av, "--mdi"));
            else if (!strcmp(*av, "--sdi") || !strcmp(*av, "--no-mdi")); */
#if R_version_less_than(4,2,0)
            else if (!strncmp(*av, "--max-mem-size", 14)) {
                if (strlen(*av) < 16) {
                    ac--; av++;
                }
            }
#endif
            /*
            else if (!strcmp(*av, "--debug")); */
            else if (!strcmp(*av, "--args")) {
                break;
            } else if (CharacterMode == RTerm && !strcmp(*av, "-f")) {
                has_input = TRUE;
                ac--; av++;
                if (!ac) {
                    Rf_errorcall(R_NilValue, _("option '%s' requires an argument"), "-f");
                }
                if (strcmp(*av, "-")) { /* av != "-" */
                    FILE = *av;
                }
            } else if (CharacterMode == RTerm && !strncmp(*av, "--file=", 7)) {
                has_input = TRUE;
                if (strcmp((*av)+7, "-")) {  /* av != "--file=-" */
                    FILE = (*av)+7;
                }
            }
            // else if (!strncmp(*av, "--workspace=", 12));
            else if (CharacterMode == RTerm && !strcmp(*av, "-e")) {
                has_input = TRUE;
                ac--; av++;
                if (!ac || !strlen(*av)) {
                    Rf_errorcall(R_NilValue, _("option '%s' requires a non-empty argument"), "-e");
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
#if R_version_at_least(3,5,0)
        if (!strcmp(*avv, "--args"))
            break;
#endif
        if (!strncmp(*avv, "--gui", 5) || !strncmp(*avv, "-g", 2)) {
            if (!strncmp(*avv, "--gui", 5) && strlen(*avv) >= 7);
            else {
                if (i + 1 < ac) {
                    avv++; ioff++;
                }
            }
            /* now remove it/them */
            for (j = i; j < ac - ioff; j++)
                av[j] = av[j + ioff];
            ac -= ioff;
            break;
        }
    }


// https://github.com/wch/r-source/blob/trunk/src/unix/system.c#L405
    common_command_line(&ac, av, enc, &has_enc, &no_site_file, &no_init_file, &no_echo);


    char path[PATH_MAX];


// https://github.com/wch/r-source/blob/trunk/src/unix/system.c#L406
    while (--ac) {
        if (**++av == '-') {
            if (!strcmp(*av, "--no-readline")) {
                no_readline = TRUE;
            }
            else if (!strcmp(*av, "-f")) {
                has_input = TRUE;
                ac--; av++;
                if (!ac) {
                    Rf_errorcall(R_NilValue, _("option '%s' requires an argument"), "-f");
                }
#define R_INIT_TREAT_F(_AV_)                                   \
                if (strcmp(_AV_, "-")) {                       \
                    if (strlen(_AV_) >= PATH_MAX) {            \
                        Rf_errorcall(R_NilValue, _("path given in -f/--file is too long"));\
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
                ac--; av++;
                if (!ac) {
                    Rf_errorcall(R_NilValue, _("option '%s' requires a non-empty argument"), "-e");
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
#if defined(HAVE_AQUA)
                // r27492: in 2003 launching from 'Finder OSX' passed this
                if (!strncmp(*av, "-psn", 4)) break;
#endif
            }
        }
    }


#endif


    default_return_shINFO;
}
