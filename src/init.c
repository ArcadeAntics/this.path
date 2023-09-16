#include <R_ext/Rdynload.h>    /* need definition of 'R_ExternalMethodDef' */
#include <R_ext/Visibility.h>  /* need definition of 'attribute_visible' */
#include "this.path.h"         /* need declarations of C functions */


#include <R.h>
SEXP do_testing do_formals
{
    do_start("testing", 1);
    return ScalarLogical(IS_SCALAR(CAR(args), STRSXP));
}


static const R_ExternalMethodDef externalRoutines[] = {


    {"testing", (DL_FUNC) &do_testing, 1},


    /* aquarootscript.c */


    {"aquarootscript", (DL_FUNC) &do_aquarootscript, 0},


    /* backports.c */


#if R_version_less_than(3, 1, 0)
    {"anyNA"              , (DL_FUNC) &do_anyNA              , 2},
    {"anyNAdataframe"     , (DL_FUNC) &do_anyNAdataframe     , 2},
    {"anyNAnumericversion", (DL_FUNC) &do_anyNAnumericversion, 1},
    {"anyNAdefault"       , (DL_FUNC) &do_anyNAdefault       , 2},
#endif


#if R_version_less_than(3, 2, 0)
    {"direxists"            , (DL_FUNC) &do_direxists            , 1},
    {"lengths"              , (DL_FUNC) &do_lengths              , 2},
    {"lengthsdefault"       , (DL_FUNC) &do_lengthsdefault       , 2},
    {"isRegisteredNamespace", (DL_FUNC) &do_isRegisteredNamespace, 1},
#endif


#if R_version_less_than(3, 3, 0)
    {"strrep"    , (DL_FUNC) &do_strrep    , 2},
    {"startsWith", (DL_FUNC) &do_startsWith, 2},
    {"endsWith"  , (DL_FUNC) &do_endsWith  , 2},
#endif


#if R_version_less_than(3, 5, 0)
    {"dotslength", (DL_FUNC) &do_dotslength, 0},
#endif


    /* basename2.c */


    {"windowsbasename2", (DL_FUNC) &do_windowsbasename2, 1},
    {"unixbasename2"   , (DL_FUNC) &do_unixbasename2   , 1},
    {"basename2"       , (DL_FUNC) &do_basename2       , 1},

    {"windowsdirname2", (DL_FUNC) &do_windowsdirname2, -1},
    {"unixdirname2"   , (DL_FUNC) &do_unixdirname2   , -1},
    {"dirname2"       , (DL_FUNC) &do_dirname2       , -1},


    /* ext.c */


    {"windowssplitext", (DL_FUNC) &do_windowssplitext, 2},
    {"unixsplitext"   , (DL_FUNC) &do_unixsplitext   , 2},
    {"splitext"       , (DL_FUNC) &do_splitext       , 2},

    {"windowsremoveext", (DL_FUNC) &do_windowsremoveext, 2},
    {"unixremoveext"   , (DL_FUNC) &do_unixremoveext   , 2},
    {"removeext"       , (DL_FUNC) &do_removeext       , 2},

    {"windowsext", (DL_FUNC) &do_windowsext, 2},
    {"unixext"   , (DL_FUNC) &do_unixext   , 2},
    {"ext"       , (DL_FUNC) &do_ext       , 2},

    {"windowsextgets", (DL_FUNC) &do_windowsextgets, 3},
    {"unixextgets"   , (DL_FUNC) &do_unixextgets   , 3},
    {"extgets"       , (DL_FUNC) &do_extgets       , 3},


    /* isabspath.c */


    {"windowsisabspath", (DL_FUNC) &do_windowsisabspath, 1},
    {"unixisabspath"   , (DL_FUNC) &do_unixisabspath   , 1},
    {"isabspath"       , (DL_FUNC) &do_isabspath       , 1},


    /* ns-hooks.c */


    {"mbcslocale"  , (DL_FUNC) &do_mbcslocale  , 0},
    // {"utf8locale"  , (DL_FUNC) &do_utf8locale  , 0},
    // {"latin1locale", (DL_FUNC) &do_latin1locale, 0},
    {"R_MB_CUR_MAX", (DL_FUNC) &do_R_MB_CUR_MAX, 0},

    {"onLoad"  , (DL_FUNC) &do_onLoad  , 2},
    {"onUnload", (DL_FUNC) &do_onUnload, 1},


    /* pathjoin.c */


    {"windowspathjoin", (DL_FUNC) &do_windowspathjoin, 0},
    {"unixpathjoin"   , (DL_FUNC) &do_unixpathjoin   , 0},
    {"pathjoin"       , (DL_FUNC) &do_pathjoin       , 0},


    /* pathsplit.c */


    {"windowspathsplit", (DL_FUNC) &do_windowspathsplit, 1},
    {"unixpathsplit"   , (DL_FUNC) &do_unixpathsplit   , 1},
    {"pathsplit"       , (DL_FUNC) &do_pathsplit       , 1},

    {"windowspathsplit1", (DL_FUNC) &do_windowspathsplit1, 1},
    {"unixpathsplit1"   , (DL_FUNC) &do_unixpathsplit1   , 1},
    {"pathsplit1"       , (DL_FUNC) &do_pathsplit1       , 1},

    {"windowspathunsplit", (DL_FUNC) &do_windowspathunsplit, 0},
    {"unixpathunsplit"   , (DL_FUNC) &do_unixpathunsplit   , 0},
    {"pathunsplit"       , (DL_FUNC) &do_pathunsplit       , 0},


    /* print.c */


    {"PrintValueEnv"               , (DL_FUNC) &do_PrintValueEnv               , 2},
    {"printThisPathDocumentContext", (DL_FUNC) &do_printThisPathDocumentContext, 2},


    /* progargs.c */


    {"asArgs", (DL_FUNC) &do_asArgs, -1},


    /* promises.c */


    {"isunevaluatedpromise"    , (DL_FUNC) &do_isunevaluatedpromise    , -1},
    {"promiseisunevaluated"    , (DL_FUNC) &do_promiseisunevaluated    , -1},
    {"getpromisewithoutwarning", (DL_FUNC) &do_getpromisewithoutwarning, -1},
    {"PRINFO"                  , (DL_FUNC) &do_PRINFO                  , -1},
    {"mkPROMISE"               , (DL_FUNC) &do_mkPROMISE               ,  2},
    {"mkEVPROMISE"             , (DL_FUNC) &do_mkEVPROMISE             ,  2},
    {"unlockEnvironment"       , (DL_FUNC) &do_unlockEnvironment       , -1},


    /* rprojroot.c */


    {"resetproj", (DL_FUNC) &do_resetproj, 0},


    /* shfile.c */


    {"shFILE", (DL_FUNC) &do_shFILE, 2},
    {"shINFO", (DL_FUNC) &do_shINFO, 0},


    /* thispath.c */


    {"thisPathUnrecognizedConnectionClassError", (DL_FUNC) &do_thisPathUnrecognizedConnectionClassError, 2},
    {"thisPathUnrecognizedMannerError"         , (DL_FUNC) &do_thisPathUnrecognizedMannerError         , 1},
    {"thisPathNotImplementedError"             , (DL_FUNC) &do_thisPathNotImplementedError             , 2},
    {"thisPathNotExistsError"                  , (DL_FUNC) &do_thisPathNotExistsError                  , 2},
    {"thisPathInZipFileError"                  , (DL_FUNC) &do_thisPathInZipFileError                  , 2},
    {"thisPathInAQUAError"                     , (DL_FUNC) &do_thisPathInAQUAError                     , 1},

    {"isclipboard"      , (DL_FUNC) &do_isclipboard      ,  1},
    {"inittoolsrstudio" , (DL_FUNC) &do_inittoolsrstudio , -1},
    {"syspathjupyter"   , (DL_FUNC) &do_syspathjupyter   , -1},
    {"setsyspathjupyter", (DL_FUNC) &do_setsyspathjupyter, -1},
    {"syspathrgui"      , (DL_FUNC) &do_syspathrgui      ,  7},
    {"syspath"          , (DL_FUNC) &do_syspath          , -1},
    {"getframenumber"   , (DL_FUNC) &do_getframenumber   ,  0},
    {"envpath"          , (DL_FUNC) &do_envpath          , -1},
    {"srcpath"          , (DL_FUNC) &do_srcpath          , -1},
    {"srclineno"        , (DL_FUNC) &do_srclineno        , -1},
    {"thispath"         , (DL_FUNC) &do_thispath         , -1},
    {"istrue"           , (DL_FUNC) &do_istrue           ,  1},
    {"isfalse"          , (DL_FUNC) &do_isfalse          ,  1},
    {"asInteger"        , (DL_FUNC) &do_asInteger        ,  1},
    {"asIntegerGE0"     , (DL_FUNC) &do_asIntegerGE0     ,  1},


    /* trycatch.c */


    {"lastcondition", (DL_FUNC) &do_lastcondition, -1},
    {"tryCatch2"    , (DL_FUNC) &do_tryCatch2    ,  0},
    {"tryCatch3"    , (DL_FUNC) &do_tryCatch3    ,  0},


    /* wrapsource.c */


    {"setprseen2"  , (DL_FUNC) &do_setprseen2  ,  1},
    {"wrapsource"  , (DL_FUNC) &do_wrapsource  , 20},
    {"setsyspath"  , (DL_FUNC) &do_setsyspath  , 21},
    {"unsetsyspath", (DL_FUNC) &do_unsetsyspath,  0},
    {"setenvpath"  , (DL_FUNC) &do_setenvpath  ,  2},
    {"setsrcpath"  , (DL_FUNC) &do_setsrcpath  ,  1},


    {NULL, NULL, 0}
};


attribute_visible
void R_init_this_path(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, NULL, externalRoutines);
    R_useDynamicSymbols(dll, FALSE);
#if R_version_at_least(3, 0, 0)
    R_forceSymbols(dll, TRUE);
#endif
}
