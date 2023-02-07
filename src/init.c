#include <R_ext/Visibility.h>
#include "this.path.h"


/*
I used to have some C functions called using .Call(.),
but I switched entirely to .External2(.) instead,
so now this array is empty.

I could remove this, but it's not causing any harm
 */
static const R_CallMethodDef callRoutines[] = {
    {NULL, NULL, 0}
};


static const R_ExternalMethodDef externalRoutines[] = {


    /* aquarootscript.c */


    {"aquarootscript", (DL_FUNC) &do_aquarootscript, 0},


    /* args.c */


    {"asargs", (DL_FUNC) &do_asargs, -1},


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


    /* hooks-for-namespace-events.c */


    {"onload"  , (DL_FUNC) &do_onload  , 2},
    {"onunload", (DL_FUNC) &do_onunload, 1},


    /* isabspath.c */


    {"windowsisabspath", (DL_FUNC) &do_windowsisabspath, 1},
    {"unixisabspath"   , (DL_FUNC) &do_unixisabspath   , 1},
    {"isabspath"       , (DL_FUNC) &do_isabspath       , 1},


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


    /* promises.c */


    {"isunevaluatedpromise"    , (DL_FUNC) &do_isunevaluatedpromise    , -1},
    {"promiseisunevaluated"    , (DL_FUNC) &do_promiseisunevaluated    , -1},
    {"getpromisewithoutwarning", (DL_FUNC) &do_getpromisewithoutwarning, -1},
    {"prinfo"                  , (DL_FUNC) &do_prinfo                  , -1},


    /* shfile.c */


    {"shfile", (DL_FUNC) &do_shfile, 2},
    {"shinfo", (DL_FUNC) &do_shinfo, 0},


    /* thispath.c */


    {"thispathunrecognizedconnectionclasserror", (DL_FUNC) &do_thispathunrecognizedconnectionclasserror, 2},
    {"thispathunrecognizedmannererror"         , (DL_FUNC) &do_thispathunrecognizedmannererror         , 1},
    {"thispathnotimplementederror"             , (DL_FUNC) &do_thispathnotimplementederror             , 2},
    {"thispathnotexistserror"                  , (DL_FUNC) &do_thispathnotexistserror                  , 2},
    {"thispathinzipfileerror"                  , (DL_FUNC) &do_thispathinzipfileerror                  , 2},
    {"thispathinaquaerror"                     , (DL_FUNC) &do_thispathinaquaerror                     , 1},

    {"isclipboard"     , (DL_FUNC) &do_isclipboard     ,  1},
    {"thispath"        , (DL_FUNC) &do_thispath        ,  5},
    {"inittoolsrstudio", (DL_FUNC) &do_inittoolsrstudio, -1},
    {"thispathrgui"    , (DL_FUNC) &do_thispathrgui    ,  5},


    /* wrapsource.c */


    {"setprseen2"   , (DL_FUNC) &do_setprseen2   ,  1},
    {"wrapsource"   , (DL_FUNC) &do_wrapsource   , 20},
    {"insidesource" , (DL_FUNC) &do_insidesource , 20},
    {"setthispath"  , (DL_FUNC) &do_setthispath  , 20},
    {"unsetthispath", (DL_FUNC) &do_unsetthispath,  0},


    {NULL, NULL, 0}
};


void attribute_visible R_init_this_path(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, callRoutines, NULL, externalRoutines);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
