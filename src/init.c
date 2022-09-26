#include <R_ext/Visibility.h>
#include "this.path.h"


static const R_CallMethodDef callRoutines[] = {
    {NULL, NULL, 0}
};


static const R_ExternalMethodDef externalRoutines[] = {


    /* basename2.c */


    {"windowsbasename2", (DL_FUNC) &do_windowsbasename2, 1},
    {"unixbasename2"   , (DL_FUNC) &do_unixbasename2   , 1},
    {"basename2"       , (DL_FUNC) &do_basename2       , 1},

    {"windowsdirname2", (DL_FUNC) &do_windowsdirname2, -1},
    {"unixdirname2"   , (DL_FUNC) &do_unixdirname2   , -1},
    {"dirname2"       , (DL_FUNC) &do_dirname2       , -1},


    /* extension.c */


    {"windowssplitext", (DL_FUNC) &do_windowssplitext, 2},
    {"unixsplitext"   , (DL_FUNC) &do_unixsplitext   , 2},
    {"splitext"       , (DL_FUNC) &do_splitext       , 2},

    {"windowsextension", (DL_FUNC) &do_windowsextension, 2},
    {"unixextension"   , (DL_FUNC) &do_unixextension   , 2},
    {"extension"       , (DL_FUNC) &do_extension       , 2},

    {"windowsremoveext", (DL_FUNC) &do_windowsremoveext, 2},
    {"unixremoveext"   , (DL_FUNC) &do_unixremoveext   , 2},
    {"removeext"       , (DL_FUNC) &do_removeext       , 2},


    /* pathjoin.c */


    {"windowspathjoin", (DL_FUNC) &do_windowspathjoin, 0},
    {"unixpathjoin"   , (DL_FUNC) &do_unixpathjoin   , 0},
    {"pathjoin"       , (DL_FUNC) &do_pathjoin       , 0},


    {NULL, NULL, 0}
};


void attribute_visible R_init_this_path(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, callRoutines, NULL, externalRoutines);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
