#include <R_ext/Rdynload.h>    /* need definition of 'R_ExternalMethodDef' */
#include <R_ext/Visibility.h>  /* need definition of 'attribute_visible' */
#include "this.path.h"         /* need declarations of C functions */


static const R_ExternalMethodDef externalRoutines[] = {


/* aquarootscript.c */


{"aquarootscript", (DL_FUNC) &do_aquarootscript, 0},


/* backports.c */


#if R_version_less_than(3, 1, 0)
{"anyNA"                , (DL_FUNC) &do_anyNA                , 2},
{"anyNA.data.frame"     , (DL_FUNC) &do_anyNA_data_frame     , 2},
{"anyNA.numeric_version", (DL_FUNC) &do_anyNA_numeric_version, 1},
{"anyNA.default"        , (DL_FUNC) &do_anyNA_default        , 2},
#endif


#if R_version_less_than(3, 2, 0)
{"dir.exists"           , (DL_FUNC) &do_dir_exists           , 1},
{"lengths"              , (DL_FUNC) &do_lengths              , 2},
{"lengths.default"      , (DL_FUNC) &do_lengths_default      , 2},
{"isRegisteredNamespace", (DL_FUNC) &do_isRegisteredNamespace, 1},
#endif


#if R_version_less_than(3, 3, 0)
{"strrep"    , (DL_FUNC) &do_strrep    , 2},
{"startsWith", (DL_FUNC) &do_startsWith, 2},
{"endsWith"  , (DL_FUNC) &do_endsWith  , 2},
#endif


#if R_version_less_than(3, 5, 0)
{"...length", (DL_FUNC) &do_dotslength, 0},
#endif


#if R_version_less_than(4, 1, 0)
{"...elt", (DL_FUNC) &do_dotselt, 1},
#endif


/* basename2.c */


{"windows_basename2", (DL_FUNC) &do_windows_basename2, 1},
{"unix_basename2"   , (DL_FUNC) &do_unix_basename2   , 1},
{"basename2"        , (DL_FUNC) &do_basename2        , 1},

{"windows_dirname2", (DL_FUNC) &do_windows_dirname2, -1},
{"unix_dirname2"   , (DL_FUNC) &do_unix_dirname2   , -1},
{"dirname2"        , (DL_FUNC) &do_dirname2        , -1},


/* ext.c */


{"windows_splitext", (DL_FUNC) &do_windows_splitext, 2},
{"unix_splitext"   , (DL_FUNC) &do_unix_splitext   , 2},
{"splitext"        , (DL_FUNC) &do_splitext        , 2},

{"windows_removeext", (DL_FUNC) &do_windows_removeext, 2},
{"unix_removeext"   , (DL_FUNC) &do_unix_removeext   , 2},
{"removeext"        , (DL_FUNC) &do_removeext        , 2},

{"windows_ext", (DL_FUNC) &do_windows_ext, 2},
{"unix_ext"   , (DL_FUNC) &do_unix_ext   , 2},
{"ext"        , (DL_FUNC) &do_ext        , 2},

{"windows_ext<-", (DL_FUNC) &do_windows_extgets, 3},
{"unix_ext<-"   , (DL_FUNC) &do_unix_extgets   , 3},
{"ext<-"        , (DL_FUNC) &do_extgets        , 3},


/* isabspath.c */


{"windows_is_abs_path", (DL_FUNC) &do_windows_is_abs_path, 1},
{"unix_is_abs_path"   , (DL_FUNC) &do_unix_is_abs_path   , 1},
{"is_abs_path"        , (DL_FUNC) &do_is_abs_path        , 1},


/* ns-hooks.c */


{"mbcslocale"  , (DL_FUNC) &do_mbcslocale  , 0},
// {"utf8locale"  , (DL_FUNC) &do_utf8locale  , 0},
// {"latin1locale", (DL_FUNC) &do_latin1locale, 0},
{"R_MB_CUR_MAX", (DL_FUNC) &do_R_MB_CUR_MAX, 0},

{"onLoad"  , (DL_FUNC) &do_onLoad  , 2},
{"onUnload", (DL_FUNC) &do_onUnload, 1},


/* pathjoin.c */


{"windows_path_join", (DL_FUNC) &do_windows_path_join, 0},
{"unix_path_join"   , (DL_FUNC) &do_unix_path_join   , 0},
{"path_join"        , (DL_FUNC) &do_path_join        , 0},


/* pathsplit.c */


{"windows_path_split", (DL_FUNC) &do_windows_path_split, 1},
{"unix_path_split"   , (DL_FUNC) &do_unix_path_split   , 1},
{"path_split"        , (DL_FUNC) &do_path_split        , 1},

{"windows_path_split_1", (DL_FUNC) &do_windows_path_split_1, 1},
{"unix_path_split_1"   , (DL_FUNC) &do_unix_path_split_1   , 1},
{"path_split_1"        , (DL_FUNC) &do_path_split_1        , 1},

{"windows_path_unsplit", (DL_FUNC) &do_windows_path_unsplit, 0},
{"unix_path_unsplit"   , (DL_FUNC) &do_unix_path_unsplit   , 0},
{"path_unsplit"        , (DL_FUNC) &do_path_unsplit        , 0},


/* print.c */


{"PrintValueEnv"                , (DL_FUNC) &do_PrintValueEnv                , 2},
{"print.ThisPathDocumentContext", (DL_FUNC) &do_print_ThisPathDocumentContext, 2},


/* progargs.c */


{"asArgs", (DL_FUNC) &do_asArgs, -1},


/* promises.c */


{"is_unevaluated_promise", (DL_FUNC) &do_is_unevaluated_promise, -1},
{"promise_is_unevaluated", (DL_FUNC) &do_promise_is_unevaluated, -1},
{"forcePromise_no_warn"  , (DL_FUNC) &do_forcePromise_no_warn  , -1},
{"PRINFO"                , (DL_FUNC) &do_PRINFO                , -1},
{"mkPROMISE"             , (DL_FUNC) &do_mkPROMISE             ,  2},
{"mkEVPROMISE"           , (DL_FUNC) &do_mkEVPROMISE           ,  2},
{"unlockEnvironment"     , (DL_FUNC) &do_unlockEnvironment     , -1},
{"is_R_MissingArg"       , (DL_FUNC) &do_is_R_MissingArg       , -1},


/* rgui_path.c */


{"CharacterMode", (DL_FUNC) &do_CharacterMode, 0},
{"RConsole"     , (DL_FUNC) &do_RConsole     , 0},


/* rprojroot.c */


{"reset_proj", (DL_FUNC) &do_reset_proj, 0},


/* shfile.c */


{"site_file", (DL_FUNC) &do_site_file, 2},
{"init_file", (DL_FUNC) &do_init_file, 2},
{"shFILE"   , (DL_FUNC) &do_shFILE   , 2},
{"shINFO"   , (DL_FUNC) &do_shINFO   , 0},


/* thispath.c */


{"ThisPathInAQUAError"                     , (DL_FUNC) &do_ThisPathInAQUAError                     , 1},
{"ThisPathInZipFileError"                  , (DL_FUNC) &do_ThisPathInZipFileError                  , 2},
{"ThisPathNotExistsError"                  , (DL_FUNC) &do_ThisPathNotExistsError                  , 2},
{"ThisPathNotFoundError"                   , (DL_FUNC) &do_ThisPathNotFoundError                   , 2},
{"ThisPathNotImplementedError"             , (DL_FUNC) &do_ThisPathNotImplementedError             , 2},
{"ThisPathUnrecognizedConnectionClassError", (DL_FUNC) &do_ThisPathUnrecognizedConnectionClassError, 2},
{"ThisPathUnrecognizedMannerError"         , (DL_FUNC) &do_ThisPathUnrecognizedMannerError         , 1},

{"is_clipboard"      , (DL_FUNC) &do_is_clipboard      ,  1},
{"jupyter_path"      , (DL_FUNC) &do_jupyter_path      , -1},
{"set_jupyter_path"  , (DL_FUNC) &do_set_jupyter_path  , -1},
{"Rgui_path"         , (DL_FUNC) &do_Rgui_path         ,  6},
{"remove_trailing_blank_string", (DL_FUNC) &do_remove_trailing_blank_string, 1},
{"fixNewlines"       , (DL_FUNC) &do_fixNewlines       ,  1},
{"set_gui_path"      , (DL_FUNC) &do_set_gui_path      ,  0},
{"sys_path"          , (DL_FUNC) &do_sys_path          , -1},
{"getframenumber"    , (DL_FUNC) &do_getframenumber    ,  0},
{"env_path"          , (DL_FUNC) &do_env_path          , -1},
{"sys.whiches"       , (DL_FUNC) &do_sys_whiches       ,  1},
{"sys.srcref"        , (DL_FUNC) &do_sys_srcref        ,  1},
{"src_path"          , (DL_FUNC) &do_src_path          , -1},
{"src_LINENO"        , (DL_FUNC) &do_src_LINENO        , -1},
{"this_path"         , (DL_FUNC) &do_this_path         , -1},


/* trycatch.c */


{"last_condition", (DL_FUNC) &do_last_condition, -1},
{"tryCatch2"     , (DL_FUNC) &do_tryCatch2     ,  0},
{"tryCatch3"     , (DL_FUNC) &do_tryCatch3     ,  0},


/* utils.c */


{"istrue"       , (DL_FUNC) &do_istrue       , 1},
{"isfalse"      , (DL_FUNC) &do_isfalse      , 1},
{"asLogical"    , (DL_FUNC) &do_asLogical    , 1},
{"asInteger"    , (DL_FUNC) &do_asInteger    , 1},
{"asIntegerGE0" , (DL_FUNC) &do_asIntegerGE0 , 1},
{"IS_SCALAR_STR", (DL_FUNC) &do_IS_SCALAR_STR, 1},
{"AS_SCALAR_STR", (DL_FUNC) &do_AS_SCALAR_STR, 1},
{"scalar_streql", (DL_FUNC) &do_scalar_streql, 2},
{"get_dyn"      , (DL_FUNC) &do_get_dyn      , 3},


/* wrapsource.c */


{"SET_PRSEEN_2"         , (DL_FUNC) &do_SET_PRSEEN_2         ,  1},
{"wrap_source"          , (DL_FUNC) &do_wrap_source          , 20},
{"set_sys_path"         , (DL_FUNC) &do_set_sys_path         , 21},
{"unset_sys_path"       , (DL_FUNC) &do_unset_sys_path       ,  0},
{"set_env_path"         , (DL_FUNC) &do_set_env_path         ,  2},
{"set_src_path"         , (DL_FUNC) &do_set_src_path         ,  1},
{"set_sys_path_function", (DL_FUNC) &do_set_sys_path_function,  1},


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
