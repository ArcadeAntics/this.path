/*
this.path : Get Executing Script's Path
Copyright (C) 2022-2026   Iris Simmons
 */


#define R_NO_REMAP
#include <R_ext/Rdynload.h>    /* need R_ExternalMethodDef */
#include <R_ext/Visibility.h>  /* need attribute_visible */
#include "devel.h"
#include "@R_PACKAGE_NAME@.h"  /* need declarations of C functions */


static const R_ExternalMethodDef externalRoutines[] = {


/* aquarootscript.c */


{"aquarootscript", (DL_FUNC) &do_aquarootscript, 0},


/* backports.c */


#if R_version_less_than(3,1,0)
{"anyNA"                , (DL_FUNC) &do_anyNA                , 2},
{"anyNA.data.frame"     , (DL_FUNC) &do_anyNA_data_frame     , 2},
{"anyNA.numeric_version", (DL_FUNC) &do_anyNA_numeric_version, 1},
{"anyNA.default"        , (DL_FUNC) &do_anyNA_default        , 2},
#endif


#if R_version_less_than(3,2,0)
{"dir.exists"           , (DL_FUNC) &do_dir_exists           , 1},
{"lengths"              , (DL_FUNC) &do_lengths              , 2},
{"lengths.default"      , (DL_FUNC) &do_lengths_default      , 2},
{"isRegisteredNamespace", (DL_FUNC) &do_isRegisteredNamespace, 1},
#endif


#if R_version_less_than(3,3,0)
{"strrep"    , (DL_FUNC) &do_strrep    , 2},
{"startsWith", (DL_FUNC) &do_startsWith, 2},
{"endsWith"  , (DL_FUNC) &do_endsWith  , 2},
#endif


#if R_version_less_than(3,5,0)
{"...length", (DL_FUNC) &do_dotslength, 0},
#endif


#if R_version_less_than(4,1,0)
{"...elt", (DL_FUNC) &do_dotselt, 1}, // R_Visible updatable
#endif


/* basename2.c */


{"basename2_windows", (DL_FUNC) &do_basename2_windows, 1},
{"basename2_unix"   , (DL_FUNC) &do_basename2_unix   , 1},
{"basename2"        , (DL_FUNC) &do_basename2        , 1},

{"dirname2_windows", (DL_FUNC) &do_dirname2_windows, -1},
{"dirname2_unix"   , (DL_FUNC) &do_dirname2_unix   , -1},
{"dirname2"        , (DL_FUNC) &do_dirname2        , -1},


/* encode_string.c */


{"encode_string", (DL_FUNC) &do_encode_string, 5},
{"URL_encode"   , (DL_FUNC) &do_URL_encode   , 3},
{"URL_decode"   , (DL_FUNC) &do_URL_decode   , 1},


/* error.c */


{"ThisPathInAQUAError"                     , (DL_FUNC) &do_ThisPathInAQUAError                     , 1},
{"ThisPathInZipFileError"                  , (DL_FUNC) &do_ThisPathInZipFileError                  , 2},
{"ThisPathNotExistsError"                  , (DL_FUNC) &do_ThisPathNotExistsError                  , 2},
{"ThisPathNotFoundError"                   , (DL_FUNC) &do_ThisPathNotFoundError                   , 2},
{"ThisPathNotImplementedError"             , (DL_FUNC) &do_ThisPathNotImplementedError             , 2},
{"ThisPathUnrecognizedConnectionClassError", (DL_FUNC) &do_ThisPathUnrecognizedConnectionClassError, 2},
{"ThisPathUnrecognizedMannerError"         , (DL_FUNC) &do_ThisPathUnrecognizedMannerError         , 1},

{"last_condition", (DL_FUNC) &do_last_condition, -1}, // R_Visible updatable
{"tryCatch2"     , (DL_FUNC) &do_tryCatch2     ,  0}, // R_Visible updatable
{"tryCatch3"     , (DL_FUNC) &do_tryCatch3     ,  0}, // R_Visible updatable


/* ext.c */


{"splitext_windows", (DL_FUNC) &do_splitext_windows, 2},
{"splitext_unix"   , (DL_FUNC) &do_splitext_unix   , 2},
{"splitext"        , (DL_FUNC) &do_splitext        , 2},

{"removeext_windows", (DL_FUNC) &do_removeext_windows, 2},
{"removeext_unix"   , (DL_FUNC) &do_removeext_unix   , 2},
{"removeext"        , (DL_FUNC) &do_removeext        , 2},

{"ext_windows", (DL_FUNC) &do_ext_windows, 2},
{"ext_unix"   , (DL_FUNC) &do_ext_unix   , 2},
{"ext"        , (DL_FUNC) &do_ext        , 2},

{"ext_windows<-", (DL_FUNC) &do_extgets_windows, 3},
{"ext_unix<-"   , (DL_FUNC) &do_extgets_unix   , 3},
{"ext<-"        , (DL_FUNC) &do_extgets        , 3},


/* files.c */


{"is_clipboard_windows", (DL_FUNC) &do_is_clipboard_windows, 1},
{"is_clipboard_unix"   , (DL_FUNC) &do_is_clipboard_unix   , 1},
{"is_clipboard"        , (DL_FUNC) &do_is_clipboard        , 1},
{"is_abs_path_windows" , (DL_FUNC) &do_is_abs_path_windows , 1},
{"is_abs_path_unix"    , (DL_FUNC) &do_is_abs_path_unix    , 1},
{"is_abs_path"         , (DL_FUNC) &do_is_abs_path         , 1},
{"fixslash"            , (DL_FUNC) &do_fixslash            , 1},
{"fixbackslash"        , (DL_FUNC) &do_fixbackslash        , 1},
{"file_URL_path"       , (DL_FUNC) &do_file_URL_path       , 1},


/* get_file_from_closure.c */


{"get_file_from_closure", (DL_FUNC) &do_get_file_from_closure, 1},


/* ns-hooks.c */


{"mbcslocale"  , (DL_FUNC) &do_mbcslocale  , 0},
// {"utf8locale"  , (DL_FUNC) &do_utf8locale  , 0},
// {"latin1locale", (DL_FUNC) &do_latin1locale, 0},
{"R_MB_CUR_MAX", (DL_FUNC) &do_R_MB_CUR_MAX, 0},

#if !defined(R_THIS_PATH_DEVEL)
{"get_ptrs", (DL_FUNC) &do_get_ptrs, 0},
#endif

{"onLoad"  , (DL_FUNC) &do_onLoad  , 2},
{"onUnload", (DL_FUNC) &do_onUnload, 1},


/* pathjoin.c */


{"path_join_windows", (DL_FUNC) &do_path_join_windows, 0},
{"path_join_unix"   , (DL_FUNC) &do_path_join_unix   , 0},
{"path_join"        , (DL_FUNC) &do_path_join        , 0},


/* pathsplit.c */


{"path_split_windows", (DL_FUNC) &do_path_split_windows, 1},
{"path_split_unix"   , (DL_FUNC) &do_path_split_unix   , 1},
{"path_split"        , (DL_FUNC) &do_path_split        , 1},

{"path_split_1_windows", (DL_FUNC) &do_path_split_1_windows, 1},
{"path_split_1_unix"   , (DL_FUNC) &do_path_split_1_unix   , 1},
{"path_split_1"        , (DL_FUNC) &do_path_split_1        , 1},

{"path_unsplit_windows", (DL_FUNC) &do_path_unsplit_windows, 0},
{"path_unsplit_unix"   , (DL_FUNC) &do_path_unsplit_unix   , 0},
{"path_unsplit"        , (DL_FUNC) &do_path_unsplit        , 0},


/* print.c */


{"PrintValueEnv"                , (DL_FUNC) &do_PrintValueEnv                , 2}, // R_Visible off
{"print.ThisPathDocumentContext", (DL_FUNC) &do_print_ThisPathDocumentContext, 2}, // R_Visible off


/* progargs.c */


{"asArgs", (DL_FUNC) &do_asArgs, -1},


/* promises.c */


{"is_unevaluated_promise", (DL_FUNC) &do_is_unevaluated_promise, -1},
{"promise_is_unevaluated", (DL_FUNC) &do_promise_is_unevaluated, -1},
{"forcePromise_no_warn"  , (DL_FUNC) &do_forcePromise_no_warn  , -1},
{"is_R_MissingArg"       , (DL_FUNC) &do_is_R_MissingArg       , -1},


/* rgui_path.c */


{"CharacterMode", (DL_FUNC) &do_CharacterMode, 0},
{"RConsole"     , (DL_FUNC) &do_RConsole     , 0},


/* rprojroot.c */


{"reset_proj", (DL_FUNC) &do_reset_proj, 0}, // R_Visible off


/* setsyspath.c */


{"wrap_source"          , (DL_FUNC) &do_wrap_source          , 20}, // R_Visible updatable
{"set_sys_path"         , (DL_FUNC) &do_set_sys_path         , 23},
{"unset_sys_path"       , (DL_FUNC) &do_unset_sys_path       ,  0}, // R_Visible off
{"set_env_path"         , (DL_FUNC) &do_set_env_path         ,  2}, // R_Visible off
{"set_src_path"         , (DL_FUNC) &do_set_src_path         ,  1}, // R_Visible off
{"set_sys_path_function", (DL_FUNC) &do_set_sys_path_function,  1}, // R_Visible off


/* shfile.c */


{"site_file", (DL_FUNC) &do_site_file, 2},
{"init_file", (DL_FUNC) &do_init_file, 2},
{"shFILE"   , (DL_FUNC) &do_shFILE   , 2},
{"shINFO"   , (DL_FUNC) &do_shINFO   , 0},


/* splitroot.c */


{"splitroot_windows" , (DL_FUNC) &do_splitroot_windows , 1},
{"splitroot_unix"    , (DL_FUNC) &do_splitroot_unix    , 1},
{"splitroot"         , (DL_FUNC) &do_splitroot         , 1},
{"splitdrive_windows", (DL_FUNC) &do_splitdrive_windows, 1},
{"splitdrive_unix"   , (DL_FUNC) &do_splitdrive_unix   , 1},
{"splitdrive"        , (DL_FUNC) &do_splitdrive        , 1},


/* startup.c */


{"with_startup_file"      , (DL_FUNC) &do_with_startup_file      , 0}, // R_Visible off
{"is_valid_init_file_expr", (DL_FUNC) &do_is_valid_init_file_expr, 0},
{"set_init_file"          , (DL_FUNC) &do_set_init_file          , 0},
{"unset_init_file"        , (DL_FUNC) &do_unset_init_file        , 0},


/* sys.c */


{"sys.srcref" , (DL_FUNC) &do_sys_srcref , 1},
{"sys.whiches", (DL_FUNC) &do_sys_whiches, 1},


/* thispath.c */


{"fixNewlines"       , (DL_FUNC) &do_fixNewlines       ,  1},
{"splitlines"        , (DL_FUNC) &do_splitlines        ,  1},
{"remove_trailing_blank_string", (DL_FUNC) &do_remove_trailing_blank_string, 1},
{"Rgui_path"         , (DL_FUNC) &do_Rgui_path         ,  6},
{"jupyter_path"      , (DL_FUNC) &do_jupyter_path      ,  4},
{"set_jupyter_path"  , (DL_FUNC) &do_set_jupyter_path  , -1}, // R_Visible off
{"set_gui_path"      , (DL_FUNC) &do_set_gui_path      ,  0}, // R_Visible off
{"custom_gui_toplevel_nframe", (DL_FUNC) &do_custom_gui_toplevel_nframe, 0},
{"sys_path"          , (DL_FUNC) &do_sys_path          , -1},
{"getframenumber"    , (DL_FUNC) &do_getframenumber    ,  0},
{"env_path"          , (DL_FUNC) &do_env_path          , -1},
{"src_path"          , (DL_FUNC) &do_src_path          , -1},
{"src_LINENO"        , (DL_FUNC) &do_src_LINENO        , -1},
{"this_path"         , (DL_FUNC) &do_this_path         , -1},


/* utils.c */


{"istrue"       , (DL_FUNC) &do_istrue       , 1},
{"isfalse"      , (DL_FUNC) &do_isfalse      , 1},
{"asLogical"    , (DL_FUNC) &do_asLogical    , 1},
{"asInteger"    , (DL_FUNC) &do_asInteger    , 1},
{"asIntegerGE0" , (DL_FUNC) &do_asIntegerGE0 , 1},
{"IS_SCALAR_STR", (DL_FUNC) &do_IS_SCALAR_STR, 1},
{"AS_SCALAR_STR", (DL_FUNC) &do_AS_SCALAR_STR, 1},
{"scalar_streql", (DL_FUNC) &do_scalar_streql, 2},
{"tolower_ASCII", (DL_FUNC) &do_tolower_ASCII, 1},
{"toupper_ASCII", (DL_FUNC) &do_toupper_ASCII, 1},
{"str_equal_useBytes", (DL_FUNC) &do_str_equal_useBytes, 2},


{NULL, NULL, 0}
};


#include "thispathdefn.h"


SEXP _ptr_PREXPR (SEXP x) { return ptr_R_PromiseExpr(x); }
SEXP _ptr_PRENV  (SEXP x) { return ptr_PRENV        (x); }
SEXP _ptr_PRVALUE(SEXP x) { return ptr_PRVALUE      (x); }


attribute_visible
void R_init_@R_PACKAGE_LIB@(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, NULL, externalRoutines);
    R_useDynamicSymbols(dll, FALSE);
#if R_version_at_least(3,0,0)
    R_forceSymbols(dll, TRUE);
#endif


    R_RegisterCCallable("@R_PACKAGE_NAME@", "ptr_PREXPR" , (DL_FUNC) _ptr_PREXPR );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "ptr_PRENV"  , (DL_FUNC) _ptr_PRENV  );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "ptr_PRVALUE", (DL_FUNC) _ptr_PRVALUE);

    R_RegisterCCallable("@R_PACKAGE_NAME@", "get_UnboundValue" , (DL_FUNC) get_UnboundValue );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "is_delayed"       , (DL_FUNC) is_delayed       );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "is_forced"        , (DL_FUNC) is_forced        );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "is_promise"       , (DL_FUNC) is_promise       );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_TYPEOF"        , (DL_FUNC) my_TYPEOF        );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_findVarInFrame", (DL_FUNC) my_findVarInFrame);
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_findVar"       , (DL_FUNC) my_findVar       );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_findValInFrame", (DL_FUNC) my_findValInFrame);
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_findVal"       , (DL_FUNC) my_findVal       );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_getVarInFrame" , (DL_FUNC) my_getVarInFrame );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_getVar"        , (DL_FUNC) my_getVar        );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "force"            , (DL_FUNC) force            );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "forceInFrame"     , (DL_FUNC) forceInFrame     );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_PREXPR"        , (DL_FUNC) my_PREXPR        );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_PRENV"         , (DL_FUNC) my_PRENV         );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_PRVALUE"       , (DL_FUNC) my_PRVALUE       );

    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_getRegisteredNamespace_c"  , (DL_FUNC) my_getRegisteredNamespace_c  );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_getRegisteredNamespace_sym", (DL_FUNC) my_getRegisteredNamespace_sym);
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_getRegisteredNamespace"    , (DL_FUNC) my_getRegisteredNamespace    );

    R_RegisterCCallable("@R_PACKAGE_NAME@", "length_DOTS"         , (DL_FUNC) length_DOTS         );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "my_PrintValueEnv"    , (DL_FUNC) my_PrintValueEnv    );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "parent_frame"        , (DL_FUNC) parent_frame        );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "R_MakeDelayedBinding", (DL_FUNC) R_MakeDelayedBinding);
    R_RegisterCCallable("@R_PACKAGE_NAME@", "R_MakeForcedBinding" , (DL_FUNC) R_MakeForcedBinding );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "R_NewEnv"            , (DL_FUNC) R_NewEnv            );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "set_R_Visible"       , (DL_FUNC) set_R_Visible_fun   );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "UNIMPLEMENTED_TYPE"  , (DL_FUNC) UNIMPLEMENTED_TYPE  );
    R_RegisterCCallable("@R_PACKAGE_NAME@", "UNIMPLEMENTED_TYPEt" , (DL_FUNC) UNIMPLEMENTED_TYPEt );
}
