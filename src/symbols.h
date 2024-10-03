#define R_NO_REMAP
#include <Rinternals.h>       /* need definition of SEXP */
#include "devel.h"
#include "rversiondefines.h"  /* need definition of R_version_less_than */


#if defined(R_THIS_PATH_DEFINE_SYMBOLS)
#undef      R_THIS_PATH_DEFINE_SYMBOLS
#ifndef R_THIS_PATH_ALREADY_DEFINED_SYMBOLS
#define R_THIS_PATH_ALREADY_DEFINED_SYMBOLS
#else
#error "symbols must not be defined multiple times"
#endif
#define extern0
#define SEXP0
#define INI_as(v) = v
#elif defined(R_THIS_PATH_INITIALIZE_SYMBOLS)
#undef        R_THIS_PATH_INITIALIZE_SYMBOLS
#ifndef R_THIS_PATH_ALREADY_INITIALIZED_SYMBOLS
#define R_THIS_PATH_ALREADY_INITIALIZED_SYMBOLS
#else
#error "symbols must not be initialized multiple times"
#endif
#define extern0
#define SEXP0 SEXP
#define INI_as(v) = NULL
#else
#define extern0 extern
#define SEXP0 SEXP
#define INI_as(v)
#endif


extern0 SEXP0
#if R_version_less_than(3,0,0)
    _this_path_valueSymbol                 INI_as(Rf_install(".this_path_value")),
    _this_path_visibleSymbol               INI_as(Rf_install(".this_path_visible")),
#endif
#if R_version_less_than(3,1,0)
    _anyNA_dispatchSymbol                  INI_as(Rf_install(".anyNA_dispatch")),
#endif
#if R_version_less_than(3,2,0)
    R_dot_packageName                      INI_as(Rf_install(".packageName")),
    R_DoubleColonSymbol                    INI_as(Rf_install("::")),
    R_TripleColonSymbol                    INI_as(Rf_install(":::")),
    sortedSymbol                           INI_as(Rf_install("sorted")),
    all_namesSymbol                        INI_as(Rf_install("all.names")),
    lsSymbol                               INI_as(Rf_install("ls")),
    topenvSymbol                           INI_as(Rf_install("topenv")),
    UseMethodSymbol                        INI_as(Rf_install("UseMethod")),
#endif
#if R_version_less_than(3,4,0)
    R_AsCharacterSymbol                    INI_as(Rf_install("as.character")),
#endif
#if R_version_less_than(4,0,0)
    R_EvalSymbol                           INI_as(Rf_install("eval")),
#endif
#if R_version_less_than(4,1,0)
    new_envSymbol                          INI_as(Rf_install("new.env")),
#endif
#if R_version_less_than(4,2,0)
    existsSymbol                           INI_as(Rf_install("exists")),
#endif
    documentcontextSymbol                  INI_as(Rf_install(".@R_PACKAGE_NAME@::document.context")),
    documentcontextsSymbol                 INI_as(Rf_install(".@R_PACKAGE_NAME@::document.contexts")),
    errcndSymbol                           INI_as(Rf_install("errcnd")),
    associated_with_fileSymbol             INI_as(Rf_install("associated.with.file")),
    setsyspathwashereSymbol                INI_as(Rf_install("set.sys.path.was.here")),
    nSymbol                                INI_as(Rf_install("n")),
    _normalizePathSymbol                   INI_as(Rf_install(".normalizePath")),
    _normalizePath_not_dirSymbol           INI_as(Rf_install(".normalizePath_not_dir")),
    _normalizePath_fix_dirSymbol           INI_as(Rf_install(".normalizePath_fix_dir")),
    _normalizePath_againstSymbol           INI_as(Rf_install(".normalizePath_against")),
    _normalizePath_not_dir_againstSymbol   INI_as(Rf_install(".normalizePath_not_dir_against")),
    _normalizePath_fix_dir_againstSymbol   INI_as(Rf_install(".normalizePath_fix_dir_against")),
    _normalizePath_srcfilealiasSymbol      INI_as(Rf_install(".normalizePath_srcfilealias")),
    _isMethodsDispatchOnSymbol             INI_as(Rf_install(".isMethodsDispatchOn")),
    stopSymbol                             INI_as(Rf_install("stop")),
    _normalizeURL_1Symbol                  INI_as(Rf_install(".normalizeURL_1")),

    sourceSymbol                           INI_as(Rf_install("source")),
    NeSymbol                               INI_as(Rf_install("Ne")),
    sys_sourceSymbol                       INI_as(Rf_install("sys.source")),

    wrap_sourceSymbol                      INI_as(Rf_install("wrap.source")),

    _GUI_RStudioSymbol                     INI_as(Rf_install(".GUI_RStudio")),
    debugSourceSymbol                      INI_as(Rf_install("debugSource")),
    _rs_sourceWithProgressSymbol           INI_as(Rf_install(".rs.sourceWithProgress")),
    statementsSymbol                       INI_as(Rf_install("statements")),

    compilerSymbol                         INI_as(Rf_install("compiler")),
    loadcmpSymbol                          INI_as(Rf_install("loadcmp")),

    utilsSymbol                            INI_as(Rf_install("utils")),
    SweaveSymbol                           INI_as(Rf_install("Sweave")),
    srcFilenamesSymbol                     INI_as(Rf_install("srcFilenames")),

    boxSymbol                              INI_as(Rf_install("box")),
    load_from_sourceSymbol                 INI_as(Rf_install("load_from_source")),
    exprsSymbol                            INI_as(Rf_install("exprs")),
    mod_nsSymbol                           INI_as(Rf_install("mod_ns")),
    infoSymbol                             INI_as(Rf_install("info")),
    source_pathSymbol                      INI_as(Rf_install("source_path")),
    info_source_pathSymbol                 INI_as(Rf_install("info$source_path")),

    knitrSymbol                            INI_as(Rf_install("knitr")),
    knitSymbol                             INI_as(Rf_install("knit")),

    plumberSymbol                          INI_as(Rf_install("plumber")),
    sourceUTF8Symbol                       INI_as(Rf_install("sourceUTF8")),
    PlumberSymbol                          INI_as(Rf_install("Plumber")),
    public_methodsSymbol                   INI_as(Rf_install("public_methods")),
    initializeSymbol                       INI_as(Rf_install("initialize")),
    privateSymbol                          INI_as(Rf_install("private")),
    parsedSymbol                           INI_as(Rf_install("parsed")),

    shinySymbol                            INI_as(Rf_install("shiny")),
    srcSymbol                              INI_as(Rf_install("src")),

    targetsSymbol                          INI_as(Rf_install("targets")),
    tar_callr_inner_trySymbol              INI_as(Rf_install("tar_callr_inner_try")),
    tar_load_globalsSymbol                 INI_as(Rf_install("tar_load_globals")),
    tar_sourceSymbol                       INI_as(Rf_install("tar_source")),
    tar_workspaceSymbol                    INI_as(Rf_install("tar_workspace")),
    scriptSymbol                           INI_as(Rf_install("script")),
    non_r_scriptsSymbol                    INI_as(Rf_install("non_r_scripts")),
    oldSymbol                              INI_as(Rf_install("old")),

    testthatSymbol                         INI_as(Rf_install("testthat")),
    source_fileSymbol                      INI_as(Rf_install("source_file")),

    methodsSymbol                          INI_as(Rf_install("methods")),
    showSymbol                             INI_as(Rf_install("show")),
    sys_callSymbol                         INI_as(Rf_install("sys.call")),
    sys_frameSymbol                        INI_as(Rf_install("sys.frame")),
    sys_functionSymbol                     INI_as(Rf_install("sys.function")),
    sys_nframeSymbol                       INI_as(Rf_install("sys.nframe")),
    sys_parentSymbol                       INI_as(Rf_install("sys.parent")),
    sys_parentsSymbol                      INI_as(Rf_install("sys.parents")),
    ofileSymbol                            INI_as(Rf_install("ofile")),
    owdSymbol                              INI_as(Rf_install("owd")),
    old_dirSymbol                          INI_as(Rf_install("old_dir")),
    wdSymbol                               INI_as(Rf_install("wd")),
    old_wdSymbol                           INI_as(Rf_install("old_wd")),
    fileSymbol                             INI_as(Rf_install("file")),
    original_filenameSymbol                INI_as(Rf_install("original$filename")),
    filenameSymbol                         INI_as(Rf_install("filename")),
    fileNameSymbol                         INI_as(Rf_install("fileName")),
    pathSymbol                             INI_as(Rf_install("path")),
    linesSymbol                            INI_as(Rf_install("lines")),
    inputSymbol                            INI_as(Rf_install("input")),
    missingSymbol                          INI_as(Rf_install("missing")),
    _in_site_fileSymbol                    INI_as(Rf_install(".in_site_file")),
    _site_file_pathSymbol                  INI_as(Rf_install(".site_file_path")),
    _init_file_pathSymbol                  INI_as(Rf_install(".init_file_path")),
    _gui_pathSymbol                        INI_as(Rf_install(".gui_path")),
    verboseSymbol                          INI_as(Rf_install("verbose")),
    originalSymbol                         INI_as(Rf_install("original")),
    for_msgSymbol                          INI_as(Rf_install("for.msg")),
    contentsSymbol                         INI_as(Rf_install("contents")),
    _custom_gui_path_functionSymbol        INI_as(Rf_install(".custom_gui_path.function")),
    guinameSymbol                          INI_as(Rf_install("guiname")),
    _jupyter_pathSymbol                    INI_as(Rf_install(".jupyter_path")),
    _site_fileSymbol                       INI_as(Rf_install(".site_file")),
    _init_fileSymbol                       INI_as(Rf_install(".init_file")),
    _shFILESymbol                          INI_as(Rf_install(".shFILE")),
    encodeStringSymbol                     INI_as(Rf_install("encodeString")),
    na_encodeSymbol                        INI_as(Rf_install("na.encode")),
    exprSymbol                             INI_as(Rf_install("expr")),
    on_exitSymbol                          INI_as(Rf_install("on.exit")),
    addSymbol                              INI_as(Rf_install("add")),
    parent_frameSymbol                     INI_as(Rf_install("parent.frame")),
    invisibleSymbol                        INI_as(Rf_install("invisible")),
    as_environmentSymbol                   INI_as(Rf_install("as.environment")),
    oenvirSymbol                           INI_as(Rf_install("oenvir")),
    withArgsSymbol                         INI_as(Rf_install("withArgs")),
    summary_connectionSymbol               INI_as(Rf_install("summary.connection")),
    _asArgsSymbol                          INI_as(Rf_install(".asArgs")),
    commandArgsSymbol                      INI_as(Rf_install("commandArgs")),
    _maybe_unembedded_shellSymbol          INI_as(Rf_install(".maybe_unembedded_shell")),
    printSymbol                            INI_as(Rf_install("print")),
    print_defaultSymbol                    INI_as(Rf_install("print.default")),
    _xDataSymbol                           INI_as(Rf_install(".xData")),
    _DataSymbol                            INI_as(Rf_install(".Data")),
    _get_jupyter_R_notebook_contentsSymbol INI_as(Rf_install(".get_jupyter_R_notebook_contents")),
    R_LengthSymbol                         INI_as(Rf_install("length")),
    file_infoSymbol                        INI_as(Rf_install("file.info")),
    is_naSymbol                            INI_as(Rf_install("is.na")),
    anySymbol                              INI_as(Rf_install("any")),
    removeSymbol                           INI_as(Rf_install("remove")),
    listSymbol                             INI_as(Rf_install("list")),
    envirSymbol                            INI_as(Rf_install("envir")),
    inheritsSymbol                         INI_as(Rf_install("inherits")),
    _get_contentsSymbol                    INI_as(Rf_install(".get_contents")),
    _get_jupyter_notebook_contentsSymbol   INI_as(Rf_install(".get_jupyter_notebook_contents")),
    _projSymbol                            INI_as(Rf_install(".proj")),
    xSymbol                                INI_as(Rf_install("x")),
    objectSymbol                           INI_as(Rf_install("object")),
    moduleSymbol                           INI_as(Rf_install(".__module__.")),
    ModuleSymbol                           INI_as(Rf_install(".__MODULE__.")),
    specSymbol                             INI_as(Rf_install("spec")),
    srcrefSymbol                           INI_as(Rf_install("srcref")),
    srcfileSymbol                          INI_as(Rf_install("srcfile")),
    wholeSrcrefSymbol                      INI_as(Rf_install("wholeSrcref")),
    isFileSymbol                           INI_as(Rf_install("isFile")),
    fixedNewlinesSymbol                    INI_as(Rf_install("fixedNewlines")),
    _fixNewlinesSymbol                     INI_as(Rf_install(".fixNewlines")),

    topLevelEnvironmentSymbol              INI_as(Rf_install("topLevelEnvironment")),

    else_Symbol                            INI_as(Rf_install("else.")),
    finallySymbol                          INI_as(Rf_install("finally")),
    funSymbol                              INI_as(Rf_install("fun")),
    _last_conditionSymbol                  INI_as(Rf_install(".last.condition")),
    cSymbol                                INI_as(Rf_install("c")),
    funsSymbol                             INI_as(Rf_install("funs")),
    tryCatchSymbol                         INI_as(Rf_install("tryCatch")),
    do_elseSymbol                          INI_as(Rf_install("do_else")),
    AssignSymbol                           INI_as(Rf_install("<-")),
    ifnotfoundSymbol                       INI_as(Rf_install("ifnotfound")),

    _utf8localeSymbol                      INI_as(Rf_install(".utf8locale")),

    withVisibleSymbol                      INI_as(Rf_install("withVisible"));


#undef extern0
#undef SEXP0
#undef INI_as
