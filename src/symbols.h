#include <Rinternals.h>       /* need definition of SEXP */
#include "rversiondefines.h"  /* need definition of R_version_less_than */
#include "devel.h"


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
#if R_version_less_than(3, 0, 0)
    _SET_PRSEEN_2Symbol                    INI_as(install(".SET_PRSEEN_2")),
#else
    _External2Symbol                       INI_as(install(".External2")),
    _C_SET_PRSEEN_2Symbol                  INI_as(install(".C_SET_PRSEEN_2")),
#endif
#if R_version_less_than(3, 1, 0)
    _anyNA_dispatchSymbol                  INI_as(install(".anyNA_dispatch")),
#endif
#if R_version_less_than(3, 2, 0)
    R_dot_packageName                      INI_as(install(".packageName")),
    R_DoubleColonSymbol                    INI_as(install("::")),
    R_TripleColonSymbol                    INI_as(install(":::")),
    sortedSymbol                           INI_as(install("sorted")),
    all_namesSymbol                        INI_as(install("all.names")),
    lsSymbol                               INI_as(install("ls")),
    topenvSymbol                           INI_as(install("topenv")),
    UseMethodSymbol                        INI_as(install("UseMethod")),
#endif
#if R_version_less_than(3, 4, 0)
    R_AsCharacterSymbol                    INI_as(install("as.character")),
#endif
#if R_version_less_than(4, 0, 0)
    R_EvalSymbol                           INI_as(install("eval")),
#endif
#if R_version_less_than(4, 1, 0)
    new_envSymbol                          INI_as(install("new.env")),
#endif
#if R_version_less_than(4, 2, 0)
    existsSymbol                           INI_as(install("exists")),
#endif
    documentcontextSymbol                  INI_as(install(".@R_PACKAGE_NAME@::document.context")),
    documentcontextsSymbol                 INI_as(install(".@R_PACKAGE_NAME@::document.contexts")),
    errcndSymbol                           INI_as(install("errcnd")),
    associated_with_fileSymbol             INI_as(install("associated.with.file")),
    setsyspathwashereSymbol                INI_as(install("set.sys.path.was.here")),
    nSymbol                                INI_as(install("n")),
    _normalizePathSymbol                   INI_as(install(".normalizePath")),
    _normalizePath_not_dirSymbol           INI_as(install(".normalizePath_not_dir")),
    _normalizePath_fix_dirSymbol           INI_as(install(".normalizePath_fix_dir")),
    _normalizePath_againstSymbol           INI_as(install(".normalizePath_against")),
    _normalizePath_not_dir_againstSymbol   INI_as(install(".normalizePath_not_dir_against")),
    _normalizePath_fix_dir_againstSymbol   INI_as(install(".normalizePath_fix_dir_against")),
    _normalizePath_srcfilealiasSymbol      INI_as(install(".normalizePath_srcfilealias")),
    _isMethodsDispatchOnSymbol             INI_as(install(".isMethodsDispatchOn")),
    stopSymbol                             INI_as(install("stop")),
    _normalizeURL_1Symbol                  INI_as(install(".normalizeURL_1")),

    sourceSymbol                           INI_as(install("source")),
    NeSymbol                               INI_as(install("Ne")),
    sys_sourceSymbol                       INI_as(install("sys.source")),

    _GUI_RStudioSymbol                     INI_as(install(".GUI_RStudio")),
    debugSourceSymbol                      INI_as(install("debugSource")),

    testthatSymbol                         INI_as(install("testthat")),
    source_fileSymbol                      INI_as(install("source_file")),

    knitrSymbol                            INI_as(install("knitr")),
    knitSymbol                             INI_as(install("knit")),

    wrap_sourceSymbol                      INI_as(install("wrap.source")),

    boxSymbol                              INI_as(install("box")),
    load_from_sourceSymbol                 INI_as(install("load_from_source")),
    exprsSymbol                            INI_as(install("exprs")),
    mod_nsSymbol                           INI_as(install("mod_ns")),
    infoSymbol                             INI_as(install("info")),
    source_pathSymbol                      INI_as(install("source_path")),
    info_source_pathSymbol                 INI_as(install("info$source_path")),

    utilsSymbol                            INI_as(install("utils")),
    SweaveSymbol                           INI_as(install("Sweave")),

    compilerSymbol                         INI_as(install("compiler")),
    loadcmpSymbol                          INI_as(install("loadcmp")),

    plumberSymbol                          INI_as(install("plumber")),
    sourceUTF8Symbol                       INI_as(install("sourceUTF8")),
    PlumberSymbol                          INI_as(install("Plumber")),
    public_methodsSymbol                   INI_as(install("public_methods")),
    initializeSymbol                       INI_as(install("initialize")),
    privateSymbol                          INI_as(install("private")),
    parsedSymbol                           INI_as(install("parsed")),

    shinySymbol                            INI_as(install("shiny")),
    srcSymbol                              INI_as(install("src")),

    targetsSymbol                          INI_as(install("targets")),
    tar_callr_inner_trySymbol              INI_as(install("tar_callr_inner_try")),
    tar_load_globalsSymbol                 INI_as(install("tar_load_globals")),
    tar_sourceSymbol                       INI_as(install("tar_source")),
    tar_workspaceSymbol                    INI_as(install("tar_workspace")),
    scriptSymbol                           INI_as(install("script")),
    non_r_scriptsSymbol                    INI_as(install("non_r_scripts")),
    oldSymbol                              INI_as(install("old")),

    methodsSymbol                          INI_as(install("methods")),
    showSymbol                             INI_as(install("show")),
    sys_callSymbol                         INI_as(install("sys.call")),
    sys_frameSymbol                        INI_as(install("sys.frame")),
    sys_functionSymbol                     INI_as(install("sys.function")),
    sys_nframeSymbol                       INI_as(install("sys.nframe")),
    sys_parentSymbol                       INI_as(install("sys.parent")),
    sys_parentsSymbol                      INI_as(install("sys.parents")),
    ofileSymbol                            INI_as(install("ofile")),
    owdSymbol                              INI_as(install("owd")),
    old_dirSymbol                          INI_as(install("old_dir")),
    wdSymbol                               INI_as(install("wd")),
    old_wdSymbol                           INI_as(install("old_wd")),
    fileSymbol                             INI_as(install("file")),
    original_filenameSymbol                INI_as(install("original$filename")),
    filenameSymbol                         INI_as(install("filename")),
    fileNameSymbol                         INI_as(install("fileName")),
    pathSymbol                             INI_as(install("path")),
    linesSymbol                            INI_as(install("lines")),
    inputSymbol                            INI_as(install("input")),
    missingSymbol                          INI_as(install("missing")),
    _in_site_fileSymbol                    INI_as(install(".in_site_file")),
    _site_file_pathSymbol                  INI_as(install(".site_file_path")),
    _init_file_pathSymbol                  INI_as(install(".init_file_path")),
    _gui_pathSymbol                        INI_as(install(".gui_path")),
    verboseSymbol                          INI_as(install("verbose")),
    originalSymbol                         INI_as(install("original")),
    for_msgSymbol                          INI_as(install("for.msg")),
    contentsSymbol                         INI_as(install("contents")),
    _custom_gui_path_functionSymbol        INI_as(install(".custom_gui_path.function")),
    guinameSymbol                          INI_as(install("guiname")),
    _jupyter_pathSymbol                    INI_as(install(".jupyter_path")),
    _site_fileSymbol                       INI_as(install(".site_file")),
    _init_fileSymbol                       INI_as(install(".init_file")),
    _shFILESymbol                          INI_as(install(".shFILE")),
    encodeStringSymbol                     INI_as(install("encodeString")),
    na_encodeSymbol                        INI_as(install("na.encode")),
    exprSymbol                             INI_as(install("expr")),
    on_exitSymbol                          INI_as(install("on.exit")),
    addSymbol                              INI_as(install("add")),
    parent_frameSymbol                     INI_as(install("parent.frame")),
    invisibleSymbol                        INI_as(install("invisible")),
    as_environmentSymbol                   INI_as(install("as.environment")),
    oenvirSymbol                           INI_as(install("oenvir")),
    withArgsSymbol                         INI_as(install("withArgs")),
#if !defined(R_CONNECTIONS_VERSION_1)
    summary_connectionSymbol               INI_as(install("summary.connection")),
#endif
    _asArgsSymbol                          INI_as(install(".asArgs")),
    commandArgsSymbol                      INI_as(install("commandArgs")),
    _maybe_unembedded_shellSymbol          INI_as(install(".maybe_unembedded_shell")),
    printSymbol                            INI_as(install("print")),
    print_defaultSymbol                    INI_as(install("print.default")),
    _xDataSymbol                           INI_as(install(".xData")),
    _DataSymbol                            INI_as(install(".Data")),
    _get_jupyter_R_notebook_contentsSymbol INI_as(install(".get_jupyter_R_notebook_contents")),
    R_LengthSymbol                         INI_as(install("length")),
    file_infoSymbol                        INI_as(install("file.info")),
    is_naSymbol                            INI_as(install("is.na")),
    anySymbol                              INI_as(install("any")),
    removeSymbol                           INI_as(install("remove")),
    listSymbol                             INI_as(install("list")),
    envirSymbol                            INI_as(install("envir")),
    inheritsSymbol                         INI_as(install("inherits")),
    _get_contentsSymbol                    INI_as(install(".get_contents")),
    _get_jupyter_notebook_contentsSymbol   INI_as(install(".get_jupyter_notebook_contents")),
    _projSymbol                            INI_as(install(".proj")),
    xSymbol                                INI_as(install("x")),
    objectSymbol                           INI_as(install("object")),
    moduleSymbol                           INI_as(install(".__module__.")),
    ModuleSymbol                           INI_as(install(".__MODULE__.")),
    specSymbol                             INI_as(install("spec")),
    srcrefSymbol                           INI_as(install("srcref")),
    srcfileSymbol                          INI_as(install("srcfile")),
    wholeSrcrefSymbol                      INI_as(install("wholeSrcref")),
    isFileSymbol                           INI_as(install("isFile")),
    fixedNewlinesSymbol                    INI_as(install("fixedNewlines")),
    _fixNewlinesSymbol                     INI_as(install(".fixNewlines")),

    else_Symbol                            INI_as(install("else.")),
    finallySymbol                          INI_as(install("finally")),
    funSymbol                              INI_as(install("fun")),
    _last_conditionSymbol                  INI_as(install(".last.condition")),
    cSymbol                                INI_as(install("c")),
    funsSymbol                             INI_as(install("funs")),
    tryCatchSymbol                         INI_as(install("tryCatch")),
    do_elseSymbol                          INI_as(install("do_else")),
    AssignSymbol                           INI_as(install("<-")),
    ifnotfoundSymbol                       INI_as(install("ifnotfound")),

#if R_version_less_than(3, 0, 0)
    _this_path_valueSymbol                 INI_as(install(".this_path_value")),
    _this_path_visibleSymbol               INI_as(install(".this_path_visible")),
#endif
    withVisibleSymbol                      INI_as(install("withVisible"));


#undef extern0
#undef SEXP0
#undef INI_as
