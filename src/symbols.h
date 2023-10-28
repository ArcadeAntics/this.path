#include <Rinternals.h>
#include "rversiondefines.h"


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
    _anyNA_dispatchSymbol                  INI_as(install(".anyNA.dispatch")),
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
    documentcontextSymbol                  INI_as(install(".this.path::document.context")),
    errcndSymbol                           INI_as(install("errcnd")),
    associated_with_fileSymbol             INI_as(install("associated.with.file")),
    setsyspathwashereSymbol                INI_as(install("set.sys.path.was.here")),
    nSymbol                                INI_as(install("n")),
    _normalizePathSymbol                   INI_as(install(".normalizePath")),
    _normalizeNotDirectorySymbol           INI_as(install(".normalizeNotDirectory")),
    _normalizeFixDirectorySymbol           INI_as(install(".normalizeFixDirectory")),
    _normalizeAgainstSymbol                INI_as(install(".normalizeAgainst")),
    _isMethodsDispatchOnSymbol             INI_as(install(".isMethodsDispatchOn")),
    stopSymbol                             INI_as(install("stop")),
    delayedAssignSymbol                    INI_as(install("delayedAssign")),
    _normalizeurl_1Symbol                  INI_as(install(".normalizeurl.1")),

    sourceSymbol                           INI_as(install("source")),
    NeSymbol                               INI_as(install("Ne")),
    sys_sourceSymbol                       INI_as(install("sys.source")),

    _gui_rstudioSymbol                     INI_as(install(".gui.rstudio")),
    _tools_rstudioSymbol                   INI_as(install(".tools:rstudio")),
    _rs_api_getActiveDocumentContextSymbol INI_as(install(".rs.api.getActiveDocumentContext")),
    _rs_api_getSourceEditorContextSymbol   INI_as(install(".rs.api.getSourceEditorContext")),
    debugSourceSymbol                      INI_as(install("debugSource")),
    _debugSourceSymbol                     INI_as(install(".debugSource")),

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
    filenameSymbol                         INI_as(install("filename")),
    fileNameSymbol                         INI_as(install("fileName")),
    pathSymbol                             INI_as(install("path")),
    linesSymbol                            INI_as(install("lines")),
    inputSymbol                            INI_as(install("input")),
    missingSymbol                          INI_as(install("missing")),
    _gui_pathSymbol                        INI_as(install(".gui.path")),
    verboseSymbol                          INI_as(install("verbose")),
    originalSymbol                         INI_as(install("original")),
    for_msgSymbol                          INI_as(install("for.msg")),
    contentsSymbol                         INI_as(install("contents")),
    _custom_gui_path_functionSymbol        INI_as(install(".custom.gui.path.function")),
    guinameSymbol                          INI_as(install("guiname")),
    _jupyter_pathSymbol                    INI_as(install(".jupyter.path")),
    _site_fileSymbol                       INI_as(install(".site.file")),
    _init_fileSymbol                       INI_as(install(".init.file")),
    _shFILESymbol                          INI_as(install(".shFILE")),
    encodeStringSymbol                     INI_as(install("encodeString")),
    na_encodeSymbol                        INI_as(install("na.encode")),
    exprSymbol                             INI_as(install("expr")),
    on_exitSymbol                          INI_as(install("on.exit")),
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
    _maybe_unembedded_shellSymbol          INI_as(install(".maybe.unembedded.shell")),
    printSymbol                            INI_as(install("print")),
    print_defaultSymbol                    INI_as(install("print.default")),
    _xDataSymbol                           INI_as(install(".xData")),
    _DataSymbol                            INI_as(install(".Data")),
    _getJupyterRNotebookContentsSymbol     INI_as(install(".getJupyterRNotebookContents")),
    R_LengthSymbol                         INI_as(install("length")),
    file_infoSymbol                        INI_as(install("file.info")),
    is_naSymbol                            INI_as(install("is.na")),
    anySymbol                              INI_as(install("any")),
    removeSymbol                           INI_as(install("remove")),
    listSymbol                             INI_as(install("list")),
    envirSymbol                            INI_as(install("envir")),
    inheritsSymbol                         INI_as(install("inherits")),
    _getContentsSymbol                     INI_as(install(".getContents")),
    _getJupyterNotebookContentsSymbol      INI_as(install(".getJupyterNotebookContents")),
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
    ifnotfoundSymbol                       INI_as(install("ifnotfound"));


#undef extern0
#undef SEXP0
#undef INI_as
