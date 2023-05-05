#include <Rinternals.h>
#include "Rversiondefines.h"


#if defined(R_THIS_PATH_DEFINE_SYMBOLS)
#define extern0
#define SEXP0
#define INI_as(v) = v
#elif defined(R_THIS_PATH_INITIALIZE_SYMBOLS)
#define extern0
#define SEXP0 SEXP
#define INI_as(v) = NULL
#else
#define extern0 extern
#define SEXP0 SEXP
#define INI_as(v)
#endif


extern0 SEXP0
#if R_version_less_than(3, 4, 0)
    R_AsCharacterSymbol                    INI_as(install("as.character")),
#endif
#if R_version_less_than(3, 2, 0)
    R_DoubleColonSymbol                    INI_as(install("::")),
    R_TripleColonSymbol                    INI_as(install(":::")),
#endif
    /* formatted weird on purpose, do not modify */
    thispathofileSymbol                    INI_as(install(
        ".this.path::ofile"
    )),
    thispathfileSymbol                     INI_as(install(
        ".this.path::file"
    )),
    thispathofilejupyterSymbol             INI_as(install(
        ".this.path::ofile.jupyter"
    )),
    thispathfilejupyterSymbol              INI_as(install(
        ".this.path::file.jupyter"
    )),
    thispathformsgSymbol                   INI_as(install(".this.path::for msg")),
    thispatherrorSymbol                    INI_as(install(".this.path::error")),
    thispathassocwfileSymbol               INI_as(install(".this.path::associated with file")),
    thispathdoneSymbol                     INI_as(install(".this.path::done")),
    insidesourcewashereSymbol              INI_as(install(".this.path::inside.source() was here")),
    thispathnSymbol                        INI_as(install(".this.path::n")),
    _normalizePathSymbol                   INI_as(install(".normalizePath")),
    _normalizeNotDirectorySymbol           INI_as(install(".normalizeNotDirectory")),
    _normalizeAgainstSymbol                INI_as(install(".normalizeAgainst")),
    stopSymbol                             INI_as(install("stop")),
    delayedAssignSymbol                    INI_as(install("delayedAssign")),
    _normalizeURL_1Symbol                  INI_as(install(".normalizeURL.1")),
    sourceSymbol                           INI_as(install("source")),
    sys_sourceSymbol                       INI_as(install("sys.source")),
    _gui_rstudioSymbol                     INI_as(install(".gui.rstudio")),
    _tools_rstudioSymbol                   INI_as(install(".tools:rstudio")),
    _rs_api_getActiveDocumentContextSymbol INI_as(install(".rs.api.getActiveDocumentContext")),
    _rs_api_getSourceEditorContextSymbol   INI_as(install(".rs.api.getSourceEditorContext")),
    debugSourceSymbol                      INI_as(install("debugSource")),
    _debugSourceSymbol                     INI_as(install(".debugSource")),
    testthatSymbol                         INI_as(install("testthat")),
    source_fileSymbol                      INI_as(install("source_file")),
    _testthat_uses_brioSymbol              INI_as(install(".testthat.uses.brio")),
    _knitr_output_dirSymbol                INI_as(install(".knitr.output.dir")),
    knitrSymbol                            INI_as(install("knitr")),
    knitSymbol                             INI_as(install("knit")),
    wrap_sourceSymbol                      INI_as(install("wrap.source")),
    boxSymbol                              INI_as(install("box")),
    load_from_sourceSymbol                 INI_as(install("load_from_source")),
    infoSymbol                             INI_as(install("info")),
    source_pathSymbol                      INI_as(install("source_path")),
    info_source_pathSymbol                 INI_as(install("info$source_path")),
    compilerSymbol                         INI_as(install("compiler")),
    loadcmpSymbol                          INI_as(install("loadcmp")),
    sys_callSymbol                         INI_as(install("sys.call")),
    sys_frameSymbol                        INI_as(install("sys.frame")),
    sys_functionSymbol                     INI_as(install("sys.function")),
    sys_nframeSymbol                       INI_as(install("sys.nframe")),
    sys_parentSymbol                       INI_as(install("sys.parent")),
    sys_parentsSymbol                      INI_as(install("sys.parents")),
    ofileSymbol                            INI_as(install("ofile")),
    owdSymbol                              INI_as(install("owd")),
    old_dirSymbol                          INI_as(install("old_dir")),
    fileSymbol                             INI_as(install("file")),
    fileNameSymbol                         INI_as(install("fileName")),
    pathSymbol                             INI_as(install("path")),
    inputSymbol                            INI_as(install("input")),
    missingSymbol                          INI_as(install("missing")),
    _this_path_toplevelSymbol              INI_as(install(".this.path.toplevel")),
    encodeStringSymbol                     INI_as(install("encodeString")),
    na_encodeSymbol                        INI_as(install("na.encode")),
    exprSymbol                             INI_as(install("expr")),
    on_exitSymbol                          INI_as(install("on.exit")),
#if R_version_at_least(3, 0, 0)
    External2Symbol                        INI_as(install(".External2")),
    _C_setprseen2Symbol                    INI_as(install(".C_setprseen2")),
#else
    _setprseen2Symbol                      INI_as(install(".setprseen2")),
#endif
    thispathtempSymbol                     INI_as(install(".this.path::*tmp*")),
    parent_frameSymbol                     INI_as(install("parent.frame")),
#if defined(R_THIS_PATH_DEFINES) && R_version_at_least(3, 0, 0)
#else
    invisibleSymbol                        INI_as(install("invisible")),
#endif
    as_environmentSymbol                   INI_as(install("as.environment")),
    oenvirSymbol                           INI_as(install("oenvir")),
    withArgsSymbol                         INI_as(install("withArgs")),
#if !defined(R_CONNECTIONS_VERSION_1)
    summary_connectionSymbol               INI_as(install("summary.connection")),
#endif
    _asArgsSymbol                          INI_as(install(".asArgs")),
    commandArgsSymbol                      INI_as(install("commandArgs")),
    _maybe_unembedded_shellSymbol           INI_as(install(".maybe.unembedded.shell")),
    insidesourcefrompackageSymbol          INI_as(install("inside.source from package this.path")),
    setthispathfrompackageSymbol           INI_as(install("set.this.path from package this.path")),
    printSymbol                            INI_as(install("print")),
    _xDataSymbol                           INI_as(install(".xData")),
    _DataSymbol                            INI_as(install(".Data")),
    _validJupyterRNotebookSymbol           INI_as(install(".validJupyterRNotebook")),
    R_LengthSymbol                         INI_as(install("length")),
    file_infoSymbol                        INI_as(install("file.info")),
    is_naSymbol                            INI_as(install("is.na")),
    anySymbol                              INI_as(install("any")),
    removeSymbol                           INI_as(install("remove")),
    listSymbol                             INI_as(install("list")),
    envirSymbol                            INI_as(install("envir")),
    inheritsSymbol                         INI_as(install("inherits")),
    for_msgSymbol                          INI_as(install("for.msg")),
    _getContentsSymbol                     INI_as(install(".getContents")),
    _this_projSymbol                       INI_as(install(".this.proj")),
    xSymbol                                INI_as(install("x"));


#undef extern0
#undef SEXP0
#undef INI_as
