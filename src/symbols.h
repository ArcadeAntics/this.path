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
        "._this.path::ofile_."
    )),
    thispathfileSymbol                     INI_as(install(
            "._this.path::file_."
    )),
    thispathformsgSymbol                   INI_as(install("._this.path::for msg_.")),
    thispatherrorSymbol                    INI_as(install("._this.path::error_.")),
    thispathassocwfileSymbol               INI_as(install("._this.path::associated with file_.")),
    thispathdoneSymbol                     INI_as(install("._this.path::done_.")),
    insidesourcewashereSymbol              INI_as(install("._this.path::inside.source() was here_.")),
    thispathnSymbol                        INI_as(install("._this.path::n_.")),
    _normalizePathSymbol                   INI_as(install(".normalizePath")),
    _normalizeAgainstSymbol                INI_as(install(".normalizeAgainst")),
    stopSymbol                             INI_as(install("stop")),
    delayedAssignSymbol                    INI_as(install("delayedAssign")),
    normalizeURL_1Symbol                   INI_as(install("normalizeURL.1")),
    sourceSymbol                           INI_as(install("source")),
    sys_sourceSymbol                       INI_as(install("sys.source")),
    gui_rstudioSymbol                      INI_as(install("gui.rstudio")),
    init_tools_rstudioSymbol               INI_as(install("init.tools:rstudio")),
    tools_rstudioSymbol                    INI_as(install("tools:rstudio")),
    _rs_api_getActiveDocumentContextSymbol INI_as(install(".rs.api.getActiveDocumentContext")),
    _rs_api_getSourceEditorContextSymbol   INI_as(install(".rs.api.getSourceEditorContext")),
    debugSourceSymbol                      INI_as(install("debugSource")),
    testthatSymbol                         INI_as(install("testthat")),
    source_fileSymbol                      INI_as(install("source_file")),
    testthat_uses_brioSymbol               INI_as(install("testthat.uses.brio")),
    knitr_output_dirSymbol                 INI_as(install("knitr.output.dir")),
    knitrSymbol                            INI_as(install("knitr")),
    knitSymbol                             INI_as(install("knit")),
    wrap_sourceSymbol                      INI_as(install("wrap.source")),
    boxSymbol                              INI_as(install("box")),
    load_from_sourceSymbol                 INI_as(install("load_from_source")),
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
    this_path_toplevelSymbol               INI_as(install(".this.path.toplevel")),
    encodeStringSymbol                     INI_as(install("encodeString")),
    na_encodeSymbol                        INI_as(install("na.encode")),
    exprSymbol                             INI_as(install("expr")),
    on_exitSymbol                          INI_as(install("on.exit")),
#if R_version_at_least(3, 0, 0)
    External2Symbol                        INI_as(install(".External2")),
    C_setprseen2Symbol                     INI_as(install("C_setprseen2")),
#else
    setprseen2Symbol                       INI_as(install("setprseen2")),
#endif
    thispathtempSymbol                     INI_as(install("._this.path::temp_.")),
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
    maybe_in_shellSymbol                   INI_as(install("maybe.in.shell"));


#undef extern0
#undef SEXP0
#undef INI_as
