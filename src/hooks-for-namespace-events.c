#include <R.h>
#include <Rinternals.h>


SEXP
    thispathofileSymbol       = NULL,
    thispathfileSymbol        = NULL,
    thispathformsgSymbol      = NULL,
    thispatherrorSymbol       = NULL,
    thispathassocwfileSymbol  = NULL,
    thispathdoneSymbol        = NULL,
    insidesourcewashereSymbol = NULL,
    _normalizePathSymbol      = NULL,
    _normalizeAgainstSymbol   = NULL,
    stopSymbol                = NULL,
    delayedAssignSymbol       = NULL,
    normalizePathSymbol       = NULL,
    winslashSymbol            = NULL,
    mustWorkSymbol            = NULL,
    normalizeURL_1Symbol      = NULL,
    sourceSymbol              = NULL,
    sys_sourceSymbol          = NULL,
    gui_rstudioSymbol         = NULL,
    debugSourceSymbol         = NULL,
    testthatSymbol            = NULL,
    source_fileSymbol         = NULL,
    testthat_uses_brioSymbol  = NULL,
    knitr_output_dirSymbol    = NULL,
    knitrSymbol               = NULL,
    knitSymbol                = NULL,
    this_pathSymbol           = NULL,
    wrap_sourceSymbol         = NULL,
    sys_callSymbol            = NULL,
    sys_frameSymbol           = NULL,
    sys_functionSymbol        = NULL,
    sys_nframeSymbol          = NULL,
    ofileSymbol               = NULL,
    owdSymbol                 = NULL,
    old_dirSymbol             = NULL,
    fileSymbol                = NULL,
    fileNameSymbol            = NULL,
    pathSymbol                = NULL,
    inputSymbol               = NULL,
    missingSymbol             = NULL,
    returnSymbol              = NULL,
    this_path_toplevelSymbol  = NULL,
    encodeStringSymbol        = NULL,
    na_encodeSymbol           = NULL,
    exprSymbol                = NULL,
    on_exitSymbol             = NULL,
    External2Symbol           = NULL,
    C_setprseen2Symbol        = NULL,
    thispathtempSymbol        = NULL,
    parent_frameSymbol        = NULL,
    invisibleSymbol           = NULL;


SEXP do_onload(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    thispathofileSymbol       = install("__this.path::ofile__");
    thispathfileSymbol        = install("__this.path::file__");
    thispathformsgSymbol      = install("__this.path::for msg__");
    thispatherrorSymbol       = install("__this.path::error__");
    thispathassocwfileSymbol  = install("__this.path::associated with file__");
    thispathdoneSymbol        = install("__this.path::done__");
    insidesourcewashereSymbol = install("__this.path::inside.source() was here__");
    _normalizePathSymbol      = install(".normalizePath");
    _normalizeAgainstSymbol   = install(".normalizeAgainst");
    stopSymbol                = install("stop");
    delayedAssignSymbol       = install("delayedAssign");
    normalizePathSymbol       = install("normalizePath");
    winslashSymbol            = install("winslash");
    mustWorkSymbol            = install("mustWork");
    normalizeURL_1Symbol      = install("normalizeURL.1");
    sourceSymbol              = install("source");
    sys_sourceSymbol          = install("sys.source");
    gui_rstudioSymbol         = install("gui.rstudio");
    debugSourceSymbol         = install("debugSource");
    testthatSymbol            = install("testthat");
    source_fileSymbol         = install("source_file");
    testthat_uses_brioSymbol  = install("testthat.uses.brio");
    knitr_output_dirSymbol    = install("knitr.output.dir");
    knitrSymbol               = install("knitr");
    knitSymbol                = install("knit");
    this_pathSymbol           = install("this.path");
    wrap_sourceSymbol         = install("wrap.source");
    sys_callSymbol            = install("sys.call");
    sys_frameSymbol           = install("sys.frame");
    sys_functionSymbol        = install("sys.function");
    sys_nframeSymbol          = install("sys.nframe");
    ofileSymbol               = install("ofile");
    owdSymbol                 = install("owd");
    old_dirSymbol             = install("old_dir");
    fileSymbol                = install("file");
    fileNameSymbol            = install("fileName");
    pathSymbol                = install("path");
    inputSymbol               = install("input");
    missingSymbol             = install("missing");
    returnSymbol              = install("return");
    this_path_toplevelSymbol  = install(".this.path.toplevel");
    encodeStringSymbol        = install("encodeString");
    na_encodeSymbol           = install("na.encode");
    exprSymbol                = install("expr");
    on_exitSymbol             = install("on.exit");
    External2Symbol           = install(".External2");
    C_setprseen2Symbol        = install("C_setprseen2");
    thispathtempSymbol        = install("__this.path::temp__");
    parent_frameSymbol        = install("parent.frame");
    invisibleSymbol           = install("invisible");
    return R_NilValue;
}
