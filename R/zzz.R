if (FALSE) {


.eval.with.message <- evalq(envir = new.env(), {
    indent <- 0L
function (sym, val)
{
    writeLines(sprintf("%sevaluating '%s'", strrep(" ", indent), sym))
    oindent <- indent
    on.exit(indent <<- oindent)
    indent <<- indent + if (indent < 16L) 4L else 2L
    val
}
})


tmp <- function (..., envir = parent.frame())
{
    if (!is.environment(envir))
        envir <- as.environment(envir)
    lapply(as.character(c(...)), function(sym) {
        if (!exists(sym, envir = envir, inherits = FALSE))
            stop(gettextf("object '%s' not found", sym, domain = "R"), domain = NA)
        expr <- call("substitute", as.symbol(sym))
        expr <- eval(expr, envir)
        expr <- call(".eval.with.message", sym, expr)
        expr <- call("delayedAssign", sym, expr)
        eval(expr, envir)
    })
    invisible()
}


tmp(
    ## promises.R ----


    ".OS_unix", ".OS_windows",
    ".GUI_RStudio",
    ".OS_unix_maybe_unembedded_shell", ".OS_windows_maybe_unembedded_shell", ".maybe_unembedded_shell",
    ".shINFO",
    ".OS_unix_console_radian", ".OS_windows_console_radian", ".console_radian",
    ".GUI_vscode",
    ".GUI_jupyter",
    ".GUI_emacs",
    ".GUI_AQUA", ".GUI_Rgui", ".GUI_Tk",
    ".OS_unix_in_shell", ".OS_windows_in_shell", ".in_shell",
    ".unrecognized_manner",
    "initwd", ".ucrt", ".GUI",


    ## relpath.R  ----


    "NET.USE.command",


    ## thispath.R ----


    ".has_shFILE",
    ".r_editor", ".untitled",
    ".identical"
)


## rprojroot.R ----
envir <- environment(.find_root)
tmp("package_here_criterion", envir = envir)
rm(envir)


## thispath.R  ----
envir <- environment(.shFILE)
tmp("ofile", "file", envir = envir)
rm(envir)


rm(tmp)


}


if (getRversion() < "3.0.0") {


    fix_External2 <- function(fn) {
        recurse <- function(e) {
            if (is.function(e)) {
                if (!is.primitive(e)) {
                    at <- attributes(e)
                    body_is_bytecode <- (typeof(.Internal(bodyCode(e))) == "bytecode")
                    formals(e) <- recurse(formals(e))
                    body(e) <- recurse(body(e))
                    if (body_is_bytecode && requireNamespace("compiler", quietly = TRUE))
                        e <- compiler::cmpfun(e)
                    if (!is.null(at))
                        attributes(e) <- at
                }
                e
            }
            else if (is.pairlist(e))
                as.pairlist(lapply(e, recurse))
            else if (is.call(e)) {
                if (typeof(e[[1L]]) == "symbol" && e[[1L]] == ".External2") {
                    # e <- body(this.path:::print.ThisPathDocumentContext)
                    e[[1L]] <- as.symbol(".External")
                    e <- append(as.list(e), after = 2L, list(
                        call("quote", e),
                        as.symbol(".External"),
                        call("environment")
                    ))
                    e <- as.call(e)
                    # e[[1L]] <- as.symbol(".External")
                    e
                }
                else as.call(lapply(e, recurse))
            }
            else e
        }
        recurse(fn)
    }
    fix_External2.function <- function(fn, envir = parent.frame()) {
        symbol <- substitute(fn)
        if (typeof(symbol) != "symbol")
            stop(gettext("not a symbol", domain = "R"), domain = NA)
        if (typeof(fn) != "closure")
            stop("not a function")
        envir[[as.character(symbol)]] <- fix_External2(fn)
    }
    fix_External2.promise <- function(..., envir = parent.frame()) {
        if (!is.environment(envir))
            envir <- as.environment(envir)
        lapply(as.character(c(...)), function(sym) {
            if (!exists(sym, envir = envir, inherits = FALSE))
                stop(gettextf("object '%s' not found", sym, domain = "R"), domain = NA)
            expr <- call("substitute", as.symbol(sym))
            expr <- eval(expr, envir)
            expr <- fix_External2(expr)
            expr <- call("delayedAssign", sym, expr)
            eval(expr, envir)
        })
        invisible()
    }


    ## backports.R  ----


    fix_External2.function(.SET_PRSEEN_2)

    fix_External2.function(anyNA)
    fix_External2.function(anyNA.data.frame)
    fix_External2.function(anyNA.numeric_version)
    fix_External2.function(.anyNA.default)

    fix_External2.function(isNamespaceLoaded)
    fix_External2.function(dir.exists)
    fix_External2.function(lengths)
    fix_External2.function(.lengths.default)

    fix_External2.function(strrep)
    fix_External2.function(startsWith)
    fix_External2.function(endsWith)

    fix_External2.function(...length)


    ## basename2.R  ----


    fix_External2.function(.windows_basename2)
    fix_External2.function(.unix_basename2)
    fix_External2.function(basename2)
    fix_External2.function(.windows_dirname2)
    fix_External2.function(.unix_dirname2)
    fix_External2.function(dirname2)


    ## checkpath.R  ----


    fix_External2.function(check.path)
    fix_External2.function(check.dir)
    fix_External2.function(check.proj)


    ## ext.R        ----


    fix_External2.function(.windows_splitext)
    fix_External2.function(.unix_splitext)
    fix_External2.function(splitext)
    fix_External2.function(.windows_removeext)
    fix_External2.function(.unix_removeext)
    fix_External2.function(removeext)
    fix_External2.function(.windows_ext)
    fix_External2.function(.unix_ext)
    fix_External2.function(ext)
    fix_External2.function(`.windows_ext<-`)
    fix_External2.function(`.unix_ext<-`)
    fix_External2.function(`ext<-`)


    ## fromshell.R  ----


    fix_External2.function(from.shell)
    fix_External2.function(is.main)


    ## lineno.R     ----


    fix_External2.function(.LINENO)
    fix_External2.function(sys.LINENO)
    fix_External2.function(env.LINENO)
    fix_External2.function(src.LINENO)
    fix_External2.function(LINENO)
    fix_External2.function(LINE)


    ## makefuns.R   ----


    fix_External2.function(path.functions)


    ## ns-hooks.R   ----


    fix_External2.function(.mbcslocale)
    # fix_External2.function(.utf8locale)
    # fix_External2.function(.latin1locale)
    fix_External2.function(.R_MB_CUR_MAX)


    fix_External2.function(.onLoad)
    fix_External2.function(.onUnload)


    ## pathjoin.R   ----


    fix_External2.function(.windows.path.join)
    fix_External2.function(.unix.path.join)
    fix_External2.function(path.join)


    ## pathsplit.R  ----


    fix_External2.function(.windows.path.split)
    fix_External2.function(.unix.path.split)
    fix_External2.function(path.split)
    fix_External2.function(.windows.path.split.1)
    fix_External2.function(.unix.path.split.1)
    fix_External2.function(path.split.1)
    fix_External2.function(.windows.path.unsplit)
    fix_External2.function(.unix.path.unsplit)
    fix_External2.function(path.unsplit)


    ## print.R      ----


    fix_External2.function(.PrintValueEnv)
    fix_External2.function(.maybePrintValueEnv)
    fix_External2.function(print.ThisPathDocumentContext)


    ## progargs.R   ----


    fix_External2.function(fileArgs)
    fix_External2.function(progArgs)
    fix_External2.function(withArgs)


    ## promises.R   ----


    fix_External2.promise(".GUI_RStudio", ".OS_windows_maybe_unembedded_shell", ".shINFO", ".GUI_Rgui")
    fix_External2.function(`.init_tools:rstudio`)
    fix_External2.function(.PRINFO)
    fix_External2.function(.mkPROMISE)
    fix_External2.function(.mkEVPROMISE)
    fix_External2.function(.unlockEnvironment)


    ## relpath.R    ----


    fix_External2.function(rel2sys.dir)
    fix_External2.function(rel2sys.proj)
    fix_External2.function(rel2env.dir)
    fix_External2.function(rel2env.proj)
    fix_External2.function(rel2src.dir)
    fix_External2.function(rel2src.proj)
    fix_External2.function(rel2here)
    fix_External2.function(rel2proj)


    ## rprojroot.R  ----


    fix_External2.function(sys.proj)
    fix_External2.function(env.proj)
    fix_External2.function(src.proj)
    fix_External2.function(this.proj)
    fix_External2.function(reset.proj)


    ## startup.R    ----


    fix_External2.function(.site_file)
    fix_External2.function(.init_file)


    ## thispath.R   ----


    fix_External2.function(.shFILE)
    fix_External2.function(.is_abs_path)
    fix_External2.function(.thisPathUnrecognizedConnectionClassError)
    fix_External2.function(.thisPathUnrecognizedMannerError)
    fix_External2.function(.thisPathNotImplementedError)
    fix_External2.function(.thisPathNotExistsError)
    fix_External2.function(.thisPathInZipFileError)
    fix_External2.function(.thisPathInAQUAError)
    fix_External2.function(.is_clipboard)
    fix_External2.function(.fixNewlines)
    fix_External2.function(.jupyter_path)
    fix_External2.function(.emacs_path)
    fix_External2.function(.Rgui_path)
    fix_External2.function(.gui.path)
    fix_External2.function(set.jupyter.path)
    fix_External2.function(set.gui.path)
    fix_External2.function(.faster_subsequent_times_test)
    fix_External2.function(sys.path)
    fix_External2.function(sys.dir)
    fix_External2.function(env.path)
    fix_External2.function(env.dir)
    fix_External2.function(src.path)
    fix_External2.function(src.dir)
    fix_External2.function(this.path)
    fix_External2.function(this.dir)
    fix_External2.function(sys.srcref)
    fix_External2.function(.here)
    fix_External2.function(sys.here)
    fix_External2.function(env.here)
    fix_External2.function(src.here)
    fix_External2.function(here)
    fix_External2.function(try.sys.path)
    fix_External2.function(try.env.path)
    fix_External2.function(try.src.path)
    fix_External2.function(try.this.path)
    fix_External2.function(FILE)


    ## trycatch.R   ----


    fix_External2.function(tryCatch2)
    fix_External2.function(.last.condition)
    fix_External2.function(last.condition)
    fix_External2.function(tryCatch3)


    ## utils.R      ----


    fix_External2.function(.istrue)
    fix_External2.function(.isfalse)
    fix_External2.function(.asLogical)
    fix_External2.function(.asInteger)
    fix_External2.function(.asIntegerGE0)
    fix_External2.function(.IS_SCALAR_STR)
    fix_External2.function(.AS_SCALAR_STR)
    fix_External2.function(.scalar_streql)
    fix_External2.function(.get.dyn)
    fix_External2.function(.getframenumber)


    ## wrapsource.R ----


    fix_External2.function(wrap.source)
    fix_External2.function(set.sys.path)
    fix_External2.function(unset.sys.path)
    fix_External2.function(set.env.path)
    fix_External2.function(set.src.path)
    fix_External2.function(set.sys.path.function)


    rm(fix_External2.promise, fix_External2.function, fix_External2)
}


if (requireNamespace("compiler", quietly = TRUE)) {
    ocompilePKGS <- compiler::compilePKGS(FALSE)
    compiler::compilePKGS(ocompilePKGS)
    if (ocompilePKGS) {
        tmp <- environment(.find_root)
        tmp$package_here_criterion_if_rprojroot_is_not_available$testfun[] <- lapply(
            tmp$package_here_criterion_if_rprojroot_is_not_available$testfun,
            compiler::cmpfun
        )
        tmp$package_here_criterion_if_rprojroot_is_not_available$find_file <-
            compiler::cmpfun(tmp$package_here_criterion_if_rprojroot_is_not_available$find_file)
        tmp$package_here_criterion_if_rprojroot_is_not_available$make_fix_file <-
            compiler::cmpfun(tmp$package_here_criterion_if_rprojroot_is_not_available$make_fix_file)
        tmp$format.root_criterion <- compiler::cmpfun(tmp$format.root_criterion)
        tmp$print.root_criterion <- compiler::cmpfun(tmp$print.root_criterion)
        rm(tmp)
    }
    rm(ocompilePKGS)
}
