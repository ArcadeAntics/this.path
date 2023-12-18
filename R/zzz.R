rm(.pkgname)


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


    ".os.unix", ".os.windows",
    ".gui.rstudio",
    ".os.unix.maybe.unembedded.shell", ".os.windows.maybe.unembedded.shell", ".maybe.unembedded.shell",
    ".shINFO",
    ".os.unix.console.radian", ".os.windows.console.radian", ".console.radian",
    ".gui.vscode",
    ".gui.jupyter",
    ".gui.emacs",
    ".gui.aqua", ".gui.rgui", ".gui.tk",
    ".os.unix.in.shell", ".os.windows.in.shell", ".in.shell",
    ".unrecognized.manner",
    "initwd", ".ucrt", ".GUI",


    ## relpath.R  ----


    "NET.USE.command",


    ## thispath.R ----


    ".has.shFILE",
    ".r.editor", ".untitled",
    ".identical"
)


## rprojroot.R ----
envir <- environment(.find.root)
tmp("default.criterion", envir = envir)
rm(envir)


## thispath.R  ----
envir <- environment(.shFILE)
tmp("ofile", "file", envir = envir)
rm(envir)


rm(tmp)


}


if (getRversion() < "3.0.0") {


    fix.External2 <- function(fn) {
        recurse <- function(e) {
            if (is.function(e)) {
                if (!is.primitive(e)) {
                    body_is_bytecode <- (typeof(.Internal(bodyCode(e))) == "bytecode")
                    formals(e) <- recurse(formals(e))
                    body(e) <- recurse(body(e))
                    if (body_is_bytecode && requireNamespace("compiler", quietly = TRUE))
                        e <- compiler::cmpfun(e)
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
    fix.External2.fun <- function(fn, envir = parent.frame()) {
        symbol <- substitute(fn)
        if (typeof(symbol) != "symbol")
            stop(gettext("not a symbol", domain = "R"), domain = NA)
        if (typeof(fn) != "closure")
            stop("not a function")
        envir[[as.character(symbol)]] <- fix.External2(fn)
    }
    fix.External2.promise <- function(..., envir = parent.frame()) {
        if (!is.environment(envir))
            envir <- as.environment(envir)
        lapply(as.character(c(...)), function(sym) {
            if (!exists(sym, envir = envir, inherits = FALSE))
                stop(gettextf("object '%s' not found", sym, domain = "R"), domain = NA)
            expr <- call("substitute", as.symbol(sym))
            expr <- eval(expr, envir)
            expr <- fix.External2(expr)
            expr <- call("delayedAssign", sym, expr)
            eval(expr, envir)
        })
        invisible()
    }


    ## backports.R  ----


    fix.External2.fun(.SET_PRSEEN_2)

    fix.External2.fun(anyNA)
    fix.External2.fun(anyNA.data.frame)
    fix.External2.fun(anyNA.numeric_version)
    fix.External2.fun(.anyNA.default)

    fix.External2.fun(isNamespaceLoaded)
    fix.External2.fun(dir.exists)
    fix.External2.fun(lengths)
    fix.External2.fun(.lengths.default)

    fix.External2.fun(strrep)
    fix.External2.fun(startsWith)
    fix.External2.fun(endsWith)

    fix.External2.fun(...length)


    ## basename2.R  ----


    fix.External2.fun(.windows.basename2)
    fix.External2.fun(.unix.basename2)
    fix.External2.fun(basename2)
    fix.External2.fun(.windows.dirname2)
    fix.External2.fun(.unix.dirname2)
    fix.External2.fun(dirname2)


    ## checkpath.R  ----


    fix.External2.fun(check.path)
    fix.External2.fun(check.dir)
    fix.External2.fun(check.proj)


    ## ext.R        ----


    fix.External2.fun(.windows.splitext)
    fix.External2.fun(.unix.splitext)
    fix.External2.fun(splitext)
    fix.External2.fun(.windows.removeext)
    fix.External2.fun(.unix.removeext)
    fix.External2.fun(removeext)
    fix.External2.fun(.windows.ext)
    fix.External2.fun(.unix.ext)
    fix.External2.fun(ext)
    fix.External2.fun(`.windows.ext<-`)
    fix.External2.fun(`.unix.ext<-`)
    fix.External2.fun(`ext<-`)


    ## fromshell.R  ----


    fix.External2.fun(from.shell)
    fix.External2.fun(is.main)


    ## lineno.R     ----


    fix.External2.fun(.LINENO)
    fix.External2.fun(sys.LINENO)
    fix.External2.fun(env.LINENO)
    fix.External2.fun(src.LINENO)
    fix.External2.fun(LINENO)
    fix.External2.fun(LINE)


    ## makefuns.R   ----


    fix.External2.fun(path.functions)


    ## ns-hooks.R   ----


    fix.External2.fun(.mbcslocale)
    # fix.External2.fun(.utf8locale)
    # fix.External2.fun(.latin1locale)
    fix.External2.fun(.R_MB_CUR_MAX)


    fix.External2.fun(.onLoad)
    fix.External2.fun(.onUnload)


    ## pathjoin.R   ----


    fix.External2.fun(.windows.path.join)
    fix.External2.fun(.unix.path.join)
    fix.External2.fun(path.join)


    ## pathsplit.R  ----


    fix.External2.fun(.windows.path.split)
    fix.External2.fun(.unix.path.split)
    fix.External2.fun(path.split)
    fix.External2.fun(.windows.path.split.1)
    fix.External2.fun(.unix.path.split.1)
    fix.External2.fun(path.split.1)
    fix.External2.fun(.windows.path.unsplit)
    fix.External2.fun(.unix.path.unsplit)
    fix.External2.fun(path.unsplit)


    ## print.R      ----


    fix.External2.fun(.PrintValueEnv)
    fix.External2.fun(.maybePrintValueEnv)
    fix.External2.fun(print.ThisPathDocumentContext)


    ## progargs.R   ----


    fix.External2.fun(fileArgs)
    fix.External2.fun(progArgs)
    fix.External2.fun(withArgs)


    ## promises.R   ----


    fix.External2.promise(".gui.rstudio", ".os.windows.maybe.unembedded.shell", ".shINFO", ".gui.rgui")
    fix.External2.fun(`.init.tools:rstudio`)
    fix.External2.fun(.PRINFO)
    fix.External2.fun(.mkPROMISE)
    fix.External2.fun(.mkEVPROMISE)
    fix.External2.fun(.unlockEnvironment)


    ## relpath.R    ----


    fix.External2.fun(rel2sys.dir)
    fix.External2.fun(rel2sys.proj)
    fix.External2.fun(rel2env.dir)
    fix.External2.fun(rel2env.proj)
    fix.External2.fun(rel2src.dir)
    fix.External2.fun(rel2src.proj)
    fix.External2.fun(rel2here)
    fix.External2.fun(rel2proj)


    ## rprojroot.R  ----


    fix.External2.fun(sys.proj)
    fix.External2.fun(env.proj)
    fix.External2.fun(src.proj)
    fix.External2.fun(this.proj)
    fix.External2.fun(reset.proj)


    ## startup.R    ----


    fix.External2.fun(.site.file)
    fix.External2.fun(.init.file)


    ## thispath.R   ----


    fix.External2.fun(.shFILE)
    fix.External2.fun(.is.abs.path)
    fix.External2.fun(.thisPathUnrecognizedConnectionClassError)
    fix.External2.fun(.thisPathUnrecognizedMannerError)
    fix.External2.fun(.thisPathNotImplementedError)
    fix.External2.fun(.thisPathNotExistsError)
    fix.External2.fun(.thisPathInZipFileError)
    fix.External2.fun(.thisPathInAQUAError)
    fix.External2.fun(.thisPathInEmacsError)
    fix.External2.fun(.is.clipboard)
    fix.External2.fun(.fixNewlines)
    fix.External2.fun(.jupyter.path)
    fix.External2.fun(.emacs.path)
    fix.External2.fun(.rgui.path)
    fix.External2.fun(.gui.path)
    fix.External2.fun(set.jupyter.path)
    fix.External2.fun(set.gui.path)
    fix.External2.fun(.faster.subsequent.times.test)
    fix.External2.fun(sys.path)
    fix.External2.fun(sys.dir)
    fix.External2.fun(env.path)
    fix.External2.fun(env.dir)
    fix.External2.fun(src.path)
    fix.External2.fun(src.dir)
    fix.External2.fun(this.path)
    fix.External2.fun(this.dir)
    fix.External2.fun(sys.srcref)
    fix.External2.fun(.here)
    fix.External2.fun(sys.here)
    fix.External2.fun(env.here)
    fix.External2.fun(src.here)
    fix.External2.fun(here)
    fix.External2.fun(try.sys.path)
    fix.External2.fun(try.env.path)
    fix.External2.fun(try.src.path)
    fix.External2.fun(try.this.path)
    fix.External2.fun(FILE)


    ## trycatch.R   ----


    fix.External2.fun(tryCatch2)
    fix.External2.fun(.last.condition)
    fix.External2.fun(last.condition)
    fix.External2.fun(tryCatch3)


    ## utils.R      ----


    fix.External2.fun(.istrue)
    fix.External2.fun(.isfalse)
    fix.External2.fun(.asInteger)
    fix.External2.fun(.asIntegerGE0)
    fix.External2.fun(.IS_SCALAR_STR)
    fix.External2.fun(.AS_SCALAR_STR)
    fix.External2.fun(.scalar_streql)
    fix.External2.fun(.get.dyn)
    fix.External2.fun(.getframenumber)


    ## wrapsource.R ----


    fix.External2.fun(wrap.source)
    fix.External2.fun(set.sys.path)
    fix.External2.fun(unset.sys.path)
    fix.External2.fun(set.env.path)
    fix.External2.fun(set.src.path)
    fix.External2.fun(set.sys.path.function)


    rm(fix.External2.promise, fix.External2.fun, fix.External2)
}


if (requireNamespace("compiler", quietly = TRUE)) {
    ocompilePKGS <- compiler::compilePKGS(FALSE)
    compiler::compilePKGS(ocompilePKGS)
    if (ocompilePKGS) {
        tmp <- environment(.find.root)
        tmp$.default_criterion_if_rprojroot_is_not_available$testfun[] <- lapply(
            tmp$.default_criterion_if_rprojroot_is_not_available$testfun,
            compiler::cmpfun
        )
        tmp$.default_criterion_if_rprojroot_is_not_available$find_file <-
            compiler::cmpfun(tmp$.default_criterion_if_rprojroot_is_not_available$find_file)
        tmp$.default_criterion_if_rprojroot_is_not_available$make_fix_file <-
            compiler::cmpfun(tmp$.default_criterion_if_rprojroot_is_not_available$make_fix_file)
        tmp$format.root_criterion <- compiler::cmpfun(tmp$format.root_criterion)
        tmp$print.root_criterion <- compiler::cmpfun(tmp$print.root_criterion)
        rm(tmp)
    }
    rm(ocompilePKGS)
}
