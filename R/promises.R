# expressions that we need to evaluate for our code to work,
# but which are NOT expected to change in a session,
# so we can evaluate them exactly once when needed


delayedAssign("shINFO", .External2(C_shinfo))


delayedAssign("os.unix"   , .Platform$OS.type == "unix"   )
delayedAssign("os.windows", .Platform$OS.type == "windows")


`.init.tools:rstudio` <- quote({
    if (`is.tools:rstudio.loaded`())
        TRUE
    else {
        if ("tools:rstudio" %in% search()) {


            env <- getNamespace("this.path")


            symbols <- c(
                "tools:rstudio",
                ".rs.api.getActiveDocumentContext",
                ".rs.api.getSourceEditorContext",
                "debugSource"
            )
            if (!all(vapply(symbols, exists, envir = env, inherits = FALSE, FUN.VALUE = NA)))
                stop("internal error; should never happen, please report!")


            val <- as.environment(symbols[[1L]])
            values <- c(list(val), lapply(symbols[-1L], get, envir = val, mode = "function", inherits = FALSE))


            for (sym in symbols) {
                if (bindingIsLocked(sym, env)) {
                    on.exit(lockBinding(sym, env), add = TRUE)
                    (unlockBinding)(sym, env)
                }
            }


            for (indx in seq_along(symbols)) {
                assign(symbols[[indx]], values[[indx]], envir = env, inherits = FALSE)
            }


            TRUE
        }
        else FALSE
    }
})


# popular GUIs for which specific methods of this.path() are implemented
delayedAssign("gui.aqua"   , os.unix    && .Platform$GUI == "AQUA"    && !gui.rstudio && !gui.vscode)
delayedAssign("gui.rgui"   , os.windows && .Platform$GUI == "Rgui"    && !gui.rstudio && !gui.vscode)
delayedAssign("gui.tk"     , os.unix    && .Platform$GUI == "Tk"      && !gui.rstudio && !gui.vscode)


eval(call("delayedAssign", "gui.rstudio", bquote({
    if (.Platform$OS.type == "RStudio") {
        local(.(`.init.tools:rstudio`))
        TRUE
    }
    else isTRUE(Sys.getpid() == as.integer(Sys.getenv("RSTUDIO_SESSION_PID")))
})))


delayedAssign("gui.vscode" , interactive() && isTRUE(Sys.getenv("TERM_PROGRAM") == "vscode") && isTRUE(shINFO[["no.input"]]))


R_EmptyEnv <- emptyenv()
`tools:rstudio` <- R_EmptyEnv
.rs.api.getActiveDocumentContext <- function (...)
{
    if (gui.rstudio)
        stop("RStudio has not finished loading")
    else stop("RStudio is not running")
}
.rs.api.getSourceEditorContext <- .rs.api.getActiveDocumentContext
debugSource <- .rs.api.getActiveDocumentContext


`is.tools:rstudio.loaded` <- function ()
!identical(`tools:rstudio`, R_EmptyEnv)


`init.tools:rstudio` <- function() NULL
body(`init.tools:rstudio`) <- bquote({
    if (gui.rstudio)
        .(`.init.tools:rstudio`)
    else FALSE
})
rm(`.init.tools:rstudio`)


delayedAssign("maybe.os.unix.in.shell"   , os.unix    && .Platform$GUI %in% c("X11"  , "unknown") &&                        "R" == basename2(commandArgs()[[1L]]) )
delayedAssign("maybe.os.windows.in.shell", os.windows && .Platform$GUI %in% c("RTerm", "unknown") && grepl("(?i)^Rterm(\\.exe)?$", basename2(commandArgs()[[1L]])))
delayedAssign("maybe.in.shell", maybe.os.unix.in.shell || maybe.os.windows.in.shell)


delayedAssign("os.unix.in.shell"   , maybe.os.unix.in.shell    && !gui.vscode)
delayedAssign("os.windows.in.shell", maybe.os.windows.in.shell && !gui.vscode)
delayedAssign("in.shell", os.unix.in.shell || os.windows.in.shell)


delayedAssign("unrecognized.manner", !in.shell && !gui.rstudio && !gui.vscode && !gui.rgui && !gui.aqua && !gui.tk)


delayedAssign("initwd", getwd())
delayedAssign("ucrt"  , identical(R.version[["crt"]], "ucrt"))
delayedAssign("utf8"  , identical(utils::localeToCharset()[1L], "UTF-8"))


getinitwd <- function ()
initwd


PRINFO <- function (x, pos = -1L, envir = as.environment(pos), inherits = TRUE,
    evaluated = FALSE)
.External2(C_prinfo, if (evaluated) x else substitute(x), envir, inherits)
