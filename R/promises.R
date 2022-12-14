# expressions that we need to evaluate for our code to work,
# but which are NOT expected to change in a session,
# so we can evaluate them exactly once when needed


delayedAssign("os.unix"   , .Platform$OS.type == "unix"   )
delayedAssign("os.windows", .Platform$OS.type == "windows")


# popular GUIs for which specific methods of this.path() are implemented
delayedAssign("gui.aqua"   , os.unix    && .Platform$GUI == "AQUA"   )
delayedAssign("gui.rgui"   , os.windows && .Platform$GUI == "Rgui"   )
delayedAssign("gui.rstudio",               .Platform$GUI == "RStudio")
delayedAssign("gui.tk"     , os.unix    && .Platform$GUI == "Tk"     )
delayedAssign("gui.vscode" , "tools:vscode" %in% search()            )


delayedAssign("tools.rstudio"                   , if (gui.rstudio) as.environment("tools:rstudio"))
delayedAssign(".rs.api.getActiveDocumentContext", if (gui.rstudio) get(".rs.api.getActiveDocumentContext", tools.rstudio, inherits = FALSE))
delayedAssign(".rs.api.getSourceEditorContext"  , if (gui.rstudio) get(".rs.api.getSourceEditorContext"  , tools.rstudio, inherits = FALSE))
delayedAssign("debugSource"                     , if (gui.rstudio) get("debugSource"                     , tools.rstudio, inherits = FALSE))


delayedAssign("os.unix.in.shell"   , os.unix    && .Platform$GUI %in% c("X11"  , "unknown") &&                        "R" == basename2(commandArgs()[[1L]])  && !gui.vscode)
delayedAssign("os.windows.in.shell", os.windows && .Platform$GUI %in% c("RTerm", "unknown") && grepl("(?i)^Rterm(\\.exe)?$", basename2(commandArgs()[[1L]])) && !gui.vscode)
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










delayedAssign("os.unix"   , {
    writeLines("evaluating 'os.unix'")
    .Platform$OS.type == "unix"
})
delayedAssign("os.windows", {
    writeLines("evaluating 'os.windows'")
    .Platform$OS.type == "windows"
})


# popular GUIs for which specific methods of this.path() are implemented
delayedAssign("gui.aqua"   , {
    writeLines("evaluating 'gui.aqua'")
    os.unix    && .Platform$GUI == "AQUA"
})
delayedAssign("gui.rgui"   , {
    writeLines("evaluating 'gui.rgui'")
    os.windows && .Platform$GUI == "Rgui"
})
delayedAssign("gui.rstudio", {
    writeLines("evaluating 'gui.rstudio'")
                  .Platform$GUI == "RStudio"
})
delayedAssign("gui.tk"     , {
    writeLines("evaluating 'gui.tk'")
    os.unix    && .Platform$GUI == "Tk"
})
delayedAssign("gui.vscode" , {
    writeLines("evaluating 'gui.vscode'")
    "tools:vscode" %in% search()
})


delayedAssign("tools.rstudio", {
    writeLines("evaluating 'tools.rstudio'")
    if (gui.rstudio)
        as.environment("tools:rstudio")
})
delayedAssign(".rs.api.getActiveDocumentContext", {
    writeLines("evaluating '.rs.api.getActiveDocumentContext'")
    if (gui.rstudio)
        get(".rs.api.getActiveDocumentContext", tools.rstudio, inherits = FALSE)
})
delayedAssign(".rs.api.getSourceEditorContext", {
    writeLines("evaluating '.rs.api.getSourceEditorContext'")
    if (gui.rstudio)
        get(".rs.api.getSourceEditorContext", tools.rstudio, inherits = FALSE)
})
delayedAssign("debugSource", {
    writeLines("evaluating 'debugSource'")
    if (gui.rstudio)
        get("debugSource", tools.rstudio, inherits = FALSE)
})


delayedAssign("os.unix.in.shell"   , {
    writeLines("evaluating 'os.unix.in.shell'")
    os.unix    && .Platform$GUI %in% c("X11"  , "unknown") &&                        "R" == basename2(commandArgs()[[1L]])  && !gui.vscode
})
delayedAssign("os.windows.in.shell", {
    writeLines("evaluating 'os.windows.in.shell'")
    os.windows && .Platform$GUI %in% c("RTerm", "unknown") && grepl("(?i)^Rterm(\\.exe)?$", basename2(commandArgs()[[1L]])) && !gui.vscode
})
delayedAssign("in.shell", {
    writeLines("evaluating 'in.shell'")
    os.unix.in.shell || os.windows.in.shell
})


delayedAssign("unrecognized.manner", {
    writeLines("evaluating 'unrecognized.manner'")
    !in.shell && !gui.rstudio && !gui.vscode && !gui.rgui && !gui.aqua && !gui.tk
})


delayedAssign("initwd", {
    writeLines("evaluating 'initwd'")
    getwd()
})
delayedAssign("ucrt"  , {
    writeLines("evaluating 'ucrt'")
    identical(R.version[["crt"]], "ucrt")
})
delayedAssign("utf8"  , {
    writeLines("evaluating 'utf8'")
    identical(utils::localeToCharset()[1L], "UTF-8")
})
