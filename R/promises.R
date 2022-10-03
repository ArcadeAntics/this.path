# expressions that we need to evaluate for our code to work,
# but which are NOT expected to change in a session,
# so we can evaluate them exactly once when needed


delayedAssign("os.unix"   , .Platform$OS.type == "unix")
delayedAssign("os.windows", .Platform$OS.type == "windows")
delayedAssign("os.macos"  , os.unix && capabilities("aqua"))


# popular GUIs for which specific methods of this.path() are implemented
delayedAssign("gui.aqua"   , os.macos   && .Platform$GUI == "AQUA"   )
delayedAssign("gui.rgui"   , os.windows && .Platform$GUI == "Rgui"   )
delayedAssign("gui.rstudio",               .Platform$GUI == "RStudio")
delayedAssign("gui.tk"     , os.unix    && .Platform$GUI == "Tk"     )
delayedAssign("in.vscode"  , "tools:vscode" %in% search()            )


delayedAssign("os.unix.in.shell"   , os.unix    && (.Platform$GUI == "X11"   || .Platform$GUI == "unknown" &&                        "R" == basename2(commandArgs()[[1L]]) ) && !in.vscode)
delayedAssign("os.windows.in.shell", os.windows && (.Platform$GUI == "RTerm" || .Platform$GUI == "unknown" && grepl("(?i)^Rterm(\\.exe)?$", basename2(commandArgs()[[1L]]))) && !in.vscode)
delayedAssign("in.shell", os.unix.in.shell || os.windows.in.shell)


delayedAssign("ucrt", identical(R.version[["crt"]], "ucrt"))
delayedAssign("utf8", identical(utils::localeToCharset()[1L], "UTF-8"))
