delayedAssign("os.unix"   , .Platform$OS.type == "unix")
delayedAssign("os.windows", .Platform$OS.type == "windows")
delayedAssign("os.macos"  , os.unix && capabilities("aqua"))


# popular GUIs for which specific methods of this.path() are implemented
delayedAssign("gui.aqua"   , os.macos   && .Platform$GUI == "AQUA"   )
delayedAssign("gui.rgui"   , os.windows && .Platform$GUI == "Rgui"   )
delayedAssign("gui.rstudio",               .Platform$GUI == "RStudio")
delayedAssign("in.vscode"  , "tools:vscode" %in% search()            )


delayedAssign("os.unix.in.shell"   , os.unix    &&                        "R" == basename(commandArgs()[[1L]])  && .Platform$GUI != "Tk" && !gui.rstudio && !in.vscode && !gui.aqua)
delayedAssign("os.windows.in.shell", os.windows && grepl("(?i)^Rterm(\\.exe)?$", basename(commandArgs()[[1L]])) &&                          !gui.rstudio && !in.vscode && !gui.rgui)
delayedAssign("in.shell", os.unix.in.shell || os.windows.in.shell)
