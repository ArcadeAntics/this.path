delayedAssign("os.unix"   , .Platform$OS.type == "unix")
delayedAssign("os.windows", .Platform$OS.type == "windows")
delayedAssign("os.macos"  , os.unix && capabilities("aqua"))


# popular GUIs for which specific methods of this.path() are implemented
delayedAssign("gui.rstudio"         , .Platform$GUI == "RStudio")
delayedAssign("in.vscode"           , "tools:vscode" %in% search())
delayedAssign("os.macos.gui.aqua"  , os.macos   && .Platform$GUI == "AQUA")
delayedAssign("os.windows.gui.rgui", os.windows && .Platform$GUI == "Rgui")


delayedAssign("os.unix.in.shell"   , os.unix    && .Platform$GUI != "Tk" && !gui.rstudio && !in.vscode && !os.macos.gui.aqua)
delayedAssign("os.windows.in.shell", os.windows &&                          !gui.rstudio && !in.vscode && !os.windows.gui.rgui)
delayedAssign("in.shell", os.unix.in.shell || os.windows.in.shell)
