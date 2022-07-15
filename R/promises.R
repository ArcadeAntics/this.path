delayedAssign("os.macos"            , os.unix && capabilities("aqua"))
delayedAssign("os.unix"             , .Platform$OS.type == "unix")
delayedAssign("os.windows"          , .Platform$OS.type == "windows")


delayedAssign("os.unix.gui.x11"     , os.unix    && .Platform$GUI == "X11")
delayedAssign("os.macos.gui.aqua"   , os.macos   && .Platform$GUI == "AQUA")
delayedAssign("os.windows.gui.rgui" , os.windows && .Platform$GUI == "Rgui")
delayedAssign("os.windows.gui.rterm", os.windows && .Platform$GUI == "RTerm")


delayedAssign("gui.rstudio"         ,               .Platform$GUI == "RStudio")
delayedAssign("in.vscode"           , "tools:vscode" %in% search())
delayedAssign("in.shell"            , os.unix.gui.x11 || os.windows.gui.rterm)
