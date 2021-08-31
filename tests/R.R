p <- this.path::ArgumentParser(


    description = this.path::dedent(if (.Platform$OS.type == "windows") "
        Start R, a system for statistical computation and graphics, with the
        specified options

        EnvVars: Environmental variables can be set by NAME=value strings
    " else "
        Start R, a system for statistical computation and graphics, with the
        specified options, or invoke and R tool via the 'R CMD' interface.
    "),


    epilogue = this.path::dedent(if (.Platform$OS.type == "windows") "
        One or more -e options can be used, but not together with -f or --file

        An argument ending in .RData (in any case) is taken as the path
        to the workspace to be restored (and implies --restore)
    " else "
        FILE may contain spaces but not shell metacharacters.
    "),


    wrap = FALSE
)


p$add.version(
    exit = this.path::dedent(sprintf("
        %s -- \"%s\"
        Copyright (C) %s The R Foundation for Statistical Computing
        Platform: %s/%s (%s-bit)

        R is free software and comes with ABSOLUTELY NO WARRANTY.
        You are welcome to redistribute it under the terms of the
        GNU General Public License versions 2 or 3.
        For more information about these matters see
        https://www.gnu.org/licenses/.
    ",  R.version$version.string, R.version$nickname,
        R.version$year,
        R.version$platform, .Platform$r_arch, 8 * .Machine$sizeof.pointer
    )),
    wrap.exit = FALSE
)

p$add.argument("--encoding", default = "native.enc",
    metavariable = casefold("enc", upper = .Platform$OS.type != "windows"),
    help = "Specify encoding to be used for stdin")

p$add.argument("--save", action = "store_true",
    help = "Do save workspace at the end of the session")

p$add.argument("--no-save", action = "store_false", destination = "save",
    help = "Don't save it")

p$add.argument("--no-environ", action = "store_false", destination = "environ",
    help = "Don't read the site and user environment files")

p$add.argument("--no-site-file", action = "store_false", destination = "site-file",
    help = "Don't read the site-wide Rprofile")

p$add.argument("--no-init-file", action = "store_false", destination = "init-file",
    help = if (.Platform$OS.type == "windows")
        "Don't read the .Rprofile or ~/.Rprofile files"
    else "Don't read the user R profile")

p$add.argument("--restore", action = "store_true", destination = "restore",
    help = "Do restore previously saved objects at startup")

p$add.argument("--no-restore-data", action = "store_false", destination = "restore-data",
    help = "Don't restore previously saved objects")

p$add.argument("--no-restore-history", action = "store_false", destination = "restore-history",
    help = "Don't restore the R history file")

p$add.argument("--no-restore", action = "store_false", destination = "restore",
    help = "Don't restore anything")

if (.Platform$OS.type == "windows")
    p$add.argument("--workspace", metavariable = "file",
        help = "Workspace to be restored")

p$add.argument("--vanilla", action = "store_true",
    help = this.path::dedent(if (.Platform$OS.type == "windows") "
        Combine --no-save, --no-restore, --no-site-file,
          --no-init-file and --no-environ
    " else "
        Combine --no-save, --no-restore, --no-site-file,
        --no-init-file and --no-environ
    "),
    wrap = FALSE
)

if (.Platform$OS.type != "windows")
    p$add.argument("--no-readline", action = "store_false", destination = "readline",
        help = "Don't use readline for command-line editing")

if (.Platform$OS.type == "windows")
    p$add.argument("--max-mem-size", type = "integer", metavariable = "N",
        help = "Set limit for memory to be used by R")

p$add.argument("--max-ppsize", type = "integer", metavariable = "N",
    help = "Set max size of protect stack to %(METAVARIABLE)s")

if (.Platform$OS.type != "windows") {
    p$add.argument("--min-nsize", type = "integer", metavariable = "N",
        help = "Set min number of fixed size obj's (\"cons cells\") to %(METAVARIABLE)s")

    p$add.argument("--min-vsize", type = "integer", metavariable = "N",
        help = "Set vector heap minimum to %(METAVARIABLE)s bytes; '4M' = 4 MegaB")
}

p$add.argument("-q", "--quiet", action = "store_true",
    help = "Don't print startup message")

p$add.argument("--silent", action = "store_true", destination = "quiet",
    help = "Same as --quiet")

p$add.argument(if (.Platform$OS.type != "windows") "-s", "--no-echo",
    action = "store_false", destination = "echo",
    help = "Make R run as quietly as possible")

if (.Platform$OS.type != "windows")
    p$add.argument("--interactive", action = "store_true",
        help = "Force an interactive session")

p$add.argument("--verbose", action = "store_true",
    help = "Print more information about progress")

if (.Platform$OS.type == "windows") {
    p$add.skip()

    p$add.argument("--ess", action = "store_true", destination = "ess",
        help = this.path::dedent("
            Don't use getline for command-line editing
            and assert interactive use"),
        wrap.help = FALSE)
} else {
    p$add.argument("-d", "--debugger", metavariable = "NAME",
        help = "Run R through debugger %(METAVARIABLE)s")

    p$add.argument("--debugger-args", metavariable = "ARGS",
        help = "Pass %(METAVARIABLE)s as arguments to the debugger")

    p$add.argument("-g", "--gui", default = "X11", metavariable = "TYPE",
        help = this.path::dedent("
            Use %(METAVARIABLE)s as GUI; possible values are 'X11' (default)
            and 'Tk'."),
        wrap.help = FALSE)

    p$add.argument("--arch", metavariable = "NAME",
        help = "Specify a sub-architecture")

    p$add.skip()
}

group1 <- p$add.argument.group()

group1$add.argument("-f", "--file",
    metavariable = casefold("file", upper = .Platform$OS.type != "windows"),
    help = "Take input from '%(METAVARIABLE)s'")

group1$add.argument("-e", action = "append", type = "expression",
    metavariable = if (.Platform$OS.type == "windows") "expression" else "EXPR",
    help = if (.Platform$OS.type == "windows") "Use '%(METAVARIABLE)s' as input" else "Execute '%(METAVARIABLE)s' and exit")





`p sub` <- p$add.subparsers()
`p sub CMD` <- `p sub`$add.parser("CMD", help = "Invoke an R tool", help.help = NULL,
    epilogue = this.path::dedent(if (.Platform$OS.type == "windows") "
        Use
          R CMD command --help
        for usage information for each command."
    else "
        Please use 'R CMD command --help' to obtain further information about
        the usage of 'command'.

        Options --arch, --no-environ, --no-init-file, --no-site-file and --vanilla
        can be placed between R and CMD, to apply to R processes run by 'command'

        Report bugs at <https://bugs.R-project.org>."),
    wrap.epilogue = FALSE)
`p sub CMD sub` <- `p sub CMD`$add.subparsers(
    title = if (.Platform$OS.type == "windows") "where 'command' is one of:" else "Commands:",
    required = TRUE)

`p sub CMD sub INSTALL`        <- `p sub CMD sub`$add.parser("INSTALL"   , help = "Install add-on packages")
`p sub CMD sub REMOVE`         <- `p sub CMD sub`$add.parser("REMOVE"    , help = "Remove add-on packages")
`p sub CMD sub SHLIB`          <- `p sub CMD sub`$add.parser("SHLIB"     , help = if (.Platform$OS.type == "windows") "Make a DLL for use with dynload" else "Build shared library for dynamic loading")
`p sub CMD sub BATCH`          <- `p sub CMD sub`$add.parser("BATCH"     , help = "Run R in batch mode")

if (.Platform$OS.type != "windows") {
    `p sub CMD sub COMPILE`    <- `p sub CMD sub`$add.parser("COMPILE"   , help = "Compile files for use with R")
    `p sub CMD sub`$reorder(c("BATCH", "COMPILE", "SHLIB", "INSTALL", "REMOVE"))
}

`p sub CMD sub build`          <- `p sub CMD sub`$add.parser("build"     , help = "Build add-on packages")
`p sub CMD sub check`          <- `p sub CMD sub`$add.parser("check"     , help = "Check add-on packages")

if (.Platform$OS.type != "windows")
    `p sub CMD sub LINK`       <- `p sub CMD sub`$add.parser("LINK"      , help = "Front-end for creating executable programs")

`p sub CMD sub Rprof`          <- `p sub CMD sub`$add.parser("Rprof"     , help = if (.Platform$OS.type == "windows") "Post process R profiling files" else "Post-process R profiling files")
`p sub CMD sub Rdconv`         <- `p sub CMD sub`$add.parser("Rdconv"    , help = "Convert Rd format to various other formats")
`p sub CMD sub Rdiff`          <- `p sub CMD sub`$add.parser("Rdiff"     , help = if (.Platform$OS.type == "windows") "difference R output files" else "Diff R output ignoring headers etc")
`p sub CMD sub Rd2pdf`         <- `p sub CMD sub`$add.parser("Rd2pdf"    , help = "Convert Rd format to PDF")
`p sub CMD sub Rd2txt`         <- `p sub CMD sub`$add.parser("Rd2txt"    , help = "Convert Rd format to pretty text")
`p sub CMD sub Stangle`        <- `p sub CMD sub`$add.parser("Stangle"   , help = if (.Platform$OS.type == "windows") "Extract S/R code from vignette" else "Extract S/R code from Sweave documentation")
`p sub CMD sub Sweave`         <- `p sub CMD sub`$add.parser("Sweave"    , help = if (.Platform$OS.type == "windows") "Process vignette documentation" else "Process Sweave documentation")

if (.Platform$OS.type != "windows")
    `p sub CMD sub`$reorder(last = "Rdiff")

`p sub CMD sub config`         <- `p sub CMD sub`$add.parser("config"    , help = "Obtain configuration information about R")

if (.Platform$OS.type == "windows") {
    `p sub CMD sub open`       <- `p sub CMD sub`$add.parser("open"      , help = "Open a file via Windows file associations")
    `p sub CMD sub texify`     <- `p sub CMD sub`$add.parser("texify"    , help = "Process a latex file")
} else {
    `p sub CMD sub javareconf` <- `p sub CMD sub`$add.parser("javareconf", help = "Update the Java configuration variables")
    `p sub CMD sub rtags`      <- `p sub CMD sub`$add.parser("rtags"     , help = "Create Emacs-style tag files from C, R, and Rd files")
}

p$print.help()
`p sub CMD`$print.help()
