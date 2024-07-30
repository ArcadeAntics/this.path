## these are expressions that are used in this package,
## but which are not expected to change value in a session,
## so we can evaluate them exactly once when needed
##
## if you've never heard of an R promise before, it's a mechanism for handling
## delayed argument evaluation and substitution. a promise consists of four
## things:
##
##   * an expression
##
##   * an environment in which the expression will be evaluated
##
##   * value of evaluating said expression in said environment, initially empty
##
##   * the number of attempts to evaluate the promise, used to detect recursive
##     references and restarting interrupted promises
##
## it is called a promise because "I promise to evaluate this code in this
## environment if / / when requested"
##
## an object like an integer, a boolean, and a string will evaluate to itself.
## other objects like a symbol, a bytecode expression, or a call will evaluate
## the same as usual.
##
## if / / when the code has been evaluated, the value of the expression is
## stored inside the promise. this means the promise does not re-evaluate the
## same piece of code multiple times. for example:
##
## ```R
## delayedAssign("x", {
##     writeLines("evaluating 'x'")
##     5 + 6
## })
## x
## x
## x
## ```
##
## which produces the following output:
##
## ```
## > delayedAssign("x", {
## +     writeLines("evaluating 'x'")
## +     5 + 6
## + })
## > x
## evaluating 'x'
## [1] 11
## > x
## [1] 11
## > x
## [1] 11
## >
## ```
##
## you'll see that the expression `writeLines("evaluating 'x'")` is only
## evaluated once, as desired! you can use `ikws.debug::PRINFO()` to examine
## the state of the promise:
##
## ```R
## delayedAssign("x", {
##     writeLines("evaluating 'x'")
##     5 + 6
## })
## ikws.debug::PRINFO(x)
## x
## ikws.debug::PRINFO(x)
## ```
##
## which produces the following output:
##
## ```
## > delayedAssign("x", {
## +     writeLines("evaluating 'x'")
## +     5 + 6
## + })
## > ikws.debug::PRINFO(x)
## $PRCODE
## {
##     writeLines("evaluating 'x'")
##     5 + 6
## }
##
## $PRENV
## <environment: R_GlobalEnv>
##
## $PREXPR
## {
##     writeLines("evaluating 'x'")
##     5 + 6
## }
##
## $PRSEEN
## [1] 0
##
## > x
## evaluating 'x'
## [1] 11
## > ikws.debug::PRINFO(x)
## $PRCODE
## {
##     writeLines("evaluating 'x'")
##     5 + 6
## }
##
## $PRENV
## NULL
##
## $PREXPR
## {
##     writeLines("evaluating 'x'")
##     5 + 6
## }
##
## $PRSEEN
## [1] 0
##
## $PRVALUE
## [1] 11
##
## >
## ```
##
## "PRCODE", "PRENV", "PRVALUE", and "PRSEEN" were all mentioned earlier, but
## "PREXPR" has not been mentioned yet. if "PRCODE" is a bytecode, then
## "PREXPR" will be the uncompiled version of "PRCODE". in all other cases,
## they are identical.
##
## notice that "PRENV" is set to `NULL` once the promise has been evaluated,
## it's not very important right now to understand why, this has to do with
## garbage collection. this allows the garbage collector to collect
## environments which are no longer in use.
##
## "PRSEEN" was mentioned earlier, it is an integer used to determine if a
## promise leads to infinite recursion or has been interrupted and is being
## restarted. for example:
##
## ```R
## delayedAssign("x", withAutoprint({
##     5 + 6
##     ikws.debug::PRINFO(x)
##     x
## }))
## ikws.debug::PRINFO(x)
## try(x)
## ikws.debug::PRINFO(x)
## ```
##
## notice "PRSEEN" is 0 when `x` is not being evaluated, it is 1 when `x` is
## being evaluated, and it is 2 when `x` has been interrupted (by an error) and
## restarted:
##
## ```
## > ikws.debug::PRINFO(x)
## $PRCODE
## withAutoprint({
##     5 + 6
##     ikws.debug::PRINFO(x)
##     x
## })
##
## $PRENV
## <environment: R_GlobalEnv>
##
## $PREXPR
## withAutoprint({
##     5 + 6
##     ikws.debug::PRINFO(x)
##     x
## })
##
## $PRSEEN
## [1] 0
##
## > try(x)
## > 5 + 6
## [1] 11
## > ikws.debug::PRINFO(x)
## $PRCODE
## withAutoprint({
##     5 + 6
##     ikws.debug::PRINFO(x)
##     x
## })
##
## $PRENV
## <environment: R_GlobalEnv>
##
## $PREXPR
## withAutoprint({
##     5 + 6
##     ikws.debug::PRINFO(x)
##     x
## })
##
## $PRSEEN
## [1] 1
##
## > x
## Error in eval(ei, envir) :
##   promise already under evaluation: recursive default argument reference or earlier problems?
## > ikws.debug::PRINFO(x)
## $PRCODE
## withAutoprint({
##     5 + 6
##     ikws.debug::PRINFO(x)
##     x
## })
##
## $PRENV
## <environment: R_GlobalEnv>
##
## $PREXPR
## withAutoprint({
##     5 + 6
##     ikws.debug::PRINFO(x)
##     x
## })
##
## $PRSEEN
## [1] 2
##
## >
## ```
##
## these promises are created when the package is built and installed. they are
## evaluated during the regular use of the package


delayedAssign(".OS_unix"   , { .Platform$OS.type == "unix"    })
delayedAssign(".OS_windows", { .Platform$OS.type == "windows" })


## we need to determine the type of GUI in use. unfortunately, .Platform$GUI is
## not good enough. for example, 'VSCode' and 'Jupyter' do not set
## .Platform$GUI, and 'RStudio' does not set .Platform$GUI until after the
## site-wide startup profile file, a user profile, and the function .First()
## have been run (see ?Startup).
##
## as such, I've made my own ways of determining the type of GUI in use


delayedAssign(".GUI_RStudio", { commandArgs()[[1L]] == "RStudio" })


delayedAssign(".GUI_Positron", {
    interactive() &&
    .scalar_streql(Sys.getenv("POSITRON"), "1") &&
    Sys.getenv("POSITRON_VERSION") != "" &&

    (
        ( .OS_unix    && .Platform$GUI %in% c("X11", "unknown", "none") ) ||
        ( .OS_windows && .Platform$GUI == "Rgui"                        )
    )
})


delayedAssign(".OS_unix_maybe_unembedded_shell"   , { .OS_unix    && .Platform$GUI %in% c("X11"  , "unknown", "none") &&                        "R" == basename2(commandArgs()[[1L]])                                             })
delayedAssign(".OS_windows_maybe_unembedded_shell", { .OS_windows && .Platform$GUI %in% c("RTerm", "unknown"        ) && grepl("(?i)^Rterm(\\.exe)?$", basename2(commandArgs()[[1L]])) && .External2(.C_CharacterMode) == "RTerm" })
delayedAssign(".maybe_unembedded_shell", { .OS_unix_maybe_unembedded_shell || .OS_windows_maybe_unembedded_shell })


## see function do_shINFO in file ./src/shfile.c
delayedAssign(".shINFO", { .External2(.C_shINFO) })


delayedAssign(".console_radian", { commandArgs()[[1L]] == "radian" })


delayedAssign(".GUI_vscode", {
    interactive() &&
    isTRUE(Sys.getenv("TERM_PROGRAM") == "vscode") &&
    (
        isFALSE(.shINFO[["has_input"]]) ||
        .console_radian
    )
})


## jupyter build a competent API challenge (impossible)
delayedAssign(".GUI_jupyter", {
    !interactive() &&

    Sys.getenv("JPY_API_TOKEN") != "" &&
    Sys.getenv("JPY_PARENT_PID") != "" &&

    .maybe_unembedded_shell &&

    isTRUE(.shINFO[["has_input"]]) &&
    is.na(.shINFO[["FILE"]]) &&
    !is.na(.shINFO[["EXPR"]]) &&

    length(commandArgs(TRUE)) == 1L &&
    file.exists(commandArgs(TRUE)) &&

    ## this is the longest check, save for last
    local({
        exprs <- tryCatch(parse(text = .shINFO[["EXPR"]], n = -1, keep.source = FALSE, srcfile = NULL),
            error = identity)
        !inherits(exprs, "error") &&
            length(exprs) &&
            .identical(
                exprs[[length(exprs)]],
                as.call(list(call("::", quote(IRkernel), quote(main))))  # quote(IRkernel::main())
            )
    })
})


delayedAssign(".GUI_emacs", {
    interactive() &&
    Sys.getenv("STATATERM") == "emacs" &&
    .maybe_unembedded_shell &&
    (.OS_unix || (.OS_windows && .shINFO[["ess"]]))
})


delayedAssign(".GUI_rkward", { commandArgs()[[1L]] == "rkward" })


delayedAssign(".GUI_powerbi", {
    !interactive() &&

    Sys.getenv("RPackagesLibrariesDirectory") != "" &&
    Sys.getenv("RScriptWrapperWorkingDirectory") != "" &&

    .maybe_unembedded_shell &&

    .scalar_streql(.shINFO[["ENC"]], "UTF-8") &&
    isTRUE(.shINFO[["has_input"]]) &&
    !is.na(.shINFO[["FILE"]]) &&
    is.na(.shINFO[["EXPR"]])
})


delayedAssign(".in_callr", {
    !interactive() &&

    .scalar_streql(Sys.getenv("CALLR_IS_RUNNING"), "true") &&

    .maybe_unembedded_shell &&

    isTRUE(.shINFO[["has_input"]]) &&
    !is.na(.shINFO[["FILE"]]) &&
    is.na(.shINFO[["EXPR"]]) &&

    "tools:callr" %in% search()
})


delayedAssign(".GUI_AQUA", { .OS_unix    && .Platform$GUI == "AQUA" && !.GUI_RStudio && !.GUI_Positron && !.GUI_vscode && !.GUI_jupyter && !.GUI_emacs && !.GUI_rkward && !.GUI_powerbi && !.in_callr                            })
delayedAssign(".GUI_Rgui", { .OS_windows && .Platform$GUI == "Rgui" && !.GUI_RStudio && !.GUI_Positron && !.GUI_vscode && !.GUI_jupyter && !.GUI_emacs && !.GUI_rkward && !.GUI_powerbi && !.in_callr && .External2(.C_RConsole) })
delayedAssign(".GUI_Tk"  , { .OS_unix    && .Platform$GUI == "Tk"   && !.GUI_RStudio && !.GUI_Positron && !.GUI_vscode && !.GUI_jupyter && !.GUI_emacs && !.GUI_rkward && !.GUI_powerbi && !.in_callr                            })


delayedAssign(".OS_unix_in_shell"   , { .OS_unix_maybe_unembedded_shell    && !.GUI_Positron && !.GUI_vscode && !.GUI_jupyter && !.GUI_emacs && !.GUI_powerbi && !.in_callr })
delayedAssign(".OS_windows_in_shell", { .OS_windows_maybe_unembedded_shell && !.GUI_Positron && !.GUI_vscode && !.GUI_jupyter && !.GUI_emacs && !.GUI_powerbi && !.in_callr })
delayedAssign(".in_shell", { .OS_unix_in_shell || .OS_windows_in_shell })


delayedAssign(".unrecognized_manner", {
    !.in_shell &&
    !.GUI_RStudio &&
    !.GUI_Positron &&
    !.GUI_vscode &&
    !.GUI_jupyter &&
    !.GUI_emacs &&
    !.GUI_rkward &&
    !.GUI_powerbi &&
    !.in_callr &&
    !.GUI_Rgui &&
    !.GUI_AQUA &&
    !.GUI_Tk
})


delayedAssign(".ucrt", { identical(R.version[["crt"]], "ucrt") })
delayedAssign(".GUI", {
    if (.in_shell) .Platform$GUI
    else if (.GUI_RStudio) "RStudio"
    else if (.GUI_Positron) "Positron"
    else if (.GUI_vscode) "vscode"
    else if (.GUI_jupyter) "jupyter"
    else if (.GUI_emacs) "emacs"
    else if (.GUI_rkward) "rkward"
    else if (.GUI_powerbi) "powerbi"
    # else if (.GUI_Rgui) "Rgui"
    # else if (.GUI_AQUA) "AQUA"
    # else if (.GUI_Tk) "Tk"
    else .Platform$GUI
})


delayedAssign("initwd", { getwd() })
getinitwd <- function ()
initwd


.unset_these_envvars <- c(
    "TERM_PROGRAM", "JPY_API_TOKEN", "JPY_PARENT_PID", "STATATERM",
    "RPackagesLibrariesDirectory", "RScriptWrapperWorkingDirectory",
    "CALLR_IS_RUNNING", "POSITRON", "POSITRON_VERSION"
)
