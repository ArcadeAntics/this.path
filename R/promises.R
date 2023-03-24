# expressions that we need to evaluate for our code to work,
# but which are NOT expected to change in a session,
# so we can evaluate them exactly once when needed
#
# if you've never heard of an R promise before, it's a mechanism for handling
# delayed argument evaluation and substitution. a promise consists of an object
# to evaluate and an environment in which to evaluate it. it is called a promise
# because "I promise to evaluate this code in this environment if / / when requested"
#
# an object like an integer, a boolean, and a string will evaluate to themself.
# other objects like a symbol, a bytecode expression, or a call will evaluate
# the same as usual.
#
# if / / when the code has been evaluated, the value of the expression is
# stored inside the promise. this means the promise does not re-evaluate the
# same piece of code multiple times. for example:
#
# ```R
# delayedAssign("x", {
#     writeLines("evaluating 'x'")
#     5 + 6
# })
# x
# x
# x
# ```
#
# which produces the following output:
#
# ```
# > delayedAssign("x", {
# +     writeLines("evaluating 'x'")
# +     5 + 6
# + })
# > x
# evaluating 'x'
# [1] 11
# > x
# [1] 11
# > x
# [1] 11
# >
# ```
#
# you'll see that the expression `writeLines("evaluating 'x'")` is only
# evaluated once, as desired! also, you can use `this.path:::PRINFO()` to
# examine the state of the promise:
#
# ```R
# delayedAssign("x", {
#     writeLines("evaluating 'x'")
#     5 + 6
# })
# this.path:::PRINFO(x)
# x
# this.path:::PRINFO(x)
# ```
#
# which produces the following output:
#
# ```
# > delayedAssign("x", {
# +     writeLines("evaluating 'x'")
# +     5 + 6
# + })
# > this.path:::PRINFO(x)
# $PRCODE
# {
#     writeLines("evaluating 'x'")
#     5 + 6
# }
#
# $PRENV
# <environment: R_GlobalEnv>
#
# $PREXPR
# {
#     writeLines("evaluating 'x'")
#     5 + 6
# }
#
# $PRSEEN
# [1] 0
#
# > x
# evaluating 'x'
# [1] 11
# > this.path:::PRINFO(x)
# $PRCODE
# {
#     writeLines("evaluating 'x'")
#     5 + 6
# }
#
# $PRENV
# NULL
#
# $PREXPR
# {
#     writeLines("evaluating 'x'")
#     5 + 6
# }
#
# $PRSEEN
# [1] 0
#
# $PRVALUE
# [1] 11
#
# >
# ```
#
# "PRCODE" / / "PREXPR" is the object that may eventually be evaluated. here,
# they are identical, but they may not be if the code is byte compiled.
#
# "PRENV" is the environment in which "PRCODE" will be evaluated. notice that
# "PRENV" is set to `NULL` once the promise has been evaluated, it's not very
# important right now to understand why, this has to do with garbage collection.
# this allows the garbage collector to collect environments which are no longer
# in use.
#
# "PRSEEN" is an integer used to determine if a promise leads to infinite
# recursion or has been interrupted and is being restarted. for example:
#
# ```R
# delayedAssign("x", withAutoprint({
#     5 + 6
#     this.path:::PRINFO(x)
#     x
# }))
# this.path:::PRINFO(x)
# try(x)
# this.path:::PRINFO(x)
# ```
#
# notice "PRSEEN" is 0 when `x` is not being evaluated, it is 1 when `x` is
# being evaluated, and it is 2 when `x` has been interrupted (by an error) and
# restarted:
#
# ```
# > this.path:::PRINFO(x)
# $PRCODE
# withAutoprint({
#     5 + 6
#     this.path:::PRINFO(x)
#     x
# })
#
# $PRENV
# <environment: R_GlobalEnv>
#
# $PREXPR
# withAutoprint({
#     5 + 6
#     this.path:::PRINFO(x)
#     x
# })
#
# $PRSEEN
# [1] 0
#
# > try(x)
# > 5 + 6
# [1] 11
# > this.path:::PRINFO(x)
# $PRCODE
# withAutoprint({
#     5 + 6
#     this.path:::PRINFO(x)
#     x
# })
#
# $PRENV
# <environment: R_GlobalEnv>
#
# $PREXPR
# withAutoprint({
#     5 + 6
#     this.path:::PRINFO(x)
#     x
# })
#
# $PRSEEN
# [1] 1
#
# > x
# Error in eval(ei, envir) :
#   promise already under evaluation: recursive default argument reference or earlier problems?
# > this.path:::PRINFO(x)
# $PRCODE
# withAutoprint({
#     5 + 6
#     this.path:::PRINFO(x)
#     x
# })
#
# $PRENV
# <environment: R_GlobalEnv>
#
# $PREXPR
# withAutoprint({
#     5 + 6
#     this.path:::PRINFO(x)
#     x
# })
#
# $PRSEEN
# [1] 2
#
# >
# ```
#
# these promises are created when the package is built and installed. they are
# evaluated during the regular use of the package


delayedAssign("os.unix"   , .Platform$OS.type == "unix"   )
delayedAssign("os.windows", .Platform$OS.type == "windows")


# we need to determine the type of GUI in use. unfortunately, .Platform$GUI is
# not good enough. for example, 'VSCode' and 'Jupyter' do not set .Platform$GUI,
# and 'RStudio' does not set .Platform$GUI until after the site-wide startup
# profile file, the user profile, and the function .First() have been run
# (see ?Startup).
#
# as such, I've made my own ways of determining the type of GUI in use
delayedAssign("gui.rstudio", commandArgs()[[1L]] == "RStudio" &&
                             isTRUE(Sys.getpid() == Sys.getenv("RSTUDIO_SESSION_PID")) &&
                             if (.Platform$GUI == "RStudio") { .External2(C_inittoolsrstudio, skipCheck = TRUE); TRUE }
                             else (
                                 (os.unix    && .Platform$GUI %in% c("X11"  , "unknown", "none")) ||
                                 (os.windows && .Platform$GUI %in% c("RTerm", "unknown"        ))
                             )
)


delayedAssign("os.unix.maybe.unembedded.shell"   , os.unix    && .Platform$GUI %in% c("X11"  , "unknown", "none") &&                        "R" == basename2(commandArgs()[[1L]]) )
delayedAssign("os.windows.maybe.unembedded.shell", os.windows && .Platform$GUI %in% c("RTerm", "unknown"        ) && grepl("(?i)^Rterm(\\.exe)?$", basename2(commandArgs()[[1L]])))
delayedAssign("maybe.unembedded.shell", os.unix.maybe.unembedded.shell || os.windows.maybe.unembedded.shell)


# see function do_shinfo in file ./src/shfile.c
delayedAssign("shINFO", .External2(C_shinfo))


delayedAssign("os.unix.console.radian"   , os.unix    && .Platform$GUI %in% c("X11"  , "unknown", "none") && commandArgs()[[1L]] == "radian")
delayedAssign("os.windows.console.radian", os.windows && .Platform$GUI %in% c("RTerm", "unknown"        ) && commandArgs()[[1L]] == "radian")
delayedAssign("console.radian", os.unix.console.radian || os.windows.console.radian)


delayedAssign("gui.vscode", interactive() && isTRUE(Sys.getenv("TERM_PROGRAM") == "vscode") && (is.na(shINFO[["ENC"]]) && isFALSE(shINFO[["has.input"]]) || console.radian))


IRkernel.main.call <- as.call(list(call("::", as.symbol("IRkernel"), as.symbol("main"))))
# jupyter build a competent API challenge (impossible)
delayedAssign("gui.jupyter",
    !interactive() &&

    Sys.getenv("JPY_API_TOKEN") != "" &&
    Sys.getenv("JPY_PARENT_PID") != "" &&

    maybe.unembedded.shell &&

    is.na(shINFO[["ENC"]]) &&
    isTRUE(shINFO[["has.input"]]) &&
    is.na(shINFO[["FILE"]]) &&
    !is.na(shINFO[["EXPR"]]) &&

    length(commandArgs(TRUE)) == 1L &&
    file.exists(commandArgs(TRUE)) &&

    # this is the longest check, save for last
    local({
        exprs <- tryCatch(parse(text = shINFO[["EXPR"]], n = -1, keep.source = FALSE, srcfile = NULL),
            error = identity)
        !inherits(exprs, "error") &&
            length(exprs) &&
            identical(exprs[[length(exprs)]], IRkernel.main.call)
    })
)


delayedAssign("gui.aqua", os.unix    && .Platform$GUI == "AQUA" && !gui.rstudio && !gui.vscode && !gui.jupyter)
delayedAssign("gui.rgui", os.windows && .Platform$GUI == "Rgui" && !gui.rstudio && !gui.vscode && !gui.jupyter)
delayedAssign("gui.tk"  , os.unix    && .Platform$GUI == "Tk"   && !gui.rstudio && !gui.vscode && !gui.jupyter)


`tools:rstudio` <- emptyenv()
.rs.api.getActiveDocumentContext <- function (...)
{
    if (gui.rstudio)
        stop("RStudio has not finished loading")
    else stop("RStudio is not running")
}
.rs.api.getSourceEditorContext <- .rs.api.getActiveDocumentContext
debugSource <- .rs.api.getActiveDocumentContext


`init.tools:rstudio` <- function ()
.External2(C_inittoolsrstudio)


delayedAssign("os.unix.in.shell"   , os.unix.maybe.unembedded.shell    && !gui.vscode && !gui.jupyter)
delayedAssign("os.windows.in.shell", os.windows.maybe.unembedded.shell && !gui.vscode && !gui.jupyter)
delayedAssign("in.shell", os.unix.in.shell || os.windows.in.shell)


delayedAssign("unrecognized.manner", !in.shell && !gui.rstudio && !gui.vscode && !gui.jupyter && !gui.rgui && !gui.aqua && !gui.tk)


delayedAssign("initwd", getwd())
delayedAssign("ucrt"  , identical(R.version[["crt"]], "ucrt"))
delayedAssign("GUI", if (in.shell) .Platform$GUI
                     else if (gui.rstudio) "RStudio"
                     else if (gui.vscode) "vscode"
                     else if (gui.jupyter) "jupyter"
                     # else if (gui.rgui) "Rgui"
                     # else if (gui.aqua) "AQUA"
                     # else if (gui.tk) "Tk"
                     else .Platform$GUI)


getinitwd <- function ()
initwd


PRINFO <- function (x, pos = -1L, envir = as.environment(pos), inherits = TRUE,
    evaluated = FALSE)
.External2(C_prinfo, if (evaluated) x else substitute(x), envir, inherits)
