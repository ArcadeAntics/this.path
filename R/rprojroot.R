.find.root <- evalq(envir = new.env(), {
    delayedAssign("default.criterion", rprojroot::has_file(".here")     |
                                       rprojroot::is_rstudio_project    |
                                       rprojroot::is_r_package          |
                                       rprojroot::is_remake_project     |
                                       rprojroot::is_projectile_project |
                                       rprojroot::is_vcs_root           )
function (path = getwd(), verbose = FALSE, criterion = default.criterion)
{
    # path <- "\\\\host\\share\\path\\to\\file\\"
    criterion <- rprojroot::as.root_criterion(criterion)
    opath <- path
    p <- path.split.1(path)
    while (n <- length(p)) {
        path <- path.unsplit(p)
        if (verbose) {
            for (i in seq_along(criterion$testfun)) {
                if (criterion$testfun[[i]](path)) {
                    cat("this.proj source: ", criterion$desc[[i]], "\n", sep = "")
                    return(path)
                }
            }
        }
        else {
            for (f in criterion$testfun) {
                if (f(path))
                    return(path)
            }
        }
        p <- p[-n]
    }
    stop(sprintf("no root directory found in %s or its parent directories\n%s",
        encodeString(opath, quote = "\""),
        paste(format(criterion), collapse = "\n")))
}
})


if (.Platform$OS.type == "windows") {
    formals(.find.root)$path <- quote(normalizePath(getwd(), "/", TRUE))
}


.proj <- evalq(envir = new.env(), {
    x <- structure(character(0), names = character(0))
function (path, verbose = FALSE)
{
    ## 'path' should be normalized
    if (indx <- match(path, names(x), 0L))
        x[[indx]]
    else (x[[path]] <<- .find.root(path, verbose))
}
})


sys.proj <- function (..., local = FALSE)
{
    base <- .External2(.C_sys.path, local)
    base <- .dir(base)
    base <- .proj(base)
    path.join(base, ...)
}


env.proj <- function (..., n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"))
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_env.path, envir, matchThisEnv)
    base <- .dir(base)
    base <- .proj(base)
    path.join(base, ...)
}


src.proj <- function (..., n = 0L, srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_src.path, srcfile)
    base <- .dir(base)
    base <- .proj(base)
    path.join(base, ...)
}


this.proj <- function (..., local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_this.path, local, envir, matchThisEnv, srcfile)
    base <- .dir(base)
    base <- .proj(base)
    path.join(base, ...)
}


reset.proj <- function ()
{
    if (sys.nframe() != .toplevel.context.number() + 1L)
        stop(gettextf("'%s' can only be called from a top level context", "reset.proj"))
    .External2(.C_reset.proj)
}


reset.this.proj <- eval(call("function", NULL, bquote(
stop(.defunctError("reset.proj", .(.pkgname), old = "reset.this.proj"))
)))
