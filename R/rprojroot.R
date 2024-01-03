tmp <- list(
    testfun = list(),
    desc = character(0),
    subdir = NULL,
    find_file = function() NULL,
    make_fix_file = function() NULL
)
class(tmp) <- "root_criterion"


i <- 0L


## has_file(".here")
i <- i + 1L
tmp$testfun[[i]] <- function (path)
.isfalse(.isdir(path.join(path, ".here")))
tmp$desc[[i]] <- "contains a file \".here\""


## is_rstudio_project
i <- i + 1L
tmp$testfun[[i]] <- function (path)
{
    files <- list.files(path, "\\.Rproj$", all.files = TRUE, full.names = TRUE)
    for (file in files[!dir.exists(files)])
        if (any(startsWith(readLines(file, 1L, warn = FALSE), "Version: ")))
            return(TRUE)
    FALSE
}
tmp$desc[[i]] <- "contains a file matching \"[.]Rproj$\" with contents matching \"^Version: \" in the first line"


## is_r_package
i <- i + 1L
tmp$testfun[[i]] <- function (path)
{
    file <- path.join(path, "DESCRIPTION")
    .isfalse(.isdir(file)) &&
        any(startsWith(readLines(file, warn = FALSE), "Package: "))
}
tmp$desc[[i]] <- "contains a file \"DESCRIPTION\" with contents matching \"^Package: \""


## is_remake_project
i <- i + 1L
tmp$testfun[[i]] <- function (path)
.isfalse(.isdir(path.join(path, "remake.yml")))
tmp$desc[[i]] <- "contains a file \"remake.yml\""


## is_projectile_project
i <- i + 1L
tmp$testfun[[i]] <- function (path)
.isfalse(.isdir(path.join(path, ".projectile")))
tmp$desc[[i]] <- "contains a file \".projectile\""


## is_vcs_root
i <- i + 1L
tmp$testfun[[i]] <- function (path)
dir.exists(path.join(path, ".git"))
tmp$desc[[i]] <- "contains a directory \".git\""
i <- i + 1L
tmp$testfun[[i]] <- function (path)
{
    file <- path.join(path, ".git")
    .isfalse(.isdir(file)) &&
        any(startsWith(readLines(file, warn = FALSE), "gitdir: "))
}
tmp$desc[[i]] <- "contains a file \".git\" with contents matching \"^gitdir: \""
i <- i + 1L
tmp$testfun[[i]] <- function (path)
dir.exists(path.join(path, ".svn"))
tmp$desc[[i]] <- "contains a directory \".svn\""


rm(i)


tmp$find_file <- function (..., path = ".")
{
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    root <- .find_root(path = path, criterion = package_here_criterion_if_rprojroot_is_not_available)
    path.join(root, ...)
}


tmp$make_fix_file <- .removeSource_from_inner_functions(
function (path = getwd(), subdir = NULL)
{
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    root <- .find_root(path = path, criterion = package_here_criterion_if_rprojroot_is_not_available)
    value <- function(...) NULL
    body(value) <- call("path.join", root, as.symbol("..."))
    environment(value) <- .mynamespace
    value
}
)


.find_root <- evalq(envir = new.env(), {
    environment(tmp$find_file) <<- environment()
    environment(tmp$make_fix_file) <<- environment()
    package_here_criterion_if_rprojroot_is_not_available <- tmp
    delayedAssign("package_here_criterion", {
        if (requireNamespace("rprojroot", quietly = TRUE))
            rprojroot::has_file(".here")     |
            rprojroot::is_rstudio_project    |
            rprojroot::is_r_package          |
            rprojroot::is_remake_project     |
            rprojroot::is_projectile_project |
            rprojroot::is_vcs_root
        else
            package_here_criterion_if_rprojroot_is_not_available
    })
    format.root_criterion <- function (x, ...)
{
    if (length(x$desc) > 1L)
        c("Root criterion: one of", paste0("- ", x$desc))
    else paste0("Root criterion: ", x$desc)
}
    print.root_criterion <- function (x, ...)
{
    cat(format(x), sep = "\n")
    invisible(x)
}
function (path = getwd(), verbose = FALSE, criterion = package_here_criterion)
{
    # path <- "\\\\host\\share\\path\\to\\file\\"
    if (is.null(path))
        stop("cannot '.find_root' as current directory is unknown")
    if (.OS_windows && missing(path))
        path <- .normalizePath(path)
    else if (grepl("^(https|http|ftp|ftps)://", path))
        stop("this.proj() does not work for URL pathnames")
    if (!inherits(criterion, "root_criterion"))
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


rm(tmp)


.proj <- evalq(envir = new.env(), {
    x <- structure(character(0), names = character(0))
function (path, verbose = FALSE)
{
    ## 'path' should be normalized
    if (indx <- match(path, names(x), 0L))
        x[[indx]]
    else (x[[path]] <<- .find_root(path, verbose))
}
})


sys.proj <- function (..., local = FALSE)
{
    base <- .External2(.C_sys_path, local)
    base <- .dir(base)
    base <- .proj(base)
    path.join(base, ...)
}


env.proj <- function (..., n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"))
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_env_path, envir, matchThisEnv)
    base <- .dir(base)
    base <- .proj(base)
    path.join(base, ...)
}


src.proj <- function (..., n = 0L, srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_src_path, srcfile)
    base <- .dir(base)
    base <- .proj(base)
    path.join(base, ...)
}


this.proj <- function (..., local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_this_path, local, envir, matchThisEnv, srcfile)
    base <- .dir(base)
    base <- .proj(base)
    path.join(base, ...)
}


reset.proj <- function ()
.External2(.C_reset_proj)


reset.this.proj <- function ()
stop(.defunctError("reset.proj", "this.path", old = "reset.this.proj"))
