find_root <- function (path = normalizePath(getwd()), verbose = FALSE, criterion = default.criterion)
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
evalq(envir = environment(find_root) <- new.env(), {
    delayedAssign("default.criterion", rprojroot::has_file(".here")     |
                                       rprojroot::is_rstudio_project    |
                                       rprojroot::is_r_package          |
                                       rprojroot::is_remake_project     |
                                       rprojroot::is_projectile_project |
                                       rprojroot::is_vcs_root           )
})


.this.proj <- function (verbose = FALSE)
{
    path <- .this.dir()
    if (indx <- match(path, names(table), 0L))
        table[[indx]]
    else (table[[path]] <<- find_root(path, verbose))
}
evalq(envir = environment(.this.proj) <- new.env(), {
    table <- structure(character(), names = character())
})


this.proj <- function (...)
{
    base <- .this.proj()
    path.join(base, ...)
}
