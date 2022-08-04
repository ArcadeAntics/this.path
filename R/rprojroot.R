find_root <- function (criterion, path = getwd(), verbose = getOption("verbose"))
{
    criterion <- rprojroot::as_root_criterion(criterion)
    opath <- path
    components <- path.split.1(path)
    add.slash <- !( grepl(URL.pattern, components[[1L]]) ||
                    grepl(UNC.pattern, components[[1L]]) )
    if (grepl(UNC.pattern, components[[1L]]))
        substr(components[[1L]], 1L, 2L) <- substr(opath, 1L, 2L)
    while (len <- length(components)) {
        path <- if (len == 1L) {
            if (add.slash) paste0(components, "/") else components
        }
        else paste(components, collapse = "/")
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
        components <- components[-len]
    }
    stop("no root directory found in ", encodeString(opath, quote = "\""), " or its' parent directories\n",
        paste(format(criterion), collapse = "\n"))
}


.this.proj <- evalq(envir = new.env(),
function (path, verbose = getOption("verbose"))
{
    if (path %in% names(table))
        table[[path]]
    else (table[[path]] <<- find_root(
        criterion = rprojroot::has_file(".here") | rprojroot::is_rstudio_project |
            rprojroot::is_r_package | rprojroot::is_remake_project |
            rprojroot::is_projectile_project | rprojroot::is_vcs_root,
        path = path,
        verbose = verbose
    ))
}
)
evalq(envir = environment(.this.proj), {
    table <- structure(character(), names = character())
})
lockEnvironment(environment(.this.proj))


this.proj <- function (...)
{
    base <- .this.proj(.this.dir(), FALSE)
    file.path(base, ...)
}
