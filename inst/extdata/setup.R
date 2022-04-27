

main <- function ()
{
    stopifnot(.Platform$OS.type == "windows")


    envir <- new.env(parent = baseenv())
    source(this.path::here("select_screen_res.R"), envir)
    screen.res <- envir$select.screen.res()$name


    path <- R.home("..")
    pattern <- sprintf("^R-(%s)$", .standard_regexps()$valid_R_system_version)
    paths <- list.files(path, pattern)
    R.versions <- as.numeric_version(sub(pattern, "\\1", paths))
    i <- order(R.versions, decreasing = TRUE)
    paths <- normalizePath(file.path(path, paths[i], "bin", "Rscript.exe"), mustWork = TRUE)
    R.versions <- R.versions[i]


    FILE <- tempfile(fileext = ".R")
    on.exit(unlink(FILE))
    writeLines('cat(identical(R.version[["crt"]], "ucrt"))', FILE)
    is.universal.c.runtime <- vapply(paths, function(name) {
        essentials::Rscript(
            options = c("--default-packages=NULL", "--vanilla"),
            file = FILE,
            name = name,
            intern = TRUE, mustWork = TRUE, quiet = TRUE
        )
    }, FUN.VALUE = "")
    is.universal.c.runtime <- as.logical(is.universal.c.runtime)


    fun <- function(...) {
        nms <- as.list(substitute(list(...)))[-1L]
        x <- list(...)
        x <- lapply(x, as.logical)
        if (is.null(names(x))) {
            names(x) <- nms
        } else if (any(no.names <- !nzchar(names(x)))) {
            nms <- as.character(nms)
            names(x)[no.names] <- nms[no.names]
        }
        i <- vapply(x, function(xx) {
            if (!any(xx))
                0L
            else which(xx)[[1L]]
        }, FUN.VALUE = 0L)
        if (any(j <- !i)) {
            msgs <- paste0(names(x), " unavailable")
            if (all(j)) {
                stop(paste(msgs, collapse = "\n "))
            } else warning(paste(msgs[j], collapse = "\n"))
            i <- i[!j]
        }
        i
    }


    i <- fun(
        `not Universal C Runtime` = !is.universal.c.runtime,
        `not Universal C Runtime` = is.universal.c.runtime
    )
    for (name in paths[i]) {
        essentials::Rscript(
            options = c("--default-packages=NULL", "--vanilla"),
            file = this.path::here("write_r_editor_regexp.R"), chdir = TRUE,
            args = screen.res,
            name = name, mustWork = TRUE
        )
    }
}


main()
