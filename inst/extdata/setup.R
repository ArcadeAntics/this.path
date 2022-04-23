path <- R.home("..")
pattern <- sprintf("^R-(%s)$", .standard_regexps()$valid_R_system_version)
paths <- list.files(path, pattern)
R.versions <- as.numeric_version(sub(pattern, "\\1", paths))
i <- order(R.versions, decreasing = TRUE)
paths <- normalizePath(file.path(path, paths[i], "bin", "Rscript.exe"), mustWork = TRUE)
R.versions <- R.versions[i]


fun <- function(...) {
    nms <- as.list(substitute(list(...)))[-1L]
    x <- list(...)
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
    `R < 4.2.0`  = R.versions <  "4.2.0",
    `R >= 4.2.0` = R.versions >= "4.2.0"
)
for (name in paths[i]) {
    essentials::Rscript(
        options = c("--default-packages=NULL", "--vanilla"),
        file = this.path::here("write_r_editor_regexp.R"), chdir = TRUE,
        name = name, mustWork = TRUE
    )
}
