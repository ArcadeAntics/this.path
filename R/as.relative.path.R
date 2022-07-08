file.URL.path <- function (path)
{
    # remove the leading "file://" from a file URL
    #
    # but specifically on Windows, where file URLs may look like
    # "file:///c:" ...
    # remove the leading "file:///" from those file URLs
    if (.Platform$OS.type == "windows" &&
        any(i <- grepl("^file:///[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]:", path))) {
        path[ i] <- substr(path[ i], 9L, 1000000L)
        path[!i] <- substr(path[!i], 8L, 1000000L)
        path
    }
    else substr(path, 8L, 1000000L)
}


normalizePath2 <- function (path, ...)
{
    # a version of normalizePath that will also normalize URLs
    if (any(i <- grepl("^file://", path)))
        path[i] <- file.URL.path(path[i])
    if (any(i <- !i & grepl("^(ftp|ftps|http|https)://", path))) {
        path[i] <- .normalizeURL(path[i])
        path[!i] <- normalizePath(path = path[!i], ...)
        path
    }
    else normalizePath(path = path, ...)
}


as.relative.path <- as.rel.path <- function (path, relative.to = this.dir(verbose = FALSE))
{
    if (!is.character(path))
        stop("invalid 'path' argument")
    path <- normalizePath2(path, winslash = "/", mustWork = FALSE)
    if (!missing(relative.to)) {
        if (!is.character(relative.to) || length(relative.to) != 1L)
            stop("invalid 'relative.to' argument")
        relative.to <- normalizePath2(relative.to, winslash = "/", mustWork = FALSE)
    }
    relative.to <- path.split(relative.to)[[1L]]
    len <- length(relative.to)
    x <- path.split(path)
    vapply(seq_along(x), function(i) {
        xx <- x[[i]]
        n <- min(len2 <- length(xx), len)


        # this will give us the location of the first FALSE value, but what we
        # really want is the last TRUE value, so that's why we subtract 1.
        # our nomatch is n + 1 because when we subtract 1, we get n back,
        # meaning the first n components are equal
        n <- match(FALSE, xx[seq_len(n)] == relative.to[seq_len(n)], n + 1L) - 1L


        # if the paths have absolutely nothing in common, return the original path
        if (n == 0L)
            return(path[[i]])


        # otherwise, return the appropriate number of .. and the tail of the path
        value <- c(rep("..", len - n), xx[seq.int(n + 1L, length.out = len2 - n)])


        # this happens when the 'path', and 'relative.to' and equal
        # simply return "."
        if (length(value) <= 0L)
            "."
        else paste(value, collapse = "/")
    }, FUN.VALUE = "")
}
