file.URL.path <- function (path)
{
    # remove the leading "file://" from a file URL
    #
    # but specifically on Windows, where file URLs may look like
    # "file:///c:" ...
    # remove the leading "file:///" from those file URLs
    if (os.windows && any(i <- grepl("^file:///.:", path, useBytes = TRUE))) {
        path[ i] <- substr(path[ i], 9L, 1000000L)
        path[!i] <- substr(path[!i], 8L, 1000000L)
        path
    }
    else substr(path, 8L, 1000000L)
}


file.URL.path.1 <- function (path)
{
    # do file.URL.path but a little bit faster when path is length 1
    if (os.windows && grepl("^file:///.:", path, useBytes = TRUE))
        substr(path, 9L, 1000000L)
    else substr(path, 8L, 1000000L)
}


extra.whitespace.pattern <- "^[\n\r]+|[\t\n\r ]+$"


normalizeURL <- function (path)
{
    # x <- "https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R"
    # print(c(x, this.path:::.normalizeURL(x)))
    # source(x)


    path.unsplit(lapply(path.split(path), function(p) {
        # x <- path.split("https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R")[[1L]]


        p <- p[p != "."]
        while (i <- match("..", p, 0L)) {
            p <- if (i == 2L) p[-2L] else p[-i + 0L:1L]
        }
        p
    }))
}


normalizeURL.1 <- function (path)
{
    # path <- "https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R"


    p <- path.split.1(path)
    p <- p[p != "."]
    while (i <- match("..", p, 0L)) {
        p <- if (i == 2L) p[-2L] else p[-i + 0L:1L]
    }
    path.unsplit(p)
}


normalizePath.and.URL <- function (path, ...)
{
    # a version of normalizePath that will also normalize URLs
    if (any(i <- grepl("^file://", path)))
        path[i] <- file.URL.path(path[i])
    if (any(i <- !i & grepl("^(https|http|ftp|ftps)://", path))) {
        path[i] <- normalizeURL(path[i])
        path[!i] <- normpath(path = path[!i], ...)
        path
    }
    else normpath(path = path, ...)
}


normalizePath.and.URL.1 <- function (path, ...)
{
    if (grepl("^file://", path))
        normalizePath(path = file.URL.path.1(path), ...)
    else if (grepl("^(ftp|ftps|http|https)://", path))
        normalizeURL.1(path)
    else normpath(path = path, ...)
}


as.relative.path <- as.rel.path <- function (path, relative.to = this.dir(verbose = FALSE))
{
    if (!is.character(path))
        stop(gettextf("invalid '%s' argument", "path", domain = "R"), domain = NA)
    path <- normalizePath.and.URL(path, winslash = "/", mustWork = FALSE)
    if (!missing(relative.to)) {
        if (!is.character(relative.to) || length(relative.to) != 1L)
            stop(gettextf("invalid '%s' argument", "relative.to", domain = "R"), domain = NA)
        relative.to <- normalizePath.and.URL.1(relative.to, winslash = "/", mustWork = FALSE)
    }
    relative.to <- path.split.1(relative.to)
    len <- length(relative.to)
    x <- path.split(path)
    vapply(seq_along(x), function(i) {
        xx <- x[[i]]
        len2 <- length(xx)
        n <- min(len2, len)


        # this will give us the location of the first FALSE value, but what we
        # really want is the last TRUE value, so that's why we subtract 1.
        # our nomatch is n + 1 because when we subtract 1, we get n back,
        # meaning the first n components are equal
        n <- match(FALSE, xx[seq_len(n)] == relative.to[seq_len(n)], n + 1L) - 1L


        # if the paths have absolutely nothing in common, return the original path
        if (n == 0L)
            return(path[[i]])


        # otherwise, return the appropriate number of .. and the tail of the path
        # if there are no .. to add, add . instead
        value <- c(
            rep("..", len - n),
            xx[seq.int(n + 1L, length.out = len2 - n)]
        )


        # this happens when 'path' and 'relative.to' are equal
        # simply return "."
        if (length(value) <= 0L)
            "."


        # the path should always start with ./ or ../
        #
        # in command line applications, this avoids confusion for paths which
        # beginning with hyphen
        #
        # for unix only, it avoids confusion for paths beginning with space
        else if (!(value[[1L]] %in% c(".", "..")))
            paste(c(".", value), collapse = "/")
        else paste(value, collapse = "/")
    }, FUN.VALUE = "")
}


rel2here <- as.rel.path


relpath <- as.rel.path
formals(relpath)["relative.to"] <- alist(getwd())





# relative.to <- this.path:::abspath("~/this.path/testing/code")
#
#
# path <- "C:\\Users\\andre\\Documents\\this.path\\testing\\this\\out"
# # path <- "C:Users\\andre\\Documents\\this.path\\testing\\this\\out"
# # path <- "test"
# # path <- "./."
# # path <- "\\\\LOCALHOST\\\\\\C$///////////////Users\\andre\\Documents\\this.path\\testing\\this\\out"
#
#
# normalizePath2 <- function (path, winslash = "\\")
# {
#     x <- path
#     p <- ""
#     delayedAssign("returnx", return(x))
#     repeat {
#         tryCatch2({
#             path <- normalizePath(path, winslash, TRUE)
#         }, error = function(e) {
#             b <- basename2(path)
#             if (b == "")
#                 returnx
#             if (b != ".")
#                 p <<- path.join(b, p)
#             path <<- dirname2(path)
#         }, else. = {
#             return(path.join(path, p))
#         })
#     }
# }
# environment(normalizePath2) <- getNamespace("this.path")
# normalizePath2 <- compiler::cmpfun(normalizePath2)
#
#
# normalizePath2(path)
# normalizePath2(relative.to)
#
#
# # path <- "C:\\Users\\andre\\Desktop"
#
#
#
# xx           <- path.split.1(normalizePath2(path))
# relative.to2 <- path.split.1(normalizePath2(relative.to))
#
#
# len <- length(relative.to2)
# len2 <- length(xx)
# n <- min(len2, len)
# n <- match(FALSE, xx[seq_len(n)] == relative.to2[seq_len(n)], n + 1L) - 1L
# if (n == 0L) {
#     path
# } else {
#     value <- c(rep("..", len - n), xx[seq.int(n + 1L, length.out = len2 - n)])
#     if (length(value) <= 0L)
#         "."
#     else if (!(value[[1L]] %in% c(".", "..")))
#         paste(c(".", value), collapse = "/")
#     else paste(value, collapse = "/")
# }
