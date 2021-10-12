path.split <- function (path)
{
    path <- path.expand(path)
    if (.Platform$OS.type == "windows")
        path <- chartr("\\", "/", path)
    value <- strsplit(path, "/+")
    if (any(i <- grepl(pattern <- "^(((ftp|ftps|http|https):)?//).*", path)))
        value[i] <- .mapply(function(x, y) {
            x[[2L]] <- paste0(y, x[[2L]])
            x[-1L]
        }, list(value[i], sub(pattern, "\\1", path[i])), NULL)
    value
}


.check.path <- function (path, x, name)
{
    if (!is.character(path) || length(path) != 1L)
        stop("invalid 'path' argument")
    x <- path.split(x)[[1L]]
    path <- path.split(path)[[1L]]
    if (length(x) < length(path))
        stop(errorCondition(.makeMessage(
            sQuote(name), " and expected path do not match\n",
            paste0("* ", format(c(name, "expected")), ": ",
                encodeString(c(
                    paste(x, collapse = "/"),
                    paste(path, collapse = "/")
                ), quote = "\""), collapse = "\n")
        ), call = sys.call(-1L)))
    x <- x[seq.int(to = length(x), along.with = path)]
    if (any(x != path))
        stop(errorCondition(paste0(
            sQuote(name), " and expected path do not match\n",
            paste0("* ", format(c(name, "expected")), ": ",
                encodeString(c(
                    paste(x, collapse = "/"),
                    paste(path, collapse = "/")
                ), quote = "\""), collapse = "\n")
        ), call = sys.call(-1L)))
    invisible()
}


check.path <- function (path)
.check.path(path, this.path(verbose = FALSE), "this.path()")


check.dir <- function (path)
.check.path(path, this.dir(verbose = FALSE), "this.dir()")


# path.split(c(
#     this.path(),
#     "~/this.path",
#     "testing//this/out",
#     "/Users/testing/this/out",
#     "//host-name/share-name/path/to/file",
#     "ftp://host/path/to/file",
#     "ftps://host/path/to/file",
#     "http://host/path/to/file",
#     "https://host/path/to/file"
# ))
#
#
# check.path("this.path/R/path.split.R")
# check.path("this.path/R/path.split.")
# check.dir("this.path/R")
# check.dir("this.path/r")
