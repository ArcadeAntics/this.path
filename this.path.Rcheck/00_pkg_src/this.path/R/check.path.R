URL.pattern <- "^((ftp|ftps|http|https)://[^/]+)(/+(.*))?$"
UNC.pattern <- "^((//[^/]+)/+([^/]+))(/+(.*))?$"


path.split <- function (path)
{
    if (!is.character(path))
        stop(gettextf("invalid '%s' argument", "path"))
    value <- vector("list", length(path))
    isURL <- grepl(URL.pattern, path)
    if (any(isURL))
        value[isURL] <- .path.split.URL(path[isURL])
    isUNC <- !isURL
    path[isUNC] <- if (.Platform$OS.type == "windows")
        chartr("\\", "/", path.expand(path[isUNC]))
    else path.expand(path[isUNC])
    isUNC <- isUNC & grepl(UNC.pattern, path)
    if (any(isUNC))
        value[isUNC] <- .path.split.UNC(path[isUNC])
    leftover <- !isURL & !isUNC
    if (any(leftover))
        value[leftover] <- .path.split.default(path[leftover])
    value
}


.path.split.URL <- function (path)
{
    root <- sub(URL.pattern, "\\1", path)
    path <- sub(URL.pattern, "\\4", path)
    .mapply(c, list(root, strsplit(path, "/+")), NULL)
}


.path.split.UNC <- function (path)
{
    root <- sub(UNC.pattern, "\\2/\\3", path)
    path <- sub(UNC.pattern, "\\5", path)
    .mapply(c, list(root, strsplit(path, "/+")), NULL)
}


.path.split.default <- function (path)
strsplit(path, "/+")


.check.path <- function (path, x, name)
{
    if (!is.character(path) || length(path) != 1L)
        stop("invalid 'path' argument")
    x <- path.split(x)[[1L]]
    path <- path.split(path)[[1L]]
    if (length(x) < length(path) || {
        x <- x[seq.int(to = length(x), along.with = path)]
        any(x != path)
    })
        error(sQuote(name), " and expected path do not match\n",
            paste0("* ", format(c(name, "expected")), ": ",
            encodeString(c(
                paste(x, collapse = "/"),
                paste(path, collapse = "/")
            ), quote = "\""), collapse = "\n"),
            call = sys.call(-1L))
    invisible(TRUE)
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
