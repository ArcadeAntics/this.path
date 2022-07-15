URL.pattern <- "^((ftp|ftps|http|https)://[^/]+)(/+(.*))?$"
UNC.pattern <- "^((//[^/]+)/+([^/]+))(/+(.*))?$"


path.split <- function (path)
{
    if (!is.character(path))
        stop(gettextf("invalid '%s' argument", "path"))
    value <- vector("list", length(path))
    if (any(URL <- grepl(URL.pattern, path)))
        value[URL] <- path.split.URL(path[URL])
    if (any(leftover <- !URL))
        value[leftover] <- path.split.UNC.and.default(path[leftover])
    value
}


path.split.URL <- function (path)
{
    root <- sub(URL.pattern, "\\1", path)
    path <- sub(URL.pattern, "\\4", path)
    .mapply(c, list(root, strsplit(path, "/+")), NULL)
}


path.split.UNC.and.default <- function (path)
{
    value <- vector("list", length(path))
    path <- if (os.windows)
        chartr("\\", "/", path.expand(path))
    else path.expand(path)
    if (any(UNC <- grepl(UNC.pattern, path)))
        value[UNC] <- path.split.UNC(path[UNC])
    if (any(leftover <- !UNC))
        value[leftover] <- path.split.default(path[leftover])
    value
}


path.split.UNC <- function (path)
{
    root <- sub(UNC.pattern, "\\2/\\3", path)
    path <- sub(UNC.pattern, "\\5", path)
    .mapply(c, list(root, strsplit(path, "/+")), NULL)
}


path.split.default <- function (path)
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
        stop(Error(
            sQuote(name), " and expected path do not match\n",
            paste0("* ", format(c(name, "expected")), ": ",
            encodeString(c(
                paste(x, collapse = "/"),
                paste(path, collapse = "/")
            ), quote = "\""), collapse = "\n"),
            call = sys.call(sys.parent())))
    invisible(TRUE)
}


check.path <- function (...)
.check.path(path = file.path(...), this.path(verbose = FALSE), "this.path()")


check.dir <- function (...)
.check.path(path = file.path(...), this.dir(verbose = FALSE), "this.dir()")


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
# check.path("this.path/R/check.path.R")
# check.path("this.path/R/check.path.")
# check.dir("this.path/R")
# check.dir("this.path/r")
