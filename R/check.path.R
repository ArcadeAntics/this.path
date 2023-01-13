

URL.pattern <- "^((?:ftp|ftps|http|https)://[^/]+)(?:/+(.*))?$"
#               ^                                            ^ start and end of the string
#                    ^^^^^^^^^^^^^^^^^^^^^^^                   ftp:// or ftps:// or http:// or https://
#                                           ^^^^^              series of non-slash characters
#                ^                               ^             the root of the URL
#                                                    ^^^^^^    more paths
#                                                           ^  more paths are optional
#                  ^^                              ^^          non-capturing groups


UNC.pattern <- "^(?:(//[^/]+)/+([^/]+))(?:/+(.*))?$"
#               ^                                 ^ start and end of the string
#                    ^^                             two slashes
#                      ^^^^^    ^^^^^               series of non-slash characters, the host and share name
#                            ^^                     one or more slashes
#                                         ^^^^^^    more paths
#                                                ^  more paths are optional
#                 ^^                    ^^          non-capturing groups


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


path.split.1 <- function (path)
{
    if (grepl(URL.pattern, path))
        path.split.URL.1(path)
    else path.split.UNC.and.default.1(path)
}


path.split.URL <- function (path)
{
    # tested a version which uses one call to regmatches and regexec,
    # thinking it might be faster than two calls to sub
    #
    # it's not lmao, this is as fast as it gets
    root <- sub(URL.pattern, "\\1", path)
    rest <- sub(URL.pattern, "\\2", path)
    .mapply(c, list(root, strsplit(rest, "/+")), NULL)
}


path.split.URL.1 <- function (path)
{
    # path <- "https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R"


    c(sub(URL.pattern, "\\1", path), strsplit(sub(URL.pattern, "\\2", path), "/+")[[1L]])
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


path.split.UNC.and.default.1 <- function (path)
{
    path <- if (os.windows)
        chartr("\\", "/", path.expand(path))
    else path.expand(path)
    if (grepl(UNC.pattern, path))
        path.split.UNC.1(path)
    else path.split.default.1(path)
}


path.split.UNC <- function (path)
{
    root <- sub(UNC.pattern, "\\1/\\2", path)
    rest <- sub(UNC.pattern, "\\3"    , path)
    .mapply(c, list(root, strsplit(rest, "/+")), NULL)
}


path.split.UNC.1 <- function (path)
{
    # path <- "//host-name/share-name/path/to/file"


    c(sub(UNC.pattern, "\\1/\\2", path), strsplit(sub(UNC.pattern, "\\3", path), "/+")[[1L]])
}


path.split.default <- function (path)
strsplit(path, "/+")


path.split.default.1 <- function (path)
strsplit(path, "/+")[[1L]]


tmp <- function (x, varname, name)
{
    k <- name
    substitute({
        expected <- path.join(...)
        if (!is.character(expected) || length(expected) != 1L)
            stop(gettextf("'%s' must be a character string", "expected", domain = "R"))
        expected <- path.split.1(expected)
        varname <- .
        varname <- path.split.1(varname)
        if (length(varname) < length(expected) || {
            varname <- varname[seq.int(to = length(varname), along.with = expected)]
            any(varname != expected)
        })
        {
            expected <- paste(expected, collapse = "/")
            varname <- paste(varname, collapse = "/")
            stop(msg1,
                paste0(msg2, {
                    encodeString(c(varname, expected), quote = "\"")
                }, collapse = "\n"))
        }
        invisible(TRUE)
    }, list(
        . = x,
        varname = as.symbol(varname),
        msg1 = sprintf("'%s' and expected path do not match\n", name),
        msg2 = as.call(c(as.symbol("c"), as.list(
                   paste0("* ", format(c(name, "expected")), ": ")
               ))),
        name = name
    ))
}


check.path <- function(...) NULL
body(check.path) <- tmp(quote(.this.path()), "thispath", "this.path()")


check.dir <- function(...) NULL
body(check.dir) <- tmp(quote(.this.dir()), "thisdir", "this.dir()")


rm(tmp)


# this.path:::path.split(c(
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
