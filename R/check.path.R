

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


# c(
#     "https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R",
#     "//host/share/path/to/file"
# )


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
