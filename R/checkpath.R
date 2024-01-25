

# URL_pattern <- "^((?:ftp|ftps|http|https)://[^/]+)(?:/+(.*))?$"
# ##              ^                                            ^ start and end of the string
# ##                   ^^^^^^^^^^^^^^^^^^^^^^^                   ftp:// or ftps:// or http:// or https://
# ##                                          ^^^^^              series of non-slash characters
# ##               ^                               ^             the root of the URL
# ##                                                   ^^^^^^    more paths
# ##                                                          ^  more paths are optional
# ##                 ^^                              ^^          non-capturing groups


# UNC_pattern <- "^(?:(//[^/]+)/+([^/]+))(?:/+(.*))?$"
# ##              ^                                 ^ start and end of the string
# ##                   ^^                             two slashes
# ##                     ^^^^^    ^^^^^               series of non-slash characters, the host and share name
# ##                           ^^                     one or more slashes
# ##                                        ^^^^^^    more paths
# ##                                               ^  more paths are optional
# ##                ^^                    ^^          non-capturing groups


# c(
#     "https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R",
#     "//host/share/path/to/file"
# )


check.path <- function (...)
{
    expected <- path.join(...)
    if (!.IS_SCALAR_STR(expected))
        stop(gettextf("'%s' must be a character string", "expected", domain = "R"))
    if (!nzchar(expected))
        stop(gettextf("'%s' must not be \"\"", "expected"))
    expected <- path.split.1(expected)
    if (check_wd <- expected[[1L]] == ".")
        expected <- expected[-1L]
    thispath <- .External2(.C_this_path)
    thispath <- path.split.1(thispath)
    if (length(thispath) < length(expected) || {
        i <- seq_along(thispath) > length(thispath) - length(expected)
        any(thispath[i] != expected)
    })
    {
        expected <- path.unsplit(expected)
        thispath <- path.unsplit(thispath[i])
        stop("'this.path()' and expected path do not match\n",
            paste0(c("* this.path(): ", "* expected   : "), {
                encodeString(c(thispath, expected), quote = "\"")
            }, collapse = "\n"))
    }
    if (check_wd) {
        expected <- path.unsplit(thispath[!i])
        if (relpath(expected) != ".") {
            stop("'getwd()' and expected path do not match\n",
                paste0(c("* getwd() : ", "* expected: "), {
                    encodeString(c(getwd(), expected), quote = "\"")
                }, collapse = "\n"))
        }
    }
    invisible(TRUE)
}


check.dir <- function (...)
{
    expected <- path.join(...)
    if (!.IS_SCALAR_STR(expected))
        stop(gettextf("'%s' must be a character string", "expected", domain = "R"))
    if (!nzchar(expected))
        stop(gettextf("'%s' must not be \"\"", "expected"))
    expected <- path.split.1(expected)
    if (check_wd <- expected[[1L]] == ".")
        expected <- expected[-1L]
    thisdir <- .External2(.C_this_path)
    thisdir <- .dir(thisdir)
    thisdir <- path.split.1(thisdir)
    if (length(thisdir) < length(expected) || {
        i <- seq_along(thisdir) > length(thisdir) - length(expected)
        any(thisdir[i] != expected)
    })
    {
        expected <- path.unsplit(expected)
        thisdir <- path.unsplit(thisdir[i])
        stop("'this.dir()' and expected path do not match\n",
            paste0(c("* this.dir(): ", "* expected  : "), {
                encodeString(c(thisdir, expected), quote = "\"")
            }, collapse = "\n"))
    }
    if (check_wd) {
        expected <- path.unsplit(thisdir[!i])
        if (relpath(expected) != ".") {
            stop("'getwd()' and expected path do not match\n",
                paste0(c("* getwd() : ", "* expected: "), {
                  encodeString(c(getwd(), expected), quote = "\"")
                }, collapse = "\n"))
        }
    }
    invisible(TRUE)
}


check.proj <- function (...)
{
    expected <- path.join(...)
    if (!.IS_SCALAR_STR(expected))
        stop(gettextf("'%s' must be a character string", "expected", domain = "R"))
    if (!nzchar(expected))
        stop(gettextf("'%s' must not be \"\"", "expected"))
    expected <- path.split.1(expected)
    if (check_wd <- expected[[1L]] == ".")
        expected <- expected[-1L]
    thispath <- .External2(.C_this_path)
    thisproj <- .proj(.dir(thispath))
    thispath <- path.split.1(thispath)
    thisproj <- path.split.1(thisproj)
    i <- seq_along(thispath) > length(thisproj)
    if (sum(i) != length(expected) || any(thispath[i] != expected))  {
        expected <- path.unsplit(expected)
        thispath <- path.unsplit(thispath[i])
        stop("path within project and expected path do not match\n",
             paste0(c("* project path: ", "* expected    : "), {
                 encodeString(c(thispath, expected), quote = "\"")
             }, collapse = "\n"))
    }
    if (check_wd) {
        expected <- path.unsplit(thispath[!i])
        if (relpath(expected) != ".") {
            stop("'getwd()' and expected path do not match\n",
                 paste0(c("* getwd() : ", "* expected: "), {
                     encodeString(c(getwd(), expected), quote = "\"")
                 }, collapse = "\n"))
        }
    }
    invisible(TRUE)
}


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
# check.path("this.path/R/checkpath.R")
# check.path("this.path/R/checkpath.")
# check.dir("this.path/R")
# check.dir("this.path/r")
