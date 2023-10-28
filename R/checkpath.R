

# URL.pattern <- "^((?:ftp|ftps|http|https)://[^/]+)(?:/+(.*))?$"
# ##              ^                                            ^ start and end of the string
# ##                   ^^^^^^^^^^^^^^^^^^^^^^^                   ftp:// or ftps:// or http:// or https://
# ##                                          ^^^^^              series of non-slash characters
# ##               ^                               ^             the root of the URL
# ##                                                   ^^^^^^    more paths
# ##                                                          ^  more paths are optional
# ##                 ^^                              ^^          non-capturing groups


# UNC.pattern <- "^(?:(//[^/]+)/+([^/]+))(?:/+(.*))?$"
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


tmp <- function (x, varname, name)
{
    varname <- as.symbol(varname)
    expr <- bquote({
        expected <- path.join(...)
        if (!.IS_SCALAR_STR(expected))
            stop(gettextf("'%s' must be a character string", "expected", domain = "R"))
        if (!nzchar(expected))
            stop(gettextf("'%s' must not be \"\"", "expected"))
        expected <- path.split.1(expected)
        if (check.wd <- expected[[1L]] == ".")
            expected <- expected[-1L]
        ..(x)
        varname <- path.split.1(varname)
        if (length(varname) < length(expected) || {
            i <- seq_along(varname) > length(varname) - length(expected)
            any(varname[i] != expected)
        })
        {
            expected <- path.unsplit(expected)
            varname <- path.unsplit(varname[i])
            stop(msg1,
                paste0(msg2, {
                    encodeString(c(varname, expected), quote = "\"")
                }, collapse = "\n"))
        }
        if (check.wd) {
            expected <- path.unsplit(varname[!i])
            if (relpath(expected) != ".") {
                stop("'getwd()' and expected path do not match\n",
                    paste0(c("* getwd() : ", "* expected: "), {
                        encodeString(c(getwd(), expected), quote = "\"")
                    }, collapse = "\n"))
            }
        }
        invisible(TRUE)
    }, splice = TRUE)
    eval(call("substitute", expr, list(
        varname = varname,
        msg1 = sprintf("'%s' and expected path do not match\n", name),
        msg2 = as.call(c(as.symbol("c"), as.list(
                   paste0("* ", format(c(name, "expected")), ": ")
               ))),
        name = name
    )))
}


check.path <- eval(call("function", as.pairlist(alist(... = )),
tmp(expression(varname <- .External2(.C_this.path)), "thispath", "this.path()")
))


check.dir <- eval(call("function", as.pairlist(alist(... = )),
tmp(expression(varname <- .External2(.C_this.path), varname <- .dir(varname)), "thisdir", "this.dir()")
))


rm(tmp)


check.proj <- function (...)
{
    expected <- path.join(...)
    if (!.IS_SCALAR_STR(expected))
        stop(gettextf("'%s' must be a character string", "expected", domain = "R"))
    if (!nzchar(expected))
        stop(gettextf("'%s' must not be \"\"", "expected"))
    expected <- path.split.1(expected)
    if (check.wd <- expected[[1L]] == ".")
        expected <- expected[-1L]
    thispath <- .External2(.C_this.path)
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
    if (check.wd) {
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
