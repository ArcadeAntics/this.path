\name{wrap.source}
\alias{wrap.source}
\alias{inside.source}
\alias{set.this.path}
\alias{unset.this.path}
\title{
Implement \code{\link{this.path}()} For Arbitrary \sourcelink{}-Like Functions
}
\description{
A \sourcelink{}-like function is any function which evaluates code from a file.

Currently, \code{\link{this.path}()} is implemented to work with
\sourcesandlistlinks{}.

\code{wrap.source()} and \code{inside.source()} can be used to implement
\code{this.path()} for any other \sourcelink{}-like functions.

\code{set.this.path()} is just an alias for \code{inside.source()}.

\code{unset.this.path()} will undo a call to \code{set.this.path()}. You will
need to use this if you wish to call \code{set.this.path()} multiple times
within a function.
}
\usage{
wrap.source(expr,
    path.only = FALSE,
    character.only = path.only,
    file.only = path.only,
    conv2utf8 = FALSE,
    allow.blank.string = FALSE,
    allow.clipboard = !file.only,
    allow.stdin = !file.only,
    allow.url = !file.only,
    allow.file.uri = !path.only,
    allow.unz = !path.only,
    allow.pipe = !file.only,
    allow.terminal = !file.only,
    allow.textConnection = !file.only,
    allow.rawConnection = !file.only,
    allow.sockconn = !file.only,
    allow.servsockconn = !file.only,
    allow.customConnection = !file.only,
    ignore.all = FALSE,
    ignore.blank.string = ignore.all,
    ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all,
    ignore.url = ignore.all,
    ignore.file.uri = ignore.all)

inside.source(file,
    path.only = FALSE,
    character.only = path.only,
    file.only = path.only,
    conv2utf8 = FALSE,
    allow.blank.string = FALSE,
    allow.clipboard = !file.only,
    allow.stdin = !file.only,
    allow.url = !file.only,
    allow.file.uri = !path.only,
    allow.unz = !path.only,
    allow.pipe = !file.only,
    allow.terminal = !file.only,
    allow.textConnection = !file.only,
    allow.rawConnection = !file.only,
    allow.sockconn = !file.only,
    allow.servsockconn = !file.only,
    allow.customConnection = !file.only,
    ignore.all = FALSE,
    ignore.blank.string = ignore.all,
    ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all,
    ignore.url = ignore.all,
    ignore.file.uri = ignore.all,
    Function = NULL)

set.this.path(file,
    path.only = FALSE,
    character.only = path.only,
    file.only = path.only,
    conv2utf8 = FALSE,
    allow.blank.string = FALSE,
    allow.clipboard = !file.only,
    allow.stdin = !file.only,
    allow.url = !file.only,
    allow.file.uri = !path.only,
    allow.unz = !path.only,
    allow.pipe = !file.only,
    allow.terminal = !file.only,
    allow.textConnection = !file.only,
    allow.rawConnection = !file.only,
    allow.sockconn = !file.only,
    allow.servsockconn = !file.only,
    allow.customConnection = !file.only,
    ignore.all = FALSE,
    ignore.blank.string = ignore.all,
    ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all,
    ignore.url = ignore.all,
    ignore.file.uri = ignore.all,
    Function = NULL)

unset.this.path()
}
\arguments{
  \item{expr}{
an (unevaluated) call to a \sourcenolink{}-like function.
  }
  \item{file}{
a \code{\link[base:connections]{connection}} or a character string giving the
pathname of the file or URL to read from.
  }
  \item{path.only}{
must \code{file} be an existing path? This implies \code{character.only} and
\code{file.only} are \code{TRUE} and implies \code{allow.file.uri} and
\code{allow.unz} are \code{FALSE}, though these can be manually changed.
  }
  \item{character.only}{
must \code{file} be a character string?
  }
  \item{file.only}{
must \code{file} refer to an existing file?
  }
  \item{conv2utf8}{
if \code{file} is a character string, should it be converted to UTF-8?
  }
  \item{allow.blank.string}{
may \code{file} be a blank string, i.e. \code{""}?
  }
  \item{allow.clipboard}{
may \code{file} be \code{"clipboard"} or a clipboard connection?
  }
  \item{allow.stdin}{
may \code{file} be \code{"stdin"}? Note that \code{"stdin"} refers to the
C-level \sQuote{standard input} of the process, differing from
\code{\link[base:showConnections]{stdin}()} which refers to the R-level
\sQuote{standard input}.
  }
  \item{allow.url}{
may \code{file} be a URL pathname or a connection of class \code{"url-libcurl"}
/ / \code{"url-wininet"}?
  }
  \item{allow.file.uri}{
may \code{file} be a \samp{file://} URI?
  }
  \item{allow.unz, allow.pipe, allow.terminal, allow.textConnection,
        allow.rawConnection, allow.sockconn, allow.servsockconn}{
may \code{file} be a connection of class \code{\link[base:connections]{"unz"}}
/ / \code{"pipe"} / / \code{"terminal"} / / \code{"textConnection"} / /
\code{"rawConnection"} / / \code{"sockconn"} / / \code{"servsockconn"}?
  }
  \item{allow.customConnection}{
may \code{file} be a custom connection?
  }
  \item{ignore.all, ignore.blank.string, ignore.clipboard, ignore.stdin,
        ignore.url, ignore.file.uri}{
ignore the special meaning of these types of strings, treating it as a path
instead?
  }
  \item{Function}{
character vector of length 1 or 2; the name of the function and package in
which \code{inside.source()} / / \code{set.this.path()} is called.
  }
}
\details{
\code{inside.source()} should be added to the body of your \sourcenolink{}-like
function before reading / / evaluating the expressions.

\code{wrap.source()}, unlike \code{inside.source()}, does not accept an
argument \code{file}. Instead, an attempt is made to extract the file from
\code{expr}, after which \code{expr} is evaluated. It is assumed that the file
is the first argument of the function, as is the case with
\sources1stargisfileandlistnolinks{}. The function of the call is evaluated,
its \code{\link[base]{formals}()} are retrieved, and then the arguments of
\code{expr} are searched for a name matching the name of the first formal
argument. If a match cannot be found by name, the first unnamed argument is
taken instead. If no such argument exists, the file is assumed missing.

\code{wrap.source()} does non-standard evaluation and does some guess work to
determine the file. As such, it is less desirable than \code{inside.source()}
when the option is available. I can think of exactly one scenario in which
\code{wrap.source()} might be preferable: suppose there is a
\sourcenolink{}-like function \code{sourcelike()} in a foreign package (a
package for which you do not have write permission). Suppose that you write
your own function in which the formals are \code{(\dots)} to wrap
\code{sourcelike()}:

\preformatted{wrapper <- function (...)
{
    # possibly more args to wrap.source()
    wrap.source(sourcelike(...))
}}

This is the only scenario in which \code{wrap.source()} is preferable, since
extracting the file from the \code{\dots} list would be a pain. Then again,
you could simply change the formals of \code{wrapper()} from \code{(\dots)} to
\code{(file, \dots)}. If this does not describe your exact scenario, use
\code{inside.source()} instead.
}
\value{
for \code{wrap.source()}, the result of evaluating \code{expr}.

for \code{inside.source()}, if \code{file} is a path, then the normalized path
with the same attributes, otherwise \code{file} itself. The return value of
\code{inside.source()} should be assigned to a variable before use, something
like:

\preformatted{
{
    file <- inside.source(file, ...)
    sourcelike(file)
}
}
}
\note{
Both functions should only be called within another function.

Suppose that the functions \sourcesandlistnolinks{} were not implemented with
\code{this.path()}. You could use \code{inside.source()} to implement each of
them as follows:

\describe{
  \item{\sourcenolink{}}{
\preformatted{
wrapper <- function(file, ...) {
    file <- inside.source(file)
    source(file = file, ...)
}
}
  }
  \item{\syssourcenolink{}}{
\preformatted{
wrapper <- function(file, ...) {
    file <- inside.source(file, path.only = TRUE)
    sys.source(file = file, ...)
}
}
  }
  \item{\debugSourcenolink{}}{
\preformatted{
wrapper <- function(fileName, ...) {
    fileName <- inside.source(fileName, character.only = TRUE,
        conv2utf8 = TRUE, allow.blank.string = TRUE)
    debugSource(fileName = fileName, ...)
}
}
  }
  \item{\sourcefilenolink{}}{
\preformatted{
wrapper <- function(path, ...) {
    # before testthat_3.1.2, source_file() used readLines() to read the
    # input lines. changed in 3.1.2, source_file() uses
    # brio::read_lines() which normalizes 'path' before reading,
    # disregarding the special meaning of the strings listed above
    path <- inside.source(path, path.only = TRUE, ignore.all =
        as.numeric_version(getNamespaceVersion("testthat")) >= "3.1.2")
    testthat::source_file(path = path, ...)
}
}
  }
  \item{\knitnolink{}}{
\preformatted{
wrapper <- function(input, ...) {
    # this works for the most part, but will not work in child mode
    input <- inside.source(input)
    knitr::knit(input = input, ...)
}
}
  }
  \item{\boxusenolink{}}{
\preformatted{
# 'box' is structured to be an incredible pain in the ass,
# so this solution is not exactly ideal, but it works so whatever
wrapper <- function(path, ...) {
    p <- this.path::path.split.1(path.expand(path))
    n <- length(p)
    prefix <- if (n < 2L)
        "."
    else p[-n]
    name <- p[[n]]
    if (dir.exists(path)) {
        paths <- this.path::path.join(path, paste0("__init__",
            c(".r", ".R")))
        paths <- paths[file.exists(paths)]
        if (!length(paths))
            stop(sprintf(
                "no file \\"__init__.r\\" or \\"__init__.R\\" found in \%s",
                encodeString(path, quote = "\\"")))
        path <- paths[[1L]]
    }
    else {
        x <- this.path::splitext(name)
        if (x[[2L]] \%in\% c(".r", ".R"))
            name <- x[[1L]]
    }
    spec <- list(name = name, prefix = prefix, attach = NULL,
        alias = name, explicit = FALSE)
    class(spec) <- c("box$mod_spec", "box$spec")
    path <- this.path::set.this.path(path, path.only = TRUE)
    info <- list(name = name, source_path =
        if (.Platform$OS.type == "windows")
        chartr("/", "\\\\", path) else path)
    class(info) <- c("box$mod_info", "box$info")
    caller <- parent.frame()
    box:::load_and_register(spec, info, caller)
    invisible()
}
}
  }
}
}
\examples{
FILE <- tempfile(fileext = ".R")
this.path:::write.code({
    this.path::this.path(verbose = TRUE)
}, FILE)


# here we have a source-like function, suppose this
# function is in a package for which you have write permission
sourcelike <- function (file, envir = parent.frame())
{
    file <- inside.source(file)
    envir <- as.environment(envir)
    exprs <- parse(n = -1, file = file)
    # this prints nicely
    this.path:::withAutoprint(exprs = exprs, evaluated = TRUE,
        local = envir, spaced = TRUE, verbose = FALSE,
        width.cutoff = 60L)
    # you could alternatively do:
    # 'for (i in seq_along(exprs)) eval(exprs[i], envir)'
    # which does no pretty printing
}


sourcelike(FILE)
sourcelike(con <- file(FILE)); close(con)


# here we have another source-like function, suppose this function
# is in a foreign package for which you do not have write permission
sourcelike2 <- function (pathname, envir = globalenv())
{
    if (!(is.character(pathname) && file.exists(pathname)))
        stop(gettextf("'\%s' is not an existing file",
             pathname, domain = "R-base"))
    envir <- as.environment(envir)
    exprs <- parse(n = -1, file = pathname)
    this.path:::withAutoprint(exprs = exprs, evaluated = TRUE,
        local = envir, spaced = TRUE, verbose = FALSE,
        width.cutoff = 60L)
}


# the above function is similar to sys.source(), and it
# expects a character string referring to an existing file
#
# with the following, you should be able
# to use 'this.path()' within 'FILE':
wrap.source(sourcelike2(FILE), path.only = TRUE)


# with R >= 4.1.0, use the forward pipe operator '|>' to
# make calls to 'wrap.source' more intuitive:
# sourcelike2(FILE) |> wrap.source(path.only = TRUE)


# 'wrap.source' can recognize arguments by name, so they
# do not need to appear in the same order as the formals
wrap.source(sourcelike2(envir = new.env(), pathname = FILE),
    path.only = TRUE)


# it it much easier to define a new function to do this
sourcelike3 <- function (...)
wrap.source(sourcelike2(...), path.only = TRUE)


# the same as before
sourcelike3(FILE)


# however, this is preferable:
sourcelike4 <- function (pathname, ...)
{
    # pathname is now normalized
    pathname <- inside.source(pathname, path.only = TRUE)
    sourcelike2(pathname = pathname, ...)
}
sourcelike4(FILE)


# perhaps you wish to run several scripts in the same function
fun <- function (paths, ...)
{
    for (pathname in paths) {
        pathname <- set.this.path(pathname, path.only = TRUE)
        sourcelike2(pathname = pathname, ...)
        unset.this.path(pathname)
    }
}
\dontshow{unlink(FILE)}}
