\name{wrap.source}
\alias{wrap.source}
\alias{inside.source}
\title{
Implement \code{this.path()} For Arbitrary \code{source}-Like Functions
}
\description{
A \code{source}-like function is any function which evaluates code from a file.

Currently, \code{\link{this.path}()} is implemented to work with
\sourceandlist{}.

\code{wrap.source()} and \code{inside.source()} can be used to implement
\code{this.path()} for any other \code{source}-like functions.
}
\usage{
wrap.source(expr, file, character.only = FALSE, file.only = FALSE)
inside.source(file, character.only = FALSE, file.only = FALSE)
}
\arguments{
  \item{expr}{
an (unevaluated) call to a \code{source}-like function.
  }
  \item{file}{
a \code{\link[base:connections]{connection}} or a character string giving the
pathname of the file or URL to read from. \code{""} indicates the connection
\code{\link[base:showConnections]{stdin}()}.
  }
  \item{character.only}{
must \code{file} be a character string?
  }
  \item{file.only}{
must \code{file} be an existing file? This means that \code{file = ""},
\code{"clipboard"}, \code{"stdin"}, a file URL, a web URL, or a connection of
class \code{"url-libcurl"}, \code{"url-wininet"}, \code{"terminal"},
\code{"clipboard"}, \code{"pipe"}, or \code{"unz"} will throw an error.
  }
}
\details{
for \code{wrap.source()}, if \code{file} is missing, an attempt is made to
extract the file from \code{expr}. It is assumed that the file is the first
argument of the function, as is the case with \sourceandlist{}. The function of
the call is evaluated, its \code{\link[base]{formals}()} are retrieved, and
then the arguments of \code{expr} are searched for a name matching the name of
the first formal argument. If a match cannot be found by name, the first
unnamed argument is taken instead.

\code{inside.source()} is preferable when the choice is available. It does no
guess work unlike \code{wrap.source()}. If you do not have write permission for
the \code{source}-like function being wrapped (perhaps in a foreign package),
then you will have to use \code{wrap.source()}. For \code{inside.source()},
just add \code{inside.source(file, character.only, file.only)} to the body of
your function.
}
\value{
for \code{wrap.source}, the result of evaluating \code{expr}.

for \code{inside.source}, \code{NULL} invisibly.
}
\note{
Both functions should only be called within another function.
}
\examples{
FILE <- tempfile(fileext = ".R")
this.path:::write.code({
    cat("> this.path::this.path(verbose = TRUE)\\n")
    print(this.path::this.path(verbose = TRUE))
}, FILE)


# here we have a source-like function, suppose this function
# is in a foreign package for which you do not have write permission
sourcelike <- function (pathname, envir = globalenv())
{
    if (!(is.character(pathname) && file.exists(pathname)))
        stop(gettextf("'\%s' is not an existing file", pathname, domain = "R-base"))
    envir <- as.environment(envir)
    exprs <- parse(n = -1, file = pathname, srcfile = NULL, keep.source = FALSE)
    for (i in seq_along(exprs)) eval(exprs[i], envir)
}


# the above function is similar to sys.source(), and it
# expects a character string referring to an existing file
#
# with the following, you should be able to use 'this.path()' within 'FILE':
wrap.source(sourcelike(FILE), character.only = TRUE, file.only = TRUE)


# with R >= 4.1.0, use the forward pipe operator '|>' to
# make calls to 'wrap.source' more intuitive:
# sourcelike(FILE) |> wrap.source(character.only = TRUE, file.only = TRUE)


# 'wrap.source' can recognize arguments by name, so they
# do not need to appear in the same order as the formals
wrap.source(sourcelike(envir = new.env(), pathname = FILE),
    character.only = TRUE, file.only = TRUE)


# it it much easier to define a new function to do this
sourcelike2 <- function (...)
wrap.source(sourcelike(...), character.only = TRUE, file.only = TRUE)


# the same as before
sourcelike2(FILE)


# here we have another source-like function, suppose this
# function is in a package for which you have write permission
sourcelike3 <- function (file, envir = parent.frame())
{
    inside.source(file)
    envir <- as.environment(envir)
    exprs <- parse(n = -1, file = file, srcfile = NULL, keep.source = FALSE)
    for (i in seq_along(exprs)) eval(exprs[i], envir)
}


# no guess work required!
sourcelike3(FILE)
\dontshow{unlink(FILE)}}
