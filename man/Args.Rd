\name{Args}
\alias{Args}
\alias{asArgs}
\alias{fileArgs}
\alias{progArgs}
\alias{withArgs}
\title{
Providing Arguments to a Script
}
\description{
\code{withArgs()} allows you to \sourcelink{} an \R script while providing
arguments. As opposed to running with \code{\link[utils]{Rscript}}, the code
will be evaluated in the same session, in an environment of your choosing.

\code{fileArgs()} / / \code{progArgs()} are generalized versions of
\code{\link[base]{commandArgs}(trailingOnly = TRUE)}, allowing you to access
the script's arguments whether it was sourced or run from a shell.

\code{asArgs()} coerces \R objects into a character vector, for use with
command line applications and \code{withArgs()}.
}
\usage{
asArgs(...)
fileArgs()
progArgs()
withArgs(...)
}
\arguments{
  \item{\dots}{
\R objects to turn into scripts arguments; typically
\code{\link[base]{logical}}, \code{\link[base]{numeric}},
\code{\link[base]{character}}, \code{\link[base:Dates]{Date}}, and
\code{\link[base:DateTimeClasses]{POSIXt}} vectors.

for \code{withArgs()}, the first argument should be an (unevaluated) call to
\sourcescompatiblewithwithArgslistlinks{}, or a \sourcenolink{}-like function
containing \code{\link{wrap.source}()} / / \code{\link{inside.source}()} / /
\code{\link{set.this.path}()}.
  }
}
\details{
\code{fileArgs()} will return the arguments associated with the executing
script, or \code{character(0)} when there is no executing script.

\code{progArgs()} will return the arguments associated with the executing
script, or \code{commandArgs(trailingOnly = TRUE)} when there is no executing
script.

\code{asArgs()} coerces objects into command-line arguments. \code{\dots} is
first put into a list, and then each non-list element is converted to
character. They are converted as follows:
\describe{
  \item{Factors (class \code{"factor"})}{
using \code{\link[base:character]{as.character.factor}()}
  }
  \item{Date-Times (class \code{"POSIXct"} and \code{"POSIXlt"})}{
using format \code{"\%Y-\%m-\%d \%H:\%M:\%OS6"} (retains as much precision as
possible)
  }
  \item{Numbers (class \code{"numeric"} and \code{"complex"})}{
with 17 significant digits (retains as much precision as possible) and
\code{"."} as the decimal point character.
  }
  \item{Raw Bytes (class \code{"raw"})}{
using \code{\link[base]{sprintf}("0x\%02x", )} (can easily convert back to raw
with \code{\link[base:raw]{as.raw}()} or
\code{\link[base:vector]{as.vector}(, "raw")})
  }
}
All others will be converted to character using
\code{\link[base:character]{as.character}()} and its methods.

The arguments will then be unlisted, and all attributes will be removed.
Arguments that are \code{NA_character_} after conversion will be converted to
\code{"NA"} (since the command-line arguments also never have missing strings).


}
\value{
for \code{asArgs()}, \code{fileArgs()}, and \code{progArgs()}, a character
vector.

for \code{withArgs()}, the result of evaluating the first argument.
}
\examples{
this.path::asArgs(NULL, c(TRUE, FALSE, NA), 1:5, pi, exp(6i),
    letters[1:5], as.raw(0:4), Sys.Date(), Sys.time(),
    list(list(list("lists are recursed"))))


FILE <- tempfile(fileext = ".R")
this.path:::write.code({
    this.path:::withAutoprint({
        this.path::this.path()
        this.path::fileArgs()
        this.path::progArgs()
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L)
}, FILE)


# wrap your source call with a call to withArgs()
this.path::withArgs(
    source(FILE, local = TRUE, verbose = FALSE),
    letters[6:10], pi, exp(1)
)
this.path::withArgs(
    sys.source(FILE, environment()),
    letters[11:15], pi + 1i * exp(1)
)
this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE,
    this.path::asArgs(letters[16:20], pi, Sys.time())))
# fileArgs() will be character(0) because there is no executing script
this.path:::.Rscript(c("--default-packages=NULL", "--vanilla",
    rbind("-e", readLines(FILE)[-2L]),
    this.path::asArgs(letters[16:20], pi, Sys.time())))


# with R >= 4.1.0, use the forward pipe operator '|>' to
# make calls to withArgs() more intuitive:
# source(FILE, local = TRUE, verbose = FALSE) |> this.path::withArgs(
#     letters[6:10], pi, exp(1))
# sys.source(FILE, environment()) |> this.path::withArgs(
#     letters[11:15], pi + 1i * exp(1))


# withArgs() also works with inside.source() and wrap.source()
sourcelike <- function (file, envir = parent.frame())
{
    file <- inside.source(file)
    envir <- as.environment(envir)
    exprs <- parse(n = -1, file = file)
    for (i in seq_along(exprs)) eval(exprs[i], envir)
}
this.path::withArgs(sourcelike(FILE), letters[21:26])


sourcelike2 <- function (file, envir = parent.frame())
{
    envir <- as.environment(envir)
    exprs <- parse(n = -1, file = file)
    for (i in seq_along(exprs)) eval(exprs[i], envir)
}
sourcelike3 <- function (file, envir = parent.frame())
{
    envir <- as.environment(envir)
    wrap.source(sourcelike2(file = file, envir = envir))
}
this.path::withArgs(sourcelike3(FILE), LETTERS[1:5])
this.path::withArgs(wrap.source(sourcelike2(FILE)), LETTERS[6:10])
\dontshow{unlink(FILE)}}
