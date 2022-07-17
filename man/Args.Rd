\name{Args}
\alias{Args}
\alias{asArgs}
\alias{fileArgs}
\alias{from.shell}
\alias{withArgs}
\title{
Providing Arguments to a Script
}
\description{
\code{withArgs} allows you \code{\link[base]{source}} \R code while providing
arguments. This would be in the circumstance that you want to run a script and
provide command-line arguments, but want the objects to appear in your
environment, or if you just don't want to \code{\link[base]{shQuote}} the
arguments.

\code{fileArgs} is a generalized version of \code{\link[base]{commandArgs}},
allowing you to access the script's arguments whether it was sourced or run
from a shell.

\code{asArgs} coerces \R objects into a character vector, for use with command
line applications and \code{withArgs}.

\code{from.shell} tells you if the \R script was run from a shell.
}
\usage{
asArgs(...)
fileArgs()
withArgs(expr, ...)

from.shell()
}
\arguments{
  \item{\dots}{
\R objects to turn into scripts arguments; typically
\code{\link[base]{logical}}, \code{\link[base]{numeric}},
\code{\link[base]{character}}, \code{\link[base:Dates]{Date}}, and
\code{\link[base:DateTimeClasses]{POSIXt}} vectors.
  }
  \item{expr}{
an (unevaluated) call to \code{\link[base]{source}},
\code{\link[base]{sys.source}},
\href{https://support.rstudio.com/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE}{
\code{debugSource}}, or
\code{\link[testthat:source_file]{testthat::source_file}}.
  }
}
\details{
\code{\dots} is first put into a list, and then each non-list element is
converted to character. They are converted as follows:
\describe{
  \item{Factors (class \code{"factor"})}{
using \code{\link[base:character]{as.character.factor}}
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
\code{\link[base:character]{as.character}} and its methods.

The arguments will then be unlisted, and all attributes will be removed.
Arguments that are \code{NA_character_} after conversion will be converted to
\code{"NA"} (since the command-line arguments also never have missing strings).

Consider that it may be better to use \code{\link{Rscript}} combined with
\code{\link[base]{save}} and \code{\link[base]{load}} or
\code{\link[base:readRDS]{saveRDS}} and \code{\link[base]{readRDS}}.

Also consider that what you want is a function, and not a script. If you're
already at the \R level, it is easier and more flexible to source a script that
creates a function, and then use that function. This only applies if the script
you are sourcing will never be run from the command-line, or at least will never
be run on its own (it will only be used in the context of other scripts, it will
\emph{NEVER} be used as a stand alone script).
}
\value{
for \code{asArgs} and \code{fileArgs}, a character vector.

for \code{withArgs}, the result of evaluating \code{expr}.
}
\examples{
asArgs(
    NULL,
    c(TRUE, FALSE, NA),
    1:5,
    pi,
    exp(6i),
    letters[1:5],
    as.raw(0:4),
    as.Date("1970-01-01"),
    as.POSIXct("1970-01-01 00:00:00"),
    list(
        list(
            list(
                "lists are recursed"
            )
        )
    )
)


this.path:::write.code(file = FILE <- tempfile(), {
    withAutoprint({


        this.path()
        fileArgs()
        from.shell()


    }, verbose = FALSE)
})


# wrap your source call with a call to 'withArgs'
withArgs(
    source(FILE, local = TRUE, verbose = FALSE),
    letters, pi, exp(1)
)
withArgs(
    sys.source(FILE, environment()),
    letters, pi + 1i * exp(1)
)
this.path:::.Rscript(c("--default-packages=this.path", "--vanilla", FILE,
    asArgs(letters, pi, as.POSIXct("2022-07-17 04:25"))))


# with R >= 4.1.0, use the forward pipe operator '|>' to
# make calls to 'withArgs' more intuitive:
# source(FILE, local = TRUE, verbose = FALSE) |> withArgs(
#     letters, pi, exp(1)
# )
# sys.source(FILE, environment()) |> withArgs(
#     letters, pi + 1i * exp(1)
# )
\dontshow{unlink(FILE)}}
