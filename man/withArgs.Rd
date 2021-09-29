\name{withArgs}
\alias{withArgs}
\title{
Source R Code, Providing Arguments to the Script
}
\description{
Evaluate \R code from a named file or URL or connection while providing
arguments. This would be in the circumstance that you want to run a script and
provide command-line arguments, but want the objects to appear in your
environment.
}
\usage{
withArgs(expr, ...)
}
\arguments{
  \item{expr}{
a call to \code{\link[base]{source}}, \code{\link[base]{sys.source}},
\href{https://support.rstudio.com/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE}{
\code{debugSource}}, or
\code{\link[testthat:source_file]{testthat::source_file}}.
  }
  \item{\dots}{
the arguments provided to the script. See section \strong{Details}.
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
the value returned by evaluating \code{expr}, see the help pages of the above
source functions.
}
\examples{
FILE <- tempfile()
writeLines(deparse(quote(withAutoprint({


    this.path()
    Args()


}, verbose = FALSE))), FILE)


# wrap your source call with a call to `withArgs`
withArgs(
    source(FILE, local = TRUE, verbose = FALSE),
    letters, pi, exp(1)
)
withArgs(
    sys.source(FILE, environment()),
    letters, pi + 1i * exp(1)
)


# with R >= 4.1.0, use the forward pipe operator `|>` to
# make calls to `withArgs` more intuitive:
# source(FILE, local = TRUE, verbose = FALSE) |> withArgs(
#     letters, pi, exp(1)
# )
# sys.source(FILE, environment()) |> withArgs(
#     letters, pi + 1i * exp(1)
# )
\dontshow{unlink(FILE)}}
