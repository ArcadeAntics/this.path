\name{LINENO}
\alias{LINENO}
\title{
Line Number of Executing Script
}
\description{
Get the line number of the executing script.
}
\usage{
LINENO()
}
\value{
An integer, \code{NA_integer_} if the line number cannot be determined.
}
\note{
\code{LINENO()} only works if the executing script has a
\code{\link[base:srcfile]{srcref}} and a \code{\link[base]{srcfile}}. Scripts
run with \code{\link[utils]{Rscript}} do not store their \code{srcref}, even
when \code{\link[base:options]{getOption}("keep.source")} is \code{TRUE}.

For \sourcelink{} or \syssourcelink{}, make sure to supply argument
\code{keep.source = TRUE} directly, or set the options \code{"keep.source"} or
\code{"keep.source.pkgs"} to \code{TRUE}.

For \debugSource{}, it has no argument \code{keep.source}, so set the option
\code{"keep.source"} to \code{TRUE} before calling.

For \sourcefilelink{}, the \code{srcref} is always stored, so you do not need
to do anything special before calling.

For \knitlink{}, there is nothing that can be done, the \code{srcref} is never
stored. I'm looking into a fix for such a thing.
}
\examples{
FILE <- tempfile(fileext = ".R")
writeLines(c(
    "LINENO()",
    "LINENO()",
    "# LINENO() respects #line directives",
    "#line 1218",
    "LINENO()"
), FILE)
# source(FILE, echo = TRUE, keep.source = TRUE)
#
# 'source(echo = TRUE, keep.source = TRUE)' does
# not respect #line directives when echoing
# 'source2' does
this.path:::source2(FILE)
\dontshow{unlink(FILE)}}
