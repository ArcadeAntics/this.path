\name{LINENO}
\alias{LINENO}
\title{Line Number of Executing Script}
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
  \codelink2{base:srcfile}{srcref} and a \codelink2{base}{srcfile}. Scripts run
  with \Rscriptlink do not store their \code{srcref}, even when
  \codelink3{base:options}{getOption}{("keep.source")} is \code{TRUE}.

  For \sourcelink or \syssourcelink, make sure to supply argument
  \code{keep.source = TRUE} directly, or set the options \string{keep.source}
  or \string{keep.source.pkgs} to \code{TRUE}.

  For \debugSourceinRStudiolink, it has no argument \code{keep.source}, so set
  the option \string{keep.source} to \code{TRUE} before calling.

  For \testthatsourcefilelink, the \code{srcref} is always stored, so you do
  not need to do anything special before calling.

  For \knitrknitlink, the \code{srcref} is never stored, there is nothing that
  can be done. I am looking into a fix for such a thing.

  For \compilerloadcmplink, the \code{srcref} is never stored for the compiled
  code, there is nothing that can be done.

  For \boxuselink, the \code{srcref} is always stored, so you do not need to do
  anything special before calling.
}
\examples{
FILE <- tempfile(fileext = ".R")
writeLines(c(
    "LINENO()",
    "LINENO()",
    "## LINENO() respects #line directives",
    "#line 1218",
    "LINENO()"
), FILE)


# ## previously used:
#
# source(FILE, echo = TRUE, verbose = FALSE,
#     max.deparse.length = Inf, keep.source = TRUE)
#
# ## but it echoes incorrectly with #line directives.
# ## source2() echoes correctly!
this.path:::source2(FILE, echo = TRUE, verbose = FALSE,
    max.deparse.length = Inf, keep.source = TRUE)
\dontshow{unlink(FILE)}}
