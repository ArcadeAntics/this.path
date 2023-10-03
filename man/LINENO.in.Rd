\name{LINENO}
\alias{sys.LINENO}
\alias{env.LINENO}
\alias{src.LINENO}
\alias{LINENO}
\title{Line Number of Executing Expression}
\description{
  Get the line number of the executing expression.
}
\usage{
sys.LINENO()
env.LINENO(n = 0, envir = parent.frame(n + 1),
           matchThisEnv = getOption("topLevelEnvironment"))
src.LINENO(n = 0, srcfile = if (n) sys.parent(n) else 0)

LINENO(n = 0, envir = parent.frame(n + 1),
       matchThisEnv = getOption("topLevelEnvironment"),
       srcfile = if (n) sys.parent(n) else 0)
}
\arguments{
  \item{n, envir, matchThisEnv, srcfile}{See \code{?\link{this.path}()}.}
}
\details{
  \code{sys.LINENO()} returns the line number of the most recent expression
  with a source reference and a source file equal to \code{\link{sys.path}()}.

  \code{env.LINENO()} returns the line number of the most recent expression
  with a source reference and a source file equal to \code{\link{env.path}()}.

  \code{src.LINENO()} returns the line number of its source file.

  \code{LINENO()} returns the line number of the most recent expression
  with a source reference and a source file equal to \code{\link{this.path}()}.

  In general, \code{LINENO()} is the most useful. It works whether your \R code
  is \sourcelink-d or modularized.
}
\value{
  An integer, \code{NA_integer_} if the line number cannot be determined.
}
\note{
  \code{LINENO()} only works if the expressions have a
  \codelink2{base:srcfile}{srcref} and a \codelink2{base}{srcfile}.

  Scripts run with \Rscriptlink do not store their \code{srcref}, even when
  \codelink3{base:options}{getOption}{("keep.source")} is \code{TRUE}.

  For \sourcelink or \syssourcelink, make sure to supply argument
  \code{keep.source = TRUE} directly, or set the options \string{keep.source}
  or \string{keep.source.pkgs} to \code{TRUE}.

  For \debugSourceinRStudiolink, it has no argument \code{keep.source}, so set
  the option \string{keep.source} to \code{TRUE} before calling.

  For \compilerloadcmplink, the \code{srcref} is never stored for the compiled
  code, there is nothing that can be done.

  For \knitrknitlink, the \code{srcref} is never stored, there is nothing that
  can be done. I am looking into a fix for such a thing.

  For \targetslink, set option \string{keep.source} to \code{TRUE} before
  calling.

  For \boxuselink, \plumberplumblink, \shinyrunApplink, and
  \testthatsourcefilelink, the \code{srcref} is always stored, so you do not
  need to do anything special before calling.
}
\examples{
FILE.R <- tempfile(fileext = ".R")
writeLines(c(
    "LINENO()",
    "LINENO()",
    "## LINENO() respects #line directives",
    "#line 15",
    "LINENO()",
    "#line 1218",
    "cat(sprintf(\\"invalid value \%d at \%s, line \%d\\n\\",",
    "",
    "            -5, { try.this.path() }, { LINENO() }))"
), FILE.R)


# ## previously used:
#
# source(FILE.R, echo = TRUE, verbose = FALSE,
#     max.deparse.length = Inf, keep.source = TRUE)
#
# ## but it echoes incorrectly with #line directives
this.path:::.source(FILE.R, echo = TRUE, verbose = FALSE,
    max.deparse.length = Inf, keep.source = TRUE)


unlink(FILE.R)
}
