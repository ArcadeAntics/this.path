\name{LINENO}
\alias{LINENO}
\title{Line Number of Executing Expression}
\description{
  Get the line number of the executing expression.
}
\usage{
LINENO(n = 0, envir = parent.frame(n + 1),
       matchThisEnv = getOption("topLevelEnvironment"),
       srcfile = if (n) sys.parent(n) else 0)
}
\arguments{
  \item{n, envir, matchThisEnv, srcfile}{See \code{?\link{this.path}()}.}
}
\details{
  \code{LINENO()} starts by examining argument \code{srcfile}. If it is a
  \code{srcref} or contains one, it will return the \code{first_line} number.

  If it does not find a line number associated with \code{srcfile}, it will
  look for a path in a similar manner to \code{\link{this.path}()} except it
  skips the \code{srcfile} and GUI aspects. If it finds a path, it will look
  for the most recent expression that has a source reference and has a source
  file equal to the path. If it finds one, it will return the \code{first_line}
  number.

  If it does not find a line number, it will return \code{NA_integer_}.
}
\value{
  integer.
}
\note{
  \code{LINENO()} only works if the expressions have a
  \codelink2{base:srcfile}{srcref}.

  Scripts run with \Rscriptlink do not store their \code{srcref}, even when
  \codelink3{base:options}{getOption}{("keep.source")} is \code{TRUE}.

  For \sourcelink and \syssourcelink, make sure to supply argument
  \code{keep.source = TRUE} directly, or set options \string{keep.source} and
  \string{keep.source.pkgs} to \code{TRUE}.

  For \debugSourceinRStudiolink, it has no argument \code{keep.source}, so set
  option \string{keep.source} to \code{TRUE} before calling.

  For \compilerloadcmplink, the \code{srcref} is never stored for the compiled
  code, there is nothing that can be done.

  For \knitrknitlink, the \code{srcref} is never stored, there is nothing that
  can be done. I am looking into a fix.

  For \targetslink, set option \string{keep.source} to \code{TRUE} before
  calling.

  For \boxuselink, \plumberplumblink, \shinyrunApplink, and
  \testthatsourcefilelink, the \code{srcref} is always stored.
}
\examples{
FILE.R <- tempfile(fileext = ".R")
writeLines(c("
LINENO()
LINENO()
## LINENO() respects #line directives
#line 15
LINENO()
#line 1218
cat(sprintf('invalid value \%d at \%s, line \%d\\\\n',
            -5, try.this.path(), LINENO()))
"), FILE.R)


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
