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

  For \utilsSweavelink, the \code{srcref} is never stored, there is nothing
  that can be done.

  For \knitrknitlink, the \code{srcref} is never stored, there is nothing that
  can be done. I am looking into a fix.

  For \targetslink, set option \string{keep.source} to \code{TRUE} before
  calling associated functions.

  For \boxuselink, \plumberplumblink, \shinyrunApplink, and
  \testthatsourcefilelink, the \code{srcref} is always stored.
}
\value{
  integer; \code{NA_integer_} if the line number is not found.
}
\note{
  You can get a more accurate line number by wrapping \code{LINENO()} in
  braces: \preformatted{{ LINENO() }}
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


if (getRversion() >= "4.3.0") {
    source(FILE.R, echo = TRUE, verbose = FALSE,
        max.deparse.length = Inf, keep.source = TRUE)
} else {
    @R_PACKAGE_NAME@:::.source(FILE.R, echo = TRUE, verbose = FALSE,
        max.deparse.length = Inf, keep.source = TRUE)
}


unlink(FILE.R)
}
