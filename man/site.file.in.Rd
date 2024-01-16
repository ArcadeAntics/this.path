\name{startup_files}
\alias{site.file}
\alias{init.file}
\alias{with_site.file}
\alias{with_init.file}
\title{Files Related to Initialization of the R Session}
\description{
  \code{site.file()} and \code{init.file()} return the
  \ifelse{html}{\codelink2{base:normalizePath}{normalized}}{normalized} paths
  of the site-wide startup profile file and the user profile that were run at
  \codelink2{base:Startup}{startup}.

  \code{with_init.file()} declares that the current script is the user profile
  then evaluates and auto-prints the sub-expressions of its argument.
}
\usage{
site.file(original = FALSE, for.msg = FALSE, default, else.)
init.file(original = FALSE, for.msg = FALSE, default, else.)

with_site.file(expr)
with_init.file(expr)
}
\arguments{
  \item{original, for.msg, default, else.}{Same as \code{\link{shFILE}()}.}

  \item{expr}{a braced expression, the sub-expressions of which to evaluate and
    auto-print.}
}
\value{
  for \code{site.file()} and \code{init.file()}, a character string, or
  \code{default} if it was not found.

  for \code{with_site.file()} and \code{with_init.file()}, \code{NULL}
  invisibly.
}
\note{
  \code{with_site.file()} is unneeded now that the site-wide startup profile
  file can be automatically detected.
}
\examples{
## if you wish to use this.path() in a user profile,
## instead of writing:
##
## <expr 1>
## <expr 2>
## <...>
##
## write this:
##
## this.path::with_init.file({
## <expr 1>
## <expr 2>
## <...>
## })
}
