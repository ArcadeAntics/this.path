\name{check.path}
\alias{check.path}
\alias{check.dir}
\alias{check.proj}
\title{Check 'this.path()' is Functioning Correctly}
\description{
  Add \code{check.path("path/to/file")} to the start of your script to
  initialize \code{\link{this.path}()} and check that it is returning the
  expected path.
}
\usage{
check.path(...)
check.dir(...)

check.proj(...)
}
\arguments{
  \item{\dots}{further arguments passed to \code{\link{path.join}()} which must
    return a character string; the path you expect \code{this.path()} or
    \code{this.dir()} to return. The specified path can be as deep as necessary
    (just the basename, the last directory and the basename, the last two
    directories and the basename, \ldots), but do not use an absolute path.
    \code{this.path()} makes \R scripts portable, but using an absolute path in
    \code{check.path()} or \code{check.dir()} makes an \R script non-portable,
    defeating a major purpose of this package.}
}
\details{
  \code{check.proj()} is a specialized version of \code{check.path()} that
  checks the path up to the project root.
}
\value{
  if the expected path / / directory matches \code{this.path()} / /
  \code{this.dir()}, then \code{TRUE} invisibly, otherwise an error is thrown.
}
\examples{
# ## I have a project called 'd_cead'
# ##
# ## Within this project, I have a folder called 'code'
# ## where I place all of my scripts.
# ##
# ## One of these scripts is called 'provrun.R'
# ##
# ## So, at the top of that R script, I could write:
#
#
# @R_PACKAGE_NAME@::check.path("d_cead", "code", "provrun.R")
#
# ## or:
#
# @R_PACKAGE_NAME@::check.path("d_cead/code/provrun.R")
}
