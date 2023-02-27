\name{check.path}
\alias{check.path}
\alias{check.dir}
\alias{check.proj}
\title{
Check \code{\link{this.path}()} is Functioning Correctly
}
\description{
Add \code{check.path("path/to/file")} to the beginning of your script to
initialize \code{\link{this.path}()}, and check that \code{this.path()} is
returning the path you expect.
}
\usage{
check.path(...)
check.dir(...)

check.proj(...)
}
\arguments{
  \item{\dots}{
further arguments passed to \code{\link{path.join}()} which must return a
character string; the path you expect \code{this.path()} or \code{this.dir()}
to return. The specified path can be as deep as necessary (just the basename,
the last directory and the basename, the last two directories and the basename,
\ldots), but using an absolute path is not intended (recommended against).
\code{this.path()} makes \R scripts portable, but using an absolute path in
\code{check.path()} or \code{check.dir()} makes an \R script non-portable,
defeating the whole purpose of this package.
  }
}
\details{
\code{check.proj()} is a specialized version of \code{check.path()} that checks
the path all the way up to the project's directory.
}
\value{
If the expected path / / directory matches \code{this.path()} / /
\code{this.dir()}, then \code{TRUE} invisibly.

Otherwise, an error is raised.
}
\examples{
# I have a project called 'EOAdjusted'
#
# Within this project, I have a folder called 'code'
# where I place all of my scripts.
#
# One of these scripts is called 'provrun.R'
#
# So, at the top of that R script, I could write:


# this.path::check.path("EOAdjusted", "code", "provrun.R")
#
# or
#
# this.path::check.path("EOAdjusted/code/provrun.R")
}
