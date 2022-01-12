\name{check.path}
\alias{check.path}
\alias{check.dir}
\title{
Check \code{this.path()} is Functioning Correctly
}
\description{
Add \code{check.path("path/to/file")} to the beginning of your script to
initialize \code{this.path()}, and also to check that \code{this.path()} is
returning the path you expect.
}
\usage{
check.path(path)
check.dir (path)
}
\arguments{
  \item{path}{
character string; the path you expect \code{this.path()} or \code{this.dir()}
to return. \code{path} can be as deep as necessary (just the basename, the last
directory name and the basename, the last two directory names and the basename,
...), but using an absolute path is not intended (recommended against).
\code{this.path} makes \R scripts portable, but using an absolute path in
\code{check.path} or \code{check.dir} makes an \R script non-portable, so it
defeats the whole point.
  }
}
\value{
If the expected path / / directory matches \code{this.path} / / \code{this.dir},
then \code{TRUE} returned invisibly.

Otherwise, an error is raised.
}
\examples{
# this.path::check.path("EOAdjusted/code/provrun.R")
}
