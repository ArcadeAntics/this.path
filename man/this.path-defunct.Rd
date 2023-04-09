\name{this.path-defunct}
\alias{this.path-defunct}
\alias{as.relative.path}
\alias{as.rel.path}
\alias{this.path2}
\alias{this.dir2}
\alias{this.dir3}
\alias{normalized.shFILE}
\title{Defunct Functions in Package \pkg{this.path}}
\description{The functions or variables listed here are no longer part of
  \pkg{this.path} as they are no longer needed.}
\usage{
# Defunct in 1.1.0
this.path2(...)
this.dir2(...)
this.dir3(...)
normalized.shFILE(default, else.)

# Defunct in 1.3.0
as.rel.path(path)        # use 'rel2here(path)' instead
as.relative.path(path)   # use 'rel2here(path)' instead
}
\details{
  \code{this.path2()}, \code{this.dir2()}, and \code{this.dir3()} were used
  before \code{\link{this.path}()} and \code{\link{this.dir}()} were given an
  argument \code{default}. Should be replaced by
  \code{\link{this.path}(default = NULL)},
  \code{\link{this.dir}(default = NULL)}, and
  \code{\link{this.dir}(default = \link[base]{getwd}())}, respectively.

  \code{normalized.shFILE()} was used before \code{\link{shFILE}()} was given
  an argument \code{original}. Should be replaced by \code{\link{shFILE}()}.

  \code{as.rel.path()} and \code{as.relative.path()} were poorly chosen names
  given their functionality. Should be replaced by \code{\link{rel2here}()}.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
