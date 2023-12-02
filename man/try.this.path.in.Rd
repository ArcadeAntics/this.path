\name{try.this.path}
\alias{try.this.path}
\alias{try.shFILE}
\title{Attempt to Determine Script's Filename}
\description{
  \code{try.this.path()} attempts to return \code{\link{this.path}()},
  returning \code{this.path(original = TRUE)} if that fails, returning
  \codelink2{base:NA}{NA_character_} if that fails as well.
}
\usage{
try.this.path(contents = FALSE, local = FALSE, n = 0,
              envir = parent.frame(n + 1),
              matchThisEnv = getOption("topLevelEnvironment"),
              srcfile = if (n) sys.parent(n) else 0)

try.shFILE()
}
\arguments{
  \item{contents, local, n, envir, matchThisEnv, srcfile}{See
    \code{?\link{this.path}()}.}
}
\details{
  This should \strong{NOT} be used to construct file paths against the script's
  directory. This should exclusively be used for diagnostic messages / /
  warnings / / errors / / logging. The returned path may not exist, may be
  relative instead of absolute, or may be undefined.
}
\value{
  character string.
}
\examples{
try.shFILE()
try.this.path()
try.this.path(contents = TRUE)
}
