\name{try.this.path}
\alias{try.sys.path}
\alias{try.env.path}
\alias{try.src.path}
\alias{try.this.path}
\alias{try.shFILE}
\title{Attempt to Determine Current Script's Filename}
\description{
  \code{try.sys.path()} attempts to return \code{\link{sys.path}()}, returning
  \code{sys.path(original = TRUE)} if that fails, returning
  \codelink2{base:NA}{NA_character_} if that fails as well.

  \code{try.env.path()}, \code{try.src.path()}, \code{try.this.path()}, and
  \code{try.shFILE} do the same with \code{\link{env.path}()},
  \code{\link{src.path}()}, \code{\link{this.path}()}, and
  \code{\link{shFILE}()}.
}
\usage{
try.sys.path(contents = FALSE, local = FALSE)
try.env.path(contents = FALSE, envir = parent.frame(),
             matchThisEnv = getOption("topLevelEnvironment"))
try.src.path(contents = FALSE, srcfile = sys.call())

try.this.path(contents = FALSE, local = FALSE,
              envir = parent.frame(),
              matchThisEnv = getOption("topLevelEnvironment"),
              srcfile = sys.call())

try.shFILE()
}
\arguments{
  \item{contents, local}{See \code{?\link{sys.path}()}.}

  \item{envir, matchThisEnv}{See \code{?\link{env.path}()}.}

  \item{srcfile}{See \code{?\link{src.path}()}.}
}
\details{
  This should \strong{NOT} be used to construct file paths against the current
  script's directory. This should exclusively be used in the scenario that you
  would like the normalized path of the executing script, perhaps for a
  diagnostic message, but it is not required to exist and can be a relative
  path or undefined.
}
\value{
  character string.
}
\examples{
try.shFILE()
try.this.path()
try.this.path(contents = TRUE)
}
