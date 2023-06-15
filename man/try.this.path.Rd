\name{try.this.path}
\alias{try.sys.path}
\alias{try.env.path}
\alias{try.this.path}
\alias{try.shFILE}
\title{Attempt to Determine Current Script's Filename}
\description{
  \code{try.sys.path()} attempts to return the
  \ifelse{html}{\codelink2{base:normalizePath}{normalized}}{normalized} path of
  the executing script, returning the original path of the executing script if
  that fails, returning \codelink2{base:NA}{NA_character_} if that fails as
  well.

  \code{try.env.path()} and \code{try.this.path()} do the same with the top
  level environment's path or the current script's path.

  \code{try.shFILE()} attempts to extract and
  \ifelse{html}{\codelink2{base:normalizePath}{normalized}}{normalized}
  \option{FILE} from either of the following: \option{-f} \option{FILE} or
  \option{--file=FILE}, returning the original \option{FILE} if that fails,
  returning \codelink2{base:NA}{NA_character_} if that fails as well.
}
\usage{
try.sys.path(contents = FALSE)
try.env.path(contents = FALSE, envir = parent.frame(),
             matchThisEnv = getOption("topLevelEnvironment"))
try.this.path(contents = FALSE, envir = parent.frame(),
              matchThisEnv = getOption("topLevelEnvironment"))
try.shFILE()
}
\arguments{
  \item{contents}{\code{TRUE} or \code{FALSE}; should the contents of the
    executing script be returned, \code{NULL} if that fails?}

  \item{envir, matchThisEnv}{See \code{?\link{this.path}()}.}
}
\details{
  This should \strong{NOT} be used to construct file paths against the
  executing script's directory. This should exclusively be used in the scenario
  that you would like the normalized path of the executing script, perhaps for
  a diagnostic message, but it is not required to exist and can be a relative
  path or undefined.
}
\value{
  character string.
}
\seealso{
  \code{\link{sys.path}()}, \code{\link{env.path}()}, \code{\link{this.path}()}

  \code{\link{shFILE}()}
}
\examples{
try.shFILE()
try.this.path()
try.this.path(contents = TRUE)
}
