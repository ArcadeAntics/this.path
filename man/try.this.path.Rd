\name{try.this.path}
\alias{try.this.path}
\alias{try.shFILE}
\title{Attempt to Determine Executing Script's Filename}
\description{
  \code{try.this.path()} attempts to return the
  \code{\link[base:normalizePath]{normalized}} path of the executing script,
  returning the original path of the executing script if that fails, and
  returning \code{\link[base:NA]{NA_character_}} if that fails as well.

  \code{try.shFILE()} attempts to extract and
  \code{\link[base:normalizePath]{normalize}} \option{FILE} from either of the
  following: \option{-f} \option{FILE} or \option{--file=FILE}, returning the
  original \option{FILE} if that fails, and returning
  \code{\link[base:NA]{NA_character_}} if that fails as well.
}
\usage{
try.this.path()
try.shFILE()
}
\details{
  This should \strong{NOT} be used to construct file paths against the
  executing script's directory. This should exclusively be used in the scenario
  that you would like the normalized path of the executing script, perhaps for
  a diagnostic message, but it is not required to exist and can be a relative
  path or undefined.
}
\value{
  character string
}
\seealso{
  \code{\link{this.path}()}

  \code{\link{shFILE}()}
}
\examples{
try.this.path()

try.shFILE()
}
