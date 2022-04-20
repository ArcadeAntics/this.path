\name{shFILE}
\alias{shFILE}
\alias{normalized.shFILE}
\title{
Get Argument \option{FILE} Provided to R by a Shell
}
\description{
Look through the command line arguments, extracting argument \option{FILE} from
either of the following: \option{--file=FILE} or \option{-f} \option{FILE}
}
\usage{
shFILE()
normalized.shFILE()
}
\details{
For \code{shFILE}, if there are no such arguments, \code{NA} is returned.

For \code{normalized.shFILE}, if there are no such arguments, an error is
raised.

For both functions, if there are multiple such arguments, an error is
raised.
}
\value{
character string. For \code{normalized.shFILE}, an absolute path.
}
\note{
Both functions will save their return values; this makes them faster when
called subsequent times.

For \code{normalized.shFILE}, the path on Windows will use / as the file
separator.
}
\seealso{
\code{\link{this.path}}, \code{\link{here}}
}
