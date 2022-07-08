\name{shFILE}
\alias{shFILE}
\alias{normalized.shFILE}
\title{
Get Argument \option{FILE} Provided to R by a Shell
}
\description{
Look through the command line arguments, extracting \option{FILE} from either
of the following: \option{--file=FILE} or \option{-f} \option{FILE}
}
\usage{
shFILE(default)
normalized.shFILE(default)
}
\arguments{
  \item{default}{
if \option{FILE} was not found, this value is returned.
  }
}
\value{
character string, or \code{default} if the command line argument \option{FILE}
was not found.
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
