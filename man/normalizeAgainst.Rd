\name{normalizeAgainst}
\alias{normalizeAgainst}
\title{
Normalize File Paths Against a Different Directory
}
\description{
Convert file paths to canonical form, using a different working directory. By
default, the different working directory would be the executing script's
directory.
}
\usage{
normalizeAgainst(..., against = this.dir(verbose = FALSE))
}
\arguments{
  \item{\dots}{
arguments passed to \code{\link[base]{normalizePath}()}.
  }
  \item{against}{
character string (or possibly \code{NULL}). The directory against which file
paths will be normalized.
  }
}
\details{
When \code{against} is a character string, the working directory is changed to
\code{against}, then the paths are \code{\link[base:normalizePath]{normalized}},
and then the working directory is changed back to its previous value.
}
\value{
A character vector, see \code{\link[base]{normalizePath}}.
}
