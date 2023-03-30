\name{ext}
\alias{splitext}
\alias{removeext}
\alias{ext}
\alias{ext<-}
\title{
File Extensions
}
\description{
\code{splitext()} splits an extension from a path.

\code{removeext()} removes an extension from a path.

\code{ext()} gets the extension of a path.

\code{ext<-()} sets the extension of a path.
}
\usage{
splitext(path, compression = FALSE)
removeext(path, compression = FALSE)
ext(path, compression = FALSE)
ext(path, compression = FALSE) <- value
}
\arguments{
  \item{path}{
character vector, containing path names.
  }
  \item{compression}{
should compression extensions \sQuote{.gz}, \sQuote{.bz2}, and \sQuote{.xz} be
taken into account when removing/getting an extension?
  }
  \item{value}{
a character vector, typically of length \code{1} or \code{length(path)}, or
\code{NULL}.
  }
}
\details{
\code{\link[base:path.expand]{tilde expansion}} of the path will be performed.

Trailing path separators are removed before dissecting the path.

Except for \code{path <- NA_character_}, it will always be true that
\code{path == paste0(removeext(path), ext(path))}.
}
\value{
for \code{splitext()}, a matrix with 2 rows and \code{length(path)} columns.
The first row will be the roots of the paths, the second row will be the
extensions of the paths.

for \code{removeext()} and \code{ext()}, a character vector the same length as
\code{path}.

for \code{ext<-()}, the updated object.
}
\examples{
splitext(character(0))
splitext("")

splitext("file.ext")

path <- c("file.tar.gz", "file.tar.bz2", "file.tar.xz")
splitext(path, compression = FALSE)
splitext(path, compression = TRUE)

path <- "this.path_1.0.0.tar.gz"
ext(path) <- ".png"
path

path <- "this.path_1.0.0.tar.gz"
ext(path, compression = TRUE) <- ".png"
path
}
