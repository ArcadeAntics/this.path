\name{basename2}
\alias{basename2}
\alias{dirname2}
\title{
Manipulate File Paths
}
\description{
\code{basename2()} removes all of the path up to and including the last path
separator (if any).

\code{dirname2()} returns the part of the \code{path} up to but excluding the
last path separator, or \code{"."} if there is no path separator.
}
\usage{
basename2(path)
dirname2(path)
}
\arguments{
  \item{path}{
character vector, containing path names.
  }
}
\details{
\code{\link[base:path.expand]{tilde expansion}} of the path will be performed.

Trailing path separators are removed before dissecting the path, and for
\code{dirname2()} any trailing file separators are removed from the result.
}
\value{
A character vector of the same length as \code{path}.
}
\section{Behaviour on Windows}{
If \code{path} is an empty string, then both \code{dirname2()} and
\code{basename2()} return an emty string.

\code{\\} and \code{/} are accepted as path separators, and \code{dirname2()}
does \strong{NOT} translate the path separators.

Recall that a network share looks like \code{"//host/share"} and a drive looks
like \code{"d:"}.

For a path which starts with a network share or drive, the path specification
is the portion of the string immediately afterward, e.g. \code{"/path/to/file"}
is the path specification of \code{"//host/share/path/to/file"} and
\code{"d:/path/to/file"}. For a path which does not start with a network share
or drive, the path specification is the entire string.

The path specification of a network share will always be empty or absolute, but
the path specification of a drive does not have to be, e.g. \code{"d:file"} is
a valid path despite the fact that the path specification does not begin with
\code{"/"}.

If the path specification of \code{path} is empty or is \code{"/"}, then
\code{dirname2()} will return \code{path} and \code{basename2()} will return an
empty string.
}
\section{Behaviour on Unix-alikes}{
If \code{path} is an empty string, then both \code{dirname2()} and
\code{basename2()} return an emty string.

Recall that a network share looks like \code{"//host/share"}.

For a path which starts with a network share, the path specification is the
portion of the string immediately afterward, e.g. \code{"/path/to/file"} is the
path specification of \code{"//host/share/path/to/file"}. For a path which does
not start with a network share, the path specification is the entire string.

If the path specification of \code{path} is empty or is \code{"/"}, then
\code{dirname2()} will return \code{path} and \code{basename2()} will return an
empty string.
}
\examples{
path <- c("/usr/lib", "/usr/", "usr", "/", ".", "..")
x <- cbind(path, dirname = dirname2(path), basename = basename2(path))
print(x, quote = FALSE, print.gap = 3)
}
