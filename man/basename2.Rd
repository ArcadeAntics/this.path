\name{basename2}
\alias{basename2}
\alias{dirname2}
\title{Manipulate File Paths}
\description{
  \code{basename2()} removes all of the path up to and including the last path
  separator (if any).

  \code{dirname2()} returns the part of the \code{path} up to but excluding the
  last path separator, or \string{.} if there is no path separator.
}
\usage{
basename2(path, expand = TRUE)
dirname2(path, expand = TRUE)
}
\arguments{
  \item{path}{character vector, containing path names.}

  \item{expand}{logical. Should tilde (see \codelink2{base}{path.expand}) be
    expanded?}
}
\details{
  Trailing path separators are removed before dissecting the path, and for
  \code{dirname2()} any trailing file separators are removed from the result.
}
\value{
  A character vector of the same length as \code{path}.
}
\section{Behaviour on Windows}{
  If \code{path} is an empty string, then both \code{dirname2()} and
  \code{basename2()} return an emty string.

  \ifelse{text}{\\}{\code{\\}} and \ifelse{text}{/}{\code{/}} are accepted as
  path separators, and \code{dirname2()} does \strong{NOT} translate the path
  separators.

  Recall that a network share looks like \string{//host/share} and a drive
  looks like \string{d:}.

  For a path which starts with a network share or drive, the path specification
  is the portion of the string immediately afterward, e.g.
  \string{/path/to/file} is the path specification of
  \string{//host/share/path/to/file} and \string{d:/path/to/file}. For a path
  which does not start with a network share or drive, the path specification is
  the entire string.

  The path specification of a network share will always be empty or absolute,
  but the path specification of a drive does not have to be, e.g.
  \string{d:file} is a valid path despite the fact that the path specification
  does not start with \string{/}.

  If the path specification of \code{path} is empty or is \string{/}, then
  \code{dirname2()} will return \code{path} and \code{basename2()} will return
  an empty string.
}
\section{Behaviour under Unix-alikes}{
  If \code{path} is an empty string, then both \code{dirname2()} and
  \code{basename2()} return an emty string.

  Recall that a network share looks like \string{//host/share}.

  For a path which starts with a network share, the path specification is the
  portion of the string immediately afterward, e.g. \string{/path/to/file} is
  the path specification of \string{//host/share/path/to/file}. For a path
  which does not start with a network share, the path specification is the
  entire string.

  If the path specification of \code{path} is empty or is \string{/}, then
  \code{dirname2()} will return \code{path} and \code{basename2()} will return
  an empty string.
}
\examples{
path <- c("/usr/lib", "/usr/", "usr", "/", ".", "..")
x <- cbind(path, dirname = dirname2(path), basename = basename2(path))
print(x, quote = FALSE, print.gap = 3)
}
