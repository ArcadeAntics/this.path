%%
%% this.path : Get Executing Script's Path
%% Copyright (C) 2022-2025   Iris Simmons
%%


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

  \describe{
    \item{On Windows:}{

      \ifelse{text}{\\}{\code{\\}} and \ifelse{text}{/}{\code{/}} are accepted
      as path separators, and \code{dirname2()} does \strong{NOT} translate the
      path separators.

      Recall that a drive looks like \string{d:} and a network
      share looks like \string{//host/share} or \string{//?/UNC/host/share}.}

    \item{Under Unix-alikes:}{

      Recall that a network share looks like \string{//host/share}.}
  }

  If the rest of the path immediately after a (possible) drive or network share
  is empty or is \string{/}, then \code{dirname2()} will return \code{path} and
  \code{basename2()} will return an empty string.
}
\value{
  A character vector of the same length as \code{path}.
}
\examples{
path <- c("/usr/lib", "/usr/", "usr", "/", ".", "..")
x <- cbind(path, dirname = dirname2(path), basename = basename2(path))
print(x, quote = FALSE, print.gap = 3)
}
