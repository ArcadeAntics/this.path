%%
%% this.path : Get Executing Script's Path
%% Copyright (C) 2026   Iris Simmons
%%


\name{splitroot}
\alias{splitroot}
\alias{splitdrive}
\title{Split File Path Into Drive, Root, and Path}
\description{
  \code{splitroot()} splits a path into drive, root, and path.

  \code{removeext()} splits a path into drive and path.
}
\usage{
splitroot(path, expand = TRUE)
splitdrive(path, expand = TRUE)
}
\arguments{
  \item{path}{character vector, containing path names.}

  \item{expand}{logical. Should tilde (see \codelink2{base}{path.expand}) be
    expanded?}
}
\details{
  \describe{
    \item{On Windows:}{

      \ifelse{text}{\\}{\code{\\}} and \ifelse{text}{/}{\code{/}} are accepted
      as path separators, and neither function translates the path separators.

      Recall that a drive looks like \string{d:} and a network
      share looks like \string{//host/share} or \string{//?/UNC/host/share}.}

    \item{Under Unix-alikes:}{

      Recall that a network share looks like \string{//host/share}.}
  }

  For \code{splitroot()}, the paths are each split into 3 components: the drive
  or network share, the path separator immediately after, and the rest of the
  string. Paths without one or more of these components will have a blank
  string in the corresponding place.

  For \code{splitdrive()}, the paths are each split into 2 components: the drive
  or network share, and the rest of the string. Paths without one or more of
  these components will have a blank string in the corresponding place.
}
\value{
  for \code{splitroot()}, a matrix with \ifelse{text}{3}{\code{3}} rows and
  \code{length(path)} columns.

  for \code{splitdrive()}, a matrix with \ifelse{text}{2}{\code{2}} rows and
  \code{length(path)} columns.
}
\examples{
#ifdef windows
path <- c(
    NA,
    "",
    "C:/path/to/file",
    "C:/",
    "C:path/to/file",
    "/path/to/file",
    "C:",
    "/",
    "path/to/file",
    "\\\\\\\\host/share/path/to/file",
    "\\\\\\\\host/share/",
    "\\\\\\\\host/share",
    "\\\\\\\\?/UNC/host/share/path/to/file",
    "\\\\\\\\?/UNC/host/share/",
    "\\\\\\\\?/UNC/host/share"
)
splitroot(path)
splitdrive(path)
#endif
#ifdef unix
path <- c(
    NA,
    "",
    "/path/to/file",
    "/",
    "path/to/file",
    "//host/share/path/to/file",
    "//host/share/",
    "//host/share"
)
splitroot(path)
splitdrive(path)
#endif
}
