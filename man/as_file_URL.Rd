%%
%% this.path : Get Executing Script's Path
%% Copyright (C) 2026   Iris Simmons
%%


\name{as_file_URL}
\alias{as_file_URL}
\alias{mk_file_URL}
\title{Create File URLs}
\description{
  \code{as_file_URL()} and \code{mk_file_URL()} will turn a file path into a
  \samp{file://} URL.
}
\usage{
as_file_URL(path, repeated = FALSE)
mk_file_URL(path, repeated = TRUE)
}
\arguments{
  \item{path}{character vector, containing path names.}

  \item{repeated}{logical. Should paths which already seem to be \samp{file://}
    URLs be operated on again?}
}
\details{
  \describe{
    \item{On Windows:}{

      All path separators \ifelse{text}{\\}{\code{\\}} are translated to
      \ifelse{text}{/}{\code{/}}.

      Recall that a drive looks like \string{d:}. For a path that begins with a
      drive, \string{file:///} is prepended, otherwise \string{file://} is
      prepended.}

    \item{Under Unix-alikes:}{

      \string{file://} is prepended.}
  }

  Missing or blank strings will be left as is.
}
\value{
  A character vector of the same length as \code{path}.
}
\examples{
path <- c("/usr/lib", "/usr/", "usr", "/", ".", "..")
as_file_URL(path)
}
