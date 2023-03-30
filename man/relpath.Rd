\name{relpath}
\alias{relpath}
\alias{rel2here}
\title{
Make a Path Relative to Another Path
}
\description{
When working with \pkg{this.path}, you will be dealing with a lot of absolute
paths. These paths are no good for saving within files, so you will need to use
\code{relpath()} and \code{rel2here()} to turn your absolute paths into
relative paths.
}
\usage{
relpath(path, relative.to = getwd())
rel2here(path)
}
\arguments{
  \item{path}{
character vector of file / / URL pathnames.
  }
  \item{relative.to}{
character string; the file / / URL pathname to make \code{path} relative to.
  }
}
\details{
Tilde-expansion (see \code{\link[base]{path.expand}()}) is first done on
\code{path} and \code{relative.to}.

If \code{path} and \code{relative.to} are equivalent, \code{"."} will be
returned. If \code{path} and \code{relative.to} have no base in common, the
\code{\link[base:normalizePath]{normalized}} \code{path} will be returned.
}
\value{
character vector of the same length as \code{path}.
}
\note{
\code{rel2here()} is a variant of \code{relpath()} in which \code{relative.to}
is \code{\link{here}()}.
}
#ifdef unix
\examples{
\dontrun{
relpath(
    c(
        # paths which are equivalent will return "."
        "/home/effective_user/Documents/this.path/man",


        # paths which have no base in common return as themselves
        paste0("https://raw.githubusercontent.com/ArcadeAntics/",
               "this.path/main/tests/this.path_w_URLs.R"),
        "//host-name/share-name/path/to/file",


        "/home/effective_user/Documents/testing",
        "/home/effective_user",
        "/home/effective_user/Documents/R/this.path.R"
    ),
    relative.to = "/home/effective_user/Documents/this.path/man"
)
}
}
#endif
#ifdef windows
\examples{
\dontrun{
relpath(
    c(
        # paths which are equivalent will return "."
        "C:/Users/effective_user/Documents/this.path/man",


        # paths which have no base in common return as themselves
        paste0("https://raw.githubusercontent.com/ArcadeAntics/",
               "this.path/main/tests/this.path_w_URLs.R"),
        "D:/",
        "//host-name/share-name/path/to/file",


        "C:/Users/effective_user/Documents/testing",
        "C:\\\\Users\\\\effective_user",
        "C:/Users/effective_user/Documents/R/this.path.R"
    ),
    relative.to = "C:/Users/effective_user/Documents/this.path/man"
)
}
}
#endif
