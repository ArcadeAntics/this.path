\name{relpath}
\alias{relpath}
\alias{rel2here}
\alias{rel2proj}
\title{Make a Path Relative to Another}
\description{
  When working with \pkg{this.path}, you will be dealing with a lot of absolute
  paths. These paths are not portable for saving within files nor tables, so
  convert them to relative paths with \code{relpath()}.
}
\usage{
#ifdef unix
relpath(path, relative.to = getwd())
#endif
#ifdef windows
relpath(path, relative.to = normalizePath(getwd(), "/", TRUE))
#endif

rel2here(path, local = FALSE, n = 0, envir = parent.frame(n + 1),
         matchThisEnv = getOption("topLevelEnvironment"),
         srcfile = if (n) sys.parent(n) else 0)

rel2proj(path, local = FALSE, n = 0,
         envir = parent.frame(n + 1),
         matchThisEnv = getOption("topLevelEnvironment"),
         srcfile = if (n) sys.parent(n) else 0)
}
\arguments{
  \item{path}{character vector of file / / URL pathnames.}

  \item{relative.to}{character string; the file / / URL pathname to make
    \code{path} relative to.}

  \item{local, n, envir, matchThisEnv, srcfile}{See
    \code{?\link{this.path}()}.}
}
\details{
  Tilde-expansion (see \codelink4{?}{base}{path.expand}{()}) is first done on
  \code{path} and \code{relative.to}.

  If \code{path} and \code{relative.to} are equivalent, \string{.} will be
  returned. If \code{path} and \code{relative.to} have no base in common, the
  \ifelse{html}{\code{\link[base:normalizePath]{normalized}}}{normalized}
  \code{path} will be returned.
}
\value{
  character vector of the same length as \code{path}.
}
#ifdef unix
\examples{
\dontrun{
relpath(
    c(
        ## paths which are equivalent will return "."
        "/home/effective_user/Documents/this.path/man",


        ## paths which have no base in common return as themselves
        paste0("https://raw.githubusercontent.com/ArcadeAntics/",
               "this.path/main/tests/sys-path-with-urls.R"),
        "//host-name/share-name/path/to/file",


        "/home/effective_user/Documents/testing",
        "/home/effective_user",
        "/home/effective_user/Documents/R/thispath.R"
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
        ## paths which are equivalent will return "."
        "C:/Users/effective_user/Documents/this.path/man",


        ## paths which have no base in common return as themselves
        paste0("https://raw.githubusercontent.com/ArcadeAntics/",
               "this.path/main/tests/sys-path-with-urls.R"),
        "D:/",
        "//host-name/share-name/path/to/file",


        "C:/Users/effective_user/Documents/testing",
        "C:\\\\Users\\\\effective_user",
        "C:/Users/effective_user/Documents/R/thispath.R"
    ),
    relative.to = "C:/Users/effective_user/Documents/this.path/man"
)
}
}
#endif
