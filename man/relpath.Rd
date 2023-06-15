\name{relpath}
\alias{relpath}
\alias{rel2sys.dir}
\alias{rel2sys.proj}
\alias{rel2env.dir}
\alias{rel2env.proj}
\alias{rel2here}
\alias{rel2proj}
\title{Make a Path Relative to Another Path}
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

rel2sys.dir(path)
rel2sys.proj(path)

rel2env.dir(path, envir = parent.frame(),
            matchThisEnv = getOption("topLevelEnvironment"))
rel2env.proj(path, envir = parent.frame(),
             matchThisEnv = getOption("topLevelEnvironment"))

rel2here(path, envir = parent.frame(),
         matchThisEnv = getOption("topLevelEnvironment"))
rel2proj(path, envir = parent.frame(),
         matchThisEnv = getOption("topLevelEnvironment"))
}
\arguments{
  \item{path}{character vector of file / / URL pathnames.}

  \item{relative.to}{character string; the file / / URL pathname to make
    \code{path} relative to.}

  \item{envir, matchThisEnv}{See \code{?\link{this.path}()}.}
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
\note{
  \code{rel2sys.dir()}, \code{rel2sys.proj()}, \code{rel2env.dir()},
  \code{rel2env.proj()}, \code{rel2here()}, and \code{rel2proj()} are variants
  of \code{relpath()} in which \code{relative.to} is \code{\link{sys.dir}()},
  \code{\link{sys.proj}()}, \code{\link{env.dir}()}, \code{\link{env.proj}()},
  \code{\link{here}()}, and \code{\link{this.proj}()}.
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
