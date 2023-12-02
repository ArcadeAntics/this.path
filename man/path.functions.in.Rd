\name{path.functions}
\alias{path.functions}
\title{Constructs Path Functions Similar to 'this.path()'}
\description{
  \code{path.functions()} accepts a pathname and constructs a set of
  path-related functions, similar to \code{this.path()} and associated.
}
\usage{
path.functions(file, local = FALSE, n = 0,
               envir = parent.frame(n + 1),
               matchThisEnv = getOption("topLevelEnvironment"),
               srcfile = if (n) sys.parent(n) else 0)
}
\arguments{
  \item{file}{a character string giving the pathname of the file or URL.}

  \item{local, n, envir, matchThisEnv, srcfile}{See \code{?\link{this.path}()}.}
}
\value{
  An environment with at least the following bindings:

  \item{this.path}{Function with formals
    \code{(original = FALSE, contents = FALSE)} which returns the
    \ifelse{html}{\codelink2{base:normalizePath}{normalized}}{normalized} file
    path, the original file path, or the contents of the file.}

  \item{this.dir}{Function of no arguments which returns the directory of the
    \ifelse{html}{\codelink2{base:normalizePath}{normalized}}{normalized} file
    path.}

  \item{here}{Function with formals \code{(..., .. = 0)} which constructs file
    paths, starting with the file's directory.}

  \item{this.proj}{Function with formals \code{(..., .. = 0)} which constructs
    file paths, starting with the project root.}

  \item{rel2here, rel2proj}{Functions with formals \code{(path)} which turn
    absolute paths into relative paths, against the file's directory / /
    project root.}

  \item{LINENO}{Function with formals \code{NULL} which returns the line number
    of the executing expression in \code{file}.}
}
