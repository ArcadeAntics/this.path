\name{make_fix_funs}
\alias{make_fix_funs}
\alias{path.functions}
\title{Constructs Path Functions Similar to 'this.path()'}
\description{
  \code{make_fix_funs()} accepts a pathname and constructs a set of
  path-related functions, similar to \code{this.path()} and associated.
}
\usage{
make_fix_funs(file, delayed = FALSE, local = FALSE, n = 0,
              envir = parent.frame(n + 1),
              matchThisEnv = getOption("topLevelEnvironment"),
              srcfile = if (n) sys.parent(n) else 0)

## alias for 'make_fix_funs'
path.functions(file, delayed = FALSE, local = FALSE, n = 0,
               envir = parent.frame(n + 1),
               matchThisEnv = getOption("topLevelEnvironment"),
               srcfile = if (n) sys.parent(n) else 0)
}
\arguments{
  \item{file}{a character string giving the pathname of the file or URL.}

  \item{delayed}{\code{TRUE} or \code{FALSE}; should the
    \ifelse{html}{\codelink2{base:normalizePath}{normalizing}}{normalizing} of
    \code{file} be delayed?}

  \item{local, n, envir, matchThisEnv, srcfile}{See \code{?\link{this.path}()}.}
}
\value{
  An environment with at least the following bindings:

  \item{this.path}{

    Function with formals \code{(original = FALSE, contents = FALSE)} which
    returns the
    \ifelse{html}{\codelink2{base:normalizePath}{normalized}}{normalized} file
    path, the original file path, or the contents of the file.}

  \item{this.dir}{

    Function with no formals which returns the directory of the
    \ifelse{html}{\codelink2{base:normalizePath}{normalized}}{normalized} file
    path.}

  \item{here, ici}{

    Function with formals \code{(..., .. = 0)} which constructs file paths,
    starting with the file's directory.}

  \item{this.proj}{

    Function with formals \code{(..., .. = 0)} which constructs file paths,
    starting with the project root.}

  \item{rel2here, rel2proj}{

    Functions with formals \code{(path)} which turn absolute paths into
    relative paths, against the file's directory / / project root.}

  \item{LINENO}{

    Function with no formals which returns the line number of the executing
    expression in \code{file}.}
}
