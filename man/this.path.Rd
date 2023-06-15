\name{this.path}
\alias{env.path}
\alias{env.dir}
\alias{this.path}
\alias{this.dir}
\title{Determine Current Script's Filename}
\description{
  \code{env.path()} returns the
  \ifelse{html}{\codelink2{base:normalizePath}{normalized}}{normalized} path
  associated with the top level environment (see
  \ifelse{html}{\code{?\link[base:ns-topenv]{topenv}}}{\code{?topenv}}).

  \code{env.dir()} returns the directory associated with the top level
  environment.

  \code{this.path()} returns the normalized path of the script in which it was
  written.

  \code{this.dir()} returns the directory of the normalized path of the script
  in which it was written.
}
\usage{
env.path(verbose = FALSE, original = FALSE, for.msg = FALSE,
    contents = FALSE, envir = parent.frame(),
    matchThisEnv = getOption("topLevelEnvironment"),
    default, else.)
env.dir(verbose = FALSE, envir = parent.frame(),
    matchThisEnv = getOption("topLevelEnvironment"),
    default, else.)

this.path(verbose = getOption("verbose"), original = FALSE,
    for.msg = FALSE, local = FALSE, contents = FALSE,
    envir = parent.frame(),
    matchThisEnv = getOption("topLevelEnvironment"),
    default, else.)
this.dir(verbose = getOption("verbose"), envir = parent.frame(),
    matchThisEnv = getOption("topLevelEnvironment"),
    default, else.)
}
\arguments{
  \item{verbose, original, for.msg, local, contents, default, else.}{See
    \code{?\link{sys.path}()}.}

  \item{envir, matchThisEnv}{arguments passed to
    \codelink3{base:ns-topenv}{topenv}{()} to determine the top level
    environment in which to search for an associated path.}
}
\details{
  There are two ways in which \code{env.path()} will find a path associated
  with the top level environment:

  \enumerate{
    \item{from a \CRANpkg{box} module's namespace.}

    \item{from an attribute \string{path}.}
  }

  If \code{env.path()} does not find an associated path, it will throw an
  error.

  \code{this.path()} determines the path of the script in which it is written
  by:

  \enumerate{
    \item{examining the call to \code{this.path()}. If that call has a source
      reference which has a source file which refers to a file, return the
      source file's \code{filename}.}

    \item{examining the top level environment, looking for an associated path,
      the same as \code{env.path()}.}

    \item{examining the call stack, looking for the path of the executing
      script, the same as \code{\link{sys.path}()}.}
  }

  \code{this.path()} and \code{this.dir()} are likely the functions you want to
  use. They can be used both inside and outisde modularized code.
  \code{env.path()} and \code{env.dir()} are provided for completeness / /
  convenience but are less useful because they must be used inside modularized
  environments. If you need to know the path of the executing script, perhaps
  for logging purposes, then you should use \code{sys.path()} and
  \code{sys.dir()}.
}
\value{
  character string.
}
\note{
  Similar to \code{\link{sys.path}()}, the first time \code{env.path()} is
  called on an environment / / \code{this.path()} is called with a source
  reference, it will
  \ifelse{html}{\codelink2{base:normalizePath}{normalize}}{normalize} the
  associated path, checking that the file exists (throwing an error if it
  does not), and save it as an attribute / / variable. When \code{env.path()} /
  / \code{this.path()} is called subsequent times, it returns the saved path.
  This will be faster and will not check for file existence.
}
\seealso{
  \code{\link{here}()}

  \code{\link{shFILE}()}

  \code{\link{wrap.source}()}, \code{\link{set.sys.path}()}

  \code{\link{this.path-package}}

  \sourceslistlinks

  \code{\link{R.from.shell}}
}
\examples{
## the important difference between 'this.path()' and 'sys.path()'


FILE1.R <- tempfile("FILE1-", fileext = ".R")
this.path:::.write.code({
    fun <- function() {
        cat("\\n> this.path()\\n")
        print({ this.path::this.path(verbose = TRUE) })
        cat("\\n> sys.path()\\n")
        print({ this.path::sys.path(verbose = TRUE) })
    }
    ## 'this.path()' and 'sys.path()' should be identical right now
    ## because the executing script is the same as the script of the
    ## source file
    fun()
}, FILE1.R)
source(FILE1.R, verbose = FALSE, keep.source = TRUE)


FILE2.R <- tempfile("FILE2-", fileext = ".R")
this.path:::.write.code({
    ## 'this.path()' and 'sys.path()' should no longer be identical
    ## since FILE2.R is now the executing script, and FILE1.R is not
    fun()
}, FILE2.R)
source(FILE2.R)


unlink(c(FILE1.R, FILE2.R))
}
