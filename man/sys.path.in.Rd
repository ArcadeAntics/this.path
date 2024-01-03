\name{sys.path}
\alias{sys.path}

\alias{src.path}
\alias{src.dir}
\alias{src.here}
\alias{src.proj}
\alias{rel2src.dir}
\alias{rel2src.proj}
\alias{src.LINENO}
\alias{try.src.path}

\alias{env.path}
\alias{env.dir}
\alias{env.here}
\alias{env.proj}
\alias{rel2env.dir}
\alias{rel2env.proj}
\alias{env.LINENO}
\alias{try.env.path}

\alias{sys.path}
\alias{sys.dir}
\alias{sys.here}
\alias{sys.proj}
\alias{rel2sys.dir}
\alias{rel2sys.proj}
\alias{sys.LINENO}
\alias{try.sys.path}

\alias{this.path-miscellaneous}

\title{Variants of Core Functions in Package 'this.path'}
\description{
  \code{\link{this.path}()} is composed of three sections:
  \enumerate{
    \item{examining argument \code{srcfile}.}

    \item{examining arguments \code{envir} and \code{matchThisEnv}.}

    \item{examining the call stack and the GUI in use.}
  }
  \code{src.path()}, \code{env.path()}, and \code{sys.path()} can be used to
  perform any of those sections individually.

  The other functions listed here are similar such variants of the core
  functions \code{\link{this.dir}()}, \code{\link{here}()},
  \code{\link{this.proj}()}, \code{\link{rel2here}()},
  \code{\link{rel2proj}()}, \code{\link{LINENO}()}, and
  \code{\link{try.this.path}()}.
}
\usage{
src.path(verbose = getOption("verbose"), original = FALSE,
         for.msg = FALSE, contents = FALSE, n = 0,
         srcfile = if (n) sys.parent(n) else 0,
         default, else.)
src.dir(verbose = getOption("verbose"), n = 0,
        srcfile = if (n) sys.parent(n) else 0,
        default, else.)
src.here(..., n = 0,
         srcfile = if (n) sys.parent(n) else 0, .. = 0)
src.proj(..., n = 0,
         srcfile = if (n) sys.parent(n) else 0)
rel2src.dir(path, n = 0,
            srcfile = if (n) sys.parent(n) else 0)
rel2src.proj(path, n = 0,
             srcfile = if (n) sys.parent(n) else 0)
src.LINENO(n = 0, srcfile = if (n) sys.parent(n) else 0)
try.src.path(contents = FALSE, n = 0,
             srcfile = if (n) sys.parent(n) else 0)

env.path(verbose = getOption("verbose"), original = FALSE,
         for.msg = FALSE, contents = FALSE, n = 0,
         envir = parent.frame(n + 1),
         matchThisEnv = getOption("topLevelEnvironment"),
         default, else.)
env.dir(verbose = getOption("verbose"), n = 0,
        envir = parent.frame(n + 1),
        matchThisEnv = getOption("topLevelEnvironment"),
        default, else.)
env.here(..., n = 0, envir = parent.frame(n + 1),
         matchThisEnv = getOption("topLevelEnvironment"), .. = 0)
env.proj(..., n = 0, envir = parent.frame(n + 1),
         matchThisEnv = getOption("topLevelEnvironment"))
rel2env.dir(path, n = 0, envir = parent.frame(n + 1),
            matchThisEnv = getOption("topLevelEnvironment"))
rel2env.proj(path, n = 0, envir = parent.frame(n + 1),
             matchThisEnv = getOption("topLevelEnvironment"))
env.LINENO(n = 0, envir = parent.frame(n + 1),
           matchThisEnv = getOption("topLevelEnvironment"))
try.env.path(contents = FALSE, n = 0,
             envir = parent.frame(n + 1),
             matchThisEnv = getOption("topLevelEnvironment"))

sys.path(verbose = getOption("verbose"), original = FALSE,
         for.msg = FALSE, contents = FALSE, local = FALSE,
         default, else.)
sys.dir(verbose = getOption("verbose"), local = FALSE,
        default, else.)
sys.here(..., local = FALSE, .. = 0)
sys.proj(..., local = FALSE)
rel2sys.dir(path, local = FALSE)
rel2sys.proj(path, local = FALSE)
sys.LINENO()
try.sys.path(contents = FALSE, local = FALSE)
}
\details{
  Before \pkg{this.path 2.0.0}, \code{this.path()} did not have arguments
  \code{srcfile}, \code{envir}, nor \code{matchThisEnv}. It only examined the
  call stack and the GUI in use. In the major update, the original
  implementation of \code{this.path()} was renamed to \code{sys.path()}.
  \code{sys.path()} and related are available for backwards compatibility.
  However, they are unlikely to be useful compared to the advancements made in
  \code{this.path()}.

  \code{env.path()} and \code{src.path()} and related are unlikely to be useful
  when called with no arguments.
}
\examples{
## the important difference between 'this.path()' and 'sys.path()'
FILE1.R <- tempfile("FILE1-", fileext = ".R")
this.path:::.writeCode({
    fun <- function() {
        cat("\\n> this.path()\\n")
        print(this.path::this.path(verbose = TRUE))
        cat("\\n> sys.path()\\n")
        print(this.path::sys.path(verbose = TRUE))
    }
    ## 'this.path()' and 'sys.path()' should be identical because the
    ## executing script is the same as the script of the source file
    fun()
}, FILE1.R)
source(FILE1.R, keep.source = TRUE)

FILE2.R <- tempfile("FILE2-", fileext = ".R")
this.path:::.writeCode({
    ## 'this.path()' and 'sys.path()' should no longer be identical
    ## since FILE2.R is now the executing script, and FILE1.R is not
    fun()
}, FILE2.R)
source(FILE2.R)

unlink(c(FILE1.R, FILE2.R))
}
\keyword{internal}
