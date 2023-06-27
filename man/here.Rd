\name{here}
\alias{sys.here}
\alias{env.here}
\alias{src.here}
\alias{here}
\alias{ici}
\title{Construct Path to File, Starting with Current Directory}
\description{
  Construct the path to a file from components / / paths in a
  platform-\strong{DEPENDENT} way, starting with \code{\link{sys.dir}()},
  \code{\link{env.dir}()}, or \code{\link{this.dir}()}.
}
\usage{
sys.here(..., .. = 0, local = FALSE)
env.here(..., .. = 0, n = 0, envir = parent.frame(n + 1),
         matchThisEnv = getOption("topLevelEnvironment"))
src.here(..., .. = 0, n = 0,
         srcfile = sys.call(if (n) sys.parent(n) else 0))

here(..., .. = 0, local = FALSE, n = 0,
     envir = parent.frame(n + 1),
     matchThisEnv = getOption("topLevelEnvironment"),
     srcfile = sys.call(if (n) sys.parent(n) else 0))

## alias for 'here'
ici(..., .. = 0, local = FALSE, n = 0,
    envir = parent.frame(n + 1),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = sys.call(if (n) sys.parent(n) else 0))
}
\arguments{
  \item{\dots}{further arguments passed to \code{\link{path.join}()}.}

  \item{..}{the number of directories to go back.}

  \item{local}{See \code{?\link{sys.path}()}.}

  \item{n}{See \code{?\link{this.path}()}.}

  \item{envir, matchThisEnv}{See \code{?\link{env.path}()}.}

  \item{srcfile}{See \code{?\link{src.path}()}.}
}
\details{
  The path to a file starts with a base. The base is \code{..} number of
  directories back from the executing script's directory (\code{this.dir()}).
  The argument is named \code{..} because \string{..} refers to the parent
  directory on Windows, under Unix-alikes, and for URL pathnames.
}
\value{
  A character vector of the arguments concatenated term-by-term, starting with
  the executing script's directory.
}
\examples{
FILE.R <- tempfile(fileext = ".R")
this.path:::.write.code({


    this.path::here()
    this.path::here(.. = 1)
    this.path::here(.. = 2)


    ## use 'here' to read input from a file located nearby
    this.path::here(.. = 1, "input", "file1.csv")


    ## or maybe to run another script
    this.path::here("script2.R")


}, FILE.R)


source(FILE.R, echo = TRUE, verbose = FALSE)


unlink(FILE.R)
}
