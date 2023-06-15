\name{here}
\alias{sys.here}
\alias{env.here}
\alias{here}
\alias{ici}
\title{Construct Path to File, Starting with Current Directory}
\description{
  Construct the path to a file from components / / paths in a
  platform-\strong{DEPENDENT} way, starting with \code{\link{sys.dir}()},
  \code{\link{env.dir}()}, or \code{\link{this.dir}()}.
}
\usage{
sys.here(..., .. = 0)
env.here(..., .. = 0L, envir = parent.frame(),
         matchThisEnv = getOption("topLevelEnvironment"))
here(..., .. = 0, envir = parent.frame(),
     matchThisEnv = getOption("topLevelEnvironment"))

## alias for 'here'
ici(..., .. = 0, envir = parent.frame(),
    matchThisEnv = getOption("topLevelEnvironment"))
}
\arguments{
  \item{\dots}{further arguments passed to \code{\link{path.join}()}.}

  \item{..}{the number of directories to go back.}

  \item{envir, matchThisEnv}{See \code{?\link{this.path}()}.}
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
FILE <- tempfile(fileext = ".R")
this.path:::.write.code({


    this.path::here()
    this.path::here(.. = 1)
    this.path::here(.. = 2)


    ## use 'here' to read input from a file located nearby
    this.path::here(.. = 1, "input", "file1.csv")


    ## or maybe to run another script
    this.path::here("script2.R")


}, FILE)


source(FILE, echo = TRUE, verbose = FALSE)


unlink(FILE)
}
