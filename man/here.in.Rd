\name{here}
\alias{here}
\alias{ici}
\alias{this.proj}
\alias{reset.proj}
\title{Construct Path to File, Starting With Script's Directory}
\description{
  \code{here()} constructs file paths starting with \code{\link{this.dir}()}.

  \code{this.proj()} constructs file paths starting with the project root of
  \code{this.dir()}.

  \code{reset.proj()} resets the path cache of \code{this.proj()}. This can be
  useful if you create a new project that you would like to be detected without
  restarting your \R session.
}
\usage{
here(..., local = FALSE, n = 0,
     envir = parent.frame(n + 1),
     matchThisEnv = getOption("topLevelEnvironment"),
     srcfile = if (n) sys.parent(n) else 0, .. = 0)

this.proj(..., local = FALSE, n = 0,
          envir = parent.frame(n + 1),
          matchThisEnv = getOption("topLevelEnvironment"),
          srcfile = if (n) sys.parent(n) else 0)

reset.proj()

## alias for 'here'
ici(..., local = FALSE, n = 0,
    envir = parent.frame(n + 1),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0, .. = 0)
}
\arguments{
  \item{\dots}{further arguments passed to \code{\link{path.join}()}.}

  \item{local, n, envir, matchThisEnv, srcfile}{See
    \code{?\link{this.path}()}.}

  \item{..}{the number of directories to go back.}
}
\details{
  For \code{this.proj()}, the project root has the same criterion as
  \code{here::here()}, but unlike \code{here::here()}, \code{this.proj()}
  supports sub-projects and multiple projects in use at once. Additionally,
  \code{this.proj()} is independent of working directory, whereas
  \code{here::here()} relies on the working directory being set somewhere
  within the project when \code{package:here} is loaded. Arguably, this makes
  it better than \code{here::here()}.
}
\value{
  A character vector of the arguments concatenated term-by-term.
}
\examples{
tmpdir <- tempfile(pattern = "dir")
dir.create(tmpdir)

writeLines("this file signifies that its directory is the project root",
    this.path::path.join(tmpdir, ".here"))

FILE.R <- this.path::path.join(tmpdir, "src", "R", "script1.R")
dir.create(this.path::dirname2(FILE.R), recursive = TRUE)
this.path:::.writeCode({
    this.path::this.path()
    this.path::this.proj()
    ## use 'here' to run another script located nearby
    this.path::here("script2.R")
    ## or maybe to read input from a file
    this.path::here(.. = 2, "input", "data1.csv")
    ## but sometimes it is easier to use the project root
    ## this allows you to move the R script up or down
    ## a directory without changing the .. number
    this.path::this.proj("input", "data1.csv")
}, FILE.R)

source(FILE.R, echo = TRUE)

unlink(tmpdir, recursive = TRUE)
}
