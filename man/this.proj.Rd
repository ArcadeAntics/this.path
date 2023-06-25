\name{this.proj}
\alias{sys.proj}
\alias{env.proj}
\alias{src.proj}
\alias{this.proj}
\alias{reset.proj}
\title{Construct Path to File, Starting with the Project's Directory}
\description{
  \code{sys.proj()}, \code{env.proj()}, \code{src.proj()}, and
  \code{this.proj()} construct paths to files starting with the project's root.

  \code{reset.proj()} will reset the paths cached by these functions. This can
  be useful if you created a new project in your \R session that you would like
  to be detected without the need to restart the \R session.
}
\usage{
sys.proj(..., local = FALSE)
env.proj(..., envir = parent.frame(),
         matchThisEnv = getOption("topLevelEnvironment"))
src.proj(..., srcfile = sys.call())

this.proj(..., local = FALSE, envir = parent.frame(),
          matchThisEnv = getOption("topLevelEnvironment"),
          srcfile = sys.call())

reset.proj()
}
\arguments{
  \item{\dots}{further arguments passed to \code{\link{path.join}()}.}

  \item{local}{See \code{?\link{sys.path}()}.}

  \item{envir, matchThisEnv}{See \code{?\link{env.path}()}.}

  \item{srcfile}{See \code{?\link{src.path}()}.}
}
\details{
  Unlike \code{here::here()}, these functions support sub-projects and multiple
  projects in use at once, and will choose which project root is appropriate
  based on \code{\link{sys.dir}()}, \code{\link{env.dir}()},
  \code{\link{src.dir}()}, or \code{\link{this.dir}()}. Additionally, it is
  independent of working directory, whereas \code{here::here()} relies on the
  working directory being set within the project's directory when the package
  is loaded. Arguably, this makes it better than \code{here::here()}.
}
\value{
  A character vector of the arguments concatenated term-by-term, starting with
  the project's root.
}
