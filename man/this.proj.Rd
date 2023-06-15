\name{this.proj}
\alias{sys.proj}
\alias{env.proj}
\alias{this.proj}
\alias{reset.proj}
\title{Construct Path to File, Starting with the Project's Directory}
\description{
  \code{sys.proj()}, \code{env.proj()}, and \code{this.proj()} constructs the
  path to a file starting with the project's root directory.

  \code{reset.proj()} will reset the paths cached by \code{sys.proj()},
  \code{env.proj()}, and \code{this.proj()}. This can be useful if you created
  a new project in your \R session that you would like to be detected without
  the need to restart the \R session.
}
\usage{
sys.proj(...)
env.proj(..., envir = parent.frame(),
         matchThisEnv = getOption("topLevelEnvironment"))
this.proj(..., envir = parent.frame(),
          matchThisEnv = getOption("topLevelEnvironment"))

reset.proj()
}
\arguments{
  \item{\dots}{further arguments passed to \code{\link{path.join}()}.}

  \item{envir, matchThisEnv}{See \code{?\link{this.path}()}.}
}
\details{
  Unlike \code{here::here()}, these functions support sub-projects and multiple
  projects in use at once, and will choose which project root is appropriate
  based on \code{\link{sys.dir}()}, \code{\link{env.dir}()}, or
  \code{\link{this.dir}()}. Additionally, it is independent of working
  directory, whereas \code{here::here()} relies on the working directory being
  set within the project's directory when the package is loaded. Arguably, this
  makes it better than \code{here::here()}.
}
\value{
  A character vector of the arguments concatenated term-by-term, starting with
  the project's root.
}
