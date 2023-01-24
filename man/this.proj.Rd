\name{this.proj}
\alias{this.proj}
\title{
Construct Path to File, Beginning with Your Project Directory
}
\description{
\code{this.proj} behaves very similarly to \code{here::here}, constructing a
path to a file starting with your project directory.

\code{this.proj} can have multiple projects in use at once, and will choose
which project directory is appropriate based on \code{\link{this.dir}()}.
Additionally, it is independent of working directory, where as
\code{here::here} relies on the working directory being set within the project
directory when the package is loaded. Arguably, this makes it better than
\code{here::here}.
}
\usage{
this.proj(...)
}
\arguments{
  \item{\dots}{
further arguments passed to \code{\link{path.join}()}.
  }
}
\value{
A character vector of the arguments concatenated term-by-term, beginning with
the project directory.
}
\seealso{
\code{\link{here}}
}
