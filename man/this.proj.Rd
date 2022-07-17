\name{this.proj}
\alias{this.proj}
\title{
Construct Path to File, Beginning with Your Project Directory
}
\description{
\code{this.proj} behaves very similarly to \code{here::here} except that you
can have multiple projects in use at once, and it will choose which project
directory is appropriate based on \code{\link{this.dir}()}. Arguably, this
makes it better than \code{here::here}.
}
\usage{
this.proj(...)
}
\arguments{
  \item{\dots}{
further arguments passed to \code{\link[base]{file.path}()}.
  }
}
\value{
A character vector of the arguments concatenated term-by-term, beginning with
the project directory.
}
\seealso{
\code{\link{here}}
}
