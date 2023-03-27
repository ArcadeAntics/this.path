\name{this.proj}
\alias{this.proj}
\alias{reset.this.proj}
\title{
Construct Path to File, Beginning with Your Project Directory
}
\description{
\code{this.proj()} behaves very similarly to \code{here::here()}, constructing
a path to a file starting with your project's directory.

\code{this.proj()} supports sub-projects and multiple projects in use at once,
and will choose which project directory is appropriate based on
\code{\link{this.dir}()}. Additionally, it is independent of working directory,
whereas \code{here::here()} relies on the working directory being set within
the project's directory when the package is loaded. Arguably, this makes it
better than \code{here::here()}.

\code{reset.this.proj()} will reset the paths saved by \code{this.proj()}. This
can be useful if you created a new project in your \R session that you would
like to be detected by \code{this.proj()} without the need to restart the \R
session.
}
\usage{
this.proj(...)

reset.this.proj()
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
\code{this.path::\link{here}()}
}
