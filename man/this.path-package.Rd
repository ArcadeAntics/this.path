\name{this.path-package}
\alias{this.path-package}
\docType{package}
\title{
\packageTitle{this.path}
}
\description{
\packageDescription{this.path}
}
\details{
There are only two functions from this package being \code{this.path} and
\code{this.dir}.

\code{\link{this.path}()} returns the
\code{\link[base:normalizePath]{normalized}} path of the executing script.

\code{\link{this.dir}()} is a shorter way of writing
\code{\link[base:basename]{dirname}(this.path())}, returning the
\code{\link[base:normalizePath]{normalized}} path of the directory in which the
executing script is located.
}
\note{
This package started because of a stack overflow posting, found at

\href{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script}{
https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script
}.
}
\author{
\packageAuthor{this.path}

Maintainer: \packageMaintainer{this.path}
}
\keyword{ package }
\seealso{
\code{\link{this.path}}

\code{\link[base]{source}}

\code{\link[base]{sys.source}}

\code{\link[testthat:source_file]{testthat::source_file}}

\code{\link{Running.R.from.command-line}}
}
