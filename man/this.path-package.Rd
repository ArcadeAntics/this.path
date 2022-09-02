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
The three most important functions from this package are \code{this.path},
\code{this.dir}, and \code{here}.

\code{\link{this.path}()} returns the
\code{\link[base:normalizePath]{normalized}} path of the executing script.

\code{\link{this.dir}()} returns the
\code{\link[base:normalizePath]{normalized}} path of the directory in which the
executing script is located.

\code{\link{here}()} constructs file paths relative to the executing script's
directory.
}
\note{
This package started from a stack overflow posting, found at:

\href{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script}{
https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script
}

If you like this package, please consider upvoting my answer so that more
people will see it! If you have an issue with this package, please use
\code{\link[utils:bug.report]{utils::bug.report}(package = "this.path")} to
report your issue.
}
\author{
\packageAuthor{this.path}

Maintainer: \packageMaintainer{this.path}
}
\keyword{ package }
\seealso{
The main functions from \pkg{this.path}: \code{\link{this.path}},
\code{\link{this.dir}}, \code{\link{here}}

Check \code{this.path()} is functioning correctly: \code{\link{check.path}},
\code{\link{check.dir}}

Extract \option{FILE} from command line arguments: \code{\link{shFILE}},
\code{\link{normalized.shFILE}}

\code{\link[base]{source}}, \code{\link[base]{sys.source}}, \href{https://support.rstudio.com/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE}{\code{debugSource}},
\code{\link[testthat:source_file]{testthat::source_file}}

\code{\link{R.from.shell}}
}
