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
\code{\link{this.path}()} returns the
\code{\link[base:normalizePath]{normalized}} path of the executing script.

\code{\link{this.dir}()} returns the
\code{\link[base:normalizePath]{normalized}} path of the directory in which the
executing script is located.

\code{\link{here}()} constructs file paths relative to the executing script's
directory.

\code{\link{path.join}()}, \code{\link{basename2}()}, and
\code{\link{dirname2}()} are replacements for \code{\link{file.path}()},
\code{\link{basename}()}, and \code{\link{dirname}()} that better handle
drives, network shares, and a few important edge cases.

\code{\link{splitext}()}, \code{\link{removeext}()}, and \code{\link{ext}()}
split paths into root and extension, remove extensions, and extract extensions,
respectively.
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
Check \code{\link{this.path}()}, \code{\link{this.dir}()} are functioning
correctly: \code{\link{check.path}{}}, \code{\link{check.dir}{}}

Extract \option{FILE} from command line arguments: \code{\link{shFILE}()},
\code{\link{normalized.shFILE}()}

Make paths relative to another path: \code{\link{as.rel.path}()}

\code{\link{R.from.shell}}
}
