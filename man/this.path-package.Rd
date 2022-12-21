\name{this.path-package}
\alias{this.path-package}
\docType{package}
\title{
Get Executing Script's Path, from \sQuote{RStudio}, \sQuote{VSCode},
\sQuote{Rgui}, \code{source()}, and \command{Rscript} (Shells Including
Windows Command Line / / Unix Terminal)
}
\description{
Determine the path of the executing script. Works when running a line
or selection in \sQuote{RStudio}, \sQuote{VSCode}, and \sQuote{Rgui},
when using \sourceandlist{}, and when running from a shell.
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

\code{\link{splitext}()}, \code{\link{removeext}()}, \code{\link{ext}()}, and
\code{\link{ext<-}()} split paths into root and extension, remove extensions,
get extensions, and set extensions, respectively.

\code{\link{check.path}()} and \code{\link{check.dir}()} check that
\code{\link{this.path}()} and \code{\link{this.dir}()} are functioning
correctly.

\code{\link{as.rel.path}()} ( or \code{\link{as.relative.path}()} ) will turn
absolute paths into relative paths.

\code{\link{asArgs}()}, \code{\link{fileArgs}()}, \code{\link{progArgs}()}, and
\code{\link{withArgs}()} provide functionality for running scripts with
arguments in the same session, as opposed to a new one with
\code{\link[utils]{Rscript}}.

\code{\link{is.main}()} and \code{\link{from.shell}()} determine if an \R
script is the main executing script or is run from a shell.

\code{\link{shFILE}()} extracts \option{FILE} from the command line arguments.

\code{\link{tryCatch2}()} adds argument \code{else.} that runs if no error is
thrown. This helps to run extra code that is not intended to be protected by
the condition handlers.
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
\sourcelinks{}

\code{\link{R.from.shell}}
}
