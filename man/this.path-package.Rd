\name{this.path-package}
\alias{this.path-package}
\docType{package}
\title{
Get Executing Script's Path, from \sQuote{Rgui}, \sQuote{\RStudio},
\sQuote{\VSCode}, \sQuote{\Jupyter}, \sourcelink{}, and
\code{\link[utils]{Rscript}} (Shells Including Windows Command-Line / / Unix
Terminal)
}
\description{
Determine the path of the executing script. Works when running a line or
selection in \sQuote{Rgui}, \sQuote{\RStudio}, \sQuote{\VSCode}, and
\sQuote{\Jupyter}, when using \sourcesandlistlinks{}, and when running from a
shell.
}
\details{
The most important functions from \pkg{this.path} are
\code{\link{this.path}()}, \code{\link{this.dir}()}, \code{\link{here}()}, and
\code{\link{this.proj}()}:

\itemize{
  \item{
\code{\link{this.path}()} returns the normalized path of the executing script.
  }
  \item{
\code{\link{this.dir}()} return the normalized path of the directory in which
the executing script is located.
  }
  \item{
\code{\link{here}()} constructs file paths against the executing script's
directory.
  }
  \item{
\code{\link{this.proj}()} constructs file paths against the executing project's
directory instead of the executing script's directory.
  }
}

\pkg{this.path} also provides functions for constructing and manipulating file
paths:

\itemize{
  \item{
\code{\link{path.join}()}, \code{\link{basename2}()}, and
\code{\link{dirname2}()} are drop in replacements for
\code{\link[base]{file.path}()}, \code{\link[base]{basename}()}, and
\code{\link[base:basename]{dirname}()} which better handle drives and network
shares.
  }
  \item{
\code{\link{splitext}()}, \code{\link{removeext}()}, \code{\link{ext}()}, and
\code{\link{ext<-}()} split a path into root and extension, remove a file
extension, get an extension, or set an extension for a file path.
  }
  \item{
\code{\link{path.split}()}, \code{\link{path.split.1}()}, and
\code{\link{path.unsplit}()} split the path to a file into components.
  }
  \item{
\code{\link{relpath}()} and \code{\link{rel2here}()} turn absolute paths into
relative paths.
  }
}

New additions to \pkg{this.path} include:

\itemize{
  \item{
\code{\link{LINENO}()} returns the line number of the executing expression in
the executing script.
  }
  \item{
\code{\link{wrap.source}()}, \code{\link{inside.source}()},
\code{\link{set.this.path}()}, and \code{\link{unset.this.path}()} implement
\code{\link{this.path}()} for any \sourcelink{}-like functions outside of
\sourcesandlistlinks{}.
  }
  \item{
\code{\link{shFILE}()} looks through the command line arguments, extracting
\option{FILE} from either of the following: \option{-f} \option{FILE} or
\option{--file=FILE}
  }
}
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
\sourceslistlinks{}

\code{\link{R.from.shell}}
}
