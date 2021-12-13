\name{this.path2}
\alias{this.path2}
\alias{this.dir2}
\alias{this.dir3}
\title{Determine Executing Script's Filename
}
\description{
Versions of \code{\link{this.path}} and \code{\link{this.dir}} which return
\code{NULL} or the \code{\link[base:getwd]{working directory}} instead of
throwing an error when there is no executing script.
}
\usage{
this.path2(...)
this.dir2(...)

this.dir3(...)
}
\arguments{
  \item{\dots}{
arguments passed to \code{this.path}.
  }
}
\details{
There are a few reasons why there would be no executing script:
\itemize{
  \item{
\R is being run from the command-line / / terminal without supplying argument
\option{-f} \option{FILE} or \option{--file=FILE}
  }
  \item{
\R is being run from the Unix terminal with option \option{-g} \option{Tk} or
\option{--gui=Tk} (GUI 'Tk' does not make use of its \option{--file=} argument)
  }
  \item{
\R is being run from \sQuote{RStudio} with no documents open
  }
  \item{
\R is being run from \sQuote{Rgui} with no documents open
  }
}
The following cases will still throw an error:
\itemize{
  \item{
unable to \code{\link[base:normalizePath]{normalize}} the executing script's
filename
  }
  \item{
using \code{this.path} within a URL
  }
  \item{
using \code{\link[base]{sys.source}} or
\code{\link[testthat:source_file]{testthat::source_file}} on a file named
\dQuote{clipboard}, \dQuote{clipboard-128}, or \dQuote{stdin}
  }
  \item{
active / / source document in \sQuote{RStudio} / / \sQuote{Rgui} does not exist
  }
  \item{
in \sQuote{AQUA}, running the current line or selection from an open \R script
(currently unimplemented)
  }
}
}
\value{
for \code{this.path2} or \code{this.dir2}, the executing script's filename
or directory, or \code{NULL} if unavailable.

for \code{this.dir3}, the executing script's directory, or the working directory
if unavailable.
}
\examples{
essentials::Rscript(exprs = r"{this.path::this.path()}" , mustWork = FALSE)
essentials::Rscript(exprs = r"{this.path::this.path2()}", mustWork = FALSE)

essentials::Rscript(exprs = r"{this.path::this.dir()}"  , mustWork = FALSE)
essentials::Rscript(exprs = r"{this.path::this.dir2()}" , mustWork = FALSE)
essentials::Rscript(exprs = r"{this.path::this.dir3()}" , mustWork = FALSE)
}
