\name{Args}
\alias{Args}
\title{
Extract Arguments Supplied to a Script / / From an Object of Class "ParsedArgs"
}
\description{
Provides access to a copy of the arguments supplied to the current \R script

\emph{OR}

to the arguments supplied to an object of class "ParsedArgs".
}
\usage{
Args(x, type = c("original", "all", "trailingOnly"))
}
\arguments{
  \item{x}{
missing or an object of class "ParsedArgs".
  }
  \item{type}{
missing or a string naming the type of arguments to extract from \code{x}.
  }
}
\details{
\code{Args()} is a replacement for \code{\link[base]{commandArgs}()}. In
addition to working from the command-line / / terminal, it also works in
combination with \code{\link{withArgs}}. When running from the command-line,
only the arguments following \code{--args} will be returned. If not being run
from the command-line or \code{\link{withArgs}}, \code{character(0)} is returned
instead.

If \code{x} and \code{type} are provided, then \code{Args} extracts the
arguments provided to that object of class "ParsedArgs". The types have the
following meaning:
\describe{
  \item{"original"}{
original arguments supplied when \code{x} was created
  }
  \item{"all"}{
arguments after resolving all from-file arguments
  }
  \item{"trailingOnly"}{
arguments supplied after the \code{"skip"} argument
  }
}
}
\value{
A character vector containing the user-supplied script arguments

\emph{OR}

a character vector containing the arguments supplied when \code{x} was created.
}
\examples{
FILE <- tempfile()
writeLines(deparse(quote(withAutoprint({


    # the regular command-line arguments
    commandArgs()


    # the script arguments
    this.path::Args()


}, verbose = FALSE))), FILE)


Rscript("--default-packages=NULL", FILE, args = pi)
withArgs(source(FILE, verbose = FALSE), pi)


unlink(FILE)
}
