\name{add.subparsers}
\alias{add.subparsers}
\alias{add.subparsers,ArgumentParser-method}
\alias{add.subparsers,this.path_ArgumentParser-method}
\title{
Declare a List of Sub-Commands for an \code{\link{ArgumentParser}}.
}
\description{
Split functionality of a script into any number of sub-commands.
}
\usage{
\S4method{add.subparsers}{this.path_ArgumentParser}(title = NA, description = NA, program = NA, required = FALSE,
    wrap = TRUE, indent = 0, exdent = 0)
}
\arguments{
  \item{title}{
character string. The name of the sub-commands group in the help message, by
default "Commands:".
  }
  \item{description}{
character vector. A brief description of the sub-commands group.
  }
  \item{program}{
character string. The name of the program to be displayed in the help message,
by default the name of the original program followed by all previous
sub-commands.
  }
  \item{required}{
logical. Are one of these sub-commands required? For example, in \command{R},
the sub-command \command{CMD} is not required, whereas in \command{R CMD}, the
sub-commands \command{INSTALL}, \command{REMOVE}, \command{...} are required,
you must provide one of them.
  }
  \item{wrap}{
logical. Should \code{description} be \code{\link[base:strwrap]{wrapped}} when
printing the help message for the argument parser?
  }
  \item{indent}{
a non-negative integer giving the indentation of the first line in a paragraph.
  }
  \item{exdent}{
a non-negative integer giving the indentation of the subsequent lines in a
paragraph.
  }
}
\details{
\code{add.subparsers} will return an object of class "subparsers", which can be
used to add individual sub-commands with \code{add.parser}.
}
\value{
An object of class "subparsers".
}
