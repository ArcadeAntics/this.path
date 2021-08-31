\name{parse.args}
\alias{parse.args}
\alias{parse.args,ArgumentParser-method}
\title{
Parse the Command-Line Arguments into an Environment
}
\description{
Parse any series of arguments into an environment using an argument parser.
}
\usage{
\S4method{parse.args}{ArgumentParser}(args = Args(),
    warnPartialMatchArgs = getOption("warnPartialMatchArgs", FALSE))
}
\arguments{
  \item{args}{
character vector. The arguments to parse into an environment.
  }
  \item{warnPartialMatchArgs}{
logical. If true, warns if partial matching is used in argument matching.
  }
}
\details{
When \code{args} is not provided, the arguments are provided from
\code{\link[base]{commandArgs}()}, but only if the executing script was run from
the command-line / / terminal.
}
\value{
an object of class "\code{ParsedCommandArgs}".
}
