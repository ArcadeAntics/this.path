\name{add.help}
\alias{add.help}
\alias{add.skip}
\alias{add.version}
\alias{add.help,ArgumentParser-method}
\alias{add.skip,ArgumentParser-method}
\alias{add.version,ArgumentParser-method}
\alias{add.help,this.path_ArgumentParser-method}
\alias{add.skip,this.path_ArgumentParser-method}
\alias{add.version,this.path_ArgumentParser-method}
\title{
Add Common Arguments to an ArgumentParser
}
\description{
Convenience functions for adding help, skip, and version arguments to an
ArgumentParser.

A help argument will print the help message when requested, a skip argument will
indicate that the following arguments are for another program (and hence, will
not be parsed by the parser), and a version argument will print the version of
the running program.
}
\usage{
\S4method{add.help}{this.path_ArgumentParser}(name.or.flags = c("-h", "--help"), action = "help",
    help = default.help("help"), wrap = FALSE, ...)

\S4method{add.skip}{this.path_ArgumentParser}(name.or.flags = "--args", action = "skip",
    help = default.help("skip"), wrap = FALSE, ...)

\S4method{add.version}{this.path_ArgumentParser}(name.or.flags = "--version", action = "exit",
    help = default.help("version"), wrap = FALSE, exit, ...)
}
\arguments{
  \item{name.or.flags}{
character vector. The name or flags of the argument.
  }
  \item{action}{
character string. Indicates what to do when this argument is encountered.
  }
  \item{help}{
character vector. A brief description of the formal argument.
  }
  \item{wrap}{
logical. Should the usage message \code{help} be
\code{\link[base:strwrap]{wrapped}} when printing the help message for the
argument parser?
  }
  \item{exit}{
character vector. The text to be printed before terminating the program.
  }
  \item{\dots}{
further arguments passed to \code{\link{add.argument}}.
  }
}
\value{
Invisible \code{NULL}.
}
\note{
Unlike Python's argparse, a version argument is a specific case of an exit
argument, an argument which will print an exit message before the program is
terminated. For example, on Unix, the \option{RHOME} argument for the \sQuote{R}
executable is an exit argument, and its exit message is the user's home
directory.
}
