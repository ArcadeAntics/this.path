\name{ArgumentParser}
\alias{ArgumentParser}
\alias{ArgumentParser-class}
\alias{this.path_ArgumentParser-class}
\title{
Argument Parser
}
\description{
Create an object of class \code{"this.path_ArgumentParser"}, for
parsing the command-line arguments into a more usable form.
}
\usage{
ArgumentParser(program = NA, usage = NA, description = NA, epilogue = NA,
    add.help = TRUE,
    wrap = TRUE, indent = 0, exdent = 0,
    wrap.description = wrap,
        indent.description = indent,
        exdent.description = exdent,
    wrap.epilogue = wrap,
        indent.epilogue = indent,
        exdent.epilogue = exdent,
    style = NA, wrap.help = FALSE, help.help = default.help("help"),
    ..., help.message = NULL)
}
\arguments{
  \item{program}{
character string. The name of the program (by default, the
\code{\link[base]{basename}} of the executing script).
  }
  \item{usage}{
character string. The usage of the program (by default, determined by
\code{program} and any formal arguments provided by \code{\link{add.argument}}).
  }
  \item{description, epilogue}{
character vector. Messages to display before and after the arguments help.
  }
  \item{add.help}{
logical. Should a help option be added to the parser?
  }
  \item{wrap.help, help.help, \dots}{
further arguments passed to \code{\link{add.help}}.
  }
  \item{wrap, wrap.description, wrap.epilogue}{
logical. Should the messages be \code{\link[base:strwrap]{wrapped}} when
printing the help message for the argument parser?
  }
  \item{indent, indent.description, indent.epilogue}{
a non-negative integer giving the indentation of the first line in a paragraph.
  }
  \item{exdent, exdent.description, exdent.epilogue}{
a non-negative integer giving the indentation of the subsequent lines in a
paragraph.
  }
  \item{style}{

  }
  \item{help.message}{
character vector or \code{NULL}. The help message to display. If \code{NULL},
the help message will be created from \code{usage}, \code{description},
\code{epilogue}, and any arguments provided by \code{\link{add.argument}}.
  }
}
\details{
All of the above arguments help form the documentation for your argument parser.
The documentation will be printed in a similar manner to R's command-line
documentation. First, the usage will be printed. This includes the name of the
program (the \code{\link[base]{basename}} of the executing script), the names of
the formal arguments, and the names of the sub-commands (if any). Second, the
description of the program will be printed


An object of class \code{"this.path_ArgumentParser"} is a
\code{\link[methods:refClass]{reference}} class. This allows us to use
argument parsers in an almost identical manner to the
\href{https://docs.python.org/3/library/argparse.html#module-argparse}{
argparse} library from Python. All arguments to \code{ArgumentParser} are used
for printing a help message.
}
\value{
An object of class \code{"this.path_ArgumentParser"}.
}
\section{Methods}{
\describe{
  \item{\code{\link{add.argument}}}{
Declare a formal argument for an argument parser.
  }
  \item{\code{\link{add.subparsers}}}{
Declare
  }
}
}
\references{
The
\href{https://docs.python.org/3/library/argparse.html#module-argparse}{
argparse} library for Python.
}
\examples{
x <- ArgumentParser()
x$print.help()


y <- ArgumentParser(description = "A description for the program",
    epilogue = c("  --- Final Message ---   ",
        "A final message for the program, do not wrap this message"),
    wrap.epilogue = FALSE)
y$print.help()
}
