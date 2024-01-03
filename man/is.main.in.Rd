\name{is.main}
\alias{is.main}
\alias{from.shell}
\title{Top-Level Code Environment}
\description{
  Determine if a program is the main program, or if a program was run from a
  shell.
}
\usage{
is.main()
from.shell()
}
\details{
  If no scripts are being \sourcelink-ed, then \code{is.main()} will be
  \code{TRUE}, and \code{FALSE} otherwise.

  If \code{is.main()} is \code{TRUE} and \R was invoked with the command-line
  arguments \option{-f} \option{FILE}, \option{--file=FILE}, or \option{-e}
  \option{EXPR}, then \code{from.shell()} will be \code{TRUE}, and \code{FALSE}
  otherwise.
}
\value{
  \code{TRUE} or \code{FALSE}.
}
\examples{
FILE.R <- tempfile(fileext = ".R")
this.path:::.writeCode({
    this.path:::.withAutoprint({
        from.shell()
        is.main()
    }, spaced = TRUE, verbose = FALSE,
       prompt.echo = "FILE.R> ", continue.echo = "FILE.R+ ")
}, FILE.R)

this.path:::.Rscript(c(
    "--default-packages=this.path", "--vanilla",
    FILE.R
), show.command = FALSE, show.output.on.console = TRUE)

source(FILE.R, verbose = FALSE)

this.path:::.Rscript(c(
    "--default-packages=this.path", "--vanilla",
    "-e", "cat(\\"\\n> from.shell()\\\\n\\")",
    "-e", "from.shell()",
    "-e", "cat(\\"\\n> is.main()\\\\n\\")",
    "-e", "is.main()",
    "-e", "cat(\\"\\n> source(commandArgs(trailingOnly = TRUE))\\\\n\\")",
    "-e", "source(commandArgs(trailingOnly = TRUE))", FILE.R
), show.command = FALSE, show.output.on.console = TRUE)

unlink(FILE.R)
}
\keyword{internal}
