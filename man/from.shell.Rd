\name{from.shell}
\alias{from.shell}
\alias{is.main}
\title{
Top-Level Code Environment
}
\description{
Determine if a program is the main program, or if an \R script was run from a
shell.
}
\usage{
from.shell()
is.main()
}
\details{
When an \R script is run from a shell, \code{from.shell()} and \code{is.main()}
will both be \code{TRUE}. If that script sources another \R script,
\code{from.shell()} and \code{is.main()} will both be \code{FALSE} for the
duration of the second script.

Otherwise, \code{from.shell()} will be \code{FALSE}. \code{is.main()} will be
\code{TRUE} when there is no executing script or when source-ing a script in a
toplevel context, and \code{FALSE} otherwise.
}
\value{
\code{TRUE} or \code{FALSE}.
}
\examples{
FILES <- tempfile(c("file1_", "file2_"), fileext = ".R")
this.path:::write.code({
    from.shell()
    is.main()
}, FILES[2])
this.path:::write.code((
    bquote(this.path:::withAutoprint({
        from.shell()
        is.main()
        source(.(FILES[2]), echo = TRUE, verbose = FALSE,
            prompt.echo = "file2> ", continue.echo = "file2+ ")
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L,
       prompt.echo = "file1> ", continue.echo = "file1+ "))
), FILES[1])


this.path:::.Rscript(c("--default-packages=this.path",
                       "--vanilla", FILES[1]))


this.path:::.Rscript(c("--default-packages=this.path", "--vanilla",
    "-e", "cat(\\"\\n> from.shell()\\\\n\\")",
    "-e", "from.shell()",
    "-e", "cat(\\"\\n> is.main()\\\\n\\")",
    "-e", "is.main()",
    "-e", "cat(\\"\\n> source(commandArgs(TRUE)[[1L]])\\\\n\\")",
    "-e", "source(commandArgs(TRUE)[[1L]])",
    FILES[1]))
\dontshow{unlink(FILES)}}
