\name{shFILE}
\alias{shFILE}
\title{
Get Argument \option{FILE} Provided to \R by a Shell
}
\description{
Look through the command line arguments, extracting \option{FILE} from either
of the following: \option{-f} \option{FILE} or \option{--file=FILE}
}
\usage{
shFILE(original = FALSE, for.msg = FALSE, default, else.)
}
\arguments{
  \item{original}{
\code{TRUE}, \code{FALSE}, or \code{NA}; should the original or the normalized
path be returned? \code{NA} means the normalized path will be returned if it
has already been forced, and the original path otherwise.
  }
  \item{for.msg}{
\code{TRUE} or \code{FALSE}; do you want the path for the purpose of printing a
diagnostic message / / warning / / error? \code{for.msg = TRUE} will ignore
\code{original = FALSE}, and will use \code{original = NA} instead.
  }
  \item{default}{
if \option{FILE} is not found, this value is returned.
  }
  \item{else.}{
missing or a function to apply if \option{FILE} is found. See
\code{\link{tryCatch2}()} for inspiration.
  }
}
\value{
character string, or \code{default} if the command line argument \option{FILE}
was not found.
}
\note{
The original and the normalized path are saved; this makes them faster when
called subsequent times.

In Windows, the normalized path will use / as the file separator.
}
\seealso{
\code{\link{this.path}()}, \code{\link{here}()}
}
\examples{
FILE <- tempfile(fileext = ".R")
this.path:::write.code({
    this.path:::withAutoprint({
        shFILE(original = TRUE)
        shFILE()
        shFILE(default = {
            stop("since 'FILE' will be found, argument 'default'\\n",
                " will not be evaluated, so this error will not be\\n",
                " thrown! you can use this to your advantage in a\\n",
                " similar manner, doing arbitrary things only if\\n",
                " 'FILE' is not found")
        })
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L)
}, FILE)
this.path:::.Rscript(c("--default-packages=this.path",
                       "--vanilla", FILE))


for (expr in c("shFILE(original = TRUE)",
               "shFILE(original = TRUE, default = NULL)",
               "shFILE()",
               "shFILE(default = NULL)"))
{
    cat("\\n\\n")
    this.path:::.Rscript(c(
        "--default-packages=this.path", "--vanilla", "-e", expr))
}
\dontshow{unlink(FILE)}}
