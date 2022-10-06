\name{shFILE}
\alias{shFILE}
\title{
Get Argument \option{FILE} Provided to R by a Shell
}
\description{
Look through the command line arguments, extracting \option{FILE} from either
of the following: \option{--file=FILE} or \option{-f} \option{FILE}
}
\usage{
shFILE(original = FALSE, default, else.)
}
\arguments{
  \item{original}{
\code{TRUE} or \code{FALSE}; should the original or the normalized
\option{FILE} be returned?
  }
  \item{default}{
if \option{FILE} is not found, this value is returned.
  }
  \item{else.}{
missing or a function to apply if \option{FILE} is found. See
\code{\link{tryCatch2}} for inspiration.
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
\code{\link{this.path}}, \code{\link{here}}
}
\examples{
this.path:::write.code(file = FILE <- tempfile(), {

    withAutoprint({

        shFILE(original = TRUE)
        shFILE()
        shFILE(default = {
            stop("interestingly enough, because 'FILE' will be found,\\n",
                "  argument 'default' won't be evaluated, and so this\\n",
                "  error won't actually print, isn't that neat? you can\\n",
                "  use this to your advantage in a similar manner, doing\\n",
                "  arbitrary things only if 'FILE' isn't found")
        })


    }, width.cutoff = 60L, verbose = FALSE)

})
this.path:::.Rscript(c("--default-packages=this.path", "--vanilla", FILE))


for (expr in c("shFILE(original = TRUE)",
               "shFILE(original = TRUE, default = NULL)",
               "shFILE()",
               "shFILE(default = NULL)"))
{
    this.path:::.Rscript(c("--default-packages=this.path", "--vanilla", "-e", expr))
    cat("\\n")
}
\dontshow{unlink(FILE)}}
