\name{shFILE}
\alias{shFILE}
\alias{normalized.shFILE}
\title{
Get Argument \option{FILE} Provided to R by a Shell
}
\description{
Look through the command line arguments, extracting \option{FILE} from either
of the following: \option{--file=FILE} or \option{-f} \option{FILE}
}
\usage{
shFILE(default, else.)
normalized.shFILE(default, else.)
}
\arguments{
  \item{default}{
if \option{FILE} was not found, this value is returned.
  }
  \item{else.}{
missing or a function to call on the return value if \option{FILE} is found.
See \code{\link{tryCatch2}} for inspiration.
  }
}
\value{
character string, or \code{default} if the command line argument \option{FILE}
was not found.
}
\note{
Both functions will save their return values; this makes them faster when
called subsequent times.

For \code{normalized.shFILE}, the path on Windows will use / as the file
separator.
}
\seealso{
\code{\link{this.path}}, \code{\link{here}}
}
\examples{
this.path:::write.code(file = FILE <- tempfile(), {

    withAutoprint({

        shFILE()
        normalized.shFILE()
        normalized.shFILE(default = {
            stop("interestingly enough, because 'FILE' will be found,\\n",
                "  argument 'default' won't be evaluated, and so this\\n",
                "  error won't actually print, isn't that neat? you can\\n",
                "  use this to your advantage in a similar manner, doing\\n",
                "  arbitrary things only if 'FILE' isn't found")
        })


    }, width.cutoff = 60L, verbose = FALSE)

})
this.path:::.Rscript(c("--default-packages=this.path", "--vanilla", FILE))


for (expr in c("shFILE()",
               "shFILE(default = NULL)",
               "normalized.shFILE()",
               "normalized.shFILE(default = NULL)"))
    this.path:::.Rscript(c("--default-packages=this.path", "--vanilla", "-e", expr))
\dontshow{unlink(FILE)}}
