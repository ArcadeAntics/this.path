\name{shFILE}
\alias{shFILE}
\title{Get 'FILE' Provided to R by a Shell}
\description{
  Look through the command line arguments, extracting \option{FILE} from either
  of the following: \option{-f} \option{FILE} or \option{--file=FILE}
}
\usage{
shFILE(original = FALSE, for.msg = FALSE, default, else.)
}
\arguments{
  \item{original}{\code{TRUE}, \code{FALSE}, or \code{NA}; should the original
    or the normalized path be returned? \code{NA} means the normalized path
    will be returned if it has already been forced, and the original path
    otherwise.}

  \item{for.msg}{\code{TRUE} or \code{FALSE}; do you want the path for the
    purpose of printing a diagnostic message / / warning / / error?
    \code{for.msg = TRUE} will ignore \code{original = FALSE}, and will use
    \code{original = NA} instead.}

  \item{default}{if \option{FILE} is not found, this value is returned.}

  \item{else.}{missing or a function to apply if \option{FILE} is found. See
    \ifelse{html}{\code{\link{tryCatch2}()}}{\code{tryCatch2()}} for
    inspiration.}
}
\value{
  character string, or \code{default} if \option{FILE} was not found.
}
\note{
  The original and the normalized path are saved; this makes them faster when
  called subsequent times.

  On Windows, the normalized path will use \ifelse{text}{/}{\code{/}} as the
  file separator.
}
\seealso{
  \code{\link{this.path}()}, \code{\link{here}()}
}
\examples{
FILE.R <- tempfile(fileext = ".R")
this.path:::.writeCode({
    this.path:::.withAutoprint({
        shFILE(original = TRUE)
        shFILE()
        shFILE(default = {
            stop("since 'FILE.R' will be found,\\n",
                 "this error will not be thrown")
        })
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L)
}, FILE.R)
this.path:::.Rscript(
    c("--default-packages=this.path", "--vanilla", FILE.R)
)
unlink(FILE.R)


for (expr in c("shFILE(original = TRUE)",
               "shFILE(original = TRUE, default = NULL)",
               "shFILE()",
               "shFILE(default = NULL)"))
{
    cat("\\n\\n")
    this.path:::.Rscript(
        c("--default-packages=this.path", "--vanilla", "-e", expr)
    )
}
}
