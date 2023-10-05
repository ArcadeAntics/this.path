\name{FILE}
\alias{FILE}
\alias{LINE}
\title{Macros in Package 'this.path'}
\description{
  \code{FILE()} and \code{LINE()} are intended to be used in a similar manner
  to the macros \code{__FILE__} and \code{__LINE__} in C. They are useful for
  generating a diagnostic message / / warning / / error to about the status of
  the program.
}
\usage{
FILE()
LINE()
}
\examples{
FILE.R <- tempfile(fileext = ".R")
writeLines("fun <- function ()
{
    message(sprintf('invalid value \%d at \%s, line \%d',
        -1, FILE(), LINE()))
}
", FILE.R)
source(FILE.R, verbose = FALSE, keep.source = TRUE)
fun()
unlink(FILE.R)
}
