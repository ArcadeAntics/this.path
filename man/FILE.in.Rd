\name{FILE}
\alias{FILE}
\alias{LINE}
\title{Active Bindings in Package 'this.path'}
\description{
  These variables are active bindings, meaning they call functions when their
  values are requested.

  \code{FILE} links to \code{\link{try.this.path}()}.

  \code{LINE} links to \code{\link{LINENO}()}.
}
\usage{
FILE
LINE
}
\details{
  These are intended to be used in a similar manner to the macros
  \code{__FILE__} and \code{__LINE__} in C. They are useful for generating an
  error message to report an inconsistency detected by the program; the message
  can state the source line at which the inconsistency was detected.
}
\examples{
FILE.R <- tempfile(fileext = ".R")
writeLines("fun <- function ()
{
    message(sprintf('invalid value \%d at \%s, line \%d',
        ## do not forget the braces around 'FILE' and 'LINE'
        ## see section Note of ?this.path for more details
        -1, { FILE }, { LINE }))
}", FILE.R)
source(FILE.R, verbose = FALSE, keep.source = TRUE)
fun()
unlink(FILE.R)
}
