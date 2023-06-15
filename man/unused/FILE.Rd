\name{FILE}
\alias{FILE}
\alias{LINE}
\title{Active Bindings in Package \pkg{this.path}}
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
