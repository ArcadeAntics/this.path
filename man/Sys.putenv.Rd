\name{Sys.putenv}
\alias{Sys.putenv}
\title{
Set Environment Variables
}
\description{
\code{Sys.putenv()} sets environment variables (for other processes called from
within \R or future calls to \code{\link[base]{Sys.getenv}()} from this \R
process).
}
\usage{
Sys.putenv(x)
}
\arguments{
  \item{x}{
a character vector, or an object coercible to character. Strings must be of the
form \code{"\var{name}=\var{value}"}.
  }
}
\value{
A logical vector, with elements being true if setting the corresponding
variable succeeded.
}
\seealso{
\code{\link[base]{Sys.setenv}()}
}
\examples{
Sys.putenv(c("R_TEST=testit", "A+C=123"))
Sys.getenv("R_TEST")
Sys.unsetenv("R_TEST") # on Unix-alike may warn and not succeed
Sys.getenv("R_TEST", unset = NA)
}
