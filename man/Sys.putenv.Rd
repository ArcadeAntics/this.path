\name{Sys.putenv}
\alias{Sys.putenv}
\title{Set Environment Variables}
\description{
  \code{Sys.putenv()} sets environment variables (for other processes called
  from within \R or future calls to \codelink3{base}{Sys.getenv}{()} from this
  \R process).
}
\usage{
Sys.putenv(x)
}
\arguments{
  \item{x}{a character vector, or an object coercible to character. Strings
    must be of the form
    \ifelse{text}{"name=value"}{\ifelse{latex}{\code{"name=value"}}{\code{"\var{name}=\var{value}"}}}.}
}
\value{
  A logical vector, with elements being true if setting the corresponding
  variable succeeded.
}
\seealso{
  \codelink3{base}{Sys.setenv}{()}
}
\examples{
Sys.putenv(c("R_TEST=testit", "A+C=123"))
Sys.getenv("R_TEST")
Sys.unsetenv("R_TEST")  ## under Unix-alikes may warn and not succeed
Sys.getenv("R_TEST", unset = NA)
}

