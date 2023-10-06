\name{GetSrcref}
\alias{GetSrcref}
\title{Get 'srcref' From Call Stack}
\description{
  This is the R-level version of the function that implements
  \code{\link{this.path}()}. It retrieves the
  \code{\link[base:srcfile]{srcref}} of the call to the function which contains
  \code{GetSrcref()}, optionally \code{n} additional generations back.
}
\usage{
GetSrcref(n = 0)
}
\arguments{
  \item{n}{See \code{?\link{this.path}()}.}
}
\value{
  \code{srcref} or \code{NULL}.
}
\examples{
fun <- function(n = 0) {
    list(
        `GetSrcref(n)` = this.path::GetSrcref(n),
        ## while this might seem like a simpler alternative,
        ## you will see it does not work in a couple cases below
        `attr(sys.call(sys.parent(n)), "srcref")` =
            attr(sys.call(sys.parent(n)), "srcref")
    )
}


fun()
print(fun())
try(print(fun()))
}
