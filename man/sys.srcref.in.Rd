\name{sys.srcref}
\alias{sys.srcref}
\alias{sys.whiches}
\title{Get 'srcref' From Call Stack}
\description{
  \code{sys.srcref()} is the R-level version of the function that implements
  \code{\link{this.path}()}. It retrieves the
  \code{\link[base:srcfile]{srcref}} of the call \code{n} generations back from
  itself.

  \code{sys.whiches()} is the R-level version of the function that implements
  \code{sys.srcref()}. It returns the frame numbers of all calls with a common
  parent frame \code{n} generations back from itself.
}
\usage{
sys.srcref(n = 1, which = if (n) sys.parent(n) else 0)
sys.whiches(n = 1, which = if (n) sys.parent(n) else 0)
}
\arguments{
  \item{n}{See \code{?\link{this.path}()}. However, note that \code{n} defaults
    to \code{0} in \code{this.path()} but defaults to \code{1} here.}

  \item{which}{frame number to inspect. An alternative to specifying \code{n}.}
}
\value{
  for \code{sys.srcref()}, a \code{srcref} object or \code{NULL}.

  for \code{sys.whiches()}, an integer vector.
}
\examples{
## this example will not work with 'Run examples'
## which uses 'package:knitr' since knitted
## documents do not store source references
fun <- function ()
{
    list(
        `sys.srcref()` = this.path::sys.srcref(),
        ## while this might seem like a simpler alternative,
        ## you will see it does not work in a couple cases below
        `attr(sys.call(sys.parent()), "srcref")` =
            attr(sys.call(sys.parent()), "srcref")
    )
}


## the braces are unnecessary when using example("sys.srcref"),
## but are needed when copied into the R Console
{ fun() }
{ print(fun()) }
{ try(print(fun())) }





fun2 <- function ()
{
    list(
        ## if copied directly into the R Console,
        ## should be 1 6 7
        ## 1 is the frame number associated with 'try'
        ## 6                                     'force'
        ## 7                                     'fun2'
        sys.whiches(),


        ## if copied directly into the R Console,
        ## should be  8 12 13
        ##  8 is the frame number associated with 'tryCatch'
        ## 12                                     'identity'
        ## 13                                     'sys.whiches'
        tryCatch(identity(sys.whiches(0)), error = function(e) NULL)
    )
}


try(force(fun2()))


}
\keyword{internal}
