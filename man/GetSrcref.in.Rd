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
GetSrcref(n = 1, which = if (n) sys.parent(n) else 0)
}
\arguments{
  \item{n}{See \code{?\link{this.path}()}. However, note that \code{n} defaults
    to \code{0} in \code{this.path()} but defaults to \code{1} here.}

  \item{which}{the frame number to inspect for source references. An
    alternative to specifying \code{n}.}
}
\value{
  A \code{srcref} object or \code{NULL}.
}
\examples{
## this example will not work with 'Run examples' which uses {knitr}
## since knitted documents do not store source references
fun <- function() {
    list(
        `GetSrcref()` = this.path::GetSrcref(),
        ## while this might seem like a simpler alternative,
        ## you will see it does not work in a couple cases below
        `attr(sys.call(sys.parent()), "srcref")` =
            attr(sys.call(sys.parent()), "srcref")
    )
}

%% the braces are needed when copied into console
{ fun() }
{ print(fun()) }
{ try(print(fun())) }
}
