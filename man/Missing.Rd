\name{Missing}
\alias{Missing}
\title{
Does an Argument have a Value?
}
\description{
\code{Missing} can be used to test whether a value is the missing argument.
}
\usage{
Missing(x, name)
}
\arguments{
  \item{x}{
an object of class \code{"ParsedArgs"}.
  }
  \item{name}{
character string or symbol, the name of the argument to test for missing-ness.
  }
}
\details{
\code{Missing} will return \code{FALSE} when an argument has a value, even if it
wasn't provided. See \strong{Examples}.

\code{Missing} does not evaluate its argument \code{name}, similar to
\code{missing}.
}
\value{
\code{TRUE} or \code{FALSE}
}
\examples{
parser <- this.path::ArgumentParser()
parser$add.argument("--arg1")
parser$add.argument("--arg2", default = "def")
pargs <- parser$parse.args()


list(
    arg1 = this.path::Missing(pargs, arg1),
    arg2 = this.path::Missing(pargs, "arg2")
)


# with R >= 4.1.0, use the forward pipe operator `|>` to
# make calls to `Missing` more intuitive:
# list(
#     arg1 = pargs |> this.path::Missing(arg1),
#     arg2 = pargs |> this.path::Missing("arg2")
# )
}
