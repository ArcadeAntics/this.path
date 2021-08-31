\name{delayedAssign2}
\alias{delayedAssign2}
\title{
Delay Evaluation
}
\description{
\code{delayedAssign2} creates a \emph{promise} to evaluate the given expression
if its value is requested.
}
\usage{
delayedAssign2(x, value, eval.env = parent.frame(1),
    assign.env = parent.frame(1), evaluated = FALSE)
}
\arguments{
  \item{x}{
a variable name (given as a quoted string in the function call)
  }
  \item{value}{
an expression to be assigned to \code{x}
  }
  \item{eval.env}{
an environment in which to evaluate \code{value}
  }
  \item{assign.env}{
an environment in which to assign \code{x}
  }
  \item{evaluated}{
logical; should \code{value} be evaluated before making the promise?
  }
}
\details{
This function is built upon \code{\link[base]{delayedAssign}}, with the extra
argument \code{evaluated}. This is helpful in situations where an expression has
already been grabbed with \code{substitute} somewhere else that you now wish to
make into a promise.

While not implemented yet, this is used for the lazy default evaluation in
\code{\link{ArgumentParser}}.
}
\value{
\code{NULL} invisibly.
}
\examples{
# this is a simplified version of what ArgumentParser does with its arguments.
# ArgumentParser hides many details that should be shown here


# make a list like ArgumentParser
args <- list()
add.argument <- function (name, default)
{
    args[[length(args) + 1]] <<-
        list(name = name, default = substitute(default))
    invisible()
}


# these would normally be added by 'ArgumentParser()$add.argument'
add.argument("--alpha", TRUE     )
add.argument("--beta" , `--alpha`)
add.argument("--gamma", `--alpha`)


# this would normally be the environment
# returned by 'ArgumentParser()$parse.args'
value <- new.env()


for (n in seq_along(args)) {


    # we have 'evaluated = TRUE' here because we don't want 'args[[n]]$default'
    # to be the expression of the promise, but whatever 'args[[n]]$default'
    # evaluates to
    delayedAssign2(args[[n]]$name, args[[n]]$default,
        eval.env = value, assign.env = value,
        evaluated = TRUE)


    # this part would normally be more complex to deal with things like
    # multiple names for 1 argument, 'type', 'choices',  and, of course,
    # providing arguments, but i only want to demonstrate 'delayedAssign2' here
}


# then we force evaluate each argument
for (n in seq_along(args)) {
    get(args[[n]]$name, envir = value, inherits = FALSE)
}


# these are all TRUE, could be all NA or FALSE if argument '--alpha=NA' or
# '--alpha=FALSE' was provided to the ArgumentParser
print(as.list(value, all.names = TRUE))
}
