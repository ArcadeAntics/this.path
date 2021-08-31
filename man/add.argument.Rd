\name{add.argument}
\alias{add.argument}
\alias{add.argument,ArgumentParser-method}
\alias{add.argument,this.path_ArgumentParser-method}
\title{
Declare a Formal Argument for an Argument Parser
}
\description{
Set the formal arguments of an \code{\link{ArgumentParser}}.
}
\usage{
\S4method{add.argument}{this.path_ArgumentParser}(..., action = NULL, nargs = NULL, constant, default,
    type = "any", choices = NULL, required = NA, help = NA,
    metavariable = NA, destination = NA, wrap = TRUE,
    overwrite = getOption("this.path.overwrite", TRUE))
}
\arguments{
  \item{...}{
character strings. A name for a positional argument or a set of flags for an
optional argument.
  }
  \item{action}{
character string. One of \code{"store"}, \code{"store_const"},
\code{"store_true"}, \code{"store_false"}, \code{"append"}, \code{"count"},
\code{"help"}, \code{"exit"}, and \code{"skip"}, indicating what to do when this
argument is encountered.
  }
  \item{nargs}{
number of arguments expected for this formal argument. A positive number
specifying exactly the accepted number of arguments, two numbers specifying the
range of the accepted number of arguments, or a character string indicating the
accepted number of arguments, see \strong{Details}.
  }
  \item{constant}{
for action \code{"store_const"}, the object to be stored when this argument is
encountered.
  }
  \item{default}{
the object to be stored when this argument is not encountered.
  }
  \item{type}{
character string naming an atomic mode or \code{"list"} or \code{"expression"}
or \code{"any"}. If \code{"any"}, the appropriate type is decided based on
\code{action} and \code{default}.
  }
  \item{choices}{

  }
  \item{required}{
logical. Is this argument required?
  }
  \item{help}{
character vector. A brief description of the formal argument.
  }
  \item{metavariable}{
character string. A name for the value in usage messages.
  }
  \item{destination}{
character string. A name for the assigned value returned by
\code{\link{parse.args}}.
  }
  \item{wrap}{
logical. Should the usage message \code{help} be
\code{\link[base:strwrap]{wrapped}} when printing the help message for the
argument parser?
  }
  \item{overwrite}{
logical. If the name or flags are already in use, should they be replaced by
this new definition?
  }
}
\details{
A positional argument is an argument in which the name of argument is not
specified before its value (its value is assigned to the next available
positional argument). The other types of arguments are arguments specified by a
leading flag, either a short flag or a long flag.

\code{action} is an instruction for what to do when this argument is found at
the command-line.
\describe{
  \item{\code{action = "store"}}{
Stores the argument's value. When multiple values are provided, the last value
is stored and the others are discarded.
  }
  \item{\code{action = "store_const"}}{
If this argument is provided, \code{constant} is stored, otherwise
\code{default} is stored.
  }
  \item{\code{action = "store_true", "store_false"}}{
Special cases of \code{action = "store_const"} in which \code{constant = TRUE},
\code{default = FALSE} and \code{constant = FALSE}, \code{default = TRUE},
respectively. \code{default} can be overwritten for special cases.
  }
  \item{\code{action = "append"}}{
Stores the argument's value. When multiple values are provided, the values are
appended and stored together.
  }
  \item{\code{action = "count"}}{
Store the number of times this argument was provided.
  }
  \item{\code{action = "help"}}{
Print the help message and exit the program.
  }
  \item{\code{action = "exit"}}{
Print an exit message and exit the program.
  }
  \item{\code{action = "skip"}}{
Skip the rest of the commmand line (the following arguments are for another
program).
  }
}

\code{nargs} can be specified in three manners:
\itemize{
  \item{
A positive number, the exact number of arguments that must be provided.
Specifying \code{nargs} in this manner does not make sense with \code{action}
\code{"store_const"}, \code{"store_true"}, \code{"store_false"}, and
\code{"count"}, or with \code{action} \code{"store"} when \code{nargs} is
greater than \code{1}, and will signal a warning.
  }
  \item{
Two non-negative numbers, a lower and upper bound on the number of arguments
that must be provided. Specifying \code{0} as the lower bound indicates no lower
limit, and \code{Inf} as the upper bound indicates no upper limit.
  }
  \item{
A character string, specifically one of the \code{\link[base]{regex}} repetition
quantifiers.
\describe{
  \item{\code{?}}{
At most one argument.
  }
  \item{\code{*}}{
Zero or more arguments.
  }
  \item{\code{+}}{
One or more arguments.
  }
  \item{\code{\{n\}}}{
Exactly \code{n} arguments.
  }
  \item{\code{\{n,\}}}{
At least \code{n} arguments.
  }
  \item{\code{\{n,m\}}}{
At least \code{n} arguments, at most \code{m} arguments.
  }
}
  }
}
When \code{nargs} is \code{NULL}, \code{required} is used in its place.
}
\value{
Invisible \code{NULL}.
}
