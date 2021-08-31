\name{commandPrompt}
\alias{commandPrompt}
\title{
Replicate the Command-Line / / Terminal Prompts Seen on a Few Common Operating
Systems
}
\description{
Get the prompt seen at the Windows command-line / / Unix terminal. Currently
well defined for Windows, macOS, and Ubuntu. All other operating systems will
return the prompt seen at the Ubuntu terminal in hopes that it looks similar
enough to be understandable.
}
\usage{
commandPrompt(OS.type = NULL)
}
\arguments{
  \item{OS.type}{
\code{NULL} or a character string; the OS prompt to return.
  }
}
\details{
The choices of OS are \code{"windows"}, \code{"macOS"}, \code{"ubuntu"}, and
\code{"unix"}, with case insensitive partial matching.

If \code{OS.type} is a character string, it is matched against the above
choices. If \code{OS.type} is \code{NULL} or does not match one of the above
choices, \code{OS.type} is pulled from option
\option{this.path::commandPrompt(OS.type)}. If this option is \code{NULL} or
does not match one of the above choices, \code{OS.type} is pulled from
environment variable \env{R_THIS_PATH_COMMAND_PROMPT_OS_TYPE}. If this
environment variable is unset or does not match one of the above choices,
\code{OS.type} is determined by \code{\link[base]{.Platform}$OS.type}
and \code{\link[base]{capabilities}("aqua")}.
}
\value{
character string.
}
\examples{
cat(commandPrompt("windows"), commandPrompt("macOS"),
    commandPrompt("ubuntu"), sep = "\\n")
}
