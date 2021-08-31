\name{python}
\alias{python}
\title{
Start a New Python Session
}
\description{
Start a new Python session with the specified options.

Make sure you have \href{https://www.python.org/downloads/}{python} installed.
}
\usage{
python(options = NULL, command = NULL, module = NULL, file = NULL,
    args = NULL, chdir = FALSE,
    intern = FALSE, wait = TRUE, mustWork = FALSE, ...,
    quiet = Sys.getenv("R_THIS_PATH_QUIET", intern))
}
\arguments{
  \item{options}{
character vector. The options to be provided for the new Python session.
  }
  \item{command}{
character vector. The expressions to be run in the new Python session, provided
to option \option{-c cmd}.
  }
  \item{module}{
character string. The library module to run as a script, provided to option
\option{-m mod}.
  }
  \item{file}{
character string. The filename of the Python script to run in the new Python
session, provided to option \option{file}
  }
  \item{args}{
Converted to character in a similar manner to \code{\link{withArgs}}.
  }
  \item{chdir}{
logical; if \code{TRUE} and \code{file} is a pathname, the \R working directory
is temporarily changed to the directory containing \code{file} for evaluating.
  }
  \item{intern}{
logical, indicates whether to make the output of the command an \R object.
  }
  \item{wait}{
should the \R interpreter wait for the command to finish? The default is to
wait, and the interpreter will always wait if \code{intern = TRUE}.
  }
  \item{mustWork}{
logical; if \code{TRUE} failure to run the command will give an \R error, if
\code{FALSE} a warning, and if \code{NA}, no \R message.
  }
  \item{\dots}{
further arguments passed to \code{\link[base]{system}()}.
  }
  \item{quiet}{
logical. Should the command-line / / terminal prompt and exit message be printed
before and after the Python session?
  }
}
\value{
The value of \code{\link[base]{system}()}. If \code{intern = FALSE}, it is
returned invisibly.
}
\examples{
\dontrun{
python(command = dedent(r"{
    print("Hello World!")
    print("An example python script...")
}"))
}
}
