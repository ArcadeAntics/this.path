\name{commandQuote}
\alias{commandQuote}
\alias{shEncode}
\title{
Quote Strings for Use in OS Shells
}
\description{
Quote a string to be passed to an operating system shell.
}
\usage{
commandQuote(string, type = NULL, unix.type = NULL, windows.type = "Rscript")

shEncode(string, type = NULL, unix.type = NULL, windows.type = "Rscript")
}
\arguments{
  \item{string}{
a character vector, usually of length one.
  }
  \item{type}{
character string: the type of quoting. Partial matching is supported.
  }
  \item{unix.type}{
character string: the type of quoting under Unix-alikes when \code{type} is
\code{NULL}.
  }
  \item{windows.type}{
character string: the type of quoting in Windows when \code{type} is
\code{NULL}.
  }
}
\details{
The choices for quoting are \code{"sh"}, \code{"Rscript"}, \code{"Rterm"}, and
\code{"R"}. The default for quoting under Unix-alikes is \code{"sh"}, while in
Windows it is \code{"Rscript"}.

From my testing with \code{\link[base]{shQuote}}, it seems like it fails in a
few edge cases that \code{commandQuote} can handle correctly.

Those few cases that \code{shQuote} handles incorrectly are:
\itemize{
  \item{under Unix-alikes}{
with \code{type = "csh"} and \code{string} contains a dollar sign or grave
accent and \code{string} has a trailing single quote, \code{string} loses its
trailing single quote.
  }
  \item{in Windows}{
with \code{type = "cmd"} and \code{string} contains a double quote preceded by
at least one backslash, the preceding backslashes aren't escaped. Also, when
\code{string} contains a set of trailing backslashes, they aren't escaped.
  }
}
}
\value{
character vector, the same length as \code{string}, with the attributes of
\code{string} (after possible coercion to character), excluding \code{class}.
}
\seealso{
\code{\link{Rscript}}, \code{\link{python}}
}
\examples{
fun <- function(string) {
    cat(c(
        "string  ", string                                , "\\n",
        "sh      ", commandQuote(string, type = "sh"     ), "\\n",
        "Rscript ", commandQuote(string, type = "Rscript"), "\\n",
        "R       ", commandQuote(string, type = "R"      ), "\\n"
    ), sep = "")
}

fun("abc$def`gh`i\\\\j")

fun("testing \\\\\\"this\\\\\\"")

fun("\\"testing\\" $this$ 'out'")


\dontrun{
this.path:::.system(paste(c(
    "perl",
    "-e",
    commandQuote(r"{print "test \\"this\\" out\\n";}", windows.type = "perl")
), collapse = " "))
}


\dontrun{
this.path:::.system(paste(c(
    "python",
    "-c",
    commandQuote(r"{print("test \\"this\\" out")}", windows.type = "python")
), collapse = " "))
}
}
