\name{setReadWriteArgsMethod}
\alias{has.ext}
\alias{scan2}
\alias{format4scan}
\alias{setReadWriteArgsMethod}
\title{
Create and Save a Method for Reading/Writing Command-Line Arguments from/to a
File
}
\description{
Create a method for reading/writing command-line arguments from/to a file,
typically by differing file extension.
}
\usage{
has.ext(file, fileext, compression = FALSE, ignore.case = TRUE)

scan2(..., sep = "", quote = if (identical(sep, "\n")) "" else "'\"",
    comment.char = "", allowEscapes = FALSE)

format4scan(x, sep = " ", quote = "\\"'", comment.char = "",
    allowEscapes = FALSE, nlines.between.comment.and.args = 0,
    nlines.between.args = 2)

setReadWriteArgsMethod(name, condition, read, write)
}
\arguments{
  \item{file}{
character vector, the files to test for a file extension.
  }
  \item{fileext}{
character string, the file extension to look for. Special
\code{\link[base]{regex}} characters must be escaped.
  }
  \item{compression}{
logical. Should compressed files be included in the pattern matching?
  }
  \item{ignore.case}{
logical. Should the case of \code{file} be ignored when matching against
\code{fileext}?
  }
  \item{\dots}{
further arguments passed to \code{\link[base]{scan}}.
  }
  \item{sep}{
Empty character string, \code{NULL}, or a character string containing just one
single-byte character.
  }
  \item{quote}{
Character string or \code{NULL}, the set of quoting characters.
  }
  \item{comment.char}{
character string containing a single character or an empty string. Use \code{""}
to turn off the adding comments.
  }
  \item{allowEscapes}{
logical. Should C-style escapes be processed?
  }
  \item{x}{
any \R object. If a list, each element will be turned into a character vector
(in the same way as \code{\link{withArgs}} converts its arguments), otherwise
will be turned into a character vector (same as \code{withArgs}). The arguments
to be written.
  }
  \item{nlines.between.comment.and.args}{
a non-negative integer specifying the number of empty lines between each set of
comments and arguments.
  }
  \item{nlines.between.args}{
a non-negative integer specifying the number of empty lines between each set of
arguments. Only used for a list \code{x}.
  }
  \item{name}{
A character string naming the method.
  }
  \item{condition}{
a function accepting a single argument \code{file} (the file in which to
read/write). Should return \code{TRUE} if the file is appropriate for
reading/writing with this method, typically by examining the file extension.
  }
  \item{read}{
a function accepting a single argument \code{file}. Should read the arguments
from \code{file}.
  }
  \item{write}{
a function accepting arguments \code{x}, \code{comments},
\code{nlines.between.comment.and.args}, \code{nlines.between.args}. Should turn
\code{x} into a character vector of arguments to write to a file. This function
does \emph{NOT} have to make use of the other arguments.
  }
}
\examples{
# suppose you wanted to define your own method for
# reading/writing command-line arguments to a file. we'll
# say the file extension will be ".myargs". with this, we
# start by making 'condition'
condition <- function(file) has.ext(file, "\\\\.myargs")


# next, we will make a reading function. this will typically
# be some variation of 'scan2', but feel free to use
# anything else that works. for this example, we'll use
# "-" as the delimiter, "`" as the quoting character, and
# "/" as the comment character
read <- function(file) {
    scan2(file = file, sep = "-", quote = "`",
        comment.char = "/")
}


# next, we will make a writing function. this will typically
# be some variation of 'format4scan', but feel free to use
# anything else that works
write <- function(x, comments = TRUE,
    nlines.between.comment.and.args = 0,
    nlines.between.args = 2) {
    format4scan(x, sep = "-", quote = "`",
        comment.char = if (comments) "/" else "",
        nlines.between.comment.and.args = nlines.between.comment.and.args,
        nlines.between.args = nlines.between.args)
}


# now, combine it all together
setReadWriteArgsMethod(
    name      = "myargs",
    condition = condition,
    read      = read,
    write     = write
)


# try writing arguments with this new format
x <- letters
comment(x) <- "testing comments"
writeArgs(x, "", name = "myargs")


# confirm that writing and reading returns the same set of arguments
FILE <- writeArgs(x, fileext = ".myargs", at = FALSE)
y <- readArgs(FILE)
stopifnot(length(x) == length(y), x == y)
}
