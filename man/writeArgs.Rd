\name{writeArgs}
\alias{writeArgs}
\title{
Write Command-Line Arguments to a File
}
\description{
When providing arguments to an \R script, it is often necessary to quote the
arguments such that they are interpreted correctly by the shell before being
passed to the script. However, this comes with the issue of different quoting
conventions for different shells, meaning that the strings will not be
interpreted the same between different shells.

You can get around this with \code{writeArgs} which will right your arguments
(and comments!) to a file, and returns a filename that can be used in the
arguments place.
}
\usage{
writeArgs(x, file = tempfile("args"), comments = TRUE,
    nlines.between.comment.and.args = 0, nlines.between.args = 2,
    at = TRUE)
}
\arguments{
  \item{x}{
any \R object. If a list, each element will be turned into a character vector
(in the same way as \code{\link{withArgs}} converts its arguments), otherwise
will be turned into a character vector (same as \code{withArgs}). The arguments
to be written.
  }
  \item{file}{
character string or \code{NULL}. Name of the file to be read from, or \code{""}
to write to \code{\link[base:showConnections]{stdout}()} (intended for verifying
the output looks as expected), or \code{NULL} to return the formatted text
without writing anywhere.
  }
  \item{comments}{
logical. Should the \code{\link[base:comment]{comments}} of \code{x} be written
as well?
  }
  \item{nlines.between.comment.and.args}{
a non-negative integer specifying the number of empty lines between each set of
comments and arguments.
  }
  \item{nlines.between.args}{
a non-negative integer specifying the number of empty lines between each set of
arguments. Only used for a list \code{x}.
  }
  \item{at}{
logical. Should an \code{"@"} character be placed before the returned filename?
  }
}
\details{
\code{writeArgs} tries to space the text such that the file will look nice to
open with both any generic text editor and \sQuote{Excel}. Each line of
\emph{arguments} (not comments) will have at most 80 characters (to look nice in
a text editor) or at most 10 columns (to look nice in \sQuote{Excel}), whichever
comes first (though this rule is broken by strings with more than 80
characters).
}
\value{
for \code{file = NULL}, the formatted text.

for \code{file = ""}, the formatted text invisibly.

otherwise, a character string naming a file containing your arguments.
}
\note{
The following characters will not be valid on Windows (under an ISO8859-1
locale):

\code{"\\x81"}, \code{"\\x8D"}, \code{"\\x8F"}, \code{"\\x90"}, and
\code{"\\x9D"}

It appears as though these characters are unused in extended-ASCII, so I have no
intention of fixing this.
}
\examples{
x <- letters; writeArgs(x, file = "")


comment(x) <- dedent("
    adding a comment
    to our arguments
"); writeArgs(x, file = "")


x <- list(


    local({
        x <- c("\\xC5", "\\xC9", "\\xD8", "\\xEC", "\\xFC")
        Encoding(x) <- "latin1"
        comment(x) <- "accented characters"
        x
    }),


    local({
        x <- c("\\u{03C3}", "\\u{03B4}")
        comment(x) <- 'greek letters (default encoding "UTF-8")'
        x
    }),


    local({
        x <- "fa\\xE7ile"
        Encoding(x) <- "latin1"  # x is intended to be in latin1
        comment(x) <- dedent(r"{
            another non-ASCII character ("unknown" in UTF-8 locale,
            "latin1" in IS08859-1, ...)
        }")
        x
    }),


    local({
        x <- "\\u{7B90}\\u{5316}\\u{5B57}"
        comment(x) <- 'chinese characters (default encoding "UTF-8")'
        x
    }),


    local({
        x <- c("\\U{0001D11E}", "\\U{0001D4D7}")
        comment(x) <- "rarer characters outside the usual 16^4 range"
        x
    }),


    local({
        x <- dedent(r"{
            this would be 'rather' annoying to quote for a `shell` on $Unix$,
            and even "more" so on Windows because of the \"double\" quotes!
        }")
        comment(x) <- "all ASCII characters, annoying to quote, hard to read"
        x
    })
)
comment(x) <- dedent("
    these are some unusual characters
    but should still behave correctly
")


FILE <- writeArgs(x, at = FALSE)


# for the purpose of comparison, we need 'x' to be a character vector
x <- unlist(x)


y <- readArgs(FILE)


# let's check that the arguments in `x` match the
# arguments written to and read back from `FILE` (in `y`)
#
# we don't use 'identical(x, y)' because 'x' has
# attributes (a comment) while 'y' does not
if (length(x) != length(y)) {
    cat(gettextf("Catastrophic failure, wrote \%d arguments, read \%d\\n",
        length(x), length(y)),
        file = stderr())
    stop("Please contact the package maintainer ",
        sQuote(utils::packageDescription("this.path")$Maintainer))
} else if (any(i <- x != y)) {
    cat(ngettext(sum(i),
        "The following argument was written or read incorrectly!\\n",
        "The following arguments were written or read incorrectly!\\n"),
        file = stderr())
    print(x[i])
    cat("\\nIncorrectly written or read as:\\n", file = stderr())
    print(y[i])
    stop("Please contact the package maintainer ",
        sQuote(utils::packageDescription("this.path")$Maintainer))
} else {
    cat("Yay! The arguments were written and read correctly!\\n")
    print(x)
}


cat(
    dedent(r"{
        parser <- ArgumentParser()
        parser$add.argument("args", nargs = "*")
        pargs <- parser$parse.args()
        print(pargs$args)
    }"),
    file = script <- tempfile(), sep = "\\n"
)


withArgs(
    source(script, local = TRUE, echo = TRUE),
    paste0("@", FILE)
)


unlink(script)


if (interactive() &&
    isTRUE(utils::askYesNo("Would you like to see the arguments file?"))) {
    file.open(FILE)
    invisible(readline("Hit <Return> to delete arguments file: "))
}
unlink(FILE)
}
