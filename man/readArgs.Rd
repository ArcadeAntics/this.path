\name{readArgs}
\alias{readArgs}
\alias{writeArgs}
\title{
Read/Write Command-Line Arguments from/to a File
}
\description{
When providing arguments to an \R script, it is often necessary to quote the
arguments such that they are interpreted correctly by the shell before being
passed to the script. However, this comes with the issue of different quoting
conventions for different shells, meaning that the strings will not be
interpreted the same between different shells. Additionally, some characters may
not be allowed for certain shells.

You can get around this with \code{writeArgs} which will right your arguments
(and comments!) to a file, and returns a filename that can be used in the
arguments place.

\code{readArgs} is the corresponding function to read those arguments back into
\R.
}
\usage{
writeArgs(x, file = tempfile(pattern = pattern, fileext = fileext),
    pattern = "args", fileext = ".Rargs", comments = TRUE,
    nlines.between.comment.and.args = 0, nlines.between.args = 2,
    at = TRUE, name = NULL)

readArgs(file, name = NULL)
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
  \item{pattern, fileext}{
character string. Allows for easier means of editing the filename created.
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
  \item{name}{
The name of the format for reading/writing, see \strong{Details}. For writing,
will be ignored if \code{file} is a non-empty character string.
  }
}
\details{
\code{readArgs} and \code{writeArgs} accept many formats for reading and writing
arguments, based on the file extension.

\itemize{
  \item{R arguments, extension \code{".Rargs"}}{
Arguments are read as though they were \R strings, separated by newline
\code{"\\n"} or semicolon \code{" ; "}. This means arguments must be quoted and
can use any of the escape sequences seen in \link[base]{Quotes}. The most
reliable method, also the slowest (but not by much). Can contain \R comments,
will be ignored when read.
  }
  \item{Python arguments, extension \code{".pyargs"}}{
Arguments are read one per line. No quoting rules apply. Arguments cannot
contain newline or carriage return, and will not be read/written properly if so.
The fastest method. Cannot contain comments.
  }
  \item{Comma-separated arguments, extension \code{".csv"}}{
Arguments are read using \code{\link[base]{scan}}, delimited/separated by comma
\code{","}. Arguments may be quoted with double quotes \code{"\\""}, and must be
quoted if they contain comma, newline, or double quotes. Arguments cannot
contain carriage return. Cannot contain comments.
  }
  \item{Tab-separated arguments, extension \code{".tsv"}}{
Arguments are read using \code{\link[base]{scan}}, delimited/separated by tab
\code{"\\t"}. Arguments may be quoted with double quotes \code{"\\""}, and must
be quoted if they contain tab, newline, or double quotes. Arguments cannot
contain carriage return.  Cannot contain comments.
  }
  \item{Other arguments, other extension}{
Arguments are read using \code{\link[base]{scan}}, delimited/separated by
\sQuote{white-space}. Arguments may be quoted with single quotes \code{"'"} and
double quotes \code{"\\""}, and must be quoted if they contain white-space,
single quotes, or double quotes. Arguments cannot contain carriage return.
Quoted arguments cannot contain an odd number of trailing backslashes, or an odd
number of backslashes before an embedded quoting character. Can contain
comments, marked by an non-quoted hash character \code{"#"}.
  }
}

For \code{"csv"}, \code{"tsv"}, and \code{"other"} methods, \code{writeArgs}
tries to space the text such that the file will look nice to open within a text
editor and \sQuote{Excel}. Each line of \emph{arguments} (not comments) will
have at most 80 characters (to look nice in a text editor) or at most 10 columns
(to look nice in \sQuote{Excel}), whichever comes first (though this rule is
broken by strings with more than 80 characters).
}
\value{
for \code{file = NULL}, the formatted text.

for \code{file = ""}, the formatted text invisibly.

otherwise, a character string naming a file containing your arguments.
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
        x <- c("\\u{7B90}", "\\u{5316}", "\\u{5B57}")
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


# the same arguments in different formats
writeArgs(x, "", name = "Rargs")
writeArgs(x, "", name = "pyargs")
writeArgs(x, "", name = "csv")
writeArgs(x, "", name = "tsv")
writeArgs(x, "", name = NULL)


FILE <- writeArgs(x, at = FALSE); y <- readArgs(FILE)


# for the purpose of comparison, we need 'x' to be a character vector
x <- this.path:::asArgs(x)


# let's check that the arguments in 'x' match the
# arguments written to and read back from 'FILE' (in 'y')
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





# Using writeArgs and withArgs, Rscript
cat(
    dedent(r"{
        withAutoprint({
            parser <- ArgumentParser()
            parser$add.argument("args", nargs = "*")
            pargs <- parser$parse.args()
            print(pargs$args)
        })
    }"),
    file = script <- tempfile(), sep = "\\n"
)
withArgs(
    source(script, local = TRUE, echo = FALSE),
    paste0("@", FILE)
)
Rscript("--default-packages=this.path", script, args = paste0("@", FILE))
unlink(FILE); unlink(script)
}
