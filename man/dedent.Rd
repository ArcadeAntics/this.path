\name{dedent}
\alias{dedent}
\title{
Remove Common Leading Whitespace
}
\description{
Remove the common leading whitespace from each line. This is useful in
multi-line strings to make them line up with the left edge of the display, while
still presenting them in indented form within the source code.
}
\usage{
dedent(x, strip = TRUE)
}
\arguments{
  \item{x}{
character vector. The strings from which to remove common leading whitespace.
  }
  \item{strip}{
logical. Should the leading and trailing whitespace line be removed from each
element of \code{x}? This will not remove the leading indent.
  }
}
\details{
\code{strip} is \code{TRUE} by default because the standard use case is to
dedent a multi-line string that appears in the source code. Refer to section
\strong{Examples}.

Tabs and spaces are both whitespace, but they are not treated equally. This is
because tab takes up a different number of spaces depending on the context.
}
\value{
character vector, the same length as \code{x}, and with the \code{dim},
\code{dimnames}, and \code{names} attributes of \code{x} (after possible
coercion to character).
}
\examples{
cat(dedent("
    here is a multi-line string that appears in the source code. we wish to
    remove the common indent from each line, and dedent should do this for us!
    ---- hopefully this works ----
"), sep = "\\n")


# for me, a tab prints as eight spaces when preceded by a newline
# but since tab isn't always eight spaces, we treat this as unsolveable :(
# there is no common leading whitespace since "        " != "\\t"
cat(dedent("
        another multi-line string, this time with no common leading whitespace
\\tanother multi-line string, this time with no common leading whitespace
"), sep = "\\n")
}
