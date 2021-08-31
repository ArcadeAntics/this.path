\name{Commands}
\alias{Commands}
\title{
Extract Commands From an Object of Class "ParsedArgs"
}
\description{
Provides access to a copy of the commands supplied to an object of class
"ParsedArgs".
}
\usage{
Commands(x, type = c("original", "string"))
}
\arguments{
  \item{x}{
 an object of class "ParsedArgs".
  }
  \item{type}{
a string naming the type to return.
  }
}
\value{
If \code{type = "original"}, a character vector of the commands supplied when
\code{x} was created.

If \code{type = "string"}, a character string of the commands supplied when
\code{x} was created, separated by \code{"/"}.
}
\examples{
FILE <- tempfile()
writeLines(r"{
    parser <- this.path::ArgumentParser()
    `parser CMD1` <- parser$add.parser("CMD1")
    `parser CMD2` <- parser$add.parser("CMD2")
    `parser CMD1 a` <- `parser CMD1`$add.parser(c("a", "b"))
    `parser CMD1 f` <- `parser CMD1`$add.parser(c("c", "d"))
    `parser CMD2 k` <- `parser CMD2`$add.parser(c("e", "f"))
    `parser CMD2 o` <- `parser CMD2`$add.parser(c("g", "h"))
    pargs <- parser$parse.args()
    print(this.path::Commands(pargs))
}", FILE)


# note that if a subparser has multiple names, the first one is returned.
# so "CMD1 a" is the same as "CMD1 b" since they
# are part of the same subparser
Rscript(file = FILE, quiet = TRUE)
Rscript(file = FILE, quiet = TRUE, args = c("CMD1"))
Rscript(file = FILE, quiet = TRUE, args = c("CMD2"))
Rscript(file = FILE, quiet = TRUE, args = c("CMD1", "a"))
Rscript(file = FILE, quiet = TRUE, args = c("CMD1", "b"))
Rscript(file = FILE, quiet = TRUE, args = c("CMD1", "c"))
Rscript(file = FILE, quiet = TRUE, args = c("CMD1", "d"))
Rscript(file = FILE, quiet = TRUE, args = c("CMD2", "e"))
Rscript(file = FILE, quiet = TRUE, args = c("CMD2", "f"))
Rscript(file = FILE, quiet = TRUE, args = c("CMD2", "g"))
Rscript(file = FILE, quiet = TRUE, args = c("CMD2", "h"))


unlink(FILE)
}
