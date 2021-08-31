\name{here}
\alias{here}
\alias{ici}
\title{
Construct Path to File, Beginning with 'this.dir()'
}
\description{
Construct the path to a file from components in a platform-independent way,
starting with \code{\link{this.dir}()}.
}
\usage{
here(..., .. = 0L)
ici(..., .. = 0L)
}
\arguments{
  \item{\dots}{
further arguments passed to \code{\link[base]{file.path}()}.
  }
  \item{..}{
the number of directories to go back.
  }
}
\details{
The path to a file begins with a base. The base is \code{..} number of
directories up from the executing script's directory (\code{this.dir()}). For
context, in Windows and under Unix-alikes, \code{".."} refers to the previous
directory, which is why I chose to name the argument \code{..} to refer to the
number of directories to traverse upwards.

If you take a look at the function body, we use
\code{\link[base:basename]{dirname}()} \code{..} number of times to go back
\code{..} number of directories. While something like
\code{file.path(this.dir(), paste(rep("..", ..), collapse = "/"), ...)} should
construct an equivalent path, the \code{".."} are not immediately expanded, so
it makes \code{\link[base]{basename}} and \code{\link[base:basename]{dirname}}
behave unexpectedly.
}
\value{
A character vector of the arguments concatenated term-by-term, beginning with
the executing script's directory.
}
\examples{
FILE <- tempfile()
writeLines(deparse(quote(withAutoprint({


    here()
    here(.. = 1)
    here(.. = 2)


    # use 'here' to read input from a file located nearby
    here(.. = 1, "input", "file1.csv")


    # or maybe to run another script
    here("script2.R")


}, verbose = FALSE))), FILE)
Rscript("--default-packages=this.path", FILE)


unlink(FILE)
}
