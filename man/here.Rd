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
directories back from the executing script's directory (\code{this.dir()}). The
argument is named \code{..} because \code{".."} refers to the parent directory
in Windows, Unix, and URL paths alike.
}
\value{
A character vector of the arguments concatenated term-by-term, beginning with
the executing script's directory.
}
\examples{
this.path:::write.code(withAutoprint({


    here()
    here(.. = 1)
    here(.. = 2)


    # use 'here' to read input from a file located nearby
    here(.. = 1, "input", "file1.csv")


    # or maybe to run another script
    here("script2.R")


}, verbose = FALSE), FILE <- tempfile())
source(FILE, verbose = FALSE)
Rscript("--default-packages=this.path", FILE)
\dontshow{unlink(FILE)}}
