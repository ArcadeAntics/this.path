\name{here}
\alias{here}
\alias{ici}
\title{
Construct Path to File, Beginning with \code{\link{this.dir}()}
}
\description{
Construct the path to a file from components/paths in a
platform-\strong{DEPENDENT} way, starting with \code{\link{this.dir}()}.
}
\usage{
here(..., .. = 0)
ici(..., .. = 0)
}
\arguments{
  \item{\dots}{
further arguments passed to \code{\link{path.join}()}.
  }
  \item{..}{
the number of directories to go back.
  }
}
\details{
The path to a file begins with a base. The base is \code{..} number of
directories back from the executing script's directory (\code{this.dir()}). The
argument is named \code{..} because \code{".."} refers to the parent directory
in Windows, Unix-alikes, and URL pathnames.
}
\value{
A character vector of the arguments concatenated term-by-term, beginning with
the executing script's directory.
}
\examples{
FILE <- tempfile(fileext = ".R")
this.path:::write.code({


    this.path::here()
    this.path::here(.. = 1)
    this.path::here(.. = 2)


    # use 'here' to read input from a file located nearby
    this.path::here(.. = 1, "input", "file1.csv")


    # or maybe to run another script
    this.path::here("script2.R")


}, FILE)


source(FILE, echo = TRUE, verbose = FALSE)
\dontshow{unlink(FILE)}}
