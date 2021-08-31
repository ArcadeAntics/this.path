\name{list.files2}
\alias{list.files2}
\alias{dir2}
\title{
List the Files in a Directory/Folder
}
\description{
Unlike \code{\link[base]{list.files}}, paths will not be converted to the native
encoding, but instead to UTF-8.

In Windows, \code{list.files2} will use
\href{https://www.python.org/downloads/}{python}, so make sure you have it
installed.

Under Unix-alikes, \code{list.files2} will just call
\code{\link[base]{list.files}}.
}
\usage{
list.files2(path = ".", pattern = NULL, all.files = FALSE,
            full.names = FALSE, recursive = FALSE,
            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

       dir2(path = ".", pattern = NULL, all.files = FALSE,
            full.names = FALSE, recursive = FALSE,
            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
}
\arguments{
  \item{path, pattern, all.files, full.names, recursive, ignore.case, include.dirs, no..}{
See \code{\link[base]{list.files}}.
  }
}
\value{
A character vector containing the names of the files in the specified
directories.
}
\seealso{
\code{\link[base]{list.files}}
}
\examples{
\dontrun{
FILE <- paste0(tempfile(tmpdir = tempfile("dir")), "_testing_\\u{03B4}.txt")
dir.create(dirname(FILE))
invisible(file.create(FILE))


x <- list.files (dirname(FILE), full.names = TRUE)
y <- list.files2(dirname(FILE), full.names = TRUE)
`names<-`(file.exists(x), x)
`names<-`(file.exists(y), y)


unlink(dirname(FILE), recursive = TRUE)
}
}
