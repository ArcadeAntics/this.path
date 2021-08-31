\name{file.open}
\alias{file.open}
\title{
Open a File or URL
}
\description{
Opens a file or URL in the user's preferred application.
}
\usage{
file.open(file)
}
\arguments{
  \item{file}{
file or URL to be opened.
  }
}
\details{
\code{file} may be any number of files or URLs. Files will be normalized before
opening, though they should already be full paths.

On macOS, files and URLs will be open using \code{open}, and under any other
Unix-alike, files and URLs will be open using \code{xdg-open}.
}
\value{
\code{file} invisibly.
}
\seealso{
\code{\link[base]{shell.exec}}
}
\examples{
\dontrun{
file.open(tempdir())
file.open("https://cran.r-project.org/doc/manuals/R-exts.html#Documenting-packages")
}
}
