\name{path.split}
\alias{path.split}
\alias{path.split.1}
\alias{path.unsplit}
\title{
Split File Path Into Individual Components
}
\description{
Split the path to a file into components in a platform-\strong{DEPENDENT} way.
}
\usage{
path.split(path)
path.split.1(path)
path.unsplit(...)
}
\arguments{
  \item{path}{
character vector.
  }
  \item{\dots}{
character vectors, or one list of character vectors.
  }
}
\value{
for \code{path.split()}, a list of character vectors.

for \code{path.split.1()} and \code{path.unsplit()}, a character vector.
}
\note{
\code{path.unsplit()} is \strong{NOT} the same as \code{\link{path.join}()}.
}
#ifdef windows
\examples{
path <- c(
    NA,
    "",
    paste0("https://raw.githubusercontent.com/ArcadeAntics/PACKAGES/",
           "src/contrib/Archive/this.path/this.path_1.0.0.tar.gz"),
    "\\\\\\\\host\\\\share\\\\path\\\\to\\\\file",
    "\\\\\\\\host\\\\share\\\\",
    "\\\\\\\\host\\\\share",
    "C:\\\\path\\\\to\\\\file",
    "C:path\\\\to\\\\file",
    "path\\\\to\\\\file",
    "\\\\path\\\\to\\\\file",
    "~\\\\path\\\\to\\\\file",
    # paths with character encodings
    `Encoding<-`("path/to/fil\\xe9", "latin1"),
    "C:/Users/iris/Documents/\\u03b4.R"
)
print(x <- path.split(path))
print(path.unsplit(x))
}
#endif
#ifdef unix
\examples{
path <- c(
    NA,
    "",
    paste0("https://raw.githubusercontent.com/ArcadeAntics/PACKAGES/",
           "src/contrib/Archive/this.path/this.path_1.0.0.tar.gz"),
    "//host/share/path/to/file",
    "//host/share/",
    "//host//share",
    "/path/to/file",
    "~/path/to/file",
    "path/to/file",
    # paths with character encodings
    `Encoding<-`("path/to/fil\\xe9", "latin1"),
    "/home/iris/Documents/\\u03b4.R"
)
print(x <- path.split(path))
print(path.unsplit(x))
}
#endif
