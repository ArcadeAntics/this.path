\name{path.join}
\alias{path.join}
\title{
Construct Path to File
}
\description{
Construct the path to a file from components/paths in a
platform-\strong{DEPENDENT} way.
}
\usage{
path.join(...)
}
\arguments{
  \item{\dots}{
character vectors.
  }
}
\details{
When constructing a path to a file, the last absolute path is selected and all
trailing components are appended. This is different from
\code{\link[base]{file.path}()} where all trailing paths are treated as
components.
}
\value{
A character vector of the arguments concatenated term-by-term and separated by
\code{"/"}.
}
\examples{
#ifdef unix
path.join("/", "test1")

path.join("/path/to/file1", "/path/to/file2")

path.join("//host-name/share-name/path/to/file1", "/path/to/file2")

path.join("testing", "/testing", "~", "~/testing", "//host",
    "//host/share", "//host/share/path/to/file", "not-an-abs-path")

path.join("/test1", "test2", "test3")

path.join("test1", "/test2", "test3", "//host/share/test4", "test5",
    "/test6", "test7", "test8", "test9")
#endif
#ifdef windows
path.join("C:", "test1")

path.join("C:/", "test1")

path.join("C:/path/to/file1", "/path/to/file2")

path.join("//host-name/share-name/path/to/file1", "/path/to/file2")

path.join("C:testing", "C:/testing", "~", "~/testing", "//host",
    "//host/share", "//host/share/path/to/file", "not-an-abs-path")

path.join("c:/test1", "c:test2", "C:test3")

path.join("test1", "c:/test2", "test3", "//host/share/test4", "test5",
    "c:/test6", "test7", "c:test8", "test9")
#endif
}
