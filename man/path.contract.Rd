\name{path.contract}
\alias{path.contract}
\title{
Contract File Paths
}
\description{
Contract a path name, for example by replacing the user's home directory (if
defined on that platform) with a leading tilde.
}
\usage{
path.contract(path, ignore.case = .Platform$OS.type == "windows")
}
\arguments{
  \item{path}{
character vector containing one or more path names.
  }
  \item{ignore.case}{
\code{TRUE} or \code{FALSE}, should case be ignored when replacing the user's
home directory?
  }
}
\details{
On Windows, paths are case insensitive, so something like
\sQuote{\code{C:\\Users\\effective_user\\Documents}} is regarded as equivalent
to \sQuote{\code{c:\\users\\effective_user\\documents}}.

Under Unix-alikes, paths are case sensitive, so something like
\sQuote{\code{/home/effective_user}} is not equivalent to
\sQuote{\code{/HOME/effective_user}}.

By default, \code{path.contract} respects each of these behaviours, but can be
changed if desired.

Additionally, on Windows, the file separator may be either \code{"/"} or
\code{"\\\\"}, for example \sQuote{\code{C:\\Users\\effective_user\\Documents}}
and \sQuote{\code{C:/Users/effective_user/Documents}} would be treated as
equivalent (or any mixture of the two, for example
\sQuote{\code{C:/Users\\effective_user/Documents}}).

The 'path names' need not exist nor be valid path names.
}
\value{
A character vector of possibly contracted path names: where the home directory
is unknown of none is specified the path is returned unchanged.
}
\note{
Using \code{./}, \code{../}, symbolic links, hard links, or multiple mounts
will \emph{NOT} be resolved before attempting to contract path names. If you
wish to resolve these before contracting path names, see
\code{\link[base]{normalizePath}}.
}
\seealso{
\code{\link[base]{path.expand}}, \code{\link[base]{basename}},
\code{\link[base]{normalizePath}}, \code{\link[base]{file.path}}.
}
\examples{
stopifnot(path.contract(path.expand(x <- c("~", "~/foo"))) == x)
# Note that this is not necessarily true the other way around (in Windows)
# simply because the path separator may have changed


tilde <- path.expand("~")
if (tilde == "~") {
    cat("the home directory is unknown or none is specified\\n")
} else {
    paths <- file.path(c(tilde, toupper(tilde), tolower(tilde)), "foo")
    print(cbind(
        Path = paths,
        `Contracted Path` = path.contract(paths),
        `Contracted Path (ignoring case)` = path.contract(paths, ignore.case = TRUE),
        `Contracted Path (with case)` = path.contract(paths, ignore.case = FALSE)
    ), quote = FALSE)
}
}
