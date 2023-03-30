\name{getinitwd}
\alias{getinitwd}
\alias{initwd}
\title{
Get Initial Working Directory
}
\description{
\code{getinitwd()} returns an absolute filepath representing the working
directory at the time of loading this package.
}
\usage{
getinitwd()
initwd
}
\value{
\code{getinitwd()} returns a character string or \code{NULL} if the initial
working directory is not available.
}
\examples{
cat("\\ninitial working directory:\\n"); getinitwd()
cat("\\ncurrent working directory:\\n"); getwd()
}
