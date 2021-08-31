\name{pseudoglobalenv}
\alias{pseudoglobalenv}
\title{
Pseudo Global Environment
}
\description{
Create an environment similar to \code{\link[base:environment]{globalenv}()}
(with the same parent environments), intended for use with
\code{\link[base]{source}()}.
}
\usage{
pseudoglobalenv(...)
}
\arguments{
  \item{\dots}{
further arguments passed to \code{\link[base:environment]{new.env}()}.
  }
}
\value{
a new (empty) environment with the same enclosure as the global environment.
}
