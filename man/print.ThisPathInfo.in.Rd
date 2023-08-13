\name{print.ThisPathInfo}
\alias{print.ThisPathInfo}
\alias{format.ThisPathInfo}
\alias{as.character.ThisPathInfo}
\title{Printing "ThisPathInfo" Objects}
\description{
  Print a \string{ThisPathInfo} object.
}
\usage{
\method{print}{ThisPathInfo}(x, ..., quote = TRUE)

\method{format}{ThisPathInfo}(x, ...)

\method{as.character}{ThisPathInfo}(x, ...)
}
\arguments{
  \item{x}{object of class \string{ThisPathInfo}.}

  \item{\dots}{unused.}

  \item{quote}{logical, indicating whether or not strings should be printed
    with surrounding quotes.}
}
\details{
  An object of class \string{ThisPathInfo} is generated upon calling
  \code{\link{set.sys.path}()}, \code{\link{wrap.source}()},
  \code{\link{sys.path}()}, \code{\link{env.path}()}, or
  \code{\link{src.path}()}, and by extension \code{\link{this.path}()}. It
  contains information about the path of the executing / / current script.

  These objects are not usually user-visible.
}
\value{
  for \code{print.ThisPathInfo()}, \code{x} invisibly.

  for \code{format.ThisPathInfo()}, a character vector of lines.

  for \code{as.character.ThisPathInfo()}, a character string.
}
\examples{
fun <- function (file)
{
    set.sys.path(file, Function = "fun")
    `.this.path::info`
}


fun()


fun("clipboard")


fun(paste0("https://raw.githubusercontent.com/ArcadeAntics/",
           "this.path/main/tests/sys-path-with-urls.R"))


FILE.R <- tempfile(fileext = ".R"); file.create(FILE.R)

x <- fun(FILE.R)
print(x)
print(x, quote = FALSE)
format(x)
as.character(x)

unlink(FILE.R)
}
