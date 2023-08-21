\name{print.ThisPathDocumentContext}
\alias{print.ThisPathDocumentContext}
\alias{format.ThisPathDocumentContext}
\alias{as.character.ThisPathDocumentContext}
\title{Printing "ThisPathDocumentContext" Objects}
\description{
  Print a \string{ThisPathDocumentContext} object.
}
\usage{
\method{print}{ThisPathDocumentContext}(x, ..., quote = TRUE)

\method{format}{ThisPathDocumentContext}(x, ...)

\method{as.character}{ThisPathDocumentContext}(x, ...)
}
\arguments{
  \item{x}{object of class \string{ThisPathDocumentContext}.}

  \item{\dots}{unused.}

  \item{quote}{logical, indicating whether or not strings should be printed
    with surrounding quotes.}
}
\details{
  An object of class \string{ThisPathDocumentContext} is generated upon calling
  \code{\link{set.sys.path}()}, \code{\link{wrap.source}()},
  \code{\link{sys.path}()}, \code{\link{env.path}()}, or
  \code{\link{src.path}()}, and by extension \code{\link{this.path}()}. It
  contains information about the path of the executing / / current script.

  These objects are not usually user-visible.
}
\value{
  for \code{print.ThisPathDocumentContext()}, \code{x} invisibly.

  for \code{format.ThisPathDocumentContext()}, a character vector of lines.

  for \code{as.character.ThisPathDocumentContext()}, a character string of
  concatenated lines.
}
\examples{
fun <- function (file)
{
    set.sys.path(file, Function = "fun")
    `.this.path::document.context`
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
