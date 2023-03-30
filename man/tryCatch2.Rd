\name{tryCatch2}
\alias{tryCatch2}
\title{
Condition Handling and Recovery
}
\description{
A variant of \code{\link[base:conditions]{tryCatch}()} that accepts an
\code{else.} argument, similar to \code{try except} in \sQuote{\Python}.
}
\usage{
tryCatch2(expr, ..., else., finally)
}
\arguments{
  \item{expr}{
expression to be evaluated.
  }
  \item{\dots}{
condition handlers.
  }
  \item{else.}{
expression to be evaluated if evaluating \code{expr} does not throw an error
nor a condition is caught.
  }
  \item{finally}{
expression to be evaluated before returning or exiting.
  }
}
\details{
The use of the \code{else.} argument is better than adding additional code to
\code{expr} because it avoids accidentally catching a condition that was not
being protected by the \code{tryCatch()} call.
}
\examples{
FILES <- tempfile(c("existent-file_", "non-existent-file_"))
writeLines("line1\\nline2", FILES[[1L]])
for (FILE in FILES) {
    con <- file(FILE)
    tryCatch2({
        open(con, "r")
    }, condition = function(cond) {
        cat("cannot open", FILE, "\\n")
    }, else. = {
        cat(FILE, "has", length(readLines(con)), "lines\\n")
    }, finally = {
        close(con)
    })
}
unlink(FILES)
}
