\name{tryCatch2}
\alias{tryCatch2}
\alias{tryCatch3}
\alias{last.condition}
\title{Condition Handling and Recovery}
\description{
  Variants of \codelink3{base:conditions}{tryCatch}{()} that accept an
  \code{else.} argument, similar to \code{try except} in \sQuote{\Python}.

  \code{last.condition} will be the last thrown and caught condition in
  \code{tryCatch3()}.
}
\usage{
tryCatch2(expr, ..., else., finally)
tryCatch3(expr, ..., else., finally)

last.condition
}
\arguments{
  \item{expr}{expression to be evaluated.}

  \item{\dots}{for \code{tryCatch2()}, condition handlers. for
    \code{tryCatch3()}, expressions to be conditionally evaluated.

    Arguments which are missing will use the next non-missing argument. If
    there is no next non-missing argument, \code{NULL} will be returned
    invisibly.}

  \item{else.}{expression to be evaluated if evaluating \code{expr} does not
    throw an error nor a condition is caught.}

  \item{finally}{expression to be evaluated before returning or exiting.}
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
    conn <- file(FILE)
    tryCatch2({
        open(conn, "r")
    }, condition = function(cond) {
        cat("cannot open", FILE, "\\n")
    }, else. = {
        cat(FILE, "has", length(readLines(conn)), "lines\\n")
    }, finally = {
        close(conn)
    })
    # ## or more naturely with tryCatch3:
    # tryCatch3({
    #     open(conn, "r")
    # }, condition = {
    #     cat("cannot open", FILE, "\\n")
    # }, else. = {
    #     cat(FILE, "has", length(readLines(conn)), "lines\\n")
    # }, finally = {
    #     close(conn)
    # })
}
unlink(FILES)
}
\keyword{error}
\keyword{internal}
