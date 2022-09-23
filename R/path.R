# .path <- function (xx, cl = c("this.path::path", "character"))
# `class<-`(xx, cl)
#
#
# `[.this.path::path` <- function (x, ...)
# {
#     .path(NextMethod(), oldClass(x))
# }
#
#
# `[[.this.path::path` <- function (x, ...)
# {
#     .path(NextMethod(), oldClass(x))
# }
#
#
# `[<-.this.path::path` <- function (x, ..., value)
# {
#     if (!length(value))
#         return(x)
#     value <- unclass(as.path(value))
#     .path(NextMethod(), oldClass(x))
# }
#
#
# `[[<-.this.path::path` <- function (x, ..., value)
# {
#     value <- unclass(as.path(value))
#     .path(NextMethod(), oldClass(x))
# }
#
#
# `/.this.path::path` <- function (e1, e2)
# {
#     value <- this.path::path.join(e1, e2)
#     if (length(value) == length(e2))
#         attributes(value) <- attributes(e2)
#     if (length(value) == length(e1))
#         attributes(value) <- attributes(e1)
#     .path(value)
# }
#
#
# `+.this.path::path` <- function (e1, e2)
# {
#     value <- paste0(e1, e2, recycle0 = TRUE)
#     if (length(value) == length(e2))
#         attributes(value) <- attributes(e2)
#     if (length(value) == length(e1))
#         attributes(value) <- attributes(e1)
#     .path(value)
# }
#
#
# delayedAssign("as.data.frame.this.path::path", as.data.frame.vector)
#
#
# `as.list.this.path::path` <- function (x, ...)
# {
#     value <- NextMethod()
#     cl <- oldClass(x)
#     for (indx in seq_along(value))
#         class(value[[indx]]) <- cl
#     value
# }
#
#
# `c.this.path::path` <- function (..., recursive = FALSE, use.names = TRUE)
# {
#     if (isTRUE(as.scalar.logical(recursive)))
#         as.path(c(rapply(list(...), as.path), use.names = use.names))
#     else as.path(c(unlist(lapply(list(...), as.path)), use.names = use.names))
# }
#
#
# `length<-.this.path::path` <- function (x, value)
# .path(NextMethod(), oldClass(x))
#
#
# `print.this.path::path` <- function (x, ...)
# {
#     cat(encodeString(class(x)[1L], quote = "\""), "object:\n")
#     xx <- x
#     oldClass(xx) <- NULL
#     print(xx)
#     invisible(x)
# }
#
#
# `rep.this.path::path` <- function (x, ...)
# {
#     .path(NextMethod(), oldClass(x))
# }
#
#
#
#
#
# as.path <- function (x, ...)
# {
#     if (!is.character(x))
#         x <- as.character(x)
#     .path(x)
# }
