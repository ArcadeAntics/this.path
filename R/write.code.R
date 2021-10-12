write.code <- function (x, file = stdout(), evaluated = FALSE, simplify = !evaluated)
{
    if (!evaluated)
        x <- substitute(x)
    x <- if (simplify && is.call(x) && x[[1]] == quote(`{`))
        vapply(as.list(x[-1]), deparse1, width.cutoff = 60L,
            collapse = "\n", FUN.VALUE = "")
    else deparse1(x, width.cutoff = 60L, collapse = "\n")
    writeLines(x, file, useBytes = TRUE)
}
