write.code <- function (x, file = stdout(), evaluated = FALSE, simplify = !evaluated,
    deparseCtrl = c("keepInteger", "showAttributes",
        "useSource", "keepNA", "digits17"))
{
    if (!evaluated)
        x <- substitute(x)
    x <- if (simplify && is.call(x) && x[[1]] == quote(`{`))
        vapply(as.list(x[-1]), deparse1, collapse = "\n",
            width.cutoff = 60L, backtick = TRUE, control = deparseCtrl,
            FUN.VALUE = "")
    else deparse1(x, collapse = "\n", width.cutoff = 60L,
        backtick = TRUE, control = deparseCtrl)
    if (is.null(file))
        x
    else {
        writeLines(x, file, useBytes = TRUE)
        invisible(x)
    }
}
