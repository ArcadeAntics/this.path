Args <- function (x, type = c("original", "all", "trailingOnly"))
{
    if (nargs())
        return(attr(x, "args")[[match.arg(type)]])
    path <- this.path2(verbose = FALSE)
    if (is.null(path))
        character()
    else if (!is.null(n <- attr(path, "this.path.n")) &&
        identical(sys.function(n - 1L), withArgs))
        get("args", envir = sys.frame(n - 1L), inherits = FALSE)
    else if (isTRUE(attr(path, "this.path.from.command-line")))
        commandArgs(trailingOnly = TRUE)
    else character()
}


withArgs <- function (expr, ...)
{
    args <- asArgs(...)
    expr
}
