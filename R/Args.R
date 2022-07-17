asArgs <- function (...)
{
    n <- nargs()
    if (!n)
        return(character())
    else if (n == 1L) {
        x <- ..1
        if (is.null(x))
            return(character())
        else if (is.object(x)) {
        }
        else if (is.numeric(x) || is.complex(x))
            return(format.default(x, trim = TRUE, digits = 17L,
                decimal.mark = ".", drop0trailing = TRUE))
        else if (is.raw(x))
            return(sprintf("0x%02x", as.integer(x)))
        else if (is.logical(x) || is.character(x)) {
            value <- as.character(x)
            if (anyNA(value))
                value[is.na(value)] <- "NA"
            return(value)
        }
        else if (!length(x))
            return(character())
    }
    value <- rapply(list(...), function(xx) {
        if (is.null(xx))
            character()
        else if (is.object(xx)) {
            if (inherits(xx, "factor"))
                as.character.factor(xx)
            else if (inherits(xx, "POSIXct"))
                format.POSIXct(xx, format = "%Y-%m-%d %H:%M:%OS6")
            else if (inherits(xx, "POSIXlt"))
                format.POSIXlt(xx, format = "%Y-%m-%d %H:%M:%OS6")
            else as.character(xx)
        }
        else if (is.numeric(xx) || is.complex(xx))
            format.default(xx, trim = TRUE, digits = 17L, decimal.mark = ".",
                drop0trailing = TRUE)
        else if (is.raw(xx))
            sprintf("0x%02x", as.integer(xx))
        else if (is.pairlist(xx))
            asArgs(as.list(xx))
        else as.character(xx)
    }, how = "replace")
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
    if (anyNA(value))
        value[is.na(value)] <- "NA"
    return(value)
}


from.shell <- function ()
isTRUE(attr(this.path(default = return(FALSE), verbose = FALSE), "this.path::from.shell"))


fileArgs <- function ()
{
    path <- this.path(default = return(character()), verbose = FALSE)
    # cat("\nhere\n")
    if (!is.null(n <- attr(path, "this.path::n")) &&
        identical(sys.function(n - 1L), withArgs))
        get("args", envir = sys.frame(n - 1L), inherits = FALSE)
    else if (isTRUE(attr(path, "this.path::from.shell")))
        commandArgs(trailingOnly = TRUE)
    else character()
}


withArgs <- function (expr, ...)
{
    args <- asArgs(...)
    expr
}
