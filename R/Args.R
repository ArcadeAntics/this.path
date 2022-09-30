.asArgs <- function (x)
{
    n <- length(x)
    if (!n)
        return(character())
    else if (n == 1L) {
        xx <- x[[1L]]
        if (is.null(xx))
            return(character())
        else if (is.object(xx)) {
        }
        else if (!length(xx))
            return(character())
        else if (is.numeric(xx) || is.complex(xx))
            return(format.default(xx, trim = TRUE, digits = 17L,
                decimal.mark = ".", drop0trailing = TRUE))
        else if (is.raw(xx))
            return(sprintf("0x%02x", as.integer(xx)))
        else if (is.logical(xx) || is.character(xx)) {
            value <- as.character(xx)
            if (anyNA(value))
                value[is.na(value)] <- "NA"
            return(value)
        }
    }
    value <- rapply(x, function(xx) {
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
        else if (!length(xx))
            character()
        else if (is.numeric(xx) || is.complex(xx))
            format.default(xx, trim = TRUE, digits = 17L, decimal.mark = ".",
                drop0trailing = TRUE)
        else if (is.raw(xx))
            sprintf("0x%02x", as.integer(xx))
        else if (is.pairlist(xx))
            .asArgs(as.vector(xx, "list"))
        else as.character(xx)
    }, how = "replace")
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
    if (anyNA(value))
        value[is.na(value)] <- "NA"
    return(value)
}


asArgs <- function (...)
.asArgs(list(...))


fileArgs <- function ()
{
    path <- this.path(verbose = FALSE, default = return(character()))
    n <- attr(path, "this.path::n")
    if (!is.null(n)) {
        if (identical(sys.function(n - 1L), withArgs))
            get("args", envir = sys.frame(n - 1L), inherits = FALSE)
        else character()
    }
    else if (isTRUE(attr(path, "this.path::from.shell")))
        commandArgs(trailingOnly = TRUE)
    else character()
}


progArgs <- function ()
{
    path <- this.path(verbose = FALSE, default = return({
        if (in.shell)
            commandArgs(trailingOnly = TRUE)
        else character()
    }))
    n <- attr(path, "this.path::n")
    if (!is.null(n)) {
        if (identical(sys.function(n - 1L), withArgs))
            get("args", envir = sys.frame(n - 1L), inherits = FALSE)
        else character()
    }
    else if (in.shell)
        commandArgs(trailingOnly = TRUE)
    else character()
}


withArgs <- function (...)
{
    args <- .External2(C_asargs, 1L)
    ..1
}
