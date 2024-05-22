.asArgs <- function (x)
{
    if (!length(x)) return(character())
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
    }, how = "list")
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
    # if (anyNA(value))
    #     value[is.na(value)] <- "NA"
    if (any(i <- is.na(value))) value[i] <- "NA"
    return(value)
}


asArgs <- function (...)
.asArgs(list(...))


progArgs <- function (ifnotfound = character())
{
    n <- .External2(.C_getframenumber)
    if (n) {
        if (n > .toplevel.nframe()) {
            whiches <- .External2(.C_sys.whiches, n)
            ## remove the last element and reverse
            if (n <- length(whiches))
                whiches <- whiches[seq.int(to = 1L, by = -1L, length.out = n - 1L)]
            for (which in whiches) {
                if (.identical(sys.function(which), withArgs)) {
                    value <- get("args", envir = sys.frame(which), inherits = FALSE)
                    return(value)
                }
            }
            ifnotfound
        }
        ## in the site-wide startup profile file or a user profile
        else ifnotfound
    }
    else if (.in_shell)
        commandArgs(trailingOnly = TRUE)
    else ifnotfound
}


withArgs <- function (...)
{
    args <- .External2(.C_asArgs, 1L)
    ..1
}
