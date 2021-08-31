normalizeAgainst <- function (..., against = this.dir(verbose = FALSE))
{
    if (is.character(against) || is.null(against)) {
        if (length(against) == 0) normalizePath(...)
        else {
            owd <- getwd()
            if (is.null(owd))
                stop("cannot 'normalizeAgainst' as current directory is unknown")
            on.exit(setwd(owd))
            setwd(against[[1L]])
            normalizePath(...)
        }
    }
    else stop("invalid 'against' argument")
}
