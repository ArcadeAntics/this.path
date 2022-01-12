here <- ici <- function (..., .. = 0L)
{
    base <- this.path(verbose = FALSE)
    if (grepl("^(ftp|ftps|http|https)://", base))
        base <- .normalizeURL(paste(c(base, "..",
            rep("..", length.out = ..)), collapse = "/"))
    else {
        base <- dirname(base)
        for (.. in seq_len(..)) base <- dirname(base)
    }
    file.path(base, ...)
}
