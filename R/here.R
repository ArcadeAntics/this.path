here <- ici <- function (..., .. = 0L)
{
    base <- this.dir(verbose = FALSE)
    # if (grepl("^(ftp|ftps|http|https)://", base))
    #     base <- normalizeURL(paste(c(".", rep("..",
    #         length.out = ..)), collapse = "/"), base)
    # else
        for (.. in seq_len(..)) base <- dirname(base)
    file.path(base, ...)
}
