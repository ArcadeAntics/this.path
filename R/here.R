here <- ici <- function (..., .. = 0L)
{
    base <- this.dir(verbose = FALSE)
    for (.. in seq_len(..)) base <- dirname(base)
    file.path(base, ...)
}
