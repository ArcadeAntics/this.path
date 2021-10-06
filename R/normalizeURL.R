normalizeURL <- function (path, against)
{
    against <- as.character(against)[[1L]]
    if (!grepl("^(ftp|ftps|http|https)://", against))
        stop("invalid 'against', is not a URL")
    if (!is.character(path))
        path <- as.character(path)
    i <- !(is.na(path) | grepl("^(ftp|ftps|http|https)://", path))
    if (any(i))
        path[i] <- paste(against, path[i], sep = "/")
    path
}
