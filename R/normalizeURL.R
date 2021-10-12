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
    value <- path.split(path)
    vapply(value, function(x) {
        x <- x[x != "."]
        while (i <- match("..", x, nomatch = 0L)) {
            if (i >= 3L)
                x <- x[-i + 0L:1L]
            else stop("HTTP status 400 Bad Request")
        }
        paste(x, collapse = "/")
    }, FUN.VALUE = "")
}
