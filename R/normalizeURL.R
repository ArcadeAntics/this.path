.normalizeURL <- function (path)
vapply(path.split(path), function(x) {
    x <- x[x != "."]
    while (i <- match("..", x, nomatch = 0L)) {
        x <- if (i >= 3L)
            x[-i + 0L:1L]
        else x[-i]
    }
    x[-1L] <- gsub("^(\n|\r)+|(\t|\n|\r| )+$", "", x[-1L])
    paste(x, collapse = "/")
}, FUN.VALUE = "")


# normalizeURL <- function (path, mustWork = NA, against = NULL)
# {
#     if (is.character(against) || is.null(against)) {
#         if (length(against) == 0)
#             .normalizeURL(path = path, mustWork = mustWork)
#         else {
#             against <- against[[1L]]
#             .normalizeURL(path = path, mustWork = mustWork)
#         }
#     }
#
#     if (missing(against)) {
#         if (!all(is.na(path) | grepl("^(ftp|ftps|http|https)://", path)))
#             5
#     }
#     else {
#         against <- as.character(against)[[1L]]
#         if (!grepl("^(ftp|ftps|http|https)://", against))
#             stop("invalid 'against', is not a URL")
#         if (!is.character(path))
#             path <- as.character(path)
#         i <- !(is.na(path) | grepl("^(ftp|ftps|http|https)://", path))
#         if (any(i))
#             path[i] <- paste(against, path[i], sep = "/")
#     }
#     value <- path.split(path)
#     vapply(value, function(x) {
#         x <- x[x != "."]
#         while (i <- match("..", x, nomatch = 0L)) {
#             if (i >= 3L)
#                 x <- x[-i + 0L:1L]
#             else stop("HTTP status 400 Bad Request")
#         }
#         paste(x, collapse = "/")
#     }, FUN.VALUE = "")
# }
