path.contract <- function (path, ignore.case = .Platform$OS.type == "windows")
{
    if (!is.character(path))
        stop("invalid 'path' argument")
    if (!length(path))
        return(character())
    attributes(path) <- NULL
    tilde <- path.expand("~")
    if (tilde == "~") # if the home directory is unknown or none is specified
        return(path)
    nc <- nchar(tilde) + 1L
    contract <- if (.Platform$OS.type == "windows") {
        if (ignore.case) {
            startsWith(chartr("/", "\\", tolower(path)), chartr("/", "\\", tolower(tilde))) &
                substr(path, nc, nc) %in% c("/", "\\", "")
        }
        else startsWith(chartr("/", "\\", path), chartr("/", "\\", tilde)) &
            substr(path, nc, nc) %in% c("/", "\\", "")
    }
    else if (ignore.case) {
        startsWith(tolower(path), tolower(tilde)) & substr(path, nc, nc) %in% c("/", "")
    }
    else startsWith(path, tilde) & substr(path, nc, nc) %in% c("/", "")
    if (any(contract))
        path[contract] <- paste0("~", substr(path[contract], nc, 1000000L))
    path
}
