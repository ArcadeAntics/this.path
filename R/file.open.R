file.open <- function (file)
{
    if (!is.character(file))
        stop("a character vector argument expected")
    if (!length(file))
        return(invisible(file))
    path <- file
    i <- !grepl("^(ftp|https?|file)://", path)
    path[i] <- normalizePath(path[i])
    if (.Platform$OS.type != "windows") {
        path <- commandQuote(path)
        if (capabilities("aqua"))
            command <- paste("open", path)
        else command <- paste("xdg-open", path)
        for (command in command) system(command)
    }
    else for (path in path) tryCatch(shell.exec(path), error = base::warning)
    return(invisible(file))
}
