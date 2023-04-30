file.URL.path <- function (path)
{
    # remove the leading "file://" from a file URL
    #
    # but specifically on Windows, where file URLs may look like
    # "file:///c:" ...
    # remove the leading "file:///" from those file URLs
    if (os.windows && any(i <- grepl("^file:///.:", path, useBytes = TRUE))) {
        path[ i] <- substr(path[ i], 9L, 1000000L)
        path[!i] <- substr(path[!i], 8L, 1000000L)
        path
    }
    else substr(path, 8L, 1000000L)
}


file.URL.path.1 <- function (path)
{
    # do file.URL.path but a little bit faster when path is length 1
    if (os.windows && grepl("^file:///.:", path, useBytes = TRUE))
        substr(path, 9L, 1000000L)
    else substr(path, 8L, 1000000L)
}


# extra.whitespace.pattern <- "^[\n\r]+|[\t\n\r ]+$"


normalizeURL <- function (path)
{
    # x <- "https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R"
    # print(c(x, this.path:::.normalizeURL(x)))
    # source(x)


    path.unsplit(lapply(path.split(path), function(p) {
        # x <- path.split("https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R")[[1L]]


        p <- p[p != "."]
        while (i <- match("..", p, 0L)) {
            p <- if (i == 2L) p[-2L] else p[-i + 0L:1L]
        }
        p
    }))
}


normalizeURL.1 <- function (path)
{
    # path <- "https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R"


    p <- path.split.1(path)
    p <- p[p != "."]
    while (i <- match("..", p, 0L)) {
        p <- if (i == 2L) p[-2L] else p[-i + 0L:1L]
    }
    path.unsplit(p)
}


normalizePath.and.URL <- function (path, ...)
{
    # a version of normalizePath that will also normalize URLs
    if (any(i <- grepl("^file://", path)))
        path[i] <- file.URL.path(path[i])
    if (any(i <- !i & grepl("^(https|http|ftp|ftps)://", path))) {
        path[i] <- normalizeURL(path[i])
        path[!i] <- normpath(path = path[!i], ...)
        path
    }
    else normpath(path = path, ...)
}


normalizePath.and.URL.1 <- function (path, ...)
{
    if (grepl("^file://", path))
        normpath(path = file.URL.path.1(path), ...)
    else if (grepl("^(ftp|ftps|http|https)://", path))
        normalizeURL.1(path)
    else normpath(path = path, ...)
}


as.relative.path <- function (path)
stop(defunctError("rel2here", pkgname, old = "as.relative.path"))


as.rel.path <- function (path)
stop(defunctError("rel2here", pkgname, old = "as.rel.path"))





tolower.ASCII <- function (x)
chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)


toupper.ASCII <- function (x)
chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", x)


casefold.ASCII <- function (x, upper = FALSE)
if (upper) toupper.ASCII(x) else tolower.ASCII(x)


delayedAssign("NET.USE.command", {
    if (os.windows)
        paste(shQuote(paste0(Sys.getenv("windir"), "\\System32\\net.exe")), "USE")
})


.relpath <- function (path, relative.to = getwd(), type = "relpath")
{
    if (!is.character(path))
        stop("a character vector argument expected", domain = "R")
    n <- length(path)
    if (n <= 0L) {
        character()
    } else {
        value <- character(n)
        value[] <- path
        if (any(i <- !(is.na(value) | value == ""))) {
            path <- value[i]
            path <- normalizePath.and.URL(path, "/", FALSE)
            switch (type, relpath = {
                if (!is.character(relative.to) || length(relative.to) != 1L)
                    stop(gettextf("'%s' must be a character string", "relative.to", domain = "R"), domain = NA)
                relative.to <- normalizePath.and.URL.1(relative.to, "/", TRUE)
            }, rel2here = {
                relative.to <- .this.dir()
            }, rel2proj = {
                relative.to <- .this.proj()
            }, {
                stop(gettextf("invalid '%s' argument", "type", domain = "R"))
            })
            path <- c(relative.to, path)
            value[i] <- if (os.windows) {
                ## replace //LOCALHOST/C$/
                ## with    C:/
                path <- sub("(?i)^[/\\\\][/\\\\](?:LOCALHOST|127\\.0\\.0\\.1)[/\\\\]([ABCDEFGHIJKLMNOPQRSTUVWXYZ])\\$([/\\\\]|$)",
                            "\\1:/", path)
                p <- path.split(path)


                ## get the first element of each path, the drive,
                ## and then keep all the unique ones
                u <- unique(vapply(p, `[[`, 1L, FUN.VALUE = ""))
                ## by drives I mean both network shares "//host/share/"
                ## and letter drives "d:"
                singular.drive <- if (length(u) == 1L) {
                    TRUE
                } else if (any(j <- grepl("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]:/$", u))) {
                    u[j] <- toupper.ASCII(u[j])
                    length(unique(u)) == 1L
                } else FALSE
                if (singular.drive) {
                    fix.local <- identity
                } else {
                    x <- system(NET.USE.command, intern = TRUE)
                    m <- regexec(" ([ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]:) +(.*?) *$", x)
                    ## one for the whole match, another two for the parenthesized sub-expressions
                    if (any(keep <- lengths(m) == 3L)) {
                        x <- regmatches(x[keep], m[keep])
                        local <- vapply(x, `[[`, 2L, FUN.VALUE = "")
                        local <- tolower.ASCII(local)
                        local <- paste0(local, "/")
                        remote <- vapply(x, `[[`, 3L, FUN.VALUE = "")
                        if (any(j <- grepl("^[/\\\\]{2}", remote)))
                            remote[j] <- chartr("\\", "/", remote[j])
                        remote <- paste0(remote, "/")
                        fix.local <- function(p) {
                            if (indx <- match(tolower.ASCII(p[[1L]]), local, 0L)) {
                                c(remote[[indx]], p[-1L])
                            } else p
                        }
                    } else fix.local <- identity
                }


                r <- p[[1L]]
                p <- p[-1L]
                r <- fix.local(r)
                ignore.case <- !grepl("^(http|https)://", r[[1L]])
                fix.case <- if (ignore.case) tolower.ASCII else identity
                r <- fix.case(r)
                len <- length(r)
                path.unsplit(lapply(p, function(p) {
                    n <- min(len, length(p))
                    q <- fix.case(fix.local(p))
                    n <- match(FALSE, q[seq_len(n)] == r[seq_len(n)], n + 1L) - 1L
                    if (n == 0L) {
                        p
                    } else {
                        value <- c(rep("..", len - n), p[-seq_len(n)])
                        if (length(value) <= 0L)
                            "."
                        else if (!value[[1L]] %in% c(".", ".."))
                            c(".", value)
                        else value
                    }
                }))
            }
            else {
                p <- path.split(path)
                r <- p[[1L]]
                len <- length(r)
                path.unsplit(lapply(p[-1L], function(p) {
                    n <- min(len, length(p))
                    n <- match(FALSE, p[seq_len(n)] == r[seq_len(n)], n + 1L) - 1L
                    if (n == 0L) {
                        p
                    } else {
                        value <- c(rep("..", len - n), p[-seq_len(n)])
                        if (length(value) <= 0L)
                            "."
                        else if (!value[[1L]] %in% c(".", ".."))
                            c(".", value)
                        else value
                    }
                }))
            }
        }
        value
    }
}


relpath <- function (path, relative.to = getwd())
.relpath(path, relative.to)


rel2here <- function (path)
.relpath(path, type = "rel2here")


rel2proj <- function (path)
.relpath(path, type = "rel2proj")
