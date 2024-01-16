.windows_basename2 <- function (path)
.External2(.C_windows_basename2, path)


.unix_basename2 <- function (path)
.External2(.C_unix_basename2, path)


basename2 <- function (path)
.External2(.C_basename2, path)


.windows_dirname2 <- function (path)
.External2(.C_windows_dirname2, path)


.unix_dirname2 <- function (path)
.External2(.C_unix_dirname2, path)


dirname2 <- function (path)
.External2(.C_dirname2, path)


.windows_splitext <- function (path, compression = FALSE)
.External2(.C_windows_splitext, path, compression)


.unix_splitext <- function (path, compression = FALSE)
.External2(.C_unix_splitext, path, compression)


splitext <- function (path, compression = FALSE)
.External2(.C_splitext, path, compression)


.windows_removeext <- function (path, compression = FALSE)
.External2(.C_windows_removeext, path, compression)


.unix_removeext <- function (path, compression = FALSE)
.External2(.C_unix_removeext, path, compression)


removeext <- function (path, compression = FALSE)
.External2(.C_removeext, path, compression)


.windows_ext <- function (path, compression = FALSE)
.External2(.C_windows_ext, path, compression)


.unix_ext <- function (path, compression = FALSE)
.External2(.C_unix_ext, path, compression)


ext <- function (path, compression = FALSE)
.External2(.C_ext, path, compression)


`.windows_ext<-` <- function (path, compression = FALSE, value)
.External2(`.C_windows_ext<-`, path, compression, value)


`.unix_ext<-` <- function (path, compression = FALSE, value)
.External2(`.C_unix_ext<-`, path, compression, value)


`ext<-` <- function (path, compression = FALSE, value)
.External2(`.C_ext<-`, path, compression, value)


.windows.path.join <- function (...)
.External2(.C_windows_path_join)


.unix.path.join <- function (...)
.External2(.C_unix_path_join)


path.join <- function (...)
.External2(.C_path_join)


.windows.path.split <- function (path)
.External2(.C_windows_path_split, path)


.unix.path.split <- function (path)
.External2(.C_unix_path_split, path)


path.split <- function (path)
.External2(.C_path_split, path)


.windows.path.split.1 <- function (path)
.External2(.C_windows_path_split_1, path)


.unix.path.split.1 <- function (path)
.External2(.C_unix_path_split_1, path)


path.split.1 <- function (path)
.External2(.C_path_split_1, path)


.windows.path.unsplit <- function (...)
.External2(.C_windows_path_unsplit)


.unix.path.unsplit <- function (...)
.External2(.C_unix_path_unsplit)


path.unsplit <- function (...)
.External2(.C_path_unsplit)


.is_abs_path <- function (path)
.External2(.C_is_abs_path, path)


.is_clipboard <- function (file)
.External2(.C_is_clipboard, file)


.fixslash <- function (s)
{
    if (.OS_windows) {
        path <- chartr("\\", "/", path)
        if (startsWith(path, "//"))
            substr(path, 1L, 2L) <- "\\\\"
    }
    path
}


.fixbackslash <- function (s)
{
    if (.OS_windows)
        path <- chartr("/", "\\", path)
    path
}


.file_uri_path <- function (path)
{
    ## remove the leading "file://" from a file URL
    ##
    ## but specifically on Windows, where file URLs may look like
    ## "file:///c:" ...
    ## remove the leading "file:///" from those file URLs
    if (.OS_windows && any(i <- grepl("^file:///.:", path, useBytes = TRUE))) {
        path[ i] <- substr(path[ i], 9L, 1000000L)
        path[!i] <- substr(path[!i], 8L, 1000000L)
        path
    }
    else substr(path, 8L, 1000000L)
}


.file_uri_path_1 <- function (path)
{
    ## do .file_uri_path but a little bit faster when path is length 1
    if (.OS_windows && grepl("^file:///.:", path, useBytes = TRUE))
        substr(path, 9L, 1000000L)
    else substr(path, 8L, 1000000L)
}


.normalizeurl <- function (path)
{
    # x <- "https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R"
    # print(c(x, this.path:::.normalizeurl(x)))
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


.normalizeurl_1 <- function (path)
{
    # path <- "https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R"


    p <- path.split.1(path)
    p <- p[p != "."]
    while (i <- match("..", p, 0L)) {
        p <- if (i == 2L) p[-2L] else p[-i + 0L:1L]
    }
    path.unsplit(p)
}


## turn a path, whether that be a basename, relative path, or absolute path,
## into an absolute path against the working directory
##
## does path expansion
.abspath <- function (path)
{
    wd <- getwd()
    if (is.null(wd))
        path.expand(path)
    else path.join(wd, path.expand(path))
}


## make a path absolute, then normalize it
.normalizeabspath <- function (path, ...)
normalizePath(path = if (.OS_windows) path else .abspath(path), ...)


.normalizeabspath_and_url <- function (path, ...)
{
    ## a version of normalizePath that will also normalize URLs
    if (any(i <- grepl("^file://", path)))
        path[i] <- .file_uri_path(path[i])
    if (any(i <- !i & grepl("^(https|http|ftp|ftps)://", path))) {
        path[i] <- .normalizeurl(path[i])
        path[!i] <- .normalizeabspath(path = path[!i], ...)
        path
    }
    else .normalizeabspath(path = path, ...)
}


.normalizeabspath_and_url_1 <- function (path, ...)
{
    if (grepl("^file://", path))
        .normalizeabspath(path = .file_uri_path_1(path), ...)
    else if (grepl("^(ftp|ftps|http|https)://", path))
        .normalizeurl_1(path)
    else .normalizeabspath(path = path, ...)
}


.normalizePath <- function (path, winslash = "/", mustWork = TRUE)
normalizePath(path, winslash, mustWork)


.normalizeNotDirectory <- function (path, winslash = "/", mustWork = TRUE)
{
    x <- normalizePath(path, winslash, mustWork)
    if (.isfalse(.isdir(x)))
        x
    else stop(sprintf("'%s' is not a regular file", path), domain = NA)
}


.normalizeFixDirectory <- function (path, winslash = "/", mustWork = TRUE)
{
    x <- normalizePath(path, winslash, mustWork)
    if (.istrue(.isdir(x)))
        path.join(x, ".")
    else x
}


.normalizeAgainst <- function (wd, path, winslash = "/", mustWork = TRUE)
{
    owd <- getwd()
    if (is.null(owd))
        stop("cannot '.normalizeAgainst' as current directory is unknown")
    on.exit(setwd(owd))
    setwd(wd)
    normalizePath(path, winslash, mustWork)
}


.normalizeNotDirectoryAgainst <- function (wd, path, winslash = "/", mustWork = TRUE)
{
    owd <- getwd()
    if (is.null(owd))
        stop("cannot '.normalizeAgainst' as current directory is unknown")
    on.exit(setwd(owd))
    setwd(wd)
    x <- normalizePath(path, winslash, mustWork)
    if (.isfalse(.isdir(x)))
        x
    else stop(sprintf("'%s' is not a regular file", path), domain = NA)
}


.normalizeFixDirectoryAgainst <- function (wd, path, winslash = "/", mustWork = TRUE)
{
    owd <- getwd()
    if (is.null(owd))
        stop("cannot '.normalizeAgainst' as current directory is unknown")
    on.exit(setwd(owd))
    setwd(wd)
    x <- normalizePath(path, winslash, mustWork)
    if (.istrue(.isdir(x)))
        path.join(x, ".")
    else x
}


.normalize_srcfilealias <- function (original, path, winslash = "/", mustWork = TRUE)
{
    value <- .External2(.C_src_path, original)
    value <- .dir(value)
    if (grepl("^(https|http|ftp|ftps)://", value)) {
        ## do not use file.path(), will convert text to native encoding
        .normalizeurl(paste(value, path, sep = "/"))
    }
    else {
        owd <- getwd()
        if (is.null(owd))
            stop("cannot '.normalize_srcfilealias' as current directory is unknown")
        on.exit(setwd(owd))
        setwd(value)
        normalizePath(path, winslash, mustWork)
    }
}


.dir <- function (path)
{
    if (grepl("^(https|http|ftp|ftps)://", path)) {
        # path <- "https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R"
        p <- path.split.1(path)
        path.unsplit(if (length(p) >= 2L) p[-length(p)] else p)
    }
    else dirname2(path)
}


.here <- function (path, .. = 0L)
{
    if (grepl("^(https|http|ftp|ftps)://", path)) {
        # path <- "https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R"
        # .. <- "2"


        p <- path.split.1(path)
        n <- length(p) - length(seq_len(..)) - 1L
        path.unsplit(if (n < 1L) p[1L] else p[seq_len(n)])
    }


    # path <- "//host/share/path/to/file"
    # path <- "C:/Users/iris/Documents/this.path/man/this.path.Rd"
    # .. <- "10"
    else .External2(.C_dirname2, path, ..)
}
