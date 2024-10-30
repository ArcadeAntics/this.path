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


.windows_path_join <- function (...)
.External2(.C_windows_path_join)


.unix_path_join <- function (...)
.External2(.C_unix_path_join)


path.join <- function (...)
.External2(.C_path_join)


.windows_path_split <- function (path)
.External2(.C_windows_path_split, path)


.unix_path_split <- function (path)
.External2(.C_unix_path_split, path)


path.split <- function (path)
.External2(.C_path_split, path)


.windows_path_split_1 <- function (path)
.External2(.C_windows_path_split_1, path)


.unix_path_split_1 <- function (path)
.External2(.C_unix_path_split_1, path)


path.split.1 <- function (path)
.External2(.C_path_split_1, path)


.windows_path_unsplit <- function (...)
.External2(.C_windows_path_unsplit)


.unix_path_unsplit <- function (...)
.External2(.C_unix_path_unsplit)


path.unsplit <- function (...)
.External2(.C_path_unsplit)


.is_abs_path <- function (path)
.External2(.C_is_abs_path, path)


.is_clipboard <- function (file)
.External2(.C_is_clipboard, file)


.fixslash <- function (s)
.External2(.C_fixslash, s)


.fixbackslash <- function (s)
.External2(.C_fixbackslash, s)


.as_file_URL <- function (path)
{
    if (!length(path))
        character(0)
    else if (.OS_windows) {
        ## on Windows we have file:///C:/path/to/file or similar
        path <- .fixslash(path)
        i <- grepl("^.:", path, useBytes = TRUE)
        if (all(i))
            paste0("file:///", path)
        else if (any(i)) {
            path[ i] <- paste0("file:///", path[ i])
            path[!i] <- paste0("file://" , path[!i])
            path
        }
        else paste0("file://", path)
    }
    else paste0("file://", path)
}


.file_URL_path <- function (path)
.External2(.C_file_URL_path, path)


.normalizeURL <- function (path)
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


.normalizeURL_1 <- function (path)
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
.normalize_abspath <- function (path, ...)
normalizePath(path = if (.OS_windows) path else .abspath(path), ...)


.normalize_abspath_and_URL <- function (path, ...)
{
    ## a version of normalizePath that will also normalize URLs
    if (any(i <- startsWith(path, "file://")))
        path[i] <- .file_URL_path(path[i])
    if (any(i <- !i & grepl("^(https|http|ftp|ftps)://", path, useBytes = TRUE))) {
        path[i] <- .normalizeURL(path[i])
        path[!i] <- .normalize_abspath(path = path[!i], ...)
        path
    }
    else .normalize_abspath(path = path, ...)
}


.normalize_abspath_and_URL_1 <- function (path, ...)
{
    if (startsWith(path, "file://"))
        .normalize_abspath(path = .file_URL_path(path), ...)
    else if (grepl("^(ftp|ftps|http|https)://", path, useBytes = TRUE))
        .normalizeURL_1(path)
    else .normalize_abspath(path = path, ...)
}


.normalizePath <- function (path, winslash = "/", mustWork = TRUE)
normalizePath(path, winslash, mustWork)


.normalizePath_not_dir <- function (path, winslash = "/", mustWork = TRUE)
{
    x <- normalizePath(path, winslash, mustWork)
    if (.isfalse(.isdir(x)))
        x
    else stop(sprintf("'%s' is not a regular file", path), domain = NA)
}


.normalizePath_fix_dir <- function (path, winslash = "/", mustWork = TRUE)
{
    x <- normalizePath(path, winslash, mustWork)
    if (.istrue(.isdir(x)))
        path.join(x, ".")
    else x
}


.normalizePath_against <- function (wd, path, winslash = "/", mustWork = TRUE)
{
    owd <- getwd()
    if (is.null(owd))
        stop("cannot '.normalizePath_against' as current directory is unknown")
    on.exit(setwd(owd))
    setwd(wd)
    normalizePath(path, winslash, mustWork)
}


.normalizePath_not_dir_against <- function (wd, path, winslash = "/", mustWork = TRUE)
{
    owd <- getwd()
    if (is.null(owd))
        stop("cannot '.normalizePath_against' as current directory is unknown")
    on.exit(setwd(owd))
    setwd(wd)
    x <- normalizePath(path, winslash, mustWork)
    if (.isfalse(.isdir(x)))
        x
    else stop(sprintf("'%s' is not a regular file", path), domain = NA)
}


.normalizePath_fix_dir_against <- function (wd, path, winslash = "/", mustWork = TRUE)
{
    owd <- getwd()
    if (is.null(owd))
        stop("cannot '.normalizePath_against' as current directory is unknown")
    on.exit(setwd(owd))
    setwd(wd)
    x <- normalizePath(path, winslash, mustWork)
    if (.istrue(.isdir(x)))
        path.join(x, ".")
    else x
}


.normalizePath_srcfilealias <- function (original, path, winslash = "/", mustWork = TRUE)
{
    value <- .External2(.C_src_path, original)
    value <- .dir(value)
    if (grepl("^(https|http|ftp|ftps)://", value, useBytes = TRUE)) {
        ## do not use file.path(), on old versions of R it will convert text to native encoding
        .normalizeURL(paste(value, path, sep = "/"))
    }
    else {
        owd <- getwd()
        if (is.null(owd))
            stop("cannot '.normalizePath_against' as current directory is unknown")
        on.exit(setwd(owd))
        setwd(value)
        normalizePath(path, winslash, mustWork)
    }
}


.dir <- function (path)
{
    if (grepl("^(https|http|ftp|ftps)://", path, useBytes = TRUE)) {
        # path <- "https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R"
        p <- path.split.1(path)
        path.unsplit(if (length(p) >= 2L) p[-length(p)] else p)
    }
    else dirname2(path)
}


.here <- function (path, .. = 0L)
{
    if (grepl("^(https|http|ftp|ftps)://", path, useBytes = TRUE)) {
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
