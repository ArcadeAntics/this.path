.tolower_ASCII <- function (x)
chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)


.toupper_ASCII <- function (x)
chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", x)


.casefold_ASCII <- function (x, upper = FALSE)
if (upper) .toupper_ASCII(x) else .tolower_ASCII(x)


delayedAssign(".net_USE_command", {
    if (.OS_windows)
        paste(shQuote(paste0(Sys.getenv("windir"), "\\System32\\net.exe")), "USE")
})


.all_drives <- paste0(c(LETTERS, letters), ":/")


.relpath <- function (path, relative.to = getwd(), normalize = .OS_windows || !missing(relative.to), normalize.path = TRUE)
{
    if (!is.character(path))
        stop("a character vector argument expected", domain = "R")
    n <- length(path)
    if (n <= 0L) {
        character()
    } else {
        value <- character(n)
        value[] <- path
        if (any(i <- !is.na(value) & nzchar(value))) {
            path <- value[i]
            if (normalize.path)
                path <- .normalize_abspath_and_URL(path, "/", FALSE)
            if (!.IS_SCALAR_STR(relative.to))
                stop(gettextf("'%s' must be a character string", "relative.to", domain = "R"), domain = NA)
            if (normalize)
                relative.to <- .normalize_abspath_and_URL_1(relative.to, "/", TRUE)
            path <- c(relative.to, path)
            value[i] <- if (.OS_windows) {
                ## replace //LOCALHOST/C$/
                ## or      //127.0.0.1/C$/
                ## with    C:/
                path <- sub("(?i)^[/\\\\][/\\\\](?:LOCALHOST|127\\.0\\.0\\.1)[/\\\\]([ABCDEFGHIJKLMNOPQRSTUVWXYZ])\\$([/\\\\]|$)",
                            "\\1:/", path)
                p <- path.split(path)


                ## get the first element of each path, the drive,
                ## and then keep all the unique ones
                u <- unique(vapply(p, `[[`, 1L, FUN.VALUE = ""))
                no_convert_local <- if (length(u) == 1L) {
                    TRUE
                } else if (!any(j <- u %in% .all_drives)) {
                    TRUE
                } else if (all(j)) {
                    length(unique(.toupper_ASCII(u))) == 1L
                } else {
                    1L == length(u <- unique(.toupper_ASCII(u[j]))) &&
                        u == .toupper_ASCII(paste0(Sys.getenv("SystemDrive"), "/"))
                }
                if (no_convert_local) {
                    fix_local <- identity
                } else {
                    x <- system(.net_USE_command, intern = TRUE)
                    m <- regexec(" ([ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]:) +(.*?) *(?:Web Client Network|Microsoft Windows Network)?$", x)
                    ## one for the whole match, another two for the parenthesized sub-expressions
                    if (any(keep <- lengths(m) == 3L)) {
                        x <- regmatches(x[keep], m[keep])
                        local <- vapply(x, `[[`, 2L, FUN.VALUE = "")
                        local <- .tolower_ASCII(local)
                        local <- paste0(local, "/")
                        remote <- vapply(x, `[[`, 3L, FUN.VALUE = "")
                        if (any(j <- grepl("^[/\\\\]{2}", remote)))
                            remote[j] <- chartr("\\", "/", remote[j])
                        remote <- paste0(remote, "/")
                        remote <- sub("(?i)^//(?:LOCALHOST|127\\.0\\.0\\.1)/([ABCDEFGHIJKLMNOPQRSTUVWXYZ])\\$/",
                            "\\1:/", remote)
                        fix_local <- function(p) {
                            if (indx <- match(.tolower_ASCII(p[[1L]]), local, 0L)) {
                                c(remote[[indx]], p[-1L])
                            } else p
                        }
                    } else fix_local <- identity
                }


                r <- p[[1L]]
                p <- p[-1L]
                r <- fix_local(r)
                ignore_case <- !grepl("^(http|https)://", r[[1L]])
                fix_case <- if (ignore_case) .tolower_ASCII else identity
                r <- fix_case(r)
                len <- length(r)
                path.unsplit(lapply(p, function(p) {
                    n <- min(len, length(p))
                    q <- fix_case(fix_local(p))
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


relpath <- if (.Platform$OS.type == "windows") {
           function (path, relative.to = normalizePath(getwd(), "/", TRUE))
.relpath(path, relative.to, normalize = !missing(relative.to))
} else {
           function (path, relative.to = getwd())
.relpath(path, relative.to, normalize = !missing(relative.to))
}


rel2sys.dir <- function (path, local = FALSE)
{
    relative.to <- .External2(.C_sys_path, local)
    relative.to <- .dir(relative.to)
    .relpath(path, relative.to, normalize = FALSE)
}


rel2sys.proj <- function (path, local = FALSE)
{
    relative.to <- .External2(.C_sys_path, local)
    relative.to <- .dir(relative.to)
    relative.to <- .proj(relative.to)
    .relpath(path, relative.to, normalize = FALSE)
}


rel2env.dir <- function (path, n = 0L, envir = parent.frame(n + 1L), matchThisEnv = getOption("topLevelEnvironment"))
{
    n <- .External2(.C_asIntegerGE0, n)
    relative.to <- .External2(.C_env_path, envir, matchThisEnv)
    relative.to <- .dir(relative.to)
    .relpath(path, relative.to, normalize = FALSE)
}


rel2env.proj <- function (path, n = 0L, envir = parent.frame(n + 1L), matchThisEnv = getOption("topLevelEnvironment"))
{
    n <- .External2(.C_asIntegerGE0, n)
    relative.to <- .External2(.C_env_path, envir, matchThisEnv)
    relative.to <- .dir(relative.to)
    relative.to <- .proj(relative.to)
    .relpath(path, relative.to, normalize = FALSE)
}


rel2src.dir <- function (path, n = 0L, srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    relative.to <- .External2(.C_src_path, srcfile)
    relative.to <- .dir(relative.to)
    .relpath(path, relative.to, normalize = FALSE)
}


rel2src.proj <- function (path, n = 0L, srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    relative.to <- .External2(.C_src_path, srcfile)
    relative.to <- .dir(relative.to)
    relative.to <- .proj(relative.to)
    .relpath(path, relative.to, normalize = FALSE)
}


rel2here <- function (path, local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    relative.to <- .External2(.C_this_path, local, envir, matchThisEnv, srcfile)
    relative.to <- .dir(relative.to)
    .relpath(path, relative.to, normalize = FALSE)
}


rel2proj <- function (path, local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    relative.to <- .External2(.C_this_path, local, envir, matchThisEnv, srcfile)
    relative.to <- .dir(relative.to)
    relative.to <- .proj(relative.to)
    .relpath(path, relative.to, normalize = FALSE)
}
