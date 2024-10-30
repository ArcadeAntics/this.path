.LINENO <- function (path, to = 1L)
{
    for (which in seq.int(to = to, by = -1L, length.out = sys.nframe() - to)) {
        call <- sys.call(which)
        srcref <- attr(call, "srcref", exact = TRUE)
        if (!is.null(srcref)) {
            ## try to get the normalized filename
            success <- tryCatch({
                value <- .External2(.C_src_path, srcref)
                TRUE
            }, error = function(e) FALSE)
            if (success) {
                ## compare the filenames
                if (.str_equal_useBytes(path, value)) {
                    # return(srcref[7L])

                    ## srcref[1L] is better, it respects #line directives
                    return(srcref[1L])
                }
            }
            else {
                ## try to get the original filename
                success <- tryCatch({
                    value <- .External2(.C_src_path, FALSE, TRUE, FALSE, FALSE, srcref)
                    TRUE
                }, error = function(e) FALSE)
                if (success) {
                    if (startsWith(value, "file://"))
                        value <- .fixslash(.file_URL_path(value))
                    else if (grepl("^(https|http|ftp|ftps)://", value, useBytes = TRUE))
                        value <- .normalizeURL(value)
                    else value <- .fixslash(value)
                    if (.str_equal_useBytes(path, value))
                        return(srcref[1L])
                }
            }
        }
    }
    NA_integer_
}


sys.LINENO <- function ()
{
    success <- tryCatch({
        n <- .External2(.C_getframenumber)
        if (is.na(n) || n < 1L)
            return(NA_integer_)
        path <- .External2(.C_sys_path)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        .LINENO(path, n + 1L)
    else NA_integer_
}


env.LINENO <- function (n = 0L, envir = parent.frame(n + 1L), matchThisEnv = getOption("topLevelEnvironment"))
{
    n <- .External2(.C_asIntegerGE0, n)
    envir
    matchThisEnv ## this is unused
    value <- NA_integer_
    if (typeof(envir) == "environment") {
        parents <- sys.parents()
        for (i in seq.int(to = 1L, by = -1L, along.with = parents)) {
            ## if the parent frame matches envir,
            ## look for a srcref on the corresponding call
            if (identical(envir, sys.frame(parents[[i]]))) {
                call <- sys.call(i)
                srcref <- attr(call, "srcref", exact = TRUE)
                if (!is.null(srcref)) {
                    value <- srcref[1L]
                    break
                }
            }
        }
    }
    value
}


src.LINENO <- function (n = 0L, srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    srcfile
    tryCatch({
        .External2(.C_src_LINENO, srcfile)
    }, error = function(e) NA_integer_)
}


LINENO <- function (n = 0L, envir = parent.frame(n + 1L), matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    srcfile
    value <- tryCatch({
        .External2(.C_src_LINENO, srcfile)
    }, error = function(e) NA_integer_)
    if (is.na(value)) {
        envir
        matchThisEnv
        value <- NA_integer_
        if (typeof(envir) == "environment") {
            parents <- sys.parents()
            for (i in seq.int(to = 1L, by = -1L, along.with = parents)) {
                if (identical(envir, sys.frame(parents[[i]]))) {
                    call <- sys.call(i)
                    srcref <- attr(call, "srcref", exact = TRUE)
                    if (!is.null(srcref)) {
                        value <- srcref[1L]
                        break
                    }
                }
            }
        }
        if (is.na(value)) {
            success <- tryCatch({
                n <- .External2(.C_getframenumber)
                if (is.na(n) || n < 1L)
                    return(NA_integer_)
                path <- .External2(.C_sys_path)
                TRUE
            }, error = function(e) FALSE)
            if (success)
                .LINENO(path, n + 1L)
            else NA_integer_
        }
        else value
    }
    else value
}


LINE <- function ()
{
    srcfile <- 0L
    value <- tryCatch({
        .External2(.C_src_LINENO, srcfile)
    }, error = function(e) NA_integer_)
    if (is.na(value)) {
        envir <- parent.frame()
        value <- NA_integer_
        parents <- sys.parents()
        for (i in seq.int(to = 1L, by = -1L, along.with = parents)) {
            if (identical(envir, sys.frame(parents[[i]]))) {
                call <- sys.call(i)
                srcref <- attr(call, "srcref", exact = TRUE)
                if (!is.null(srcref)) {
                    value <- srcref[1L]
                    break
                }
            }
        }
        if (is.na(value)) {
            success <- tryCatch({
                n <- .External2(.C_getframenumber)
                if (is.na(n) || n < 1L)
                    return(NA_integer_)
                path <- .External2(.C_sys_path)
                TRUE
            }, error = function(e) FALSE)
            if (success)
                .LINENO(path, n + 1L)
            else NA_integer_
        }
        else value
    }
    else value
}
