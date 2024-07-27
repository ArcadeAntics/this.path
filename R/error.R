.defunctError <- function (new, package = NULL, msg, old = as.character(sys.call(sys.parent()))[1L])
{
    msg <- if (missing(msg)) {
        msg <- gettextf("'%s' is defunct.\n", old, domain = "R-base")
        if (!missing(new))
            msg <- c(msg, gettextf("Use '%s' instead.\n", new, domain = "R-base"))
        c(msg, if (!is.null(package))
            gettextf("See help(\"Defunct\") and help(\"%s-defunct\").", package, domain = "R-base")
        else gettext("See help(\"Defunct\")", domain = "R-base"))
    }
    else as.character(msg)
    msg <- paste(msg, collapse = "")
    if (missing(new))
        new <- NULL
    errorCondition(msg, old = old, new = new, package = package,
        class = "defunctError")
}


.getCurrentCall <- function (n = 2L, which = sys.parent(n))
{
    n <- .External2(.C_asIntegerGE0, n)
    if (which)
        sys.call(which)
    else NULL
}


.ThisPathInAQUAError <- function (call = .getCurrentCall(), call. = TRUE)
.External2(.C_ThisPathInAQUAError, if (call.) call)


.ThisPathInZipFileError <- function (description, call = .getCurrentCall(), call. = TRUE)
.External2(.C_ThisPathInZipFileError, if (call.) call, description)


.ThisPathNotExistsError <- function (..., call. = TRUE, domain = NULL, call = .getCurrentCall())
.External2(.C_ThisPathNotExistsError, .makeMessage(..., domain = domain), call = if (call.) call)


delayedAssign("thisPathNotExistsError", { .ThisPathNotExistsError })


.ThisPathNotFoundError <- function (..., call. = TRUE, domain = NULL, call = .getCurrentCall())
.External2(.C_ThisPathNotFoundError, .makeMessage(..., domain = domain), call = if (call.) call)


delayedAssign("thisPathNotFoundError", { .ThisPathNotFoundError })


.ThisPathNotImplementedError <- function (..., call. = TRUE, domain = NULL, call = .getCurrentCall())
.External2(.C_ThisPathNotImplementedError, .makeMessage(..., domain = domain), call = if (call.) call)


.ThisPathUnrecognizedConnectionClassError <- function (con, call = .getCurrentCall(), call. = TRUE)
.External2(.C_ThisPathUnrecognizedConnectionClassError, if (call.) call, con)


.ThisPathUnrecognizedMannerError <- function (call = .getCurrentCall(), call. = TRUE)
.External2(.C_ThisPathUnrecognizedMannerError, if (call.) call)





tryCatch2 <- function (expr, ..., else., finally)
.External2(.C_tryCatch2)


.last.condition <- function (c)
.External2(.C_last_condition, c)


last.condition <- function ()
.External2(.C_last_condition)


tryCatch3 <- function (expr, ..., else., finally)
.External2(.C_tryCatch3)


# tryCatch3(message("testing"), message = , error = writeLines("caught error"), warning = 6, test = , finally = writeLines("finally"), else. = writeLines("else."))
