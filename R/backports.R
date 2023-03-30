if (getRversion() < "4.2.0") {


# gettext(trim = TRUE) was added in R 4.2.0
gettext <- function (..., domain = NULL, trim = TRUE)
{
    gettext(..., domain = domain)
}
environment(gettext) <- .BaseNamespaceEnv


}


if (getRversion() < "4.1.0") {


# bquote(splice = TRUE) was added in R 4.1.0
bquote <- function (expr, where = parent.frame(), splice = FALSE)
{
    if (!is.environment(where))
        where <- as.environment(where)
    unquote <- function(e) {
        if (is.pairlist(e))
            as.pairlist(lapply(e, unquote))
        else if (is.call(e)) {
            if (typeof(e[[1L]]) == "symbol" && e[[1L]] == ".")
                eval(e[[2L]], where)
            else if (splice) {
                if (typeof(e[[1L]]) == "symbol" && e[[1L]] == "..")
                    stop("can only splice inside a call", call. = FALSE)
                else as.call(unquote.list(e))
            }
            else as.call(lapply(e, unquote))
        }
        else e
    }
    is.splice.macro <- function(e) is.call(e) && typeof(e[[1L]]) == "symbol" && e[[1L]] == ".."
    unquote.list <- function(e) {
        p <- Position(is.splice.macro, e, nomatch = NULL)
        if (is.null(p))
            lapply(e, unquote)
        else {
            n <- length(e)
            head <- if (p == 1)
                NULL
            else e[1:(p - 1)]
            tail <- if (p == n)
                NULL
            else e[(p + 1):n]
            macro <- e[[p]]
            mexp <- eval(macro[[2L]], where)
            if (!is.vector(mexp) && !is.expression(mexp))
                stop("can only splice vectors")
            c(lapply(head, unquote), mexp, as.list(unquote.list(tail)))
        }
    }
    unquote(substitute(expr))
}


}


if (getRversion() < "3.6.0") {


errorCondition <- function (message, ..., class = NULL, call = NULL)
structure(list(message = as.character(message), call = call, ...),
    class = c(class, "error", "condition"))
environment(errorCondition) <- .BaseNamespaceEnv


str2expression <- function (text)
{
    if (typeof(text) != "character")
        stop("argument must be character", domain = "R")
    parse(text = text, n = -1, keep.source = FALSE, srcfile = NULL)
}


str2lang <- function (s)
{
    if (typeof(s) != "character")
        stop("argument must be character", domain = "R")
    if (length(s) != 1L)
        stop("argument must be a character string", domain = "R")
    ans <- parse(text = s, n = -1, keep.source = FALSE, srcfile = NULL)
    if (length(ans) != 1L)
        stop(gettextf("parsing result not of length one, but %d", length(ans), domain = "R"), domain = NA)
    ans[[1L]]
}


}


if (getRversion() < "3.5.0") {


...length <- function ()
.External2(C_dotslength)


isTRUE <- function (x)
is.logical(x) && length(x) == 1L && !is.na(x) && x
environment(isTRUE) <- .BaseNamespaceEnv


isFALSE <- function (x)
is.logical(x) && length(x) == 1L && !is.na(x) && !x
environment(isFALSE) <- .BaseNamespaceEnv


}


if (getRversion() < "3.4.0") {


withAutoprint <- function (exprs, evaluated = FALSE, local = parent.frame(), print. = TRUE,
    echo = TRUE, max.deparse.length = Inf, width.cutoff = max(20,
        getOption("width")), deparseCtrl = c("keepInteger", "showAttributes",
        "keepNA"), spaced = FALSE, ...)
{
    if (!evaluated) {
        exprs <- substitute(exprs)
        if (is.call(exprs)) {
            if (exprs[[1]] == quote(`{`))
                exprs <- as.list(exprs[-1])
        }
    }
    if (!is.expression(exprs))
        exprs <- as.expression(exprs)
    conn <- textConnection(code2character(exprs, width.cutoff = width.cutoff, deparseCtrl = deparseCtrl))
    on.exit(close(conn))
    source(file = conn, local = local, print.eval = print., echo = echo,
        max.deparse.length = max.deparse.length, ...)
}


} else withAutoprint <- base::withAutoprint


if (getRversion() < "3.3.0") {


strrep <- function (x, times)
{
    if (!is.character(x))
        x <- as.character(x)
    .External2(C_strrep, x, as.integer(times))
}


startsWith <- function (x, prefix)
.External2(C_startsWith, x, prefix)


endsWith <- function (x, suffix)
.External2(C_endsWith, x, suffix)


}


if (getRversion() < "3.2.0") {


isNamespaceLoaded <- function (name)
.External2(C_isRegisteredNamespace, name)


dir.exists <- function (paths)
.External2(C_direxists, paths)


lengths <- function (x, use.names = TRUE)
.External2(C_lengths, x, use.names)


# file.info() did not have argument extra_cols at this time
file.mtime <- function (...)
file.info(...)$mtime


} else {


isNamespaceLoaded <- function (name)
isNamespaceLoaded(name)
environment(isNamespaceLoaded) <- .BaseNamespaceEnv


}


if (getRversion() < "3.1.0") {


anyNA <- function (x, recursive = FALSE)
.External2(C_anyNA, x, recursive)


}


if (getRversion() < "3.0.0") {


delayedAssign("C_mapply", getNativeSymbolInfo("do_mapply", PACKAGE = "base"))


.mapply <- function (FUN, dots, MoreArgs)
.Call(C_mapply, match.fun(FUN), dots, MoreArgs, environment())


setprseen2 <- function (ptr)
.External2(C_setprseen2, ptr)


parse <- function (file = "", n = NULL, text = NULL, prompt = "?", keep.source = getOption("keep.source"),
    srcfile = NULL, encoding = "unknown")
{
    if (!missing(keep.source)) {
        opt.keep.source <- getOption("keep.source")
        if (isTRUE(keep.source) != isTRUE(opt.keep.source)) {
            on.exit(options(keep.source = opt.keep.source))
            options(keep.source = keep.source)
        }
    }
    if (missing(srcfile))
        parse(file = file, n = n, text = text, prompt = prompt, srcfile = , encoding = encoding)
    else parse(file = file, n = n, text = text, prompt = prompt, srcfile = srcfile, encoding = encoding)
}
environment(parse) <- .BaseNamespaceEnv


}
