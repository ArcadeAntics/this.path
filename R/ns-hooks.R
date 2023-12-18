## a series of functions which need to be
## converted to active bindings during loading
.mbcslocale <- function ()
.External2(.C_mbcslocale)


.utf8locale <- function ()
l10n_info()[[2L]]


.latin1locale <- function ()
l10n_info()[[3L]]


.R_MB_CUR_MAX <- function ()
.External2(.C_R_MB_CUR_MAX)


## function to be run from '.onLoad'
.fix.plumber.parseUTF8 <- function (pkgname, pkgpath)
{
    ## the arguments are unused, they only exist to match the arguments
    ## provided to a user hook (see loadNamespace, specifically runUserHook)
    nsname <- "plumber"
    ns <- .getNamespace(nsname)
    sym <- "parseUTF8"
    fun <- ns[[sym]]
    if (typeof(fun) != "closure" || length(body(fun)) < 6L)
        return(invisible())
    old <- quote(exprs <- try(parse(file, keep.source = TRUE, srcfile = src, encoding = enc)))
    if (!identical(body(fun)[[6L]], old))
        return(invisible())
    body(fun)[[6L]] <- quote(exprs <- try(parse(file, keep.source = FALSE, srcfile = src, encoding = enc)))
    if (bindingIsLocked(sym, ns)) {
        (unlockBinding)(sym, ns)
        assign(sym, fun, envir = ns, inherits = FALSE)
        lockBinding(sym, ns)
    }
    else assign(sym, fun, envir = ns, inherits = FALSE)
    invisible()
}


.maybe.setHook.packageEvent.plumber.fix.plumber.parseUTF8 <- function ()
{
    hookName <- packageEvent("plumber")
    hooks <- getHook(hookName)
    if (length(hooks) && !(list(.fix.plumber.parseUTF8) %in% hooks))
        setHook(hookName, .fix.plumber.parseUTF8, "prepend")
}


.onLoad <- function (libname, pkgname)
.External2(.C_onLoad, libname, pkgname)


.onUnload <- function (libpath)
.External2(.C_onUnload, libpath)
