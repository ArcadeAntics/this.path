## functions to be converted to active bindings from '.onLoad' ----


.mbcslocale <- function ()
.External2(.C_mbcslocale)


.utf8locale <- function ()
l10n_info()[[2L]]


.latin1locale <- function ()
l10n_info()[[3L]]


.R_MB_CUR_MAX <- function ()
.External2(.C_R_MB_CUR_MAX)


## functions to be run from '.onLoad' ----


.fix_utils_Sweave <- function (pkgname, pkgpath)
{
    ## the arguments are unused, they only exist to match the arguments
    ## provided to a user hook (see loadNamespace, specifically runUserHook)
    nsname <- "utils"
    ns <- .getNamespace(nsname)
    sym <- "Sweave"
    fun <- ns[[sym]]
    if (typeof(fun) == "closure" &&
        is.call(b <- body(fun)) && length(b) >= 7L &&
        identical(b[[6L]], quote(text <- SweaveReadFile(file, syntax, encoding = encoding))) &&
        identical(b[[7L]], quote(attr(file, "encoding") <- encoding <- attr(text, "encoding"))))
    {
        body(fun) <- as.call(append(as.list(b), after = 6L,
            expression(file <- attr(text, "files")[1L])
        ))
        if (bindingIsLocked(sym, ns)) {
            (unlockBinding)(sym, ns)
            assign(sym, fun, envir = ns, inherits = FALSE)
            lockBinding(sym, ns)
        }
        else assign(sym, fun, envir = ns, inherits = FALSE)
    }
    invisible()
}


.fix_utils_RweaveLatexRuncode <- function (pkgname, pkgpath)
{
    ## the arguments are unused, they only exist to match the arguments
    ## provided to a user hook (see loadNamespace, specifically runUserHook)
    nsname <- "utils"
    ns <- .getNamespace(nsname)
    sym <- "RweaveLatexRuncode"
    fun <- ns[[sym]]
    if (typeof(fun) == "closure" &&
        is.call(b <- body(fun)) && length(b) >= 34L &&
        is.call(b_33 <- b[[indx33 <- 33L + identical(b[[29L]], quote(refline <- NA))]]) && length(b_33) >= 4L &&
        is.call(b_33_4 <- b_33[[4L]]) && length(b_33_4) >= 6L &&
        is.call(b_33_4_6 <- b_33_4[[6L]]) && length(b_33_4_6) >= 3L &&
        is.call(b_33_4_6_3 <- b_33_4_6[[3L]]) && length(b_33_4_6_3) >= 4L &&
        is.call(b_33_4_6_3_4 <- b_33_4_6_3[[4L]]) && length(b_33_4_6_3_4) >= 3L &&
        is.call(b_33_4_6_3_4_3 <- b_33_4_6_3_4[[3L]]) && length(b_33_4_6_3_4_3) >= 2L &&
        is.call(b_33_4_6_3_4_3_2 <- b_33_4_6_3_4_3[[2L]]) && length(b_33_4_6_3_4_3_2) >= 2L &&
        identical(b_33_4_6_3_4_3_2[[2L]], quote(ce)))
    {
        b[[c(indx33, 4L, 6L, 3L, 4L, 3L, 2L, 2L)]] <- quote(chunkexps[nce])
        body(fun) <- b
        if (bindingIsLocked(sym, ns)) {
            (unlockBinding)(sym, ns)
            assign(sym, fun, envir = ns, inherits = FALSE)
            lockBinding(sym, ns)
        }
        else assign(sym, fun, envir = ns, inherits = FALSE)
    }
    invisible()
}


.fix_utils <- function (pkgname, pkgpath)
{
    .fix_utils_Sweave(pkgname, pkgpath)
    .fix_utils_RweaveLatexRuncode(pkgname, pkgpath)
}


.fix_plumber_parseUTF8 <- function (pkgname, pkgpath)
{
    ## the arguments are unused, they only exist to match the arguments
    ## provided to a user hook (see loadNamespace, specifically runUserHook)
    nsname <- "plumber"
    ns <- .getNamespace(nsname)
    sym <- "parseUTF8"
    fun <- ns[[sym]]
    if (typeof(fun) == "closure" &&
        is.call(b <- body(fun)) && length(b) >= 6L &&
        identical(b[[6L]], quote(exprs <- try(parse(file, keep.source = TRUE, srcfile = src, encoding = enc)))))
    {
        b[[6L]] <- quote(exprs <- try(parse(file, keep.source = FALSE, srcfile = src, encoding = enc)))
        body(fun) <- b
        if (bindingIsLocked(sym, ns)) {
            (unlockBinding)(sym, ns)
            assign(sym, fun, envir = ns, inherits = FALSE)
            lockBinding(sym, ns)
        }
        else assign(sym, fun, envir = ns, inherits = FALSE)
    }
    invisible()
}


.maybe_setHook_packageEvent_utils_fix_utils <- function ()
{
    hookName <- packageEvent("utils")
    hooks <- getHook(hookName)
    if (!length(hooks)) {
        setHook(hookName, .fix_utils_RweaveLatexRuncode, "prepend")
        setHook(hookName, .fix_utils_Sweave, "prepend")
    }
    else {
        if (!(list(.fix_utils_RweaveLatexRuncode) %in% hooks))
            setHook(hookName, .fix_utils_RweaveLatexRuncode, "prepend")
        if (!(list(.fix_utils_Sweave) %in% hooks))
            setHook(hookName, .fix_utils_Sweave, "prepend")
    }
}


.maybe_setHook_packageEvent_plumber_fix_plumber_parseUTF8 <- function ()
{
    hookName <- packageEvent("plumber")
    hooks <- getHook(hookName)
    if (!length(hooks) || !(list(.fix_plumber_parseUTF8) %in% hooks))
        setHook(hookName, .fix_plumber_parseUTF8, "prepend")
}


## '.onLoad' and '.onUnload' ----


.onLoad <- function (libname, pkgname)
.External2(.C_onLoad, libname, pkgname)


.onUnload <- function (libpath)
.External2(.C_onUnload, libpath)
