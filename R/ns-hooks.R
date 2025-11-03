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
        # is.call(b_33 <- b[[33L]]) && length(b_33) >= 4L &&
        ## compatibility with R < 3.1
        is.call(b_33 <- b[[indx33 <- 33L + identical(b[[29L]], quote(refline <- NA))]]) && length(b_33) >= 4L &&
        is.call(b_33_4 <- b_33[[4L]]) && length(b_33_4) >= 6L &&
        is.call(b_33_4_6 <- b_33_4[[6L]]) && length(b_33_4_6) >= 3L &&
        is.call(b_33_4_6_3 <- b_33_4_6[[3L]]) && length(b_33_4_6_3) >= 4L &&
        is.call(b_33_4_6_3_4 <- b_33_4_6_3[[4L]]) && length(b_33_4_6_3_4) >= 3L &&
        is.call(b_33_4_6_3_4_3 <- b_33_4_6_3_4[[3L]]) && length(b_33_4_6_3_4_3) >= 2L)
    {
        i <- NULL
        if (is.call(b_33_4_6_3_4_3_2 <- b_33_4_6_3_4_3[[2L]])) {
            if (length(b_33_4_6_3_4_3_2) >= 2L &&
                identical(b_33_4_6_3_4_3_2[[2L]], quote(ce)))
            {
                i <- c(indx33, 4L, 6L, 3L, 4L, 3L, 2L, 2L)
            }
        }
        ## compatibility with R < 3.0
        else if (identical(b_33_4_6_3_4_3_2, quote(ce))) {
            i <- c(indx33, 4L, 6L, 3L, 4L, 3L, 2L)
        }
        if (!is.null(i)) {
            b[[i]] <- quote(chunkexps[nce])
            body(fun) <- b
            if (bindingIsLocked(sym, ns)) {
                (unlockBinding)(sym, ns)
                assign(sym, fun, envir = ns, inherits = FALSE)
                lockBinding(sym, ns)
            }
            else assign(sym, fun, envir = ns, inherits = FALSE)
        }
    }
    invisible()
}


.fix_utils_makeRweaveLatexCodeRunner <- function (pkgname, pkgpath)
{
    ## the arguments are unused, they only exist to match the arguments
    ## provided to a user hook (see loadNamespace, specifically runUserHook)
    nsname <- "utils"
    ns <- .getNamespace(nsname)
    sym <- "makeRweaveLatexCodeRunner"
    fun <- ns[[sym]]
    if (typeof(fun) == "closure" &&
        is.call(b <- body(fun)) && length(b) == 2L &&
        is.call(b_2 <- b[[2L]]) && length(b_2) >= 3L &&
        is.call(b_2_3 <- b_2[[3L]]) && length(b_2_3) >= 34L &&
        # is.call(b_2_3_33 <- b_2_3[[33L]]) && length(b_2_3_33) >= 4L &&
        ## compatibility with R < 3.1
        is.call(b_2_3_33 <- b_2_3[[indx33 <- 33L + identical(b_2_3[[29L]], quote(refline <- NA))]]) && length(b_2_3_33) >= 4L &&
        is.call(b_2_3_33_4 <- b_2_3_33[[4L]]) && length(b_2_3_33_4) >= 6L &&
        is.call(b_2_3_33_4_6 <- b_2_3_33_4[[6L]]) && length(b_2_3_33_4_6) >= 3L &&
        is.call(b_2_3_33_4_6_3 <- b_2_3_33_4_6[[3L]]) && length(b_2_3_33_4_6_3) >= 4L &&
        is.call(b_2_3_33_4_6_3_4 <- b_2_3_33_4_6_3[[4L]]) && length(b_2_3_33_4_6_3_4) >= 3L &&
        is.call(b_2_3_33_4_6_3_4_3 <- b_2_3_33_4_6_3_4[[3L]]) && length(b_2_3_33_4_6_3_4_3) >= 2L)
    {
        i <- NULL
        if (is.call(b_2_3_33_4_6_3_4_3_2 <- b_2_3_33_4_6_3_4_3[[2L]])) {
            if (length(b_2_3_33_4_6_3_4_3_2) >= 2L &&
                identical(b_2_3_33_4_6_3_4_3_2[[2L]], quote(ce)))
            {
                i <- c(2L, 3L, indx33, 4L, 6L, 3L, 4L, 3L, 2L, 2L)
            }
        }
        ## compatibility with R < 3.0
        else if (identical(b_2_3_33_4_6_3_4_3_2, quote(ce))) {
            i <- c(2L, 3L, indx33, 4L, 6L, 3L, 4L, 3L, 2L)
        }
        if (!is.null(i)) {
            b[[i]] <- quote(chunkexps[nce])
            body(fun) <- b
            if (bindingIsLocked(sym, ns)) {
                (unlockBinding)(sym, ns)
                assign(sym, fun, envir = ns, inherits = FALSE)
                lockBinding(sym, ns)
            }
            else assign(sym, fun, envir = ns, inherits = FALSE)
        }
    }
    invisible()
}


.fix_utils <- function (pkgname, pkgpath)
{
    .fix_utils_Sweave(pkgname, pkgpath)
    .fix_utils_RweaveLatexRuncode(pkgname, pkgpath)
    .fix_utils_makeRweaveLatexCodeRunner(pkgname, pkgpath)
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
    old <- getHook(hookName)
    new <- list(.fix_utils_Sweave, .fix_utils_RweaveLatexRuncode, .fix_utils_makeRweaveLatexCodeRunner)
    if (!length(old))
        setHook(hookName, new, "prepend")
    else if (any(i <- !(new %in% old)))
        setHook(hookName, new[i], "prepend")
}


.maybe_setHook_packageEvent_plumber_fix_plumber_parseUTF8 <- function ()
{
    hookName <- packageEvent("plumber")
    old <- getHook(hookName)
    new <- list(.fix_plumber_parseUTF8)
    if (!length(old) || !(new %in% old))
        setHook(hookName, new, "prepend")
}





if (!isTRUE(getOption("R_THIS_PATH_DEVEL"))) {
tmp <- new.env()
.get_ptrs <- evalq(envir = tmp, {
    x <- NULL
             function ()
{
    chname <- "this_path_reg_ptrs"
    path <- getNamespaceInfo("@R_PACKAGE_NAME@", "path")
    path <- paste0(path, "/dlls")
    r_arch <- .Platform$r_arch
    chname1 <- paste0(chname, .Platform$dynlib.ext)
    DLLpath <- if (nzchar(r_arch)) paste0(path, "/", r_arch) else path
    file <- paste0(DLLpath, "/", chname1)
    if (file.exists(file)) {
        dyn.load(file, DLLpath = DLLpath)
        x <<- file
        .External2(.C_get_ptrs)
    }
}
})
.maybe_dyn_unload <- evalq(envir = tmp, {
                     function ()
if (!is.null(x)) dyn.unload(x)
})
rm(tmp)
}


## '.onLoad' and '.onUnload' ----


.onLoad <- function (libname, pkgname)
.External2(.C_onLoad, libname, pkgname)


.onUnload <- function (libpath)
.External2(.C_onUnload, libpath)
