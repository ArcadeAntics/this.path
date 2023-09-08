# .onLoad <- evalq(envir = new.env(), {
#     delayedAssign(".otoplevel", Sys.getenv("R_THIS_PATH_TOP_LEVEL", NA))
# function (libname, pkgname)
# {
#     if (is.na(.otoplevel))
#         Sys.setenv(R_THIS_PATH_TOP_LEVEL = TRUE)
#     else if (otoplevel)
#         Sys.setenv(R_THIS_PATH_TOP_LEVEL = FALSE)
# }
# })
#
#
# .onUnload <- evalq(envir = environment(.onLoad),
# function (libpath)
# {
#     if (is.na(otoplevel))
#         Sys.unsetenv("R_THIS_PATH_TOP_LEVEL")
#     else if (otoplevel)
#         Sys.setenv(R_THIS_PATH_TOP_LEVEL = TRUE)
# })


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
    if (is.null(ns <- .getNamespace(nsname))) {
        setHook(packageEvent(nsname), .fix.plumber.parseUTF8, "prepend")
    } else {
        sym <- "parseUTF8"
        fun <- ns[[sym]]
        if (typeof(fun) == "closure" && 6L <= length(body(fun))) {
            old.expr <- quote(exprs <- try(parse(file, keep.source = TRUE, srcfile = src, encoding = enc)))
            new.expr <- quote(exprs <- try(parse(file, keep.source = FALSE, srcfile = src, encoding = enc)))
            if (identical(body(fun)[[6L]], old.expr)) {
                body(fun)[[6L]] <- new.expr
                if (bindingIsLocked(sym, ns)) {
                    (unlockBinding)(sym, ns)
                    assign(sym, fun, envir = ns, inherits = FALSE)
                    lockBinding(sym, ns)
                }
                else assign(sym, fun, envir = ns, inherits = FALSE)
            }
        }
    }
    invisible()
}


.onLoad <- function (libname, pkgname)
.External2(.C_onLoad, libname, pkgname)


.onUnload <- function (libpath)
.External2(.C_onUnload, libpath)
