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


# FILE <- function ()
# tryCatch(.External2(.C_thispath, FALSE, FALSE, FALSE, FALSE, FALSE),
#     error = function(e) {
#          .External2(.C_thispath, FALSE, FALSE, TRUE , FALSE, FALSE)
#     })


# delayedAssign("LINE", LINENO)


.onLoad <- function (libname, pkgname)
.External2(.C_onLoad, libname, pkgname)


# if (getRversion() < "4.3.0") {
#
#
# .onAttach <- function (libname, pkgname)
# {
#     ns <- asNamespace(pkgname, base.OK = FALSE)
#     nsname <- getNamespaceName(ns)
#     attname <- paste0("package:", nsname)
#     env <- as.environment(attname)
#
#
#     list <- c("FILE", "LINE")
#     remove(list = list, envir = env, inherits = FALSE)
#     for (sym in list) {
#         fun <- activeBindingFunction(sym, ns)
#         makeActiveBinding(sym, fun, env)
#     }
# }
#
#
# }


.onUnload <- eval(call("function", as.pairlist(alist(libpath = )), bquote({
    .External2(.C_onUnload, libpath)
    library.dynam.unload(.(.pkgname), libpath)
})))
