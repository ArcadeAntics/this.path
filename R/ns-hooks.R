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


.onLoad <- function (libname, pkgname)
.External2(.C_onLoad, libname, pkgname)


.onUnload <- function (libpath)
.External2(.C_onUnload, libpath)
