# .onLoad <- evalq(envir = new.env(), function (libname, pkgname)
# {
#     if (is.na(otoplevel <<- Sys.getenv("R_THIS_PATH_TOP_LEVEL", NA)))
#         Sys.setenv(R_THIS_PATH_TOP_LEVEL = TRUE)
#     else if (otoplevel)
#         Sys.setenv(R_THIS_PATH_TOP_LEVEL = FALSE)
# })
# evalq(envir = environment(.onLoad), {
#     otoplevel <- NULL
# })
#
#
# .onUnload <- evalq(envir = environment(.onLoad), function (libpath)
# {
#     if (is.na(otoplevel))
#         Sys.unsetenv("R_THIS_PATH_TOP_LEVEL")
#     else if (otoplevel)
#         Sys.setenv(R_THIS_PATH_TOP_LEVEL = TRUE)
# })


## a series of functions which need to be
## converted to active bindings during loading
mbcslocale <- function ()
.External2(C_mbcslocale)


utf8locale <- function ()
l10n_info()[[2L]]


latin1locale <- function ()
l10n_info()[[3L]]


R_MB_CUR_MAX <- function ()
.External2(C_R_MB_CUR_MAX)


if (getRversion() >= "2.15.0") {


.onLoad <- function (libname, pkgname)
.External2(C_onload, libname, pkgname)


} else {


.onLoad <- function (libname, pkgname)
{
    dlls <- getLoadedDLLs()
    print(dlls)
    dll <- dlls[["this.path"]]
    print(dll)
    print(names(dll))
    print(dll$onload)
    routines <- getDLLRegisteredRoutines(pkgname)
    print(routines)
    type <- routines[[".External"]]
    print(type)
    C_onload <- type[["onload"]]
    print(C_onload)
    .External2(C_onload, libname, pkgname)
    assign("OS.type", NULL, envir = getNamespace(pkgname))
}


}


.onUnload <- function (libpath)
{
    .External2(C_onunload, libpath)
    library.dynam.unload(pkgname, libpath)
}
