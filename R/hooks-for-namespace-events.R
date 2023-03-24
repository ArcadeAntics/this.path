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


# .onLoad <- function (libname, pkgname)
# {
#     print(environmentIsLocked(getNamespace(pkgname)))
#     print(system.file(package = pkgname))
# }


# .onLoad <- function (libname, pkgname)
# {
#     # cat("> libname\n")
#     # print(libname)
#     # cat("> pkgname\n")
#     # print(pkgname)
#     # sink("~/temp3.txt", append = TRUE)
#     # on.exit(sink())
#     # cat('> Sys.getenv("_R_CHECK_PACKAGE_NAME_")\n')
#     # print(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))
#     # cat("> tryCatch2\n")
#     # print(tryCatch2)
#     # cat("> commandArgs()\n")
#     # print(commandArgs())
#     # cat("\n\n\n\n\n")
# }


# utf8locale <- function ()
# .External2(C_utf8locale)


mbcslocale <- function ()
.External2(C_mbcslocale)


# latin1locale <- function ()
# .External2(C_latin1locale)


R_MB_CUR_MAX <- function ()
.External2(C_R_MB_CUR_MAX)


utf8 <- function ()
switch(utils::localeToCharset()[1L], `UTF-8` = TRUE, FALSE)


.onLoad <- function (libname, pkgname)
.External2(C_onload, libname, pkgname)
# {
#     lockEnvironment(environment(.shFILE), bindings = TRUE)
#     getinitwd()  # force the promise for the initial working directory
#     assign("libname", libname, getNamespace(pkgname))
#     .External2(C_onload, libname, pkgname)
#     # cat("\n> .Platform\n"); print(.Platform);
#     # warning("this version is only for debugging purposes\n go to the archive and install the previous version")
#     # print(list(libname = libname, pkgname = pkgname))
# }


.onUnload <- function (libpath)
{
    .External2(C_onunload, libpath)
    library.dynam.unload(pkgname, libpath)
}
