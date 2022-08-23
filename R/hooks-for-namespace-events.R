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


tryCatch2 <- function (expr, ..., else., finally)
{
    stop("cannot use while checking package")
}


.onLoad <- function (libname, pkgname)
{
    # cat("> libname\n")
    # print(libname)
    # cat("> pkgname\n")
    # print(pkgname)
    if (Sys.getenv("_R_CHECK_PACKAGE_NAME_") != pkgname)
        sys.source(
            file = system.file(package = pkgname, "more-R", "tryCatch2.R", mustWork = TRUE),
            envir = getNamespace(pkgname),
            keep.source = FALSE,
            keep.parse.data = FALSE
        )
    # sink("~/temp3.txt", append = TRUE)
    # on.exit(sink())
    # cat('> Sys.getenv("_R_CHECK_PACKAGE_NAME_")\n')
    # print(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))
    # cat("> tryCatch2\n")
    # print(tryCatch2)
    # cat("> commandArgs()\n")
    # print(commandArgs())
    # cat("\n\n\n\n\n")
}
