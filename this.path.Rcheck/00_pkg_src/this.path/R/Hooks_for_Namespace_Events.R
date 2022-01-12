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

