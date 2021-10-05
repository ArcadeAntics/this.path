.onLoad <- function (libname, pkgname)
{
    if (is.na(otoplevel <<- Sys.getenv("R_THIS_PATH_TOPLEVEL", NA)))
        Sys.setenv(R_THIS_PATH_TOPLEVEL = TRUE)
    else if (otoplevel)
        Sys.setenv(R_THIS_PATH_TOPLEVEL = FALSE)
}
environment(.onLoad) <- new.env()
evalq({
    otoplevel <- NULL
}, environment(.onLoad))


.onUnload <- function (libpath)
{
    if (is.na(otoplevel))
        Sys.unsetenv("R_THIS_PATH_TOPLEVEL")
    else if (otoplevel)
        Sys.setenv(R_THIS_PATH_TOPLEVEL = TRUE)
}
environment(.onUnload) <- environment(.onLoad)
