.onLoad <- function (libname, pkgname)
{
    initialize.__file__()
    if (is.na(toplevel <- Sys.getenv("R_THIS_PATH_TOPLEVEL", NA)))
        Sys.setenv(R_THIS_PATH_TOPLEVEL = TRUE)
    else if (toplevel)
        Sys.setenv(R_THIS_PATH_TOPLEVEL = FALSE)
    invisible()
}


.onUnload <- function (libpath)
{
    if (!is.na(Sys.getenv("R_THIS_PATH_TOPLEVEL", NA)))
        Sys.unsetenv("R_THIS_PATH_TOPLEVEL")
    invisible()
}
