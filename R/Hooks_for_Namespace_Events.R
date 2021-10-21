.onLoad <- evalq(envir = new.env(), function (libname, pkgname)
{
    if (is.na(otoplevel <<- Sys.getenv("R_THIS_PATH_TOPLEVEL", NA)))
        Sys.setenv(R_THIS_PATH_TOPLEVEL = TRUE)
    else if (otoplevel)
        Sys.setenv(R_THIS_PATH_TOPLEVEL = FALSE)
})
evalq(envir = environment(.onLoad), {
    otoplevel <- NULL
})


.onUnload <- evalq(envir = environment(.onLoad), function (libpath)
{
    if (is.na(otoplevel))
        Sys.unsetenv("R_THIS_PATH_TOPLEVEL")
    else if (otoplevel)
        Sys.setenv(R_THIS_PATH_TOPLEVEL = TRUE)
})
