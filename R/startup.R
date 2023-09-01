site.file <- function ()
{
    p <- Sys.getenv("R_PROFILE", NA_character_)
    if (!is.na(p)) {
        if (nzchar(p)) return(path.expand(p))
        else return(NA_character_)
    }
    if (nzchar(p <- .Platform$r_arch)) {
        p <- sprintf("%s/etc/%s/Rprofile.site", R.home(), p)
        if (file.exists(p)) return(p)
    }
    p <- sprintf("%s/etc/Rprofile.site", R.home())
    return(p)
}


init.file <- function ()
{
    p <- Sys.getenv("R_PROFILE_USER", NA_character_)
    if (!is.na(p)) {
        if (!nzchar(p)) return(NA_character_)
        return(path.expand(p))
    }
    if (file.exists(p <- ".Rprofile"))
        return(p)
    if ((p <- path.expand("~")) == "~")
        return(NA_character_)
    if (file.exists(p <- sprintf("%s/.Rprofile", p)))
        return(p)
    return(NA_character_)
}
