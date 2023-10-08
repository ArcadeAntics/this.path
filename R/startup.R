.site.file <- evalq(envir = new.env(), {
    ## https://github.com/wch/r-source/blob/trunk/src/main/startup.c#L91
    delayedAssign("filename", local({
        p <- Sys.getenv("R_PROFILE", NA_character_)
        if (!is.na(p)) {
            if (nzchar(p)) return(p)
            else return(NA_character_)
        }
        if (nzchar(p <- .Platform$r_arch)) {
            p <- sprintf("%s/etc/%s/Rprofile.site", R.home(), p)
            if (file.exists(p)) return(p)
        }
        p <- sprintf("%s/etc/Rprofile.site", R.home())
        return(p)
    }))
    delayedAssign("ofile",
        if (isTRUE(.shINFO[["no.site.file"]]))
            NA_character_
        else if (file.exists(filename))
            filename
        else NA_character_
    )
    delayedAssign("file", .normalizePath(ofile))
function (original = TRUE, for.msg = FALSE)
.External2(.C_site.file, original, for.msg)
})


delayedAssign(".has.site.file", !is.na(.site.file()))


site.file <- function (original = FALSE, for.msg = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.))
            .site.file(original, for.msg)
        else stop("'site.file' with 'else.' but not 'default' makes no sense")
    }
    else {
        if (missing(else.)) {
            if (.has.site.file)
                .site.file(original, for.msg)
            else if (for.msg)
                NA_character_
            else default
        }
        else {
            if (.has.site.file) {
                value <- .site.file(original, for.msg)
                (else.)(value)
            }
            else if (for.msg) {
                value <- NA_character_
                (else.)(value)
            }
            else default
        }
    }
}


# {
#     p <- Sys.getenv("R_PROFILE_USER", NA_character_)
#     if (!is.na(p)) {
#         if (!nzchar(p)) return(NA_character_)
#         return(p)
#     }
#     if (file.exists(p <- ".Rprofile"))
#         return(p)
#     if ((p <- path.expand("~")) == "~")
#         return(NA_character_)
#     if (file.exists(p <- sprintf("%s/.Rprofile", p)))
#         return(p)
#     return(NA_character_)
# }


.init.file <- evalq(envir = new.env(), {
    delayedAssign("filename", local({
        ## https://github.com/wch/r-source/blob/trunk/src/gnuwin32/sys-win32.c#L40
        if (.os.windows) {
            p <- Sys.getenv("R_PROFILE_USER", NA_character_)
            if (!is.na(p)) {
                if (!nzchar(p)) return(NA_character_)
                return(p)
            }
            if (file.exists(p <- ".Rprofile"))
                return(p)
            p <- Sys.getenv("R_USER", NA_character_)
            if (!is.na(p)) {
                p <- sprintf("%s/.Rprofile", p)
            }
            return(p)
        ## https://github.com/wch/r-source/blob/trunk/src/unix/sys-unix.c#L68
        } else {
            p <- Sys.getenv("R_PROFILE_USER", NA_character_)
            if (!is.na(p)) {
                if (!nzchar(p)) return(NA_character_)
                return(p)
            }
            if (file.exists(p <- ".Rprofile"))
                return(p)
            if (is.na(home <- Sys.getenv("HOME", NA_character_)))
                return(NA_character_)
            p <- sprintf("%s/.Rprofile", home)
            return(p)
        }
    }))
    delayedAssign("ofile",
        if (isTRUE(.shINFO[["no.init.file"]]))
            NA_character_
        else if (file.exists(filename))
            filename
        else NA_character_
    )
    delayedAssign("file", .normalizePath(ofile))
function (original = TRUE, for.msg = FALSE)
.External2(.C_init.file, original, for.msg)
})


delayedAssign(".has.init.file", !is.na(.init.file()))


init.file <- function (original = FALSE, for.msg = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.))
            .init.file(original, for.msg)
        else stop("'init.file' with 'else.' but not 'default' makes no sense")
    }
    else {
        if (missing(else.)) {
            if (.has.init.file)
                .init.file(original, for.msg)
            else if (for.msg)
                NA_character_
            else default
        }
        else {
            if (.has.init.file) {
                value <- .init.file(original, for.msg)
                (else.)(value)
            }
            else if (for.msg) {
                value <- NA_character_
                (else.)(value)
            }
            else default
        }
    }
}
