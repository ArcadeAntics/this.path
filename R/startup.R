.site_file <- evalq(envir = new.env(), {
    ## https://github.com/wch/r-source/blob/trunk/src/main/startup.c#L91
    delayedAssign("filename", {
        local({
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
        })
    })
    delayedAssign("ofile", {
        if (isTRUE(.shINFO[["no_site_file"]]))
            NA_character_
        else if (file.exists(filename))
            filename
        else NA_character_
    })
    delayedAssign("file", { .normalizePath(ofile) })
function (original = TRUE, for.msg = FALSE)
.External2(.C_site_file, original, for.msg)
})


delayedAssign(".has_site_file", { !is.na(.site_file()) })


site.file <- function (original = FALSE, for.msg = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.))
            .site_file(original, for.msg)
        else stop("'site.file' with 'else.' but not 'default' makes no sense")
    }
    else {
        if (missing(else.)) {
            if (.has_site_file)
                .site_file(original, for.msg)
            else if (for.msg)
                NA_character_
            else default
        }
        else {
            if (.has_site_file) {
                value <- .site_file(original, for.msg)
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


.init_file <- evalq(envir = new.env(), {
    delayedAssign("filename", {
        local({
            ## https://github.com/wch/r-source/blob/trunk/src/gnuwin32/sys-win32.c#L40
            if (.OS_windows) {
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
        })
    })
    delayedAssign("ofile", {
        if (isTRUE(.shINFO[["no_init_file"]]))
            NA_character_
        else if (file.exists(filename))
            filename
        else NA_character_
    })
    delayedAssign("file", { .normalizePath(ofile) })
function (original = TRUE, for.msg = FALSE)
.External2(.C_init_file, original, for.msg)
})


delayedAssign(".has_init_file", { !is.na(.init_file()) })


init.file <- function (original = FALSE, for.msg = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.))
            .init_file(original, for.msg)
        else stop("'init.file' with 'else.' but not 'default' makes no sense")
    }
    else {
        if (missing(else.)) {
            if (.has_init_file)
                .init_file(original, for.msg)
            else if (for.msg)
                NA_character_
            else default
        }
        else {
            if (.has_init_file) {
                value <- .init_file(original, for.msg)
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
