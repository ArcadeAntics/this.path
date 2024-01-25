.site_file <- evalq(envir = new.env(), {
    delayedAssign("file_info", {
        local({
            filename <- local({
                ## https://github.com/wch/r-source/blob/trunk/src/main/startup.c#L98
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
            list(filename = filename, wd = getwd(), exists = file.exists(filename))
        })
    })
    delayedAssign("ofile", {
        if (isTRUE(.shINFO[["no_site_file"]]))
            NA_character_
        else if (file_info[["exists"]])
            file_info[["filename"]]
        else NA_character_
    })
    delayedAssign("file", {
        if (is.na(ofile))
            .normalizePath(ofile)
        else if (is.null(file_info[["wd"]]))
            .normalizePath(ofile)
        else .normalizePath_against(file_info[["wd"]], ofile)
    })
              function (original = TRUE, for.msg = FALSE)
.External2(.C_site_file, original, for.msg)
})


.init_file <- evalq(envir = new.env(), {
    delayedAssign("file_info", {
        local({
            filename <- local({
                if (.OS_windows) {
                    ## https://github.com/wch/r-source/blob/trunk/src/gnuwin32/sys-win32.c#L47
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
                } else {
                    ## https://github.com/wch/r-source/blob/trunk/src/unix/sys-unix.c#L68
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
            list(filename = filename, wd = getwd(), exists = file.exists(filename))
        })
    })
    delayedAssign("ofile", {
        if (isTRUE(.shINFO[["no_init_file"]]))
            NA_character_
        else if (file_info[["exists"]])
            file_info[["filename"]]
        else NA_character_
    })
    delayedAssign("file", {
        if (is.na(ofile))
            .normalizePath(ofile)
        else if (is.null(file_info[["wd"]]))
            .normalizePath(ofile)
        else .normalizePath_against(file_info[["wd"]], ofile)
    })
              function (original = TRUE, for.msg = FALSE)
.External2(.C_init_file, original, for.msg)
})





.startup_info <- evalq(envir = new.env(), {
    delayedAssign("has_init_file", { !is.na(.init_file()) })
    delayedAssign("has_site_file", { !is.na(.site_file()) })
    environment()
})


.in_site_file <- evalq(envir = new.env(), {
    x <- TRUE
    if (getRversion() >= "4.2.0") {
                 function ()
{
    if (x) {
        sym <- ".Library.site"
        if (exists(sym, envir = .GlobalEnv, inherits = FALSE) &&
            bindingIsActive(sym, .GlobalEnv) &&
            exists(sym, envir = .BaseNamespaceEnv, inherits = FALSE) &&
            bindingIsActive(sym, .BaseNamespaceEnv) &&
            identical(
                activeBindingFunction(sym, .GlobalEnv),
                activeBindingFunction(sym, .BaseNamespaceEnv)
            ))
        {
            ## we are still in the site file, so do nothing
        }
        else {
            ## we are no longer in the site file, permanently set to FALSE
            x <<- FALSE
            lockBinding("x", parent.env(environment()))
        }
    }
    x
}
    } else {
                 function ()
{
    if (x) {
        if (sys.parents()[1L] == 1L) {
            ## we are still in the site file, so do nothing
        }
        else {
            ## we are no longer in the site file, permanently set to FALSE
            x <<- FALSE
            lockBinding("x", parent.env(environment()))
        }
    }
    x
}
    }
})





site.file <- function (original = FALSE, for.msg = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.))
            .site_file(original, for.msg)
        else stop("'site.file' with 'else.' but not 'default' makes no sense")
    }
    else {
        if (missing(else.)) {
            if (.startup_info[["has_site_file"]])
                .site_file(original, for.msg)
            else if (for.msg)
                NA_character_
            else default
        }
        else {
            if (.startup_info[["has_site_file"]]) {
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


init.file <- function (original = FALSE, for.msg = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.))
            .init_file(original, for.msg)
        else stop("'init.file' with 'else.' but not 'default' makes no sense")
    }
    else {
        if (missing(else.)) {
            if (.startup_info[["has_init_file"]])
                .init_file(original, for.msg)
            else if (for.msg)
                NA_character_
            else default
        }
        else {
            if (.startup_info[["has_init_file"]]) {
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





with_site.file <- function (expr)
{
    .External2(.C_with_startup_file)
    invisible()
}


with_init.file <- function (expr)
{
    if (
        ## this condition must be first because it checks the validity of 'expr'
        .External2(.C_is_valid_init_file_expr) &&

        ## check that this expression is part of the top of the call stack
        ## i.e. evaluated in global environment WITHOUT using .Internal(eval())
        sys.parent() == 0L &&
        identical(parent.frame(), .GlobalEnv) &&
        .External2(.C_sys.whiches, 0L)[1L] == 1L &&

        ## check that the first call has no source reference
        ## it might if GUI runs a file
        is.null(attributes(sys.call(1L))) &&

        ## check that there are no other files being run right now
        .External2(.C_getframenumber) == 0L &&

        ## this condition must be last because it forces the promises in .init_file
        .startup_info[["has_init_file"]])
    {
        .External2(.C_set_init_file)
        on.exit(.External2(.C_unset_init_file))
    }
    .External2(.C_with_startup_file)
    invisible()
}
