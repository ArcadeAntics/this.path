oopt <- options(keep.source = FALSE)
path.functions <- function (file)
{
    if (missing(file)) {
        envir <- parent.frame()
        matchThisEnv <- getOption("topLevelEnvironment")
        ofile <- .External2(.C_thispath, FALSE, TRUE, envir, matchThisEnv)
        file <- .External2(.C_thispath, envir, matchThisEnv)
        rm(matchThisEnv, envir)
    }
    else {
        ofile <- file
        file <- set.sys.path(file, path.only = TRUE, allow.url = TRUE,
            allow.file.uri = TRUE)
        unset.sys.path()
    }
    directory <- .dir(file)


this.path <- function (original = FALSE, contents = FALSE)
{
    ## force the promise
    original
    if (contents) {
        if (.isfalse(original))
            readLines(file)
        else stop(gettextf("'%s' must be FALSE when '%s' is TRUE", "original", "contents"))
    }
    else {
        if (.istrue(original))
            ofile
        else file
    }
}


this.dir <- function ()
directory


here <- function (..., .. = 0L)
{
    base <- .here(file, ..)
    path.join(base, ...)
}


this.proj <- function (...)
{
    base <- .proj(directory)
    path.join(base, ...)
}


rel2here <- function (path)
.relpath(path, directory, normalize = FALSE)


rel2proj <- function (path)
.relpath(path, .proj(directory), normalize = FALSE)


LINENO <- function ()
.LINENO(file)


    `attr<-`(environment(), "name", paste0("path.functions:", file))
    lockEnvironment(environment(), bindings = TRUE)
    environment()
}
options(oopt)
rm(oopt)
