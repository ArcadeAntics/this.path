path.functions <- .removeSource_from_inner_functions(
                  function (file, delayed = FALSE, local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0L)
{
    if (missing(file)) {
        n <- .External2(.C_asIntegerGE0, n)
        ofile <- .External2(.C_this_path, FALSE, TRUE, FALSE, FALSE, local, envir, matchThisEnv, srcfile)
        file <- .External2(.C_this_path, local, envir, matchThisEnv, srcfile)
        directory <- .dir(file)
        `attr<-`(environment(), "name", paste0("make_fix_funs:", file))
    }
    else if (delayed) {
        ofile <- file
        delayedAssign(
            "file",
            fun(),
            evalq(envir = new.env(), {
                if (.is_abs_path(ofile)) {
                    fun <- function() {
                        (set.sys.path(ofile, path.only = TRUE, allow.url = TRUE,
                            allow.file.uri = TRUE))
                    }
                }
                else {
                    wd <- getwd()
                    fun <- function() {
                        owd <- getwd()
                        on.exit(setwd(owd))
                        setwd(wd)
                        (set.sys.path(ofile, path.only = TRUE, allow.url = TRUE,
                            allow.file.uri = TRUE))
                    }
                }
                environment()
            })
        )
        delayedAssign("directory", .dir(file))
        `attr<-`(environment(), "name", paste0("make_fix_funs:", ofile))
    }
    else {
        ofile <- file
        file <- set.sys.path(file, path.only = TRUE, allow.url = TRUE,
            allow.file.uri = TRUE)
        ## remove the variable(s) created by set.sys.path()
        unset.sys.path()
        directory <- .dir(file)
        `attr<-`(environment(), "name", paste0("make_fix_funs:", file))
    }
    rm(srcfile, matchThisEnv, envir, n, local, delayed)
    this.path <- function(original = FALSE, contents = FALSE) {
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
    this.dir <- function() directory
    here <- function(..., .. = 0L) {
        base <- .here(file, ..)
        path.join(base, ...)
    }
    delayedAssign("ici", { here })
    this.proj <- function(...) {
        base <- .proj(directory)
        path.join(base, ...)
    }
    rel2here <- function(path) .relpath(path, directory, normalize = FALSE)
    rel2proj <- function(path) .relpath(path, .proj(directory), normalize = FALSE)
    LINENO <- function() .LINENO(file)
    lockEnvironment(environment(), bindings = TRUE)
    environment()
}
)


delayedAssign("make_fix_funs", { path.functions })
