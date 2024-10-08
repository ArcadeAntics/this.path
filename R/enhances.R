with_script_path <- function (expr, local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"), srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    local
    envir
    matchThisEnv
    srcfile
    new_script_path <- tryCatch3({
        .External2(.C_this_path, local, envir, matchThisEnv, srcfile)
    }, thisPathNotFoundError = {
        if (is.null(wd <- getwd()))
            "."
        else path.join(wd, ".")
    })
    if (isNamespaceLoaded("box")) {
        ## if the namespace is loaded, this is simple.
        ##
        ## get the original script path,
        ## set the new script path,
        ## and on exit set the script path back to the original script path
        oscript_path <- box::script_path()
        ## if 'expr' unloads the namespace, do not reload it in the on.exit
        on.exit(if (isNamespaceLoaded("box")) box::set_script_path(oscript_path))
        box::set_script_path(new_script_path)
    }
    else {
        ## this makes R CMD check happy
        set_script_path_expr <- NULL
        ## make a promise that will do the same as the previous block
        delayedAssign("set_script_path_expr", {
            oscript_path <- box::script_path()
            on.exit(
                if (isNamespaceLoaded("box")) box::set_script_path(oscript_path),
                add = TRUE
            )
            box::set_script_path(new_script_path)
            ## value of the promise is NULL, not that it matters
            NULL
        })
        ## a function that will evaluate the previous promise when called
        set_script_path_fun <- function(pkgname, pkgpath) set_script_path_expr
        ## append the function to the package:box onLoad hook list,
        ## and on exit remove the hook from the list
        ##
        ## this leaves the hook list the same as it was before entering
        ##
        ## why use a hook? because we do not want to force package:box to load
        ## the user will specifically choose when it is loaded
        ##
        ## so now, the code that relies on package:box being loaded is only run
        ## after package:box has been loaded by the user's code
        hookName <- packageEvent("box")
        on.exit({
            hooks <- getHook(hookName)
            for (i in seq_along(hooks)) {
                if (.identical(hooks[[i]], set_script_path_fun)) {
                    setHook(hookName, hooks[-i], "replace")
                    break
                }
            }
        })
        setHook(hookName, set_script_path_fun)
    }
    expr
}


make_fix_file <- function (criterion, local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"), srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    path <- .External2(.C_this_path, local, envir, matchThisEnv, srcfile)
    path <- .dir(path)
    if (grepl("^(https|http|ftp|ftps)://", path))
        stop("make_fix_file() does not work for URL pathnames")
    if (!inherits(criterion, "root_criterion"))
        criterion <- rprojroot::as.root_criterion(criterion)
    path.functions(criterion$find_file(path = path))$here
}
