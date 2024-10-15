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


    ## a function that will make and evaluate a promise when called
    eval.env <- environment()
    first_time <- TRUE
    set_script_path_fun <- function(pkgname, pkgpath) {
        set_script_path_expr <- NULL  ## this makes R CMD check happy
        delayedAssign(
            "set_script_path_expr",
            {
                oscript_path <- box::script_path()
                ## first time only, set another on.exit to restore script_path
                if (first_time) {
                    on.exit(
                        if (isNamespaceLoaded("box")) box::set_script_path(oscript_path),
                        add = TRUE
                    )
                    first_time <- FALSE
                }
                box::set_script_path(new_script_path)
                ## value of the promise is NULL, not that it matters
                NULL
            },
            eval.env
        )
        set_script_path_expr
    }
    ## append the function to the package:box onLoad hook list,
    ## and on exit remove the hook from the list
    ##
    ## this leaves the hook list the same as it was before entering
    ##
    ## why use a hook? we do not want to force package:box to load, and we want
    ## to re-run the function if the user reloads package:box
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
    if (isNamespaceLoaded("box")) set_script_path_fun()


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
