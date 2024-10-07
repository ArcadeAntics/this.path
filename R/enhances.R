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
        oscript_path <- box::script_path()
        on.exit(if (isNamespaceLoaded("box")) box::set_script_path(oscript_path))
        box::set_script_path(new_script_path)
    }
    else {
        set_script_path_expr <- NULL
        delayedAssign("set_script_path_expr", {
            oscript_path <- box::script_path()
            on.exit(
                if (isNamespaceLoaded("box")) box::set_script_path(oscript_path),
                add = TRUE
            )
            box::set_script_path(new_script_path)
            NULL
        })
        set_script_path_fun <- function(pkgname, pkgpath) set_script_path_expr
        ## on exit, remove the hook from the list
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
