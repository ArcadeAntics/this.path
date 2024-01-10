with_script_path <- function (expr, local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"), srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    local
    envir
    matchThisEnv
    srcfile
    oscript_path <- box::script_path()
    on.exit(box::set_script_path(oscript_path))
    box::set_script_path(tryCatch3({
        .External2(.C_this_path, local, envir, matchThisEnv, srcfile)
    }, thisPathNotExistsError = ,
       thisPathNotFoundError  = {
        if (!is.null(wd <- getwd()))
            path.join(wd, ".")
        else "."
    }))
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
