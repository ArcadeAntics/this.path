.system <- function (command, intern, wait, mustWork, ..., quiet)
{
    intern <- if (intern) TRUE else FALSE
    wait <- if (wait) TRUE else FALSE
    mustWork <- tryCatch(if (mustWork) TRUE else FALSE, error = function(c) NA)
    quiet <- if (quiet) TRUE else FALSE


    .quiet <- Sys.getenv("R_THIS_PATH_QUIET", NA_character_)
    if (is.na(.quiet)) {
        on.exit(Sys.unsetenv("R_THIS_PATH_QUIET"))
        Sys.setenv(R_THIS_PATH_QUIET = quiet)
    }
    else if (.quiet != quiet) {
        on.exit(Sys.setenv(R_THIS_PATH_QUIET = .quiet))
        Sys.setenv(R_THIS_PATH_QUIET = quiet)
    }
    if (!quiet) cat(commandPrompt(), command, "\n", sep = "")
    value <- system(command, intern = intern, wait = wait, ...)
    if (intern) value
    else {
        if (!value || is.na(mustWork)) {
            if (!quiet)
                cat("\nProcess finished with exit code ",
                    value, "\n", sep = "")
        }
        else if (mustWork) {
            if (value == -1L)
                stop(gettextf("'%s' could not be run",
                    command), domain = NA)
            else stop(gettextf("'%s' execution failed with error code %d",
                command, value), domain = NA)
        }
        else if (value == -1L)
            warning(gettextf("'%s' could not be run", command),
                domain = NA)
        else warning(gettextf("'%s' execution failed with error code %d",
            command, value), domain = NA)
        invisible(value)
    }
}





python <- function (options, command = NULL, module = NULL, file, args,
    chdir,
    intern, wait, mustWork, ..., quiet)
{
    name <- "python"
    if (is.character(file) || is.null(file)) {
        if (length(file) == 0) file <- NULL
        else {
            if (length(file) > 1) {
                warning("first element used of 'file' argument")
                file <- file[[1L]]
            }
            if (grepl("^(ftp|http|https|file)://", file)) {
                if (chdir) warning("'chdir = TRUE' makes no sense for a URL")
            }
            else if (chdir && (path <- dirname(file)) != ".") {
                file <- basename(file)
                owd <- getwd()
                if (is.null(owd))
                    stop("cannot 'chdir' as current directory is unknown")
                on.exit(setwd(owd))
                setwd(path)
            }
            file <- commandQuote(file, windows.type = name)
        }
    }
    else stop("invalid 'file' argument")


    if (is.character(command) || is.null(command)) {
        if (length(command) == 0) command <- NULL
        else {
            command <- paste(command, collapse = "\n")
            command <- paste("-c", commandQuote(command, windows.type = name))
        }
    }
    else stop("invalid 'command' argument")


    if (is.character(module) || is.null(module)) {
        if (length(module) == 0) module <- NULL
        else {
            if (length(module) > 1) {
                warning("first element used of 'module' argument")
                module <- module[[1L]]
            }
            module <- paste("-m", commandQuote(module, windows.type = name))
        }
    }
    else stop("invalid 'module' argument")


    if (sum(!is.null(command), !is.null(module), !is.null(file)) > 1)
        stop("cannot use more than one of 'command', 'module', and 'file'")


    args <- asArgs(args)
    args <- commandQuote(args, windows.type = name)


    options <- asArgs(options)
    options <- commandQuote(options, windows.type = name)
    options <- c(name, options, command, module, file, args)
    command <- paste(options, collapse = " ")
    .system(command, intern = intern, wait = wait, mustWork = mustWork,
        ..., quiet = quiet)
}





.R <- function (options, file, exprs,
    # exprs.literal,
    args,
    # width.cutoff, deparseCtrl,
    chdir,
    # keep.source,
    intern, wait, mustWork, ..., quiet,
    name, extra)
{
    if (is.character(file) || is.null(file)) {
        if (length(file) == 0) file <- NULL
        else {
            if (length(file) > 1) {
                warning("first element used of 'file' argument")
                file <- file[[1L]]
            }
            if (grepl("^(ftp|ftps|http|https|file)://", file)) {
                if (chdir) warning("'chdir = TRUE' makes no sense for a URL")
            }
            else if (chdir && (path <- dirname(file)) != ".") {
                # if 'file' is a relative path, we change it to only the
                # basename. hopefully no one has a problem with this decision,
                # though we could use 'normalizePath' if this becomes an issue.
                # it seems to work well for both windows drive, windows unc,
                # unix drive, and unix unc (from what i can tell)
                #
                #
                # for local files, it's only 12 times faster, but for network
                # files, it's 5000 times faster


                # file <- normalizePath(file)
                file <- basename(file)


                owd <- getwd()
                if (is.null(owd))
                    stop("cannot 'chdir' as current directory is unknown")
                on.exit(setwd(owd))
                setwd(path)
            }
            file <- commandQuote(file, windows.type = name)
            if (extra) file <- paste0("--file=", file)
        }
    }
    else stop("invalid 'file' argument")


    # if (is.null(exprs) || exprs.literal && is.character(exprs)) {}
    # else {
    #     if (is.call(exprs) && exprs[[1]] == quote(`{`))
    #         exprs <- as.list(exprs[-1])
    #     exprs <- as.expression(exprs)
    #     exprs <- lapply(exprs, "deparse", width.cutoff = width.cutoff,
    #         backtick = TRUE, control = unique(c(deparseCtrl,
    #             if (keep.source) "useSource")))
    #     exprs <- unlist(exprs)
    # }
    # if (length(exprs) == 0) exprs <- NULL
    # else exprs <- paste("-e", commandQuote(exprs, windows.type = name))


    if (is.character(exprs) || is.null(exprs)) {
        if (length(exprs) == 0) exprs <- NULL
        else exprs <- paste("-e", commandQuote(exprs, windows.type = name))
    }
    else stop("invalid 'exprs' argument")


    if (!is.null(file) && !is.null(exprs))
        stop("cannot use 'exprs' with 'file'")


    args <- asArgs(args)
    if (length(args)) {
        args <- commandQuote(args, windows.type = name)
        if (extra)
            args <- c("--args", args)
    }


    options <- asArgs(options)
    options <- commandQuote(options, windows.type = name)
    options <- c(name, options, file, exprs, args)
    command <- paste(options, collapse = " ")
    .system(command, intern = intern, wait = wait, mustWork = mustWork,
        ..., quiet = quiet)
}


R <- function (options, file, exprs,
    # exprs.literal,
    args,
    # width.cutoff, deparseCtrl,
    chdir,
    # keep.source,
    intern, wait, mustWork, ..., quiet)
{
    .R(options = options, file = file, exprs = exprs,
        # exprs.literal = exprs.literal,
        args = args,
        # width.cutoff = width.cutoff, deparseCtrl = deparseCtrl,
        chdir = chdir,
        # keep.source = keep.source,
        intern = intern, wait = wait, mustWork = mustWork, ..., quiet = quiet,
        name = "R", extra = TRUE)
}


Rcmd <- function (options, command = "", args,
    intern, wait, mustWork, ..., quiet)
{
    options <- asArgs(options)
    options <- commandQuote(options, windows.type = "R")
    command <- asArgs(command)[[1L]]
    command <- commandQuote(command, windows.type = "Rterm")
    args <- asArgs(args)
    args <- commandQuote(args, windows.type = "R CMD")
    command <- paste(c("R", options, "CMD", command, args), collapse = " ")
    .system(command, intern = intern, wait = wait, mustWork = mustWork,
        ..., quiet = quiet)
}


Rgui <- function (options, args, intern, wait, mustWork, ..., quiet)
{
    .R(options = options, file = NULL, exprs = NULL,
        args = args,

        intern = intern, wait = wait, mustWork = mustWork, ..., quiet = quiet,
        name = "Rgui", extra = TRUE)
}


Rscript <- function (options, file, exprs,
    # exprs.literal,
    args,
    # width.cutoff, deparseCtrl,
    chdir,
    # keep.source,
    intern, wait, mustWork, ..., quiet)
{
    .R(options = options, file = file, exprs = exprs,
        # exprs.literal = exprs.literal,
        args = args,
        # width.cutoff = width.cutoff, deparseCtrl = deparseCtrl,
        chdir = chdir,
        # keep.source = keep.source,
        intern = intern, wait = wait, mustWork = mustWork, ..., quiet = quiet,
        name = "Rscript", extra = FALSE)
}


Rterm <- function (options, file, exprs,
    # exprs.literal,
    args,
    # width.cutoff, deparseCtrl,
    chdir,
    # keep.source,
    intern, wait, mustWork, ..., quiet)
{
    .R(options = options, file = file, exprs = exprs,
        # exprs.literal = exprs.literal,
        args = args,
        # width.cutoff = width.cutoff, deparseCtrl = deparseCtrl,
        chdir = chdir,
        # keep.source = keep.source,
        intern = intern, wait = wait, mustWork = mustWork, ..., quiet = quiet,
        name = if (.Platform$OS.type == "windows") "Rterm" else "R", extra = TRUE)
}





defaults <- quote(list(
    options       = NULL,
    file          = NULL,
    exprs         = NULL,
    # exprs.literal = FALSE,
    args          = NULL,
    # width.cutoff  = 60L,
    # deparseCtrl   = c("keepInteger", "showAttributes", "keepNA", "digits17"),
    chdir         = FALSE,
    # keep.source   = TRUE,
    intern        = FALSE,
    wait          = TRUE,
    mustWork      = FALSE,
    quiet         = Sys.getenv("R_THIS_PATH_QUIET", intern)
))
defaults <- as.pairlist(as.list(defaults[-1]))


i <- c("intern", "wait", "mustWork", "quiet")
j <- c("options", "args", i)
k <- c("file", "chdir", j)


formals(.system)[i              ] <- defaults[i]
formals(python )[k              ] <- defaults[k]
formals(R      )[names(defaults)] <- defaults
formals(Rcmd   )[j              ] <- defaults[j]
formals(Rgui   )[j              ] <- defaults[j]
formals(Rscript)[names(defaults)] <- defaults
formals(Rterm  )[names(defaults)] <- defaults


remove(defaults, i, j, k)
