.Rscript <- function (options = NULL, trailing = character(), dry.run = FALSE,
    show.command = TRUE, intern = TRUE, show.output.on.console = show.command,
    ...)
{
    command <- path.join(
        R.home("bin"),
        if (.OS_windows) "Rscript.exe" else "Rscript"
    )
    args <- c(command, options)
    args <- c(shQuote(args), trailing)
    command <- paste(args, collapse = " ")
    if (dry.run)
        return(command)
    if (show.command)
        cat(command, "\n", sep = "")
    value <- suppressWarnings(system(command = command, intern = intern, ...))
    if (show.output.on.console)
        writeLines(value)
    invisible(value)
}


.build.this <- function (chdir = FALSE, file = here(), which = "tar")
{
    ## .build.this               package:this.path               R Documentation
    ##
    ## Building Packages
    ##
    ##
    ##
    ## Description:
    ##
    ## This provides a more general method of making packages, not specifically
    ## R packages, for distribution and version control.
    ##
    ##
    ##
    ## Usage:
    ##
    ## .build.this(chdir = FALSE, file = here(), which = "tar")
    ##
    ##
    ##
    ## Arguments:
    ##
    ## chdir
    ##
    ##     TRUE or FALSE; change directory to directory of package source
    ##     before building tar/zip archives? This will not affect the build
    ##     process, only the location in which the archive is made.
    ##
    ## file
    ##
    ##     name of a directory which is to be packaged
    ##
    ## which
    ##
    ##     which type of archive do you want to make, "tar" or "zip"?
    ##     Can be both i.e. 'which = c("tar", "zip")'
    ##
    ##
    ##
    ## Details:
    ##
    ## .build.this builds package archives similar to 'R CMD build', and takes
    ## the name of the package and its version from the 'DESCRIPTION' file.
    ## If you are unfamiliar with the 'DESCRIPTION' file, here is a outline:
    ##
    ## Package: example
    ## Version: 0.1.0
    ## Date: %Y-%m-%d
    ## License: What license is it under?
    ## Title: What the Package Does (Title Case)
    ## Description: More about what it does (maybe more than one line)
    ##     Use four spaces when indenting paragraphs within the Description.
    ## Author: Who wrote it
    ## Maintainer: The package maintainer <yourself@somewhere.net>
    ##
    ## The package name should start with a letter, end with a letter or
    ## number, and contain any combination of letters, numbers, underscores,
    ## periods, and hyphens in the middle.
    ##
    ## The package version should be two or more numbers separated by hyphens
    ## and/or periods like '12.34.56-78'.



    ## determine which types of archive we want to make
    choices <- c("tar", "zip")
    nms <- c("tarball", "zip archive")
    which <- unique(match.arg(which, choices, several.ok = TRUE))
    nms <- nms[match(which, choices)]


    ## check that 'file' is valid, and 'chdir' if required
    if (!.IS_SCALAR_STR(file)) {
        stop(gettextf("'%s' must be a character string", "file", domain = "R"), domain = NA)
    } else if (grepl("^(ftp|ftps|http|https)://", file, useBytes = TRUE)) {
        stop("cannot '.build.this' on a URL")
    } else if (chdir && (path <- file) != ".") {
        file <- "."
        owd <- getwd()
        if (is.null(owd))
            stop("cannot 'chdir' as current directory is unknown")
        on.exit(setwd(owd))
        setwd(path)
    }


    # op <- options(error = quote({
    #     quit(save = "no", status = 1, runLast = FALSE)
    # }))
    # on.exit(options(op), add = TRUE)


    ## look for the appropriate applications to create archives
    ## if all are unavailable, throw an error
    ## if some are unavailable, throw a warning and continue with the rest
    val <- Sys.which(which)
    i <- is.na(val) | val == ""
    if (any(i)) {
        msgs <- paste0(which, " unavailable; cannot create ", nms)
        if (all(i)) {
            stop(paste(msgs, collapse = "\n "))
        } else warning(paste(msgs[i], collapse = "\n"))
        which <- which[!i]
        nms <- nms[!i]
    }


    ## check for file "DESCRIPTION"
    DESCRIPTION <- path.join(file, "DESCRIPTION")
    cat("* checking for file '", DESCRIPTION, "' ... ", sep = "")
    if (!file.exists(DESCRIPTION)) {
        cat("NO\n\n")
        stop("cannot create ", paste(nms, collapse = ", "),
            " without file '", DESCRIPTION, "'")
    }
    cat("OK\n")


    packageInfo <- read.dcf(DESCRIPTION,
        fields = c("Package", "Version"))
    pkgname <- packageInfo[[1L, "Package"]]
    version <- packageInfo[[1L, "Version"]]


    ## check that the package name and version exist in "DESCRIPTION"
    problems <- c(
        if (is.na(pkgname))
            "invalid 'Package' field\n\n",
        if (is.na(version))
            "invalid 'Version' field\n\n"
    )
    if (!is.null(problems)) {
        cat(" ERROR\n", problems, sep = "")
        stop("invalid package DESCRIPTION file")
    }


    ## check that the package name and version are valid names and versions
    cat("* checking DESCRIPTION meta-information ... ", sep = "")
    valid_package_name <- "([[:alpha:]][[:alnum:]_.-]*[[:alnum:]])"
    valid_package_version <- "(([[:digit:]]+[.-]){1,}[[:digit:]]+)"
    problems <- c(
        if (!grepl(paste0("^", valid_package_name   , "$"), pkgname))
            "Malformed package name\n\n",
        if (!grepl(paste0("^", valid_package_version, "$"), version))
            "Malformed package version.\n\n"
    )
    if (!is.null(problems)) {
        cat("ERROR\n", problems, sep = "")
        stop("invalid package DESCRIPTION file")
    }
    cat("OK\n")


    ## files to exclude, and all files
    exclude <- NULL
    files <- list.files(path = file, all.files = TRUE, recursive = TRUE, include.dirs = TRUE)


    ## directories to always exclude
    i <- basename2(files) %in% c(

        ## directories from source control systems
        "CSV", ".svn", ".arch-ids", ".bzr", ".git", ".hg",

        ## directories from eclipse
        ".metadata",

        "check", "chm"
    )


    ## more directories to exclude, ending with Old or old
    i <- i | grepl("(Old|old)$", files)


    ## and, of course, are directories
    i <- i & dir.exists(path.join(file, files))


    if (any(i)) {
        exclude <- c(exclude, files[i])
        files <- files[!i]
    }


    ## files to always exclude
    i <- grepl(
        pattern = paste0(
            "(",
            c(
                "^GNUmakefile$",
                "^Read-and-delete-me$",

                ## starts with .#
                "^\\.#",

                ## starts and ends with #
                "^#.*#$",

                ## ends with ~ or .bak or .swp
                "~$", "\\.bak$", "\\.swp$"
            ),
            ")", collapse = "|"
        ),
        basename2(files)
    )


    if (any(i)) {
        exclude <- c(exclude, files[i])
        files <- files[!i]
    }


    ## do not put previous archives in the new archives
    prev_build_patterns <- paste0(
        "^",
        gsub(".", "\\.", pkgname, fixed = TRUE),
        "_",
        valid_package_version,
        c("\\.tar\\.gz", "\\.tgz", "\\.zip"),
        "$"
    )


    for (exclude_pattern in prev_build_patterns) {
        if (any(i <- grepl(exclude_pattern, files))) {
            exclude <- c(exclude, files[i])
            files <- files[!i]
        }
    }


    ## look for a ".buildignore" file containing
    ## a list of Perl patterns (case insensitive)
    ## specifying files to ignore when archiving
    ignore_file <- path.join(file, ".buildignore")
    if (file.exists(ignore_file)) {
        for (exclude_pattern in readLines(ignore_file, warn = FALSE, encoding = "UTF-8")) {
            if (any(i <- grepl(exclude_pattern, files, ignore.case = TRUE, perl = TRUE))) {
                exclude <- c(exclude, files[i])
                files <- files[!i]
            }
        }
    }


    ## for directories in 'exclude', also
    ## exclude the files within said directories
    exclude_dirs <- exclude[dir.exists(path.join(file, exclude))]
    for (exclude_prefix in paste0(exclude_dirs, "/", recycle0 = TRUE)) {
        if (any(i <- startsWith(files, exclude_prefix))) {
            exclude <- c(exclude, files[i])
            files <- files[!i]
        }
    }


    ## create a new directory to hold the temporary files and archives
    dir.create(my_tmpdir <- tempfile("dir"))
    on.exit(unlink(my_tmpdir, recursive = TRUE, force = TRUE, expand = FALSE), add = TRUE)


    ## create another directory to hold the temporary files
    dir.create(pkgdir <- path.join(my_tmpdir, pkgname))


    ## within said directory, make the appropriate sub-directories
    isdir <- dir.exists(path.join(file, files))
    dirs <- files[isdir]
    for (path in path.join(pkgdir, dirs))
        dir.create(path, showWarnings = TRUE, recursive = TRUE)


    ## fill the directory and sub-directories with their files,
    ## while maintaining file modify time
    if (any(i <- !file.copy(
        path.join(file  , files[!isdir]),
        path.join(pkgdir, files[!isdir]),
        copy.date = TRUE
    )))
        stop(ngettext(sum(i), "unable to copy file: ",
                              "unable to copy files:\n  "),
             paste(encodeString(files[!isdir][i], quote = "\""), collapse = "\n  "))


    ## set the modify time of the sub-directories to their original values
    Sys.setFileTime(
                   path.join(pkgdir, dirs),
        file.mtime(path.join(file  , dirs))
    )


    for (apk in which) {
        switch(apk, tar = {
            build_name <- paste0(pkgname, "_", version, ".tar.gz")
            build_path <- path.join(my_tmpdir, build_name)
            args <- c("tar", "-czf", shQuote(build_path), "-C", shQuote(my_tmpdir), shQuote(pkgname))
            command <- paste(args, collapse = " ")
            cat("* building '", build_name, "'\n", sep = "")
            res <- system(command, ignore.stdout = TRUE)
            if (res == -1L) {
                stop("'", command, "' could not be run")
            } else if (res) {
                stop("'", command, "' execution failed with error code ", res)
            } else if (i <- !file.copy(
                build_path,
                path.join(file, build_name),
                overwrite = TRUE
            )) {
                stop("unable to move:\n  ",
                     encodeString(build_path, quote = "\""),
                     "\nto:\n  ",
                     encodeString(path.join(file, build_name), quote = "\""))
            }
        }, zip = {
            build_name <- paste0(pkgname, "_", version, ".zip")
            build_path <- path.join(my_tmpdir, build_name)
            args <- c("zip", "-r", shQuote(build_path), shQuote(pkgname))
            command <- paste(args, collapse = " ")
            cat("* building '", build_name, "'\n", sep = "")
            res <- .with_chdir(my_tmpdir, {
                system(command, ignore.stdout = TRUE)
            })
            if (res == -1L) {
                stop("'", command, "' could not be run")
            } else if (res) {
                stop("'", command, "' execution failed with error code ", res)
            } else if (i <- !file.copy(
                build_path,
                path.join(file, build_name),
                overwrite = TRUE
            )) {
                stop("unable to move:\n  ",
                     encodeString(build_path, quote = "\""),
                     "\nto:\n  ",
                     encodeString(path.join(file, build_name), quote = "\""))
            }
        }, stop("invalid 'which'; should not happen, please report!"))
    }
}


.with_chdir <- function (wd, expr)
{
    owd <- getwd()
    if (is.null(owd))
        stop("cannot 'chdir' as current directory is unknown", domain = "R-base")
    on.exit(setwd(owd))
    setwd(wd)
    expr
}


.R_BraceSymbol <- as.symbol("{")


.maybeQuote <- function (expr, evaluated, simplify.brace = TRUE)
{
    ## possibly quote an expression
    ##
    ## when 'evaluated' is missing, 'expr' is quoted only if it is an
    ## (unevaluated) call to `{`
    ##
    ## when 'evaluated' is FALSE, 'expr' is always quoted
    ##
    ## when 'evaluated' is TRUE, 'expr' is evaluated as normal
    ##
    ## this is intended to be called inside another function, like match.fun()


    if (missing(expr))
        expr


    if (missing(evaluated)) {
        if (is.call(e <- eval(as.call(list(substitute, substitute(expr))), parent.frame())) &&
            identical(e[[1L]], .R_BraceSymbol))
        {
            expr <- e
        }
    }
    else if (!evaluated)
        expr <- eval(as.call(list(substitute, substitute(expr))), parent.frame())


    ## force the evaluated of 'expr'
    else expr


    if (is.call(expr)) {
        ## always turn a call into an expression so that it
        ## cannot be misinterpreted later
        if (simplify.brace) {
            if (identical(expr[[1L]], .R_BraceSymbol))
                as.expression(as.list(expr[-1L]))
            else as.expression(list(expr))
        }
        else as.expression(list(expr))
    }
    else expr
}


.code2character <- function (x, width.cutoff = 60L,
    deparseCtrl = c("keepInteger", "showAttributes", "useSource", "keepNA", "digits17"))
{
    ## if 'x' is an (unevaluated) call to `{`
    ## deparse each sub-expression
    ##
    ## if 'x' is an expression() vector
    ## make a list of deparsed expressions
    ##
    ## if 'x' is a language object
    ## deparse the expression
    ##
    ## if 'x' is a character vector
    ## leave as is


    fun <- function(xx) {
        deparse1(xx, collapse = "\n", width.cutoff = width.cutoff,
            backtick = TRUE, control = deparseCtrl)
    }


    if (is.expression(x)) {
        if ("useSource" %in% deparseCtrl) {
            getLines <- function(srcref) {
                srcfile <- attr(srcref, "srcfile", exact = TRUE)
                getSrcLines(srcfile, srcref[7L], srcref[8L])
            }
            trimLines <- function(lines, srcref) {
                if (length(lines)) {
                    enc <- Encoding(lines)
                    Encoding(lines) <- "bytes"
                    if (length(lines) > srcref[8L] - srcref[7L])
                        lines[length(lines)] <- substring(lines[length(lines)], 1L, srcref[4L])
                    lines[1L] <- substring(lines[1L], srcref[2L])
                    Encoding(lines) <- enc
                }
                lines
            }
            # x <- parse(text = "  5 +\n 6\t; 7 +8  ;  ", keep.source = TRUE)
            srcref <- attr(x, "wholeSrcref", exact = TRUE)
            if (!is.null(srcref))
                tryCatch2({
                    lines <- getLines(srcref)
                }, error = function(e) {
                }, else. = {
                    lines <- trimLines(lines, srcref)
                    return(paste(lines, collapse = "\n"))
                })
            srcrefs <- attr(x, "srcref", exact = TRUE)
            if (!is.null(srcrefs)) {
                y <- character(length(srcrefs))
                for (i in seq_along(y)) {
                    srcref <- srcrefs[[i]]
                    y[[i]] <- if (!is.null(srcref))
                        tryCatch2({
                            lines <- getLines(srcref)
                        }, error = function(e) {
                            fun(x[[i]])
                        }, else. = {
                            lines <- trimLines(lines, srcref)
                            paste(lines, collapse = "\n")
                        })
                    else fun(x[[i]])
                }
                y
            }
            else vapply(x, fun, "")
        }
        else vapply(x, fun, "")
    }
    else if (is.language(x))
        fun(x)
    else if (is.character(x))
        x
    else stop("invalid 'x', expected a language object or character vector")
}


.writeCode <- function (x, file = stdout(), evaluated, simplify.brace = TRUE,
    width.cutoff = 60L, deparseCtrl = c("keepInteger", "showAttributes", "useSource", "keepNA", "digits17"))
{
    x <- .maybeQuote(x, evaluated, simplify.brace)
    x <- .code2character(x, width.cutoff, deparseCtrl)
    if (is.null(file))
        x
    else {
        writeLines(x, file, useBytes = !.utf8locale)
        invisible(x)
    }
}


if (getRversion() < "3.2.0") {


    tmp <- formals(.code2character)[["deparseCtrl"]]
    formals(.code2character)[["deparseCtrl"]] <- tmp[!vapply(tmp, identical, "digits17", FUN.VALUE = NA)]


    tmp <- formals(.writeCode)[["deparseCtrl"]]
    formals(.writeCode)[["deparseCtrl"]] <- tmp[!vapply(tmp, identical, "digits17", FUN.VALUE = NA)]


    rm(tmp)
}


.readFiles <- function (files)
vapply(files, function(file) paste0(readLines(file), "\n", collapse = ""), "")


.envvars <- function (...)
{
    args <- list(...)
    if (length(args) == 0L)
        as.list(Sys.getenv())
    else {
        visible <- FALSE
        if (length(args) == 1L &&
            typeof(args[[1L]]) %in% c("NULL", "pairlist", "language",
                "...", "list", "expression") &&
            is.null(names(args)))
        {
            args <- args[[1L]]
            if (typeof(args) == "...") {
                # ... <- args
                assign("...", args)
                args <- list(...)
            }
            else if (!is.vector(args) || is.object(args))
                args <- as.list(args)
        }
        if (!length(args))
            value <- structure(list(), names = character())
        else if (is.null(tags <- names(args)) ||
                 all(i <- !nzchar(tags)))
        {
            visible <- TRUE
            tags <- vapply(args, .AS_SCALAR_STR, "", USE.NAMES = FALSE)
            value <- as.list(Sys.getenv(tags, unset = NA, names = TRUE))
        }
        else {
            args <- lapply(args, .AS_SCALAR_STR)
            if (any(i)) {
                visible <- TRUE
                tags[i] <- names(args)[i] <- as.character(args[i])
                # args[i] <- list(NULL)
            }
            tags[is.na(tags)] <- "NA"
            if (.Platform$OS.type == "windows")
                tags <- tolower(tags)
            i <- !i
            base_case <- function(args, i) {
                # print(args)
                # cat("\n\n\n\n\n")
                value <- as.list(Sys.getenv(names(args), unset = NA, names = TRUE))
                args <- args[i]
                na <- is.na(args)
                if (any(na)) {
                    Sys.unsetenv(names(args)[na])
                    args <- args[!na]
                }
                if (length(args))
                    do.call("Sys.setenv", args)
                value
            }
            recurse <- function(args, tags, i) {
                if (first_setter <- match(TRUE, i, 0L)) {
                    n <- length(args)
                    if (dup <- anyDuplicated(tags[first_setter:n])) {
                        dup <- dup - 1L + first_setter
                        indxs <- seq_len(dup - 1L)
                        value <- base_case(args[indxs], i[indxs])
                        indxs <- dup:n
                        c(value, recurse(args[indxs], tags[indxs], i[indxs]))
                    }
                    else base_case(args, i)
                }
                else as.list(Sys.getenv(names(args), unset = NA, names = TRUE))
            }
            value <- recurse(args, tags, i)
        }
        if (visible)
            value
        else invisible(value)
    }
}


.istrue <- function (x)
.External2(.C_istrue, x)


.isfalse <- function (x)
.External2(.C_isfalse, x)


.asLogical <- function (x)
.External2(.C_asLogical, x)


.asInteger <- function (x)
.External2(.C_asInteger, x)


.asIntegerGE0 <- function (x)
.External2(.C_asIntegerGE0, x)


.IS_SCALAR_STR <- function (x)
.External2(.C_IS_SCALAR_STR, x)


.AS_SCALAR_STR <- function (x)
{
    switch (typeof(x),
    logical = ,
    integer = ,
    double = ,
    complex = ,
    character = ,
    raw = {
        if (is.object(x))
            x <- unclass(x)
        if (length(x))
            as.character(x[[1L]])
        else NA_character_
    },
    char = as.character(list(x)),
    symbol = as.character(x),
    NA_character_)
}


.AS_SCALAR_STR <- function (x)
.External2(.C_AS_SCALAR_STR, x)


.scalar_streql <- function (e1, e2)
.External2(.C_scalar_streql, e1, e2)


.identical <- if (getRversion() >= "4.2.0") {


              function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE,
    ignore.bytecode = FALSE, ignore.environment = FALSE, ignore.srcref = FALSE,
    extptr.as.ref = TRUE)


} else if (getRversion() >= "3.4.0") {


              function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE,
    ignore.bytecode = FALSE, ignore.environment = FALSE, ignore.srcref = FALSE)


} else if (getRversion() >= "3.0.0") {


              function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE,
    ignore.bytecode = FALSE, ignore.environment = FALSE)


} else if (getRversion() >= "2.14.0") {


              function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE,
    ignore.bytecode = FALSE)


} else if (getRversion() >= "2.10.0") {


              function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE)


} else {


              function (x, y)
identical(x, y)


}


.source <- function (file, local = FALSE, echo = verbose, print.eval = echo,
    exprs, spaced = use_file, verbose = getOption("verbose"),
    prompt.echo = getOption("prompt"), max.deparse.length = 150,
    width.cutoff = 60L, deparseCtrl = "showAttributes", chdir = FALSE,
    encoding = getOption("encoding"), continue.echo = getOption("continue"),
    skip.echo = 0, keep.source = getOption("keep.source"))
{
    file <- set.sys.path(file)
    envir <- if (isTRUE(local))
        parent.frame()
    else if (isFALSE(local))
        .GlobalEnv
    else if (is.environment(local))
        local
    else stop("'local' must be TRUE, FALSE or an environment")
    if (!missing(echo)) {
        if (!is.logical(echo))
            stop("'echo' must be logical")
        if (!echo && verbose) {
            warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'")
            echo <- TRUE
        }
    }
    if (verbose) {
        cat("'envir' chosen:")
        print(envir)
    }
    if (use_file <- missing(exprs)) {
        ofile <- file
        from_file <- FALSE
        srcfile <- NULL
        if (is.character(file)) {
            if (!length(file) || file == "")
                stop("empty file/url name")
            have_encoding <- !missing(encoding) && !identical(encoding, "unknown")
            if (identical(encoding, "unknown")) {
                enc <- utils::localeToCharset()
                encoding <- enc[length(enc)]
            }
            else enc <- encoding
            if (length(enc) > 1L) {
                encoding <- NA
                owarn <- options(warn = 2)
                for (e in enc) {
                  if (is.na(e))
                    next
                  zz <- file(file, encoding = e)
                  res <- tryCatch(readLines(zz, warn = FALSE),
                    error = identity)
                  close(zz)
                  if (!inherits(res, "error")) {
                    encoding <- e
                    break
                  }
                }
                options(owarn)
            }
            if (is.na(encoding))
                stop("unable to find a plausible encoding")
            if (verbose)
                cat(gettextf("encoding = \"%s\" chosen", encoding),
                  "\n", sep = "")
            {
                filename <- file
                file <- file(filename, "r", encoding = encoding)
                on.exit(close(file))
                if (isTRUE(keep.source)) {
                  lines <- readLines(file, warn = FALSE)
                  on.exit()
                  close(file)
                  srcfile <- srcfilecopy(filename, lines, file.mtime(filename)[1],
                    isFile = .is_abs_path(filename))
                }
                else {
                  from_file <- TRUE
                  srcfile <- filename
                }
                loc <- utils::localeToCharset()[1L]
                encoding <- if (have_encoding)
                  switch(loc, `UTF-8` = "UTF-8", `ISO8859-1` = "latin1",
                    "unknown")
                else "unknown"
            }
        }
        else {
            lines <- readLines(file, warn = FALSE)
            srcfile <- if (isTRUE(keep.source))
                srcfilecopy(deparse1(substitute(file), ""), lines)
            else deparse1(substitute(file), "")
        }
        if (verbose) {
            cat(sprintf(" --> from_file='%s'\n lines:", from_file))
            utils::str(lines)
        }
        exprs <- if (!from_file) {
            if (length(lines) && is.character(lines))
                parse(file = stdin(), n = -1, text = lines, keep.source = FALSE, srcfile = srcfile, encoding = encoding)
            else expression()
        }
        else parse(file = file, n = -1, keep.source = FALSE, srcfile = srcfile, encoding = encoding)
        on.exit()
        if (from_file)
            close(file)
        if (verbose)
            cat("--> parsed", length(exprs), "expressions; now eval(.)ing them:\n")
        if (chdir) {
            if (is.character(ofile)) {
                if (grepl("^(ftp|ftps|http|https|file)://", ofile, useBytes = TRUE))
                  warning("'chdir = TRUE' makes no sense for a URL")
                else if ((path <- dirname(ofile)) != ".") {
                  owd <- getwd()
                  if (is.null(owd))
                    stop("cannot 'chdir' as current directory is unknown")
                  on.exit(setwd(owd), add = TRUE)
                  setwd(path)
                }
            }
            else {
                warning("'chdir = TRUE' makes no sense for a connection")
            }
        }
    }
    else {
        if (!missing(file))
            stop("specify either 'file' or 'exprs' but not both")
        if (!is.expression(exprs))
            exprs <- as.expression(exprs)
    }
    Ne <- length(exprs)
    if (echo) {
        sd <- "\""
        nos <- "[^\"]*"
        oddsd <- paste0("^", nos, sd, "(", nos, sd, nos, sd,
            ")*", nos, "$")
        trySrcLines <- function(srcfile, showfrom, showto) {
            tryCatch(suppressWarnings(getSrcLines(srcfile, showfrom,
                showto)), error = function(e) character())
        }
    }
    yy <- NULL
    lastshown <- 0
    srcrefs <- attr(exprs, "srcref", exact = TRUE)
    if (verbose && !is.null(srcrefs)) {
        cat("has srcrefs:\n")
        utils::str(srcrefs)
    }
    for (i in seq_len(Ne + echo)) {
        tail <- i > Ne
        if (!tail) {
            if (verbose)
                cat("\n>>>> eval(expression_nr.", i, ")\n\t\t =================\n")
            ei <- exprs[i]
        }
        if (echo) {
            nd <- 0
            srcref <- if (tail)
                attr(exprs, "wholeSrcref", exact = TRUE)
            else if (i <= length(srcrefs))
                srcrefs[[i]]
            if (!is.null(srcref)) {
                if (i == 1)
                  lastshown <- min(skip.echo, srcref[8L] - 1)
                if (lastshown < srcref[8L]) {
                  srcfile <- attr(srcref, "srcfile", exact = TRUE)
                  dep <- trySrcLines(srcfile, lastshown + 1,
                    srcref[8L])
                  if (length(dep)) {
                    leading <- if (tail)
                      length(dep)
                    else srcref[7L] - lastshown
                    lastshown <- srcref[8L]
                    while (length(dep) && grepl("^[[:blank:]]*$",
                      dep[1L])) {
                      dep <- dep[-1L]
                      leading <- leading - 1L
                    }
                    dep <- paste0(rep.int(c(prompt.echo, continue.echo),
                      pmax(0L, c(leading, length(dep) - leading))),
                      dep, collapse = "\n")
                    nd <- nchar(dep, "c")
                  }
                  else srcref <- NULL
                }
            }
            if (is.null(srcref)) {
                if (!tail) {
                  dep <- substr(paste(deparse(ei, width.cutoff = width.cutoff,
                    control = deparseCtrl), collapse = "\n"),
                    12L, 1000000L)
                  dep <- paste0(prompt.echo, gsub("\n", paste0("\n",
                    continue.echo), dep, fixed = TRUE))
                  nd <- nchar(dep, "c") - 1L
                }
            }
            if (nd) {
                do.trunc <- nd > max.deparse.length
                dep <- substr(dep, 1L, if (do.trunc)
                  max.deparse.length
                else nd)
                cat(if (spaced)
                  "\n", dep, if (do.trunc)
                  paste(if (grepl(sd, dep) && grepl(oddsd, dep))
                    " ...\" ..."
                  else " ....", "[TRUNCATED] "), "\n", sep = "")
            }
        }
        if (!tail) {
            yy <- withVisible(eval(ei, envir))
            i.symbol <- mode(ei[[1L]]) == "name"
            if (!i.symbol) {
                curr.fun <- ei[[1L]][[1L]]
                if (verbose) {
                  cat("curr.fun:")
                  utils::str(curr.fun)
                }
            }
            if (verbose >= 2) {
                cat(".... mode(ei[[1L]])=", mode(ei[[1L]]), "; paste(curr.fun)=")
                utils::str(paste(curr.fun))
            }
            if (print.eval && yy$visible)
                .PrintValueEnv(yy$value, envir)
            if (verbose)
                cat(" .. after ", sQuote(deparse(ei, control = unique(c(deparseCtrl,
                  "useSource")))), "\n", sep = "")
        }
    }
    invisible(yy)
}


source.exprs <- function (exprs, evaluated = FALSE, envir = parent.frame(), echo = TRUE, print.eval = TRUE)
{
    if (!evaluated) {
        exprs <- substitute(exprs)
        if (is.call(exprs)) {
            if (identical(exprs[[1]], .R_BraceSymbol))
                exprs <- as.list(exprs)[-1]
        }
    }
    if (!is.expression(exprs))
        exprs <- as.expression(exprs)
    Ne <- length(exprs)
    if (echo)
        trySrcLines <- function(srcfile, from, to) {
            tryCatch(suppressWarnings(getSrcLines(srcfile, from, to)),
                error = function(e) character())
        }
    yy <- NULL
    lastshown <- 0L
    srcrefs <- attr(exprs, "srcref")
    for (i in seq_len(Ne + echo)) {
        tail <- i > Ne
        if (!tail)
            ei <- exprs[i]
        if (echo) {
            srcref <- if (tail)
                attr(exprs, "wholeSrcref")
            else if (i <= length(srcrefs))
                srcrefs[[i]]
            if (!is.null(srcref)) {
                firstl <- srcref[7L]
                lastl <- srcref[8L]
                if (i == 1L)
                    lastshown <- firstl - 1L
                if (lastshown < lastl) {
                    srcfile <- attr(srcref, "srcfile")
                    dep <- trySrcLines(srcfile, lastshown + 1L, lastl)
                    if (length(dep)) {
                        leading <- if (tail)
                            length(dep)
                        else firstl - lastshown
                        lastshown <- lastl
                        dep <- paste0(rep.int(c(getOption("prompt"), getOption("continue")),
                            pmax(0L, c(         leading            , length(dep) - leading))),
                            dep, collapse = "\n")
                        cat(dep, "\n", sep = "")
                    }
                }
            }
        }
        if (!tail) {
            yy <- withVisible(eval(ei, envir))
            if (print.eval && yy$visible)
                .PrintValueEnv(yy$value, envir)
        }
    }
    invisible(yy)
}
