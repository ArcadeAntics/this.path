.Rscript <- function (options = NULL, trailing = character(), dry.run = FALSE, print.command = TRUE, ...)
{
    command <- path.join(R.home("bin"), if (os.windows)
        "Rscript.exe"
    else "Rscript")
    args <- c(command, options)
    args <- c(shQuote(args), trailing)
    command <- paste(args, collapse = " ")
    if (dry.run)
        return(command)
    if (print.command)
        cat(command, "\n", sep = "")
    invisible(system(command = command, ...))
}


# .Rterm <- function (options = NULL, trailing = character(), dry.run = FALSE, print.command = TRUE, ...)
# {
#     command <- path.join(R.home("bin"), if (os.windows)
#         "Rterm.exe"
#     else "R")
#     args <- c(command, options)
#     args <- c(shQuote(args), trailing)
#     command <- paste(args, collapse = " ")
#     if (dry.run)
#         return(command)
#     if (print.command)
#         cat(command, "\n", sep = "")
#     invisible(system(command = command, ...))
# }


build_this <- function(chdir = FALSE, file = here(), which = "tar") NULL
body(build_this) <- bquote({
    # build_this {this.path}                                     R Documentation
    #
    # Building Packages
    #
    #
    #
    # Description:
    #
    # This provides a more general method of making packages, not specifically
    # R packages, for distribution and version control.
    #
    #
    #
    # Usage:
    #
    # build_this(chdir = FALSE, file = here(), which = "tar")
    #
    #
    #
    # Arguments:
    #
    # chdir
    #
    #     TRUE or FALSE; change directory to directory of package source before
    #     building tar/zip archives? This will not affect the build process,
    #     only the location in which the archive is made.
    #
    # file
    #
    #     name of a directory which is to be packaged
    #
    # which
    #
    #     which type of archive do you want to make, "tar" or "zip"?
    #     Can be both i.e. 'which = c("tar", "zip")'
    #
    #
    #
    # Details:
    #
    # build_this builds package archives similar to 'R CMD build', and takes
    # the name of the package and its version from the 'DESCRIPTION' file.
    # If you are unfamiliar with the 'DESCRIPTION' file, here is a outline:
    #
    # Package: example
    # Version: 0.1.0
    # License: What license is it under?
    # Title: What the Package Does (Title Case)
    # Description: More about what it does (maybe more than one line)
    #     Use four spaces when indenting paragraphs within the Description.
    # Author: Who wrote it
    # Maintainer: The package maintainer <yourself@somewhere.net>
    #
    # The package name should start with a letter, end with a letter or number,
    # and contain any combination of letters, numbers, underscores, periods,
    # and hyphens in the middle.
    #
    # The package version should be two or more numbers separated by hyphens
    # and/or periods like '12.34.56-78'.



    # determine which types of archive we want to make
    choices <- c("tar", "zip")
    nms <- c("tarball", "zip archive")
    which <- unique(match.arg(which, choices, several.ok = TRUE))
    nms <- nms[match(which, choices)]


    # check that 'file' is valid, and 'chdir' if required
    if (!is.character(file) || length(file) != 1L) {
        stop(gettextf("'%s' must be a character string", "file", domain = "R"), domain = NA)
    } else if (grepl("^(ftp|ftps|http|https)://", file)) {
        stop("cannot 'build_this' on a URL")
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


    # look for the appropriate applications to create archives
    # if all are unavailable, raise an error
    # if some are unavailable, raise a warning and continue with the rest
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


    # check for file "DESCRIPTION"
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


    # check that the package name and version exist in "DESCRIPTION"
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


    # check that the package name and version are valid names and versions
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


    # files to exclude, and all files
    exclude <- NULL
    files <- list.files(path = file, all.files = TRUE, recursive = TRUE, include.dirs = TRUE)


    # directories to always exclude
    i <- basename2(files) %in% c(

        # directories from source control systems
        "CSV", ".svn", ".arch-ids", ".bzr", ".git", ".hg",

        # directories from eclipse
        ".metadata",

        "check", "chm"
    )


    # more directories to exclude, ending with Old or old
    i <- i | grepl("(Old|old)$", files)


    # and, of course, are directories
    i <- i & dir.exists(path.join(file, files))


    if (any(i)) {
        exclude <- c(exclude, files[i])
        files <- files[!i]
    }


    # files to always exclude
    i <- grepl(
        pattern = paste0(
            "(",
            c(
                "^GNUmakefile$",
                "^Read-and-delete-me$",

                # starts with .#
                "^\\.#",

                # starts or ends with #
                "^#", "#$",

                # ends with ~ or .bak or .swp
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


    # do not put previous archives in the new archives
    prev_build_patterns <- paste0(
        "^",
        gsub(".", "\\.", pkgname, fixed = TRUE),
        "_",
        valid_package_version,
        c("\\.tar\\.gz", "\\.zip"),
        "$"
    )


    for (exclude.pattern in prev_build_patterns) {
        if (any(i <- grepl(exclude.pattern, files))) {
            exclude <- c(exclude, files[i])
            files <- files[!i]
        }
    }


    # look for a ".buildignore" file, a list of Perl patterns (case insensitive)
    # specifying files to ignore when archiving
    ignore.file <- path.join(file, ".buildignore")
    if (file.exists(ignore.file)) {
        for (exclude.pattern in readLines(ignore.file, warn = FALSE, encoding = "UTF-8")) {
            if (any(i <- grepl(exclude.pattern, files, ignore.case = TRUE, perl = TRUE))) {
                exclude <- c(exclude, files[i])
                files <- files[!i]
            }
        }
    }


    # for directories in 'exclude', also exclude the files within said directories
    exclude.dirs <- exclude[dir.exists(path.join(file, exclude))]
    for (exclude.prefix in paste0(exclude.dirs, "/", recycle0 = TRUE)) {
        if (any(i <- startsWith(files, exclude.prefix))) {
            exclude <- c(exclude, files[i])
            files <- files[!i]
        }
    }


    # create a new directory to hold the temporary files and archives
    dir.create(my.tmpdir <- tempfile("dir"))
    on.exit(
        if (getRversion() >= "4.0.0")
            (unlink)(my.tmpdir, recursive = TRUE, force = TRUE, expand = FALSE)
        else unlink(my.tmpdir, recursive = TRUE, force = TRUE),
        add = TRUE
    )


    # create another directory to hold the temporary files
    dir.create(pkgdir <- path.join(my.tmpdir, pkgname))


    # within said directory, make the appropriate sub-directories
    isdir <- dir.exists(path.join(file, files))
    dirs <- files[isdir]
    for (path in path.join(pkgdir, dirs))
        dir.create(path, showWarnings = TRUE, recursive = TRUE)


    # fill the directory and sub-directories with their files,
    # while maintaining file modify time
    if (any(i <- !file.copy(
        ..(
            if (getRversion() < "3.1.0")
                expression(
                    from <- path.join(file  , files[!isdir]),
                    to <- path.join(pkgdir, files[!isdir])
                )
            else
                expression(
                    path.join(file  , files[!isdir]),
                    path.join(pkgdir, files[!isdir]),
                    copy.date = TRUE
                )
        )
    )))
        stop(ngettext(sum(i), "unable to copy file: ", "unable to copy files:\n  "),
             paste(encodeString(files[!isdir][i], quote = "\""), collapse = "\n  "))
    else ..(
        if (getRversion() < "3.1.0")
            expression(Sys.setFileTime(to, file.info(from)$mtime))
        else expression()
    )


    # set the modify time of the sub-directories to their original values
    Sys.setFileTime(
                   path.join(pkgdir, dirs),
        file.mtime(path.join(file  , dirs))
    )


    for (apk in which) {
        switch(apk, tar = {
            build.name <- paste0(pkgname, "_", version, ".tar.gz")
            build.path <- path.join(my.tmpdir, build.name)
            args <- c("tar", "-czf", shQuote(build.path), "-C", shQuote(my.tmpdir), shQuote(pkgname))
            command <- paste(args, collapse = " ")
            cat("* building '", build.name, "'\n", sep = "")
            res <- system(command, ignore.stdout = TRUE)
            if (res == -1L) {
                stop("'", command, "' could not be run")
            } else if (res) {
                stop("'", command, "' execution failed with error code ", res)
            } else if (i <- !file.copy(
                build.path,
                path.join(file, build.name),
                overwrite = TRUE
            )) {
                stop("unable to move:\n  ",
                     encodeString(build.path, quote = "\""),
                     "\nto:\n  ",
                     encodeString(path.join(file, build.name), quote = "\""))
            }
        }, zip = {
            build.name <- paste0(pkgname, "_", version, ".zip")
            build.path <- path.join(my.tmpdir, build.name)
            args <- c("zip", "-r", shQuote(build.path), shQuote(pkgname))
            command <- paste(args, collapse = " ")
            cat("* building '", build.name, "'\n", sep = "")
            res <- do_with_wd(system(command, ignore.stdout = TRUE), my.tmpdir)
            if (res == -1L) {
                stop("'", command, "' could not be run")
            } else if (res) {
                stop("'", command, "' execution failed with error code ", res)
            } else if (i <- !file.copy(
                build.path,
                path.join(file, build.name),
                overwrite = TRUE
            )) {
                stop("unable to move:\n  ",
                     encodeString(build.path, quote = "\""),
                     "\nto:\n  ",
                     encodeString(path.join(file, build.name), quote = "\""))
            }
        }, stop("invalid 'which'; should not happen, please report!"))
    }
}, splice = TRUE)


build.this <- build_this


do_with_wd <- function (expr, wd)
{
    owd <- getwd()
    on.exit(setwd(owd))
    setwd(wd)
    expr
}


R_BraceSymbol <- as.symbol("{")


maybeQuote <- function (expr, evaluated, simplify.brace = TRUE)
{
    # possibly quote an expression
    #
    # when 'evaluated' is missing, 'expr' is quoted only if it is an
    # (unevaluated) call to `{`
    #
    # when 'evaluated' is FALSE, 'expr' is always quoted
    #
    # when 'evaluated' is TRUE, 'expr' is evaluated as normal
    #
    # this is intended to be called inside another function, like match.fun()


    if (missing(expr))
        expr


    if (missing(evaluated)) {
        if (is.call(e <- eval(substitute(substitute(expr)), parent.frame())) &&
            identical(e[[1L]], R_BraceSymbol))
        {
            expr <- e
        }
    }
    else if (!evaluated)
        expr <- eval(substitute(substitute(expr)), parent.frame())


    # force the evaluated of 'expr'
    else expr


    if (is.call(expr)) {
        # always turn a call into an expression so that it
        # cannot be misinterpreted later
        if (simplify.brace) {
            if (identical(expr[[1L]], R_BraceSymbol))
                as.expression(as.list(expr[-1L]))
            else as.expression(list(expr))
        }
        else as.expression(list(expr))
    }
    else expr
}


# this function does not exist in R < 4.0.0
deparse1 <- function (expr, collapse = " ", width.cutoff = 500L, ...)
paste(deparse(expr, width.cutoff, ...), collapse = collapse)
environment(deparse1) <- getNamespace("base")


code2character <- function (x, width.cutoff = 60L,
    deparseCtrl = c("keepInteger", "showAttributes", "useSource", "keepNA", "digits17"))
{
    # if 'x' is an (unevaluated) call to `{`
    # deparse each sub-expression
    #
    # if 'x' is an expression() vector
    # make a list of deparsed expressions
    #
    # if 'x' is a language object
    # deparse the expression
    #
    # if 'x' is a character vector
    # leave as is


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


write.code <- function (x, file = stdout(), evaluated, simplify.brace = TRUE,
    width.cutoff = 60L, deparseCtrl = c("keepInteger", "showAttributes", "useSource", "keepNA", "digits17"))
{
    x <- maybeQuote(x, evaluated, simplify.brace)
    x <- code2character(x, width.cutoff, deparseCtrl)
    if (is.null(file))
        x
    else {
        writeLines(x, file, useBytes = !utf8)
        invisible(x)
    }
}


if (getRversion() < "3.2.0") {


    tmp <- formals(code2character)[["deparseCtrl"]]
    formals(code2character)[["deparseCtrl"]] <- tmp[!vapply(tmp, identical, "digits17", FUN.VALUE = NA)]


    tmp <- formals(write.code)[["deparseCtrl"]]
    formals(write.code)[["deparseCtrl"]] <- tmp[!vapply(tmp, identical, "digits17", FUN.VALUE = NA)]


}


readFiles <- function (files)
vapply(files, function(file) paste0(readLines(file), "\n", collapse = ""), "")
