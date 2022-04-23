build_this <- function (chdir = FALSE, file = here(), which = "tar")
{
    # determine which types of archive we want to make
    choices <- c("tar", "zip")
    nms <- c("tarball", "zip archive")
    which <- unique(match.arg(which, choices, several.ok = TRUE))
    nms <- nms[match(which, choices)]


    # check that 'file' is valid, and chdir if required
    if (!is.character(file) || length(file) != 1L) {
        stop("invalid 'file'")
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
    DESCRIPTION <- file.path(file, "DESCRIPTION")
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
    i <- basename(files) %in% c(

        # directories from source control systems
        "CSV", ".svn", ".arch-ids", ".bzr", ".git", ".hg",

        # directories from eclipse
        ".metadata",

        "check", "chm"
    )


    # more directories to exclude, ending with Old or old
    i <- i | grepl("(Old|old)$", files)


    # and, of course, are directories
    i <- i & dir.exists(file.path(file, files))


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
        basename(files)
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
    ignore.file <- file.path(file, ".buildignore")
    if (file.exists(ignore.file)) {
        for (exclude.pattern in readLines(ignore.file, warn = FALSE, encoding = "UTF-8")) {
            if (any(i <- grepl(exclude.pattern, files, ignore.case = TRUE, perl = TRUE))) {
                exclude <- c(exclude, files[i])
                files <- files[!i]
            }
        }
    }


    # for directories in 'exclude', also exclude the files within said directories
    exclude.dirs <- exclude[dir.exists(file.path(file, exclude))]
    for (exclude.prefix in paste0(exclude.dirs, "/", recycle0 = TRUE)) {
        if (any(i <- startsWith(files, exclude.prefix))) {
            exclude <- c(exclude, files[i])
            files <- files[!i]
        }
    }


    # create a new directory to hold the temporary files and archives
    dir.create(my.tmpdir <- tempfile("dir"))
    on.exit(unlink(my.tmpdir, recursive = TRUE, force = TRUE), add = TRUE)


    # create another directory to hold the temporary files
    dir.create(pkgdir <- file.path(my.tmpdir, pkgname))


    # within said directory, make the appropriate sub-directories
    isdir <- dir.exists(file.path(file, files))
    dirs <- files[isdir]
    for (path in file.path(pkgdir, dirs))
        dir.create(path, showWarnings = TRUE, recursive = TRUE)


    # fill the directory and sub-directories with their files,
    # while maintaining file modify time
    if (any(i <- !file.copy(
        file.path(file  , files[!isdir]),
        file.path(pkgdir, files[!isdir]),
        copy.date = TRUE
    )))
        stop(ngettext(sum(i), "unable to copy file: ", "unable to copy files:\n  "),
             paste(encodeString(files[!isdir][i], quote = "\""), collapse = "\n  "))


    # set the modify time of the sub-directories to their original values
    Sys.setFileTime(
                  file.path(pkgdir, dirs),
        file.info(file.path(file  , dirs), extra_cols = FALSE)$mtime
    )


    for (apk in which) {
        switch(apk, tar = {
            build.name <- paste0(pkgname, "_", version, ".tar.gz")
            build.path <- file.path(my.tmpdir, build.name)
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
                file.path(file, build.name),
                overwrite = TRUE
            )) {
                stop("unable to move:\n  ",
                     encodeString(build.path, quote = "\""),
                     "\nto:\n  ",
                     encodeString(file.path(file, build.name), quote = "\""))
            }
        }, zip = {
            build.name <- paste0(pkgname, "_", version, ".zip")
            build.path <- file.path(my.tmpdir, build.name)
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
                file.path(file, build.name),
                overwrite = TRUE
            )) {
                stop("unable to move:\n  ",
                     encodeString(build.path, quote = "\""),
                     "\nto:\n  ",
                     encodeString(file.path(file, build.name), quote = "\""))
            }
        }, stop("invalid 'which'; should not happen, please report!"))
    }
}


do_with_wd <- function (expr, wd)
{
    owd <- getwd()
    on.exit(setwd(owd))
    setwd(wd)
    expr
}





# tar <- Sys.which("tar")
# if (is.na(tar) || tar == "") {
#     cat("tar unavailable; cannot create tarball\n")
#     quit(save = "no", status = 1, runLast = FALSE)
# }
#
#
# owd <- getwd()
# tryCatch(finally = setwd(owd), {
#
#
#     setwd(this.path::here())
#
#
#     DESCRIPTION.file <- "DESCRIPTION"
#     cat("* checking for file '",
#         this.path::as.rel.path(this.path::here(DESCRIPTION.file), owd),
#         "' ... ", sep = "")
#     if (!file.exists(DESCRIPTION.file)) {
#         cat("NO\n\n")
#         quit(save = "no", status = 1, runLast = FALSE)
#     }
#     cat("OK\n")
#
#
#     DESCRIPTION <- read.dcf(DESCRIPTION.file, fields = c(
#         "Package",
#         "Version"
#     ))
#     package <- DESCRIPTION[[1L, "Package"]]
#     version <- DESCRIPTION[[1L, "Version"]]
#     valid_package_name <- "([[:alpha:]][[:alnum:]_]*[[:alnum:]])"
#     valid_package_version <- "(([[:digit:]]+[.-]){1,}[[:digit:]]+)"
#
#
#     if (!is.null(problems <- c(
#         if (is.na(package))
#             "invalid 'Package' field\n\n",
#         if (is.na(version))
#             "invalid 'Version' field\n\n"
#     ))) {
#         cat(" ERROR\n", problems, sep = "")
#         quit(save = "no", status = 1, runLast = FALSE)
#     }
#
#
#     cat("* checking ", DESCRIPTION.file, " meta-information ... ", sep = "")
#     if (!is.null(problems <- c(
#         if (!grepl(paste0("^", valid_package_name   , "$"), package))
#             "Malformed package name\n\n",
#         if (!grepl(paste0("^", valid_package_version, "$"), version))
#             "Malformed package version.\n\n"
#     ))) {
#         cat("ERROR\n", problems, sep = "")
#         quit(save = "no", status = 1, runLast = FALSE)
#     }
#     cat("OK\n")
#
#
#     build.name <- paste0(
#         package,
#         "_",
#         version,
#         ".tar.gz"
#     )
#
#
#     # we will exclude the build itself
#     exclude <- build.name
#
#
#     files <- list.files(all.files = TRUE, recursive = TRUE, include.dirs = TRUE)
#
#
#     # the directories to always exclude
#     if (any(i <-
#             (
#                 (basename(files) %in% c(
#
#                     # directories from source control systems
#                     "CSV", ".svn", ".arch-ids", ".bzr", ".git", ".hg",
#
#                     # directories from eclipse
#                     ".metadata",
#
#                     "check", "chm"
#
#                 )) |
#
#                 # directories ending with Old or old
#                 grepl("(Old|old)$", files)
#             ) & dir.exists(files))) {
#         exclude <- c(exclude, files[i])
#         files <- files[!i]
#     }
#
#
#     # the files to always exclude
#     if (any(i <-
#             grepl(paste0("(", c(
#                 "^GNUmakefile$",
#                 "^Read-and-delete-me$",
#                 "^\\.#",
#                 "^#", "#$",
#                 "~$", "\\.bak$", "\\.swp$"
#             ), ")", collapse = "|"), basename(files)))) {
#         exclude <- c(exclude, files[i])
#         files <- files[!i]
#     }
#
#
#     for (exclude.pattern in paste0("^", package, "_", valid_package_version, c("\\.tar\\.gz", "\\.zip"), "$")) {
#         if (any(i <- grepl(exclude.pattern, files))) {
#             exclude <- c(exclude, files[i])
#             files <- files[!i]
#         }
#     }
#
#
#     ignore.file <- ".tarignore"
#     if (file.exists(ignore.file)) {
#         for (exclude.pattern in readLines(ignore.file, warn = FALSE, encoding = "UTF-8")) {
#             if (any(i <- grepl(exclude.pattern, files, ignore.case = TRUE, perl = TRUE))) {
#                 exclude <- c(exclude, files[i])
#                 files <- files[!i]
#             }
#         }
#     }
#
#
#     exclude <- paste("--exclude", shQuote(unique(exclude)), recycle0 = TRUE)
#
#
#     # transform <- paste0("--transform s,^,", package, "/")
#     # args <- c("tar", exclude, transform, "-czvf", build.name, "*")
#
#
#     # args <- c("tar", exclude, "-czf", build.name, "-C", "..", paste0(shQuote(basename(getwd())), "/*"))
#
#
#     args <- c("tar", "-czf", build.name, exclude, "-C", "..", shQuote(basename(getwd())))
#     # args <- c("tar", "-czf", FILE <- tempfile(fileext = ".tar.gz"), exclude, "-C", "..", shQuote(basename(getwd())))
#
#
#     command <- paste(args, collapse = " ")
#
#
#     cat("* building '", build.name, "'\n", sep = "")
#     res <- system(command)
#     if (res == -1L)
#         stop("'", command, "' could not be run")
#     else if (res)
#         stop("'", command, "' execution failed with error code ", res)
#
#
# })
