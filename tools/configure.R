main <- function ()
{
    if (getRversion() < "3.2.0") {
        dir.exists <- function(paths) {
            isdir <- file.info(paths)$isdir
            !is.na(isdir) & isdir
        }
        file.size <- function(...) file.info(...)$size
    }
    oopt <- options(encoding = "native.enc")
    on.exit(options(oopt))


    info_path <- "./tools/info.dcf"
    info <- read.dcf(info_path)
    if (nrow(info) != 1L)
        stop("contains a blank line", call. = FALSE)
    ## re-read the file, keeping white space this time
    info <- read.dcf(info_path, keep.white = colnames(info))
    if (nrow(info) != 1L)
        stop("contains a blank line", call. = FALSE)
    info <- info[1L, ]
    write_info <- function() write.dcf(t(info), info_path, keep.white = names(info))
    if (file.exists("./tools/for-r-mac-builder")) {
        devel <- TRUE
    } else {
        devel <- as.logical(getOption("R_THIS_PATH_DEVEL", NA))
        if (is.na(devel))
            devel <- as.logical(Sys.getenv("R_THIS_PATH_DEVEL"))
        if (is.na(devel))
            devel <- info[["devel"]]
        devel <- if (devel) TRUE else FALSE
    }


    regexQuote <- function(x) gsub("([.\\\\|()[{^$*+?])", "\\\\\\1", x)
    RdQuote <- function(x) gsub("([%{}\\])", "\\\\\\1", x, useBytes = TRUE)


    ## we need the contents of the DESCRIPTION file
    desc <- local({
        ## see tools:::.read_description
        desc <- read.dcf("./DESCRIPTION", keep.white = c("Description", "Authors@R", "Author", "Built", "Packaged"))
        if (nrow(desc) != 1L)
            stop("contains a blank line", call. = FALSE)
        desc <- desc[1L, ]
        if (getRversion() >= "3.4.0") {
            if (!is.na(encoding <- desc["Encoding"])) {
                if (encoding == "UTF-8") {
                    Encoding(desc) <- "UTF-8"
                    ind <- validUTF8(desc)
                    if (!all(ind)) {
                        pos <- which(!ind)
                        desc[pos] <- iconv(desc[pos], "UTF-8", "UTF-8", sub = "byte")
                    }
                } else if (encoding == "latin1") {
                    Encoding(desc) <- "latin1"
                } else desc <- iconv(desc, encoding, "", sub = "byte")
            }
        } else {
            if (!is.na(encoding <- desc["Encoding"])) {
                if (encoding %in% c("latin1", "UTF-8")) {
                    Encoding(desc) <- encoding
                } else desc <- iconv(desc, encoding, "", sub = "byte")
            }
        }
        desc
    })
    local({
        pkgname <- Sys.getenv("R_PACKAGE_NAME")
        if (pkgname != desc["Package"])
            stop(gettextf("DESCRIPTION file is for package '%s', not '%s'",
                desc["Package"], pkgname))
    })


    building <- local({
        if (is.null(wd <- getwd())) {
            FALSE
        } else {
            if (.Platform$OS.type == "windows")
                wd <- chartr("\\", "/", wd)
            ## we need to know if the package is being installed by 'R CMD build'
            ## (i.e. "installing the package to process help pages")
            ##
            ## we know a package is being installed by 'R CMD build'
            ## if the working directory matches the following pattern
            pattern <- sprintf("/Rtmp[\001-\056\060-\177]{6}/Rbuild[0123456789abcdef]+/%s$", regexQuote(desc["Package"]))
            ##                   ^^^^^^^^^^^^^^^^^^^^^^^                                 6 ASCII characters excluding \0 and /
            ##                                                 ^^^^^^^^^^^^^^^^^^^       at least 1 hex digit
            ##                                                                     ^^^   ends with the package name
            grepl(pattern, wd, useBytes = TRUE)
        }
    })


    ## on the maintainer's computer, prevent the source from being destroyed
    ##
    ## file './tools/maintainers-copy' should not exist on other computers
    ## because it is excluded by the build process and does not exist on GitHub
    if (!building && file.exists("./tools/maintainers-copy"))
        stop("must 'R CMD build' before 'R CMD INSTALL' since the files are destructively modified")


    if (!building) {


        make_backports.Rd <- function() {
            make_backports <- function(...) {
                x <- list(...)
                if (!length(x) || length(x) %% 3L)
                    stop("invalid arguments")
                version <- R_system_version(as.character(x[c(TRUE , FALSE, FALSE)]))
                alias <- I(lapply(x[c(FALSE, TRUE , FALSE)], as.character))
                usage <- I(lapply(x[c(FALSE, FALSE, TRUE )], as.character))
                data.frame(version, alias, usage)
            }
            backports <- make_backports(
                "3.0.0",
                c(".mapply", "list.files", "parse"),
                c(
                    ".mapply(FUN, dots, MoreArgs)",
                    "list.files(path = \".\", pattern = NULL, all.files = FALSE,",
                    "           full.names = FALSE, recursive = FALSE,",
                    "           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)",
                    "parse(file = \"\", n = NULL, text = NULL, prompt = \"?\",",
                    "      keep.source = getOption(\"keep.source\"),",
                    "      srcfile = NULL, encoding = \"unknown\")"
                ),
                "3.1.0",
                c("anyNA", "anyNA.data.frame", "anyNA.numeric_version", "anyNA.POSIXlt"),
                c(
                    "anyNA(x, recursive = FALSE)",
                    "\\method{anyNA}{data.frame}(x, recursive = FALSE)",
                    "\\method{anyNA}{numeric_version}(x, recursive = FALSE)",
                    "\\method{anyNA}{POSIXlt}(x, recursive = FALSE)"
                ),
                "3.2.0",
                c("isNamespaceLoaded", "dir.exists", "file.copy", "lengths", "file.mtime", "file.size", "file.info"),
                c(
                    "isNamespaceLoaded(name)",
                    "dir.exists(paths)",
                    "file.copy(from, to, overwrite = recursive, recursive = FALSE,",
                    "          copy.mode = TRUE, copy.date = FALSE)",
                    "lengths(x, use.names = TRUE)",
                    "file.mtime(...)",
                    "file.size(...)",
                    "file.info(..., extra_cols = TRUE)"
                ),
                "3.3.0",
                c("strrep", "startsWith", "endsWith"),
                c(
                    "strrep(x, times)",
                    "startsWith(x, prefix)",
                    "endsWith(x, suffix)"
                ),
                "3.4.0",
                c("print.connection", "withAutoprint"),
                c(
                    "\\method{print}{connection}(x, ...)",
                    "withAutoprint(exprs, evaluated = FALSE, local = parent.frame(),",
                    "              print. = TRUE, echo = TRUE, max.deparse.length = Inf,",
                    "              width.cutoff = max(20, getOption(\"width\")),",
                    "              deparseCtrl = c(\"keepInteger\", \"showAttributes\", \"keepNA\"),",
                    "              skip.echo = 0, spaced = FALSE, ...)"
                ),
                "3.5.0",
                c("...length", "isTRUE", "isFALSE"),
                c(
                    "...length()",
                    "isTRUE(x)",
                    "isFALSE(x)"
                ),
                "3.6.0",
                c("errorCondition", "str2expression", "str2lang", "Sys.setFileTime"),
                c(
                    "errorCondition(message, ..., class = NULL, call = NULL)",
                    "str2expression(text)",
                    "str2lang(s)",
                    "Sys.setFileTime(path, time)"
                ),
                "4.0.0",
                c("deparse1", "unlink"),
                c(
                    "deparse1(expr, collapse = \" \", width.cutoff = 500L, ...)",
                    "unlink(x, recursive = FALSE, force = FALSE, expand = TRUE)"
                ),
                "4.1.0",
                c("...elt", "bquote"),
                c(
                    "...elt(n)",
                    "bquote(expr, where = parent.frame(), splice = FALSE)"
                ),
                "4.2.0",
                c("gettext", "gettextf"),
                c(
                    "gettext(..., domain = NULL, trim = TRUE)",
                    "gettextf(fmt, ..., domain = NULL, trim = TRUE)"
                )
            )
            backports <- backports[getRversion() < backports$version, , drop = FALSE]
            if (nrow(backports))
                writeLines(c(
                    "\\name{backports}",
            sprintf("\\alias{%s}", unlist(backports$alias)),
                    "\\title{Backports For Older R Versions}",
                    "\\description{",
            sprintf("  Reimplementations of functions introduced since \\R-\\code{%s}.", backports$version[1L]),
                    "}",
                    "\\usage{",
                    paste(
                        vapply(seq_len(nrow(backports)), function(ind) {
                            value <- c(
            sprintf("# Introduced in R %s", backports[[ind, "version"]]),
                    backports[[ind, "usage"]]
                            )
                            paste(value, collapse = "\n")
                        }, ""),
                        collapse = "\n\n"
                    ),
                    "}",
                    "\\keyword{internal}"
                ), con = "./man/backports.Rd")
        }
        make_backports.Rd()


    }


    files <- list.files(all.files = TRUE, full.names = TRUE)
    files <- setdiff(files, c("./.", "./..", "./.git", "./.Rproj.user", "./build", "./tools"))
    files <- grep("\\.Rcheck$", files, value = TRUE, invert = TRUE)
    files <- grep("(\\.tar\\.gz|\\.zip|\\.tgz)$", files, value = TRUE, invert = TRUE)
    files <- unlist(lapply(files, function(file) {
        if (dir.exists(file))
            list.files(file, all.files = TRUE, full.names = TRUE, recursive = TRUE)
        else file
    }))
    files <- grep("\\.dat$", files, value = TRUE, invert = TRUE)


    gsub_from_DESCRIPTION <- function(x) {
        # x <- c(a = "testing", b = "@R_PACKAGE_NAME@ @R_PACKAGE_VERSION@ (@R_PACKAGE_DATE@)"); stop("comment out this later")
        patterns <- c(
            "@R_PACKAGE_NAME@", "@R_PACKAGE_VERSION@", "@R_PACKAGE_DATE@",
            "@R_PACKAGE_AUTHOR@", "@R_PACKAGE_MAINTAINER@",
            "@R_PACKAGE_LIB@"
        )
        replacements <- c(
            desc[c("Package", "Version", "Date")],
            RdQuote(desc[c("Author", "Maintainer")]),
            Library = gsub(".", "_", desc[["Package"]], fixed = TRUE)
        )
        Encoding(replacements) <- "bytes"
        pattern <- paste(regexQuote(patterns), collapse = "|")
        m <- gregexpr(pattern, x)
        regmatches(x, m) <- lapply(regmatches(x, m), function(xx) {
            Encoding(xx) <- "bytes"
            i <- match(xx, patterns, 0L)
            xx[i > 0L] <- replacements[i]
            xx
        })
        x
    }


    if (info[["renamed_files"]]) {
    } else {
        new <- gsub_from_DESCRIPTION(files)
        if (any(i <- files != new)) {
            old <- files[i]
            new <- new[i]
            files[i] <- new
            if (any(i <- !file.rename(old, new)))
                stop(sprintf(ngettext(sum(i), "unable to rename file %s",
                                              "unable to rename files %s"),
                     paste(sQuote(old), collapse = ", ")))
        }
        info[["renamed_files"]] <- "TRUE"
        write_info()
    }


    ## if we have already substituted the file contents, do not bother to read
    ## all files, only a small subset
    if (info[["substituted_file_contents"]]) {
        pattern <- paste(c(
            "^\\./NEWS$",
            "^\\./inst/NEWS\\.in\\.Rd$",
            if (getRversion() < "3.2.0")
                "^\\./man/[^/]+\\.Rd$",
            "^\\./R/0\\.R$",
            "^\\./src/devel\\.h$"
        ), collapse = "|")
        files <- grep(pattern, files, value = TRUE)
    }


    text <- vapply(files, function(file) {
        readChar(file, file.size(file), useBytes = TRUE)
    }, "")
    Encoding(text) <- "bytes"
    o <- text
    if (info[["substituted_file_contents"]]) {
    } else {
        text <- gsub_from_DESCRIPTION(text)
        info[["substituted_file_contents"]] <- "TRUE"
    }
    if (info[["dedented_NEWS_subsections"]]) {
    } else {
        local({
            if (!devel)
                if (i <- match("./NEWS", names(text), 0L)) {
                    text[i] <<- gsub("(?<=^|\r\n|[\r\n])  (?! )", "", text[i], perl = TRUE)
                    info[["dedented_NEWS_subsections"]] <<- "TRUE"
                }
        })
    }


    if (!building) {
        ## we need to add the common macros to the files which do not
        ## have access. for R < 3.2.0, this is ALL of the Rd files.
        ## otherwise, only the news files will need them
        macros <- unlist(lapply(
            list.files("./man/macros", "\\.Rd$", all.files = TRUE, full.names = TRUE, ignore.case = TRUE),
            function(file) {
                conn <- file(file, "rb")
                on.exit(close(conn))
                readLines(conn)
            }
        ))
        if (length(macros)) {
            Encoding(macros) <- "bytes"
            pattern <- "^\\./inst/NEWS\\.in\\.Rd$"
            if (getRversion() < "3.2.0")
                pattern <- paste(pattern, "^\\./man/[^/]+\\.Rd$", sep = "|")
            keep <- grep(pattern, names(text))
            Rdtext <- text[keep]
            # Rdtext <- readChar("./inst/NEWS.in.Rd", file.size("./inst/NEWS.in.Rd"), useBytes = TRUE); stop("comment out this later")
            m <- gregexpr("(?<=^|\r\n|[\r\n])\\\\title\\{", Rdtext, perl = TRUE)
            if (any(i <- vapply(m, length, 0L, USE.NAMES = FALSE) > 1L))
                stop(gettextf("in %s:\n multiple lines that start with \\title{",
                     paste(sQuote(names(Rdtext)[i]), collapse = ", ")))
            if (any(i <- as.integer(m) == -1L))
                stop(gettextf("in %s:\n no lines that start with \\title{",
                     paste(sQuote(names(Rdtext)[i]), collapse = ", ")))
            regmatches(Rdtext, m) <- mapply(
                regmatches(Rdtext, m),
                ifelse(grepl("\r\n", Rdtext), "\r\n", "\n"),
                FUN = function(xx, line_end) {
                    paste0(
                        paste0(macros, line_end, collapse = ""),
                        xx
                    )
                },
                SIMPLIFY = FALSE, USE.NAMES = FALSE
            )
            text[keep] <- Rdtext
        }
        ## remove the macros folder so 'R CMD check' will not complain about it
        if (getRversion() < "3.2.0")
            unlink("./man/macros", recursive = TRUE, force = TRUE)
    }


    if (!building) {
        ## (possibly) enable development features
        if (devel) {
            text["./R/0.R"] <- paste(
                "options(R_THIS_PATH_DEVEL = TRUE)",
                if (grepl("\r\n", text[["./R/0.R"]])) "\r\n" else "\n",
                text[["./R/0.R"]],
                sep = ""
            )
            text["./src/devel.h"] <- paste(
                "#ifndef R_THIS_PATH_DEVEL",
                "#define R_THIS_PATH_DEVEL",
                "#endif",
                text[["./src/devel.h"]],
                ## *.c and *.h must use Unix (LF) under Unix-alikes
                sep = "\n"
            )
        }
    }


    if (any(i <- o != text)) {
        mapply(function(description, text) {
            ## "wb" does not convert line endings
            conn <- file(description, "wb")
            on.exit(close(conn))
            writeLines(text, conn, sep = "", useBytes = TRUE)
        }, names(text)[i], text[i], SIMPLIFY = FALSE, USE.NAMES = FALSE)
    }
    write_info()


    if (info[["replaced_CRLF_with_LF"]]) {
    } else {
        ## change the line endings to \n
        local({
            file <- "./tools/this_path_reg_ptrs.c"
            text <- readChar(file, file.size(file), useBytes = TRUE)
            Encoding(text) <- "bytes"
            text <- gsub("\r\n", "\n", text, fixed = TRUE, useBytes = TRUE)
            conn <- file(file, "wb")
            on.exit(close(conn))
            writeLines(text, conn, sep = "", useBytes = TRUE)
        })
        info[["replaced_CRLF_with_LF"]] <- "TRUE"
        write_info()
    }


    if (!building) {
        if (!devel) {
            make_this_path_reg_ptrs_lib <- function() {
                run_shlib <- function(arch) {
                    chname <- "this_path_reg_ptrs"
                    chname1 <- paste0(chname, .Platform$dynlib.ext)
                    DLLpath <- DLLpath_top <- "./dlls"
                    if (nzchar(arch))
                        DLLpath <- paste0(DLLpath, arch)
                    dir.create(DLLpath, showWarnings = FALSE, recursive = TRUE)
                    file <- paste0(DLLpath, "/", chname1)
                    if (.Platform$OS.type == "windows") {
                        args <- paste0(R.home(), "/bin", arch, "/Rcmd.exe")
                    } else {
                        args <- paste0(R.home(), "/bin/R")
                        if (nzchar(arch))
                            args <- c(args, paste0("--arch=", substr(arch, 2, 1000)))
                        args <- c(args, "CMD")
                    }
                    args <- c(args, "SHLIB", paste0("--output=", file),
                        "--clean", "--preclean", paste0("./tools/", chname, ".c"))
                    command <- paste(shQuote(args), collapse = " ")
                    # cat("$ ", command, "\n", sep = "")
                    system(command, ignore.stdout = TRUE, ignore.stderr = TRUE)
                    if (!length(dir(DLLpath))) {
                        unlink(DLLpath, recursive = TRUE, force = TRUE)
                        if (DLLpath_top != DLLpath && !length(dir(DLLpath_top)))
                            unlink(DLLpath_top, recursive = TRUE, force = TRUE)
                    }
                }
                if (.Platform$OS.type == "windows") {
                    f <- dir(paste0(R.home(), "/bin"))
                    archs <- f[f %in% c("i386", "x64")]
                    for (arch in archs) {
                        ra <- paste0("/", arch)
                        run_shlib(ra)
                    }
                } else {
                    # wd2 <- setwd(paste0(R.home(), "/bin/exec"))
                    # archs <- Sys.glob("*")
                    # setwd(wd2)
                    archs <- dir(paste0(R.home(), "/bin/exec"))
                    for (arch in archs) {
                        if (arch == "R") {
                            run_shlib("")
                        } else {
                            ra <- paste0("/", arch)
                            run_shlib(ra)
                        }
                    }
                }
                if (file.exists("./dlls")) {
                    file.copy("./dlls", Sys.getenv("R_PACKAGE_DIR"), recursive = TRUE)
                    unlink("./dlls", recursive = TRUE, force = TRUE)
                }
            }
            make_this_path_reg_ptrs_lib()
        }
    }


    if (!building) {
        old <- "./inst/NEWS.in.Rd"
        new <- "./inst/NEWS.Rd"
        if (!file.rename(old, new))
            stop(sprintf("unable to rename file '%s'", old))
    }


    invisible()
}


main()
