main <- function ()
{
    devel <- TRUE


    pkgname <- Sys.getenv("R_PACKAGE_NAME")


    regexQuote <- function(x) gsub("([.\\\\|()[{^$*+?])", "\\\\\\1", x)
    RdQuote <- function(x) gsub("([%{}\\])", "\\\\\\1", x, useBytes = TRUE)


    building <- if (is.null(wd <- getwd())) {
        FALSE
    } else {
        if (.Platform$OS.type == "windows")
            wd <- chartr("\\", "/", wd)
        pattern <- paste0("/Rtmp[\001-\056\060-\177]{6}/Rbuild[0123456789abcdef]+/",
                          regexQuote(pkgname), "$")
        grepl(pattern, wd, useBytes = TRUE)
    }


    ## on the maintainer's computer, prevent the source from being destroyed
    if (!building && file.exists("./tools/maintainers-copy"))
        stop("must 'R CMD build' before 'R CMD INSTALL' since the files are destructively modified")


    write.dcf(data.frame(
        Timestamp = Sys.time(),
        `Working Directory` = encodeString(getwd()),
        building,
        check.names = FALSE
    ), "~/test69", append = TRUE, indent = 8, width = 72)


    if (file.exists("./tools/for-r-mac-builder"))
        options(R_THIS_PATH_DEFINES = TRUE)


    # if (devel)
    #     cat("\n",
    #         sprintf("getwd(): '%s'\n", encodeString(if (is.null(L <- getwd())) "NULL" else L, na.encode = FALSE)),
    #         sprintf(".libPaths(): %s\n", paste(sprintf("\n\t'%s'", encodeString(.libPaths(), na.encode = FALSE)), collapse = "")),
    #         sprintf("R_LIBRARY_DIR: '%s'\n", encodeString(Sys.getenv("R_LIBRARY_DIR"), na.encode = FALSE)),
    #         sprintf("R_PACKAGE_DIR: '%s'\n", encodeString(Sys.getenv("R_PACKAGE_DIR"), na.encode = FALSE)),
    #         sprintf("R_PACKAGE_NAME: '%s'\n", encodeString(Sys.getenv("R_PACKAGE_NAME"), na.encode = FALSE)),
    #         sep = ""
    #     )


    ## we need the contents of the DESCRIPTION file
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
    if (pkgname != desc["Package"])
        stop(gettextf("DESCRIPTION file is for package '%s', not '%s'",
            desc["Package"], pkgname))


    readLines2 <- function(description, ...) {
        conn <- file(description, "rb", encoding = "")
        on.exit(close(conn))
        readLines(con = conn, ...)
    }


    ## "w" will use the native line end, "wb" will use Unix (LF)
    writeLines2 <- function(text, description, mode = "w", useBytes = TRUE) {
        conn <- file(description, mode, encoding = "")
        on.exit(close(conn))
        writeLines(text, conn, useBytes = useBytes)
    }


    replace.devel.with.current.version <- if (devel) {
        ## replace 'devel' in files with the current package version
        function(file.in, old, new, file = sub("\\.in|in\\.", "", file.in)) {
            if (file.exists(file.in)) {
                x <- readLines2(file.in)
                if (i <- match(old, x, 0L)) {
                    x[[i]] <- new
                    writeLines2(x, file)
                    if (!file.remove(file.in))
                        stop(sprintf("unable to remove file '%s'", file.in))
                } else if (!file.rename(file.in, file))
                    stop(sprintf("unable to rename file '%s' to '%s'", file.in, file))
            }
        }
    } else {
        ## just rename the files
        function(file.in, old, new, file = sub("\\.in|in\\.", "", file.in)) {
            if (file.exists(file.in)) {
                if (!file.rename(file.in, file))
                    stop(sprintf("unable to rename file '%s' to '%s'", file.in, file))
            }
        }
    }


    replace.devel.with.current.version(
        "./NEWS.in",
        sprintf("CHANGES IN %s devel:", desc["Package"]),
        sprintf("CHANGES IN %s %s (%s):", desc["Package"], desc["Version"], desc["Date"])
    )
    replace.devel.with.current.version(
        "./inst/NEWS.in.in.Rd",
        "\\section{CHANGES IN VERSION devel}{",
        sprintf("\\section{CHANGES IN VERSION %s (%s)}{", desc["Version"], desc["Date"])
    )
    replace.devel.with.current.version(
        sprintf("./man/%s-defunct.in.in.Rd", desc["Package"]),
        "# Defunct in devel",
        sprintf("# Defunct in %s", desc["Version"])
    )


    if (!building) {
        ## we need to add the common macros to the files which do not
        ## have access. for R < 3.2.0, this is ALL of the Rd files.
        ## otherwise, only the news files will need them
        Rdfiles.in <- list.files("./inst", "^NEWS(\\.[[:digit:]]+)?\\.in\\.Rd$", full.names = TRUE)
        files.in <- list.files("./man", "\\.in\\.Rd$", all.files = TRUE, full.names = TRUE, ignore.case = TRUE)
        if (getRversion() < "3.2.0") {
            Rdfiles.in <- c(Rdfiles.in, files.in)
        } else {
            ## rename the files, do not add the common macros
            files <- sub("\\.in(\\.Rd$)", "\\1", files.in, ignore.case = TRUE)
            if (any(i <- !file.rename(files.in, files)))
                stop(sprintf(ngettext(sum(i), "unable to rename file %s",
                                              "unable to rename files %s"),
                     paste(sQuote(files.in[i]), collapse = ", ")))
        }
        if (length(Rdfiles.in)) {
            macros <- c(
                unlist(lapply(list.files("./man/macros", "\\.Rd$", all.files = TRUE, full.names = TRUE, ignore.case = TRUE), readLines2)),
                if (getRversion() < "3.2.0") {
                    ## \packageAuthor{} and \packageMaintainer{} are not defined for
                    ## R < 3.2.0, we will define them here ourselves
                    RdQuote.and.conv2ASCII <- function(x) {
                        conv2ASCII <- function(x, include.hash = TRUE) {
                            paste0("rawToChar(as.raw(c(", paste0(as.integer(charToRaw(x)), "L", collapse = ", "), if (include.hash) ", as.null(\"#1\")", ")))")
                        }
                        value <- conv2ASCII(RdQuote(x))
                        if (Encoding(x) != "unknown")
                            value <- paste0("`Encoding<-`(", value, ", ", conv2ASCII(Encoding(x), include.hash = FALSE), ")")
                        value
                    }
                    c(
                        paste0("\\newcommand{\\packageAuthor",     "}{\\Sexpr[results=rd,stage=build]{", RdQuote.and.conv2ASCII(desc["Author"])    , "}}"),
                        paste0("\\newcommand{\\packageMaintainer", "}{\\Sexpr[results=rd,stage=build]{", RdQuote.and.conv2ASCII(desc["Maintainer"]), "}}"),
                        paste0("\\newcommand{\\CRANpkg"          , "}{\\href{https://CRAN.R-project.org/package=#1}{\\pkg{#1}}}")
                    )
                }
            )
            for (Rdfile.in in Rdfiles.in) {
                x <- readLines2(Rdfile.in)
                ## add the common macros directly before the title
                n <- grep("^\\\\title\\{", x) - 1L
                if (length(n) > 1L)
                    stop(gettextf("in '%s':\n multiple lines that start with \\title{", Rdfile.in))
                if (length(n) < 1L)
                    stop(gettextf("in '%s':\n no lines that start with \\title{", Rdfile.in))
                x <- if (n) {
                    c(x[seq_len(n)], macros, x[-seq_len(n)])
                } else c(macros, x)
                writeLines2(x, sub("\\.in(\\.Rd$)", "\\1", Rdfile.in, ignore.case = TRUE))
                if (!file.remove(Rdfile.in))
                    stop(sprintf("unable to remove file '%s'", Rdfile.in))
            }
        }
        ## remove the macros folder so R CMD check will not complain about it
        if (getRversion() < "3.2.0")
            unlink("./man/macros", recursive = TRUE, force = TRUE)


        ## (possibly) enable development features
        R_THIS_PATH_DEFINES <- as.logical(getOption("R_THIS_PATH_DEFINES", NA))
        if (is.na(R_THIS_PATH_DEFINES))
            R_THIS_PATH_DEFINES <- as.logical(Sys.getenv("R_THIS_PATH_DEFINES"))
        if (is.na(R_THIS_PATH_DEFINES)) {
            if (devel) {
                libname <- Sys.getenv("R_LIBRARY_DIR")
                R_THIS_PATH_DEFINES <- !(
                    is.na(libname)                                            ||
                    !nzchar(libname)                                          ||
                    grepl("/this\\.path\\.Rcheck$", libname, useBytes = TRUE)
                )
            } else R_THIS_PATH_DEFINES <- FALSE
        }
        if (R_THIS_PATH_DEFINES)
            writeLines2(c(
                "#ifndef R_THIS_PATH_DEFINES",
                "#define R_THIS_PATH_DEFINES",
                "#endif"
            ),
            ## *.c and *.h must use Unix (LF) under Unix-alikes
            "./src/defines.h", "wb")
    }
}


main()
