main <- function ()
{
    devel <- TRUE


    FILE <- "./tools/building-initial-tarball"
    if (first <- file.exists(FILE)) {
        if (!file.remove(FILE))
            stop(sprintf("unable to remove file '%s'", FILE))
    }


    FILE <- "./tools/for-r-mac-builder"
    if (file.exists(FILE))
        Sys.setenv(R_THIS_PATH_DEFINES = "TRUE")


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
    pkgname <- Sys.getenv("R_PACKAGE_NAME")
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


    if (first && devel) {
        ## replace 'devel' in files with the current package version
        replace.devel.with.current.version <- function(file, old, new) {
            x <- readLines2(file)
            if (i <- match(old, x, 0L)) {
                x[[i]] <- new
                writeLines2(x, file)
            }
        }


        replace.devel.with.current.version(
            "./NEWS",
            sprintf("CHANGES IN %s devel:", desc["Package"]),
            sprintf("CHANGES IN %s %s (%s):", desc["Package"], desc["Version"], desc["Date"])
        )
        replace.devel.with.current.version(
            "./inst/NEWS.Rd",
            "\\section{CHANGES IN VERSION devel}{",
            sprintf("\\section{CHANGES IN VERSION %s (%s)}{", desc["Version"], desc["Date"])
        )
        replace.devel.with.current.version(
            sprintf("./man/%s-defunct.Rd", desc["Package"]),
            "# Defunct in devel",
            sprintf("# Defunct in %s", desc["Version"])
        )
    }


    ## we need to add the common macros to the files which do not
    ## have access. for R < 3.2.0, this is ALL of the Rd files.
    ## otherwise, only the news files will need them
    Rdfiles <- if (first) list.files("./inst", "^NEWS(\\.[[:digit:]]+)?\\.Rd$", full.names = TRUE)
    if (!first && getRversion() < "3.2.0")
        Rdfiles <- c(Rdfiles, list.files("./man", "\\.Rd$", all.files = TRUE, full.names = TRUE, ignore.case = TRUE))
    if (length(Rdfiles)) {
        macros <- c(
            unlist(lapply(list.files("./man/macros", "\\.Rd$", all.files = TRUE, full.names = TRUE, ignore.case = TRUE), readLines2)),
            if (getRversion() < "3.2.0") {
                ## \packageAuthor{} and \packageMaintainer{} are not defined for
                ## R < 3.2.0, we will define them here ourselves
                fun <- function(x) {
                    .fun <- function(x, include.hash = TRUE) {
                        paste0("rawToChar(as.raw(c(", paste0(as.integer(charToRaw(x)), "L", collapse = ", "), if (include.hash) ", as.null(\"#1\")", ")))")
                    }
                    value <- .fun(gsub("([%{}\\])", "\\\\\\1", x, useBytes = TRUE))
                    if (Encoding(x) != "unknown")
                        value <- paste0("`Encoding<-`(", value, ", ", .fun(Encoding(x), include.hash = FALSE), ")")
                    value
                }
                c(
                    paste0("\\newcommand{\\packageAuthor",     "}{\\Sexpr[results=rd,stage=build]{", fun(desc["Author"])    , "}}"),
                    paste0("\\newcommand{\\packageMaintainer", "}{\\Sexpr[results=rd,stage=build]{", fun(desc["Maintainer"]), "}}")
                )
            }
        )
        for (Rdfile in Rdfiles) {
            x <- readLines2(Rdfile)
            ## add the common macros directly before the title
            n <- grep("^\\\\title\\{", x) - 1L
            if (length(n) > 1L)
                stop(gettextf("in '%s':\n multiple lines that start with \\title{", Rdfile))
            if (length(n) < 1L)
                stop(gettextf("in '%s':\n no lines that start with \\title{", Rdfile))
            x <- if (n) {
                c(x[seq_len(n)], macros, x[-seq_len(n)])
            } else c(macros, x)
            writeLines2(x, Rdfile)
        }
    }
    if (!first && getRversion() < "3.2.0")
        unlink("./man/macros", recursive = TRUE, force = TRUE)


    if (!first) {
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
