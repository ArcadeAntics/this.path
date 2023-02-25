main <- function ()
{
    # Sys.setenv(R_THIS_PATH_DEFINES = "TRUE"); warning("in ./tools/configure.R:\n comment out this line later", call. = FALSE, immediate. = TRUE)


    cat("\n",
        sprintf("getwd(): '%s'\n", encodeString(if (is.null(L <- getwd())) "NULL" else L, na.encode = FALSE)),
        sprintf(".libPaths(): %s\n", paste(sprintf("\n\t'%s'", encodeString(.libPaths(), na.encode = FALSE)), collapse = "")),
        sprintf("R_LIBRARY_DIR: '%s'\n", encodeString(Sys.getenv("R_LIBRARY_DIR"), na.encode = FALSE)),
        sprintf("R_PACKAGE_DIR: '%s'\n", encodeString(Sys.getenv("R_PACKAGE_DIR"), na.encode = FALSE)),
        sprintf("R_PACKAGE_NAME: '%s'\n", encodeString(Sys.getenv("R_PACKAGE_NAME"), na.encode = FALSE)),
        sep = ""
        # , file = "~/temp8.txt", append = TRUE
    )


    if (getRversion() < "3.2.0") {
        pkgname <- Sys.getenv("R_PACKAGE_NAME")
        desc <- read.dcf("./DESCRIPTION", keep.white = c("Description", "Authors@R", "Author", "Built", "Packaged"))
        if (nrow(desc) != 1L)
            stop("contains a blank line", call. = FALSE)
        desc <- desc[1L, ]
        if (!is.na(encoding <- desc["Encoding"])) {
            if (encoding %in% c("latin1", "UTF-8"))
                Encoding(desc) <- encoding
            else desc <- iconv(desc, encoding, "", sub = "byte")
        }
        if (pkgname != desc["Package"])
            stop(gettextf("DESCRIPTION file is for package '%s', not '%s'",
                desc["Package"], pkgname))
        fun <- function(x) {
            .fun <- function(x, include.hash = TRUE) {
                paste0("rawToChar(as.raw(c(", paste0(as.integer(charToRaw(x)), "L", collapse = ", "), if (include.hash) ", as.null(\"#1\")", ")))")
            }
            value <- .fun(gsub("([%{}\\])", "\\\\\\1", x, useBytes = TRUE))
            if (Encoding(x) != "unknown")
                value <- paste0("`Encoding<-`(", value, ", ", .fun(Encoding(x), include.hash = FALSE), ")")
            value
        }
        commonmacros <- c(
            readLines("./man/macros/commonmacros.Rd", encoding = "UTF-8"),
            paste0("\\newcommand{\\packageAuthor",     "}{\\Sexpr[results=rd,stage=build]{", fun(desc["Author"])    , "}}"),
            paste0("\\newcommand{\\packageMaintainer", "}{\\Sexpr[results=rd,stage=build]{", fun(desc["Maintainer"]), "}}")
        )
        Rdfiles <- list.files("./man", "\\.Rd$", all.files = TRUE, recursive = FALSE, full.names = TRUE, ignore.case = TRUE)
        for (Rdfile in Rdfiles) {
            x <- readLines(Rdfile, encoding = "UTF-8")
            n <- grep("^\\\\description\\{", x) - 1L
            if (length(n) > 1L)
                stop(gettextf("in %s:\n multiple lines that start with \\description{", Rdfile))
            if (length(n) < 1L)
                stop(gettextf("in %s:\n no lines that start with \\description{", Rdfile))
            x <- if (n) {
                c(x[seq_len(n)], commonmacros, x[-seq_len(n)])
            } else c(commonmacros, x)
            writeLines(x, Rdfile, useBytes = TRUE)
        }
        unlink("./man/macros", recursive = TRUE, force = TRUE)
    }


    conn <- file("./src/defines.h", "wb", encoding = "")
    on.exit(close(conn))
    writeLines(con = conn, {
        R_THIS_PATH_DEFINES <- as.logical(Sys.getenv("R_THIS_PATH_DEFINES"))
        if (is.na(R_THIS_PATH_DEFINES)) {
            libname <- Sys.getenv("R_LIBRARY_DIR")
            R_THIS_PATH_DEFINES <- !(
                is.na(libname)                                            ||
                !nzchar(libname)                                          ||
                grepl("/this\\.path\\.Rcheck$", libname, useBytes = TRUE) ||
                grepl("/CRAN", libname, fixed = TRUE, useBytes = TRUE)
            )
        }
        if (R_THIS_PATH_DEFINES) {
            c(
                "#ifndef R_THIS_PATH_DEFINES",
                "#define R_THIS_PATH_DEFINES",
                "#endif"
            )
        } else character(0)
    })
}


main()
