main <- function ()
{
    # Sys.setenv(R_THIS_PATH_USE_R_NON_API = "TRUE"); warning("in ./tools/configure.R:\n comment out this line later", call. = FALSE, immediate. = TRUE)


    cat("\n",
        sprintf("getwd(): '%s'\n", encodeString(if (is.null(L <- getwd())) "NULL" else L, na.encode = FALSE)),
        sprintf(".libPaths(): %s\n", paste(sprintf("\n\t'%s'", encodeString(.libPaths(), na.encode = FALSE)), collapse = "")),
        sprintf("R_LIBRARY_DIR: '%s'\n", encodeString(Sys.getenv("R_LIBRARY_DIR"), na.encode = FALSE)),
        sprintf("R_PACKAGE_DIR: '%s'\n", encodeString(Sys.getenv("R_PACKAGE_DIR"), na.encode = FALSE)),
        sprintf("R_PACKAGE_NAME: '%s'\n", encodeString(Sys.getenv("R_PACKAGE_NAME"), na.encode = FALSE)),
        sep = ""
        # , file = "~/temp8.txt", append = TRUE
    )


    conn <- file("./src/use-r-non-api.h", "wb", encoding = "")
    on.exit(close(conn))
    writeLines(con = conn, {
        R_THIS_PATH_USE_R_NON_API <- as.logical(Sys.getenv("R_THIS_PATH_USE_R_NON_API"))
        if (is.na(R_THIS_PATH_USE_R_NON_API)) {
            libname <- Sys.getenv("R_LIBRARY_DIR")
            R_THIS_PATH_USE_R_NON_API <- !(
                libname == "" ||
                endsWith(libname, "/this.path.Rcheck") ||
                grepl("/CRAN", libname, fixed = TRUE, useBytes = TRUE)
            )
        }
        if (R_THIS_PATH_USE_R_NON_API) {
            c(
                "#ifndef R_THIS_PATH_USE_R_NON_API",
                "#define R_THIS_PATH_USE_R_NON_API",
                "#endif"
            )
        } else character(0)
    })
}


main()
