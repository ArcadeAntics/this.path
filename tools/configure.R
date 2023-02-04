main <- function ()
{
    # cat("\n",
    #     sprintf("getwd(): '%s'\n", encodeString(if (is.null(L <- getwd())) "NULL" else L, na.encode = FALSE)),
    #     sprintf("R_LIBRARY_DIR: '%s'\n", encodeString(Sys.getenv("R_LIBRARY_DIR"), na.encode = FALSE)),
    #     sprintf("R_PACKAGE_DIR: '%s'\n", encodeString(Sys.getenv("R_PACKAGE_DIR"), na.encode = FALSE)),
    #     sprintf("R_PACKAGE_NAME: '%s'\n", encodeString(Sys.getenv("R_PACKAGE_NAME"), na.encode = FALSE)),
    #     sep = ""
    #     , file = "~/temp8.txt", append = TRUE
    # )


    conn <- file("./src/use-r-non-api.h", "wb", encoding = "")
    on.exit(close(conn))
    writeLines(con = conn, {
        libname <- Sys.getenv("R_LIBRARY_DIR")
        if (libname == "" || endsWith(libname, "/this.path.Rcheck")) {
            character(0)
        } else {
            c(
                "#ifndef R_THIS_PATH_USE_R_NON_API",
                "#define R_THIS_PATH_USE_R_NON_API",
                "#endif"
            )
        }
    })
}


main()
