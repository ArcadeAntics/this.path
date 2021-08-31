Check_This <- function (with.keep.source = TRUE, multiarch = FALSE, no.multiarch = !multiarch,
    build = TRUE, as.cran = TRUE, chdir = FALSE, file = here())
{
    if (chdir && (path <- dirname(file)) != ".") {
        # file <- normalizePath(file)
        file <- basename(file)


        owd <- getwd()
        if (is.null(owd))
            stop("cannot 'chdir' as current directory is unknown")
        on.exit(setwd(owd))
        setwd(path)
    }


    Rcmd(command = "build", args = file, mustWork = TRUE)
    cat("\n")


    version <- read.dcf(file.path(file, "DESCRIPTION"), fields = "Version")
    file2 <- paste0(file, "_", version, ".tar.gz")


    Rcmd(command = "INSTALL", args = c(
        if (build) "--build",
        if (no.multiarch) "--no-multiarch",
        if (with.keep.source) "--with-keep.source",
        file2
    ), mustWork = TRUE)
    cat("\n")


    Rcmd(command = "check", args = c(
        if (as.cran) "--as-cran",
        paste0(file, "_", packageVersion(basename(file)), ".tar.gz")
    ), mustWork = TRUE)
    cat("\n")
}
