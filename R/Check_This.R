Check_This <- function (with.keep.source = NA, multiarch = FALSE, no.multiarch = !multiarch,
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
    if (is.na(version) ||
        !grepl(paste0("^", .standard_regexps()$valid_package_version, "$"),
            version))
        stop("invalid package DESCRIPTION file")
    file2 <- paste0(file, "_", version, ".tar.gz")


    Rcmd(command = "INSTALL", args = c(
        if (build) "--build",
        if (no.multiarch) "--no-multiarch",
        tryCatch({
            if (with.keep.source)
                "--with-keep.source"
            else "--without-keep.source"
        }, error = function(c) character()),
        file2
    ), mustWork = TRUE)
    cat("\n")


    Rcmd(command = "check", args = c(
        if (as.cran) "--as-cran",
        file2
    ), mustWork = TRUE)
    cat("\n")
}
