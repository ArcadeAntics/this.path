Check_This <- function (build = FALSE, multiarch = TRUE, no.multiarch = !multiarch,
    keep.source = NA, as.cran = FALSE, chdir = FALSE, file = here())
{
    # Check_This {this.path}                                     R Documentation
    #
    # Build, Install, and Check a Package Conveniently
    #
    #
    #
    # Description:
    #
    # Performs 'R CMD build', 'R CMD INSTALL', and 'R CMD check' on a source
    # package specified by 'file'.
    #
    #
    #
    # Usage:
    #
    # Check_This(
    #
    #     build = FALSE, multiarch = TRUE, no.multiarch = !multiarch,
    #     keep.source = NA,
    #
    #     as.cran = FALSE,
    #
    #     chdir = FALSE, file = here())
    #
    #
    #
    # Arguments:
    #
    # build, multiarch, no.multiarch, keep.source
    #
    #     further arguments passed to 'R CMD INSTALL'
    #
    # as.cran
    #
    #     further arguments passed to 'R CMD check'
    #
    # chdir
    #
    #     temporarily change the working directory to the directory containing
    #     `file`?
    #
    # file
    #
    #     character string; the directory of the package source, by default
    #     the executing script's directory


    if (!is.character(file) || length(file) != 1L)
        stop("invalid 'file'")
    else if (grepl("^(ftp|ftps|http|https)://", file))
        stop("cannot 'Check_This' on a URL")
    else if (chdir && (path <- dirname(file)) != ".") {
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
            if (keep.source)
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
