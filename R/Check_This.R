Check_This <- function (
    build.vignettes = TRUE, no.build.vignettes = !build.vignettes,
    manual = TRUE, no.manual = !manual,
    resave.data = !no.resave.data, no.resave.data = FALSE,

    build = FALSE,
    multiarch = TRUE, no.multiarch = !multiarch,
    keep.source = NA,

    as.cran = FALSE,

    chdir = FALSE, file = here())
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
    #     build.vignettes = TRUE, no.build.vignettes = !build.vignettes,
    #     manual = TRUE, no.manual = !manual,
    #     resave.data = !no.resave.data, no.resave.data = FALSE,
    #
    #     build = FALSE,
    #     multiarch = TRUE, no.multiarch = !multiarch,
    #     keep.source = NA,
    #
    #     as.cran = FALSE,
    #
    #     chdir = FALSE, file = here()
    #
    # )
    #
    #
    #
    # Arguments:
    #
    #
    # build.vignettes, no.build.vignettes, manual, no.manual, resave.data,
    # no.resave.data
    #
    #     further arguments passed to 'R CMD build'
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


    Rcmd(command = "build", args = c(
        if (no.build.vignettes) "--no-build-vignettes",
        if (no.manual) "--no-manual",
        if (is.null(resave.data)) {}
        else tryCatch({
            if (resave.data)
                "--resave-data"
            else "--no-resave-data"
        }, error = function(c) {
            match.arg(as.character(resave.data), c("no", "best", "gzip"))
        }),
        file
    ), mustWork = TRUE)
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


    value <- Rcmd(command = "check", args = c(
        if (as.cran) "--as-cran",
        file2
    ), mustWork = TRUE)
    cat("\n")
    invisible(value)
}
