switch2 <- function (EXPR, TRUE.expr = invisible(), FALSE.expr = invisible(),
    alt.expr = invisible())
{
    if (is.character(EXPR)) {
        switch(EXPR, T = , `TRUE` = , True = , true = {
            TRUE.expr
        }, F = , `FALSE` = , False = , false = {
            FALSE.expr
        }, if (!is.na(EXPR))
            alt.expr
        else if (EXPR) {
        })
    }
    else if (EXPR)
        TRUE.expr
    else FALSE.expr
}


# fun <- function (resave.data = FALSE, no.resave.data = FALSE)
# {
#     switch2(resave.data, TRUE.expr = {
#         "--resave-data"
#     }, FALSE.expr = {
#         if (no.resave.data)
#             "--no-resave-data"
#     }, alt.expr = {
#         match.arg(resave.data, c("no", "best", "gzip"))
#     })
# }


Check_This <- function (
    build.vignettes = TRUE, no.build.vignettes = !build.vignettes,
    manual = TRUE, no.manual = !manual,
    resave.data = FALSE, no.resave.data = FALSE,

    build = FALSE,
    multiarch = TRUE, no.multiarch = !multiarch,
    keep.source = NA,

    check = TRUE, as.cran = FALSE,

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
    #     resave.data = FALSE, no.resave.data = FALSE,
    #
    #     build = FALSE,
    #     multiarch = TRUE, no.multiarch = !multiarch,
    #     keep.source = NA,
    #
    #     check = TRUE, as.cran = FALSE,
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
    # check
    #     should 'R CMD check' be run?
    #
    # as.cran
    #
    #     further arguments passed to 'R CMD check' (if check is TRUE)
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
        switch2(resave.data, TRUE.expr = {
            "--resave-data"
        }, FALSE.expr = {
            if (no.resave.data)
                "--no-resave-data"
        }, alt.expr = {
            match.arg(resave.data, c("no", "best", "gzip"))
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


    if (check) {
        value <- Rcmd(command = "check", args = c(
            if (as.cran) "--as-cran",
            file2
        ), mustWork = TRUE)
        cat("\n")
    }
    if (.Platform$GUI == "RStudio")
        tools.rstudio(".rs.api.restartSession")(command = deparse1(
            call("library", as.symbol(basename(file))),
        collapse = "\n", width.cutoff = 80L))
    invisible(value)
}
