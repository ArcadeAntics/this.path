local({
    FILE.R <- tempfile(fileext = ".R")
    on.exit(unlink(FILE.R), add = TRUE)
    @R_PACKAGE_NAME@:::.writeCode(file = FILE.R, {
        if (requireNamespace("microbenchmark", quietly = TRUE)) {
            print(@R_PACKAGE_NAME@:::.faster_subsequent_times_test())
        } else cat("\n'package:microbenchmark' is not available :(\n")
    })
    cat("\n")
    @R_PACKAGE_NAME@:::.Rscript(c(
        "--default-packages=NULL", "--no-save", FILE.R
    ))


    cat("\n> source(FILE.R, chdir = FALSE)\n")
    source(FILE.R, chdir = FALSE)


    cat("\n> source(FILE.R, chdir = TRUE)\n")
    source(FILE.R, chdir = TRUE)
})
