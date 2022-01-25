main <- function ()
{
    if (!requireNamespace("microbenchmark", quietly = TRUE))
        return()
    FILE <- tempfile(fileext = ".R")
    on.exit(unlink(FILE))
    this.path:::write.code(file = FILE, {
        invisible(loadNamespace("this.path"))
        rbind(
            microbenchmark::microbenchmark(
                `1st time` = this.path::this.path(),
                times = 1
            ),
            microbenchmark::microbenchmark(
                subsequent = this.path::this.path(),
                times = 100
            )
        )
    })
    this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE))
}


main()
