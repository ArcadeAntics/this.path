if (requireNamespace("microbenchmark", quietly = TRUE)) {
    FILE <- tempfile(fileext = ".R")
    tryCatch({


        writeLines('
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
        ', FILE)
        this.path::Rscript(c("--default-packages=NULL", "--vanilla"),
            file = FILE)


    }, finally = unlink(FILE))
}
