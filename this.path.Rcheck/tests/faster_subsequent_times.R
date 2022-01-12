if (requireNamespace("microbenchmark", quietly = TRUE)) {
    FILE <- tempfile(fileext = ".R")
    tryCatch({


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
        this.path:::.Rscript(c(
            "--default-packages=NULL", "--vanilla", FILE
        ))


    }, finally = unlink(FILE))
}
