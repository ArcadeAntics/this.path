local({
    FILE <- tempfile(fileext = ".R")
    on.exit(unlink(FILE), add = TRUE)
    this.path:::write.code(file = FILE, {
        invisible(loadNamespace("microbenchmark"))
        invisible(loadNamespace("this.path"))
        first.time <- microbenchmark::microbenchmark(
            first.time = this.path::this.path(verbose = FALSE),
            times = 1
        )
        subsequent <- microbenchmark::microbenchmark(
            subsequent = this.path::this.path(verbose = FALSE),
            times = 100
        )
        print(rbind(first.time, subsequent))
    })
    this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE))
})
