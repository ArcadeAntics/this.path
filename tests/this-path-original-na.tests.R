this.path:::write.code({
    this.path:::withAutoprint({
        this.path::this.path(original = TRUE)
        this.path::this.path(original = NA)
        writeLines(sQuote(this.path::this.path(verbose = TRUE)))
        this.path::this.path(original = TRUE)
        this.path::this.path(original = NA)
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L)
}, FILE <- tempfile(fileext = ".R"))


source(FILE, environment(), verbose = FALSE)
sys.source(FILE, environment())
if (.Platform$GUI == "RStudio")
    debugSource(FILE, local = environment())
if (requireNamespace("testthat", quietly = TRUE))
    testthat::source_file(FILE, environment(), chdir = FALSE, wrap = FALSE)
this.path:::.Rscript(c("--default-packages=this.path", "--vanilla", FILE))


unlink(FILE)
