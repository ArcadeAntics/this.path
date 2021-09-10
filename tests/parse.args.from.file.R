if (!is.null(owd <- getwd())) tryCatch({
    FILE2 <- this.path::writeArgs(letters)
    FILE1 <- this.path::writeArgs(paste0("@", basename(FILE2)))
    parser <- this.path::ArgumentParser()
    parser$add.argument("strings", nargs = "*")
    pargs <- parser$parse.args(FILE1)
    stopifnot(
        identical(this.path::Args(pargs, "all"), letters)
    )
}, finally = unlink(substring(c(FILE1, FILE2), 2L)))
