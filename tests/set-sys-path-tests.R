local({


    FILE.R <- tempfile(fileext = ".R")
    on.exit(unlink(FILE.R))
    writeLines("test.this.path <- function ()
{
    try(print({ this.path::sys.path(verbose = TRUE) }))
    try(print({ this.path::env.path(verbose = TRUE) }))
    try(print({ this.path::src.path(verbose = TRUE) }))
    try(print({ this.path::this.path(verbose = TRUE) }))
}


test.this.path()", FILE.R)


    sourcelike <- function(file) {
        ofile <- file
        file <- this.path::set.sys.path(file, Function = "sourcelike")
        lines <- readLines(file, warn = FALSE)
        filename <- this.path::sys.path(local = TRUE, for.msg = TRUE)
        isFile <- !is.na(filename)
        if (isFile) {
            timestamp <- file.info(filename)$mtime[1]
            if (is.na(timestamp))
                timestamp <- Sys.time()
        }
        else {
            filename <- if (is.character(ofile)) ofile else "<connection>"
            timestamp <- Sys.time()
        }
        srcfile <- srcfilecopy(filename, lines, timestamp, isFile)
        this.path::set.src.path(srcfile)
        envir <- new.env(hash = TRUE, parent = .BaseNamespaceEnv)
        envir$.packageName <- filename
        oopt <- options(topLevelEnvironment = envir)
        on.exit(options(oopt))
        this.path::set.env.path(envir)
        exprs <- parse(text = lines, srcfile = srcfile)
        this.path::source.exprs(exprs, evaluated = TRUE, envir)
        `.this.path::document.context`
    }


    this.path::source.exprs({


x <- sourcelike(this.path::relpath(FILE.R))
print(x, quote = FALSE)


y <- local({
    FILE.zip <- tempfile(fileext = ".zip")
    on.exit(unlink(FILE.zip))
    local({
        owd <- getwd()
        on.exit(setwd(owd))
        setwd(this.path::dirname2(FILE.R))
        if (!utils::zip(FILE.zip, this.path::basename2(FILE.R))) {
            local({
                conn <- unz(this.path::relpath(FILE.zip), this.path::basename2(FILE.R))
                on.exit(close(conn))
                sourcelike(conn)
            })
        }
    })
})
print(y, quote = FALSE)


    })


})
