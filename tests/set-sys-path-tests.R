local({


    FILE.R <- tempfile(fileext = ".R")
    on.exit(unlink(FILE.R))
    writeLines("
test.this.path <- function ()
{
    try(print(@R_PACKAGE_NAME@::sys.path(verbose = TRUE)))
    try(print(@R_PACKAGE_NAME@::env.path(verbose = TRUE)))
    try(print(@R_PACKAGE_NAME@::src.path(verbose = TRUE)))
    try(print(@R_PACKAGE_NAME@::this.path(verbose = TRUE)))
}


test.this.path()", FILE.R)


    sourcelike <- function(file) {
        ofile <- file
        file <- @R_PACKAGE_NAME@::set.sys.path(file, Function = "sourcelike")
        lines <- readLines(file, warn = FALSE)
        filename <- @R_PACKAGE_NAME@::sys.path(local = TRUE, for.msg = TRUE)
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
        @R_PACKAGE_NAME@::set.src.path(srcfile)
        envir <- new.env(hash = TRUE, parent = .BaseNamespaceEnv)
        envir$.packageName <- filename
        oopt <- options(topLevelEnvironment = envir)
        on.exit(options(oopt))
        @R_PACKAGE_NAME@::set.env.path(envir)
        exprs <- parse(text = lines, srcfile = srcfile)
        @R_PACKAGE_NAME@::source.exprs(exprs, evaluated = TRUE, envir)
        `.@R_PACKAGE_NAME@::document.context`
    }


    @R_PACKAGE_NAME@::source.exprs({


x <- sourcelike(@R_PACKAGE_NAME@::relpath(FILE.R))
print(x, quote = FALSE)


y <- local({
    FILE.zip <- tempfile(fileext = ".zip")
    on.exit(unlink(FILE.zip))
    local({
        owd <- getwd()
        on.exit(setwd(owd))
        setwd(@R_PACKAGE_NAME@::dirname2(FILE.R))
        status <- utils::zip(FILE.zip, @R_PACKAGE_NAME@::basename2(FILE.R))
        on.exit()
        setwd(owd)
        if (status == 0L) {
            local({
                conn <- unz(@R_PACKAGE_NAME@::relpath(FILE.zip), @R_PACKAGE_NAME@::basename2(FILE.R))
                on.exit(close(conn))
                sourcelike(conn)
            })
        }
    })
})
print(y, quote = FALSE)


    })


})
