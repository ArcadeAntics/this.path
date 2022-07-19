local({
    owd <- getwd()
    if (is.null(owd)) {
        message("cannot 'chdir' as current directory is unknown")
        return(invisible())
    }
    else on.exit(setwd(owd), add = TRUE)
    .unload <- !isNamespaceLoaded("testthat")
    if (.unload)
        on.exit(unloadNamespace("testthat"), add = TRUE)


    ## test for 3 specific cases of sourcing
    ## * sourcing a file by specifying its basename
    ## * sourcing a file by specifying its absolute path
    ## * sourcing a file by specifying one of its relative paths that is not its basename
    base.path  <- "test.R"
    full.path  <- this.path::here(base.path)
    short.path <- file.path(basename(dirname(full.path)), base.path)


    on.exit(unlink(full.path), add = TRUE)
    this.path:::write.code({
        cat("Working directory:\n", if (!is.null(wd <- getwd()))
            sQuote(wd)
            else "NULL", "\n\n", sep = "")
        cat("Executing script's path:\n")
        cat(sQuote(this.path::this.path(verbose = TRUE)), "\n\n", sep = "")
        cat("\n\n\n\n")
    }, file = full.path)


    ## for 'base::source' and 'debugSource' specifically,
    ## try sourcing with a file URL
    url.base.path  <- paste0("file://", base.path )
    url.full.path  <- paste0("file://", full.path )
    url.short.path <- paste0("file://", short.path)


    ## the directories that lead to the 3 paths from above
    base.path.dir  <- dirname(full.path)
    short.path.dir <- dirname(base.path.dir)
    full.path.dir  <- tempdir(check = TRUE)


    ## try using source in all possible manners
    setwd(base.path.dir)
    source(base.path                , local = TRUE, chdir = FALSE)            # from a  basename                     without changing directory
    source(base.path                , local = TRUE, chdir = TRUE )            # from a  basename                     with    changing directory (shouldn't do anything)
    source(url.base.path            , local = TRUE)                           # from a  URL basename
    source(x <- file(base.path)     , local = TRUE)               ; close(x)  # from a  basename connection
    source(x <- file(url.base.path) , local = TRUE)               ; close(x)  # from a  URL basename connection
    setwd(short.path.dir)
    source(short.path               , local = TRUE, chdir = FALSE)            # from a  relative path                without changing directory
    source(short.path               , local = TRUE, chdir = TRUE )            # from a  relative path                with    changing directory
    source(url.short.path           , local = TRUE)                           # from a  URL relative path
    source(x <- file(short.path)    , local = TRUE)               ; close(x)  # from a  relative path connection
    source(x <- file(url.short.path), local = TRUE)               ; close(x)  # from a  URL relative path connection
    setwd(full.path.dir)
    source(full.path                , local = TRUE, chdir = FALSE)            # from an absolute path                without changing directory
    source(full.path                , local = TRUE, chdir = TRUE )            # from an absolute path                with    changing directory
    source(url.full.path            , local = TRUE)                           # from a  URL absolute path
    source(x <- file(full.path)     , local = TRUE)               ; close(x)  # from an absolute path connection
    source(x <- file(url.full.path) , local = TRUE)               ; close(x)  # from a  URL absolute path connection


    ## 'base::sys.source' cannot handle file URLs or connections
    setwd(base.path.dir)
    sys.source(base.path , envir = environment(), chdir = FALSE)
    sys.source(base.path , envir = environment(), chdir = TRUE )
    setwd(short.path.dir)
    sys.source(short.path, envir = environment(), chdir = FALSE)
    sys.source(short.path, envir = environment(), chdir = TRUE )
    setwd(full.path.dir)
    sys.source(full.path , envir = environment(), chdir = FALSE)
    sys.source(full.path , envir = environment(), chdir = TRUE )


    ## 'debugSource' cannot handle connections
    if (.Platform$GUI == "RStudio") {
        fun <- get("debugSource", "tools:rstudio", inherits = FALSE)
        setwd(base.path.dir)
        fun(base.path     )
        fun(base.path     )
        fun(url.base.path )
        setwd(short.path.dir)
        fun(short.path    )
        fun(short.path    )
        fun(url.short.path)
        setwd(full.path.dir)
        fun(full.path     )
        fun(full.path     )
        fun(url.full.path )
    }


    ## 'testthat::source_file' cannot handle file URLs or connections
    if (requireNamespace("testthat", quietly = TRUE)) {
        setwd(base.path.dir)
        testthat::source_file(base.path , env = environment(), chdir = FALSE, wrap = FALSE)
        testthat::source_file(base.path , env = environment(), chdir = FALSE, wrap = TRUE )
        testthat::source_file(base.path , env = environment(), chdir = TRUE , wrap = FALSE)
        testthat::source_file(base.path , env = environment(), chdir = TRUE , wrap = TRUE )
        setwd(short.path.dir)
        testthat::source_file(short.path, env = environment(), chdir = FALSE, wrap = FALSE)
        testthat::source_file(short.path, env = environment(), chdir = FALSE, wrap = TRUE )
        testthat::source_file(short.path, env = environment(), chdir = TRUE , wrap = FALSE)
        testthat::source_file(short.path, env = environment(), chdir = TRUE , wrap = TRUE )
        setwd(full.path.dir)
        testthat::source_file(full.path , env = environment(), chdir = FALSE, wrap = FALSE)
        testthat::source_file(full.path , env = environment(), chdir = FALSE, wrap = TRUE )
        testthat::source_file(full.path , env = environment(), chdir = TRUE , wrap = FALSE)
        testthat::source_file(full.path , env = environment(), chdir = TRUE , wrap = TRUE )
    }
})
