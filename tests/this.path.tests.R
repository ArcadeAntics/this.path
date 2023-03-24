local({
    owd <- getwd()
    if (is.null(owd)) {
        message("cannot 'chdir' as current directory is unknown")
        return(invisible())
    } else on.exit(setwd(owd), add = TRUE)


    fun <- function(expr, envir = parent.frame()) {
        if (!is.environment(envir))
            stop("not an environment", domain = "R")
        expr <- call("bquote", substitute(expr), as.symbol("envir"))
        expr <- eval(expr)
        # dep <- deparse1(expr, "\n", 60L)
        # replace with this for R < 4.0.0:
        dep <- paste(deparse(expr, 60L), collapse = "\n")
        dep <- gsub("\n", "\n+ ", dep, fixed = TRUE, useBytes = TRUE)
        dep <- paste0("> ", dep)
        cat("\n\n\n\n\n\n\n\n\n\n", dep, "\n", sep = "")
        eval(expr, parent.frame())
    }


    ## test for 3 specific cases of sourcing
    ## * sourcing a file by specifying its basename
    ## * sourcing a file by specifying its absolute path
    ## * sourcing a file by specifying one of its relative paths that is not its basename
    base.path  <- "test.R"
    full.path  <- this.path::here(base.path)
    short.path <- file.path(basename(dirname(full.path)), base.path)


    on.exit(unlink(full.path), add = TRUE)
    this.path:::write.code({
        cat("\n> getwd()\n")
        print(getwd())
        cat("\n> this.path(verbose = TRUE)\n")
        print(this.path::this.path(verbose = TRUE))
    }, file = full.path)


    ## for 'base::source' and 'debugSource' specifically,
    ## try sourcing with a file URL
    as.file.uri <- function(path) {
        if (!length(path))
            return(character())
        if (!is.character(path))
            path <- as.character(path)
        if (.Platform$OS.type == "windows") {
            path <- chartr("\\", "/", path)
            three.slash <- grepl("^.:", path, useBytes = TRUE)
            if (all(three.slash))
                paste0("file:///", path)
            else if (any(three.slash)) {
                x <- character(length(path))
                x[three.slash] <- paste0("file:///", path[three.slash])
                x[!three.slash] <- paste0("file://", path[!three.slash])
                x
            }
            else paste0("file://", path)
        }
        else paste0("file://", path)
    }
    url.base.path  <- as.file.uri(base.path )
    url.full.path  <- as.file.uri(full.path )
    url.short.path <- as.file.uri(short.path)


    ## the directories that lead to the 3 paths from above
    base.path.dir  <- dirname(full.path)
    short.path.dir <- dirname(base.path.dir)
    full.path.dir  <- if (getRversion() >= "3.5.0") tempdir(check = TRUE) else tempdir()


    ## try using source in all possible manners
    setwd(base.path.dir)
    fun(source(.(base.path)                         , local = TRUE, chdir = FALSE))               # from a  basename                     without changing directory
    fun(source(.(base.path)                         , local = TRUE, chdir = TRUE ))               # from a  basename                     with    changing directory (shouldn't do anything)
    fun(source(.(url.base.path)                     , local = TRUE))                              # from a  URL basename
    fun(source(print(conn <- file(.(base.path)))    , local = TRUE))               ; close(conn)  # from a  basename connection
    fun(source(print(conn <- file(.(url.base.path))), local = TRUE))               ; close(conn)  # from a  URL basename connection
    setwd(short.path.dir)
    fun(source(.(short.path)                         , local = TRUE, chdir = FALSE))               # from a  relative path                without changing directory
    fun(source(.(short.path)                         , local = TRUE, chdir = TRUE ))               # from a  relative path                with    changing directory
    fun(source(.(url.short.path)                     , local = TRUE))                              # from a  URL relative path
    fun(source(print(conn <- file(.(short.path)))    , local = TRUE))               ; close(conn)  # from a  relative path connection
    fun(source(print(conn <- file(.(url.short.path))), local = TRUE))               ; close(conn)  # from a  URL relative path connection
    setwd(full.path.dir)
    fun(source(.(full.path)                          , local = TRUE, chdir = FALSE))               # from an absolute path                without changing directory
    fun(source(.(full.path)                          , local = TRUE, chdir = TRUE ))               # from an absolute path                with    changing directory
    fun(source(.(url.full.path)                      , local = TRUE))                              # from a  URL absolute path
    fun(source(print(conn <- file(.(full.path)))     , local = TRUE))               ; close(conn)  # from an absolute path connection
    fun(source(print(conn <- file(.(url.full.path))) , local = TRUE))               ; close(conn)  # from a  URL absolute path connection


    ## 'base::sys.source' cannot handle file URLs or connections
    setwd(base.path.dir)
    fun(sys.source(.(base.path) , envir = environment(), chdir = FALSE))
    fun(sys.source(.(base.path) , envir = environment(), chdir = TRUE ))
    setwd(short.path.dir)
    fun(sys.source(.(short.path), envir = environment(), chdir = FALSE))
    fun(sys.source(.(short.path), envir = environment(), chdir = TRUE ))
    setwd(full.path.dir)
    fun(sys.source(.(full.path) , envir = environment(), chdir = FALSE))
    fun(sys.source(.(full.path) , envir = environment(), chdir = TRUE ))


    ## 'debugSource' cannot handle connections
    if (.Platform$GUI == "RStudio") {
        debugSource<- get("debugSource", "tools:rstudio", inherits = FALSE)
        setwd(base.path.dir)
        fun(debugSource(.(base.path)     ))
        fun(debugSource(.(url.base.path) ))
        setwd(short.path.dir)
        fun(debugSource(.(short.path)    ))
        fun(debugSource(.(url.short.path)))
        setwd(full.path.dir)
        fun(debugSource(.(full.path)     ))
        fun(debugSource(.(url.full.path) ))
    }


    ## 'testthat::source_file' cannot handle file URLs or connections
    if (requireNamespace("testthat", quietly = TRUE)) {
        setwd(base.path.dir)
        fun(testthat::source_file(.(base.path) , env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(base.path) , env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(base.path) , env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(base.path) , env = environment(), chdir = TRUE , wrap = TRUE ))
        setwd(short.path.dir)
        fun(testthat::source_file(.(short.path), env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(short.path), env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(short.path), env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(short.path), env = environment(), chdir = TRUE , wrap = TRUE ))
        setwd(full.path.dir)
        fun(testthat::source_file(.(full.path) , env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(full.path) , env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(full.path) , env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(full.path) , env = environment(), chdir = TRUE , wrap = TRUE ))
    }
})


# print(this.path:::HAVE_AQUA)
# print(this.path:::PATH_MAX)
