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
        ## replace with this for R < 4.0.0:
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
    base.path.R  <- "test.R"
    full.path.R  <- this.path::sys.here(base.path.R)
    short.path.R <- file.path(basename(dirname(full.path.R)), base.path.R)


    on.exit(unlink(full.path.R), add = TRUE)
    this.path:::.write.code({
        cat("\n> getwd()\n")
        print(getwd())
        cat("\n> sys.path(verbose = TRUE)\n")
        print(this.path::sys.path(verbose = TRUE))
    }, file = full.path.R)


    ## for 'source' and 'debugSource' specifically,
    ## try sourcing with a file URI
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
    base.path.R.uri  <- as.file.uri(base.path.R )
    full.path.R.uri  <- as.file.uri(full.path.R )
    short.path.R.uri <- as.file.uri(short.path.R)


    ## the directories that lead to the 3 paths from above
    base.path.dir  <- dirname(full.path.R)
    short.path.dir <- dirname(base.path.dir)
    full.path.dir  <- if (getRversion() >= "3.5.0") tempdir(check = TRUE) else tempdir()


    ## try using source in all possible manners
    setwd(base.path.dir)
    fun(source(.(base.path.R)                          , local = TRUE, chdir = FALSE))               ## from a  basename                          without changing directory
    fun(source(.(base.path.R)                          , local = TRUE, chdir = TRUE ))               ## from a  basename                          with    changing directory (shouldn't do anything)
    fun(source(.(base.path.R.uri)                      , local = TRUE))                              ## from a  basename file URI
    fun(source(print(conn <- file(.(base.path.R)))     , local = TRUE))               ; close(conn)  ## from a  basename connection
    fun(source(print(conn <- file(.(base.path.R.uri))) , local = TRUE))               ; close(conn)  ## from a  basename file URI connection
    setwd(short.path.dir)
    fun(source(.(short.path.R)                         , local = TRUE, chdir = FALSE))               ## from a  relative path                     without changing directory
    fun(source(.(short.path.R)                         , local = TRUE, chdir = TRUE ))               ## from a  relative path                     with    changing directory
    fun(source(.(short.path.R.uri)                     , local = TRUE))                              ## from a  relative path file URI
    fun(source(print(conn <- file(.(short.path.R)))    , local = TRUE))               ; close(conn)  ## from a  relative path connection
    fun(source(print(conn <- file(.(short.path.R.uri))), local = TRUE))               ; close(conn)  ## from a  relative path file URI connection
    setwd(full.path.dir)
    fun(source(.(full.path.R)                          , local = TRUE, chdir = FALSE))               ## from an absolute path                     without changing directory
    fun(source(.(full.path.R)                          , local = TRUE, chdir = TRUE ))               ## from an absolute path                     with    changing directory
    fun(source(.(full.path.R.uri)                      , local = TRUE))                              ## from a  absolute path file URI
    fun(source(print(conn <- file(.(full.path.R)))     , local = TRUE))               ; close(conn)  ## from an absolute path connection
    fun(source(print(conn <- file(.(full.path.R.uri))) , local = TRUE))               ; close(conn)  ## from a  absolute path file URI connection


    ## 'sys.source' cannot handle file URIs nor connections
    setwd(base.path.dir)
    fun(sys.source(.(base.path.R) , envir = environment(), chdir = FALSE))
    fun(sys.source(.(base.path.R) , envir = environment(), chdir = TRUE ))
    setwd(short.path.dir)
    fun(sys.source(.(short.path.R), envir = environment(), chdir = FALSE))
    fun(sys.source(.(short.path.R), envir = environment(), chdir = TRUE ))
    setwd(full.path.dir)
    fun(sys.source(.(full.path.R) , envir = environment(), chdir = FALSE))
    fun(sys.source(.(full.path.R) , envir = environment(), chdir = TRUE ))


    ## 'debugSource' cannot handle connections
    if (.Platform$GUI == "RStudio") {
        debugSource <- get("debugSource", "tools:rstudio", inherits = FALSE)
        setwd(base.path.dir)
        fun(debugSource(.(base.path.R)     ))
        fun(debugSource(.(base.path.R.uri) ))
        setwd(short.path.dir)
        fun(debugSource(.(short.path.R)    ))
        fun(debugSource(.(short.path.R.uri)))
        setwd(full.path.dir)
        fun(debugSource(.(full.path.R)     ))
        fun(debugSource(.(full.path.R.uri) ))
    }


    ## 'testthat::source_file' cannot handle file URIs nor connections
    if (requireNamespace("testthat", quietly = TRUE)) {
        setwd(base.path.dir)
        fun(testthat::source_file(.(base.path.R) , env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(base.path.R) , env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(base.path.R) , env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(base.path.R) , env = environment(), chdir = TRUE , wrap = TRUE ))
        setwd(short.path.dir)
        fun(testthat::source_file(.(short.path.R), env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(short.path.R), env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(short.path.R), env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(short.path.R), env = environment(), chdir = TRUE , wrap = TRUE ))
        setwd(full.path.dir)
        fun(testthat::source_file(.(full.path.R) , env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(full.path.R) , env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(full.path.R) , env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(full.path.R) , env = environment(), chdir = TRUE , wrap = TRUE ))
    }


    ## 'knitr::knit' cannot handle file URIs
    if (requireNamespace("knitr", quietly = TRUE)) {
        base.path.Rmd  <- "test.Rmd"
        full.path.Rmd  <- this.path::sys.here(base.path.Rmd)
        short.path.Rmd <- file.path(basename(dirname(full.path.Rmd)), base.path.Rmd)


        on.exit(unlink(full.path.Rmd), add = TRUE)
        writeLines(c(
            "```{r}",
            readLines(full.path.R)[c(2L, 4L)],
            "```"
        ), full.path.Rmd)


        base.path.Rmd.uri  <- as.file.uri(base.path.Rmd )
        full.path.Rmd.uri  <- as.file.uri(full.path.Rmd )
        short.path.Rmd.uri <- as.file.uri(short.path.Rmd)


        setwd(base.path.dir)
        fun(knitr::knit(.(base.path.Rmd)                         , output = stdout(), quiet = TRUE))
        fun(knitr::knit(print(conn <- file(.(base.path.Rmd)))    , output = stdout(), quiet = TRUE)) ; close(conn)
        fun(knitr::knit(print(conn <- file(.(base.path.Rmd.uri))), output = stdout(), quiet = TRUE)) ; close(conn)
        setwd(short.path.dir)
        fun(knitr::knit(.(short.path.Rmd)                         , output = stdout(), quiet = TRUE))
        fun(knitr::knit(print(conn <- file(.(short.path.Rmd)))    , output = stdout(), quiet = TRUE)); close(conn)
        fun(knitr::knit(print(conn <- file(.(short.path.Rmd.uri))), output = stdout(), quiet = TRUE)); close(conn)
        setwd(full.path.dir)
        fun(knitr::knit(.(full.path.Rmd)                          , output = stdout(), quiet = TRUE))
        fun(knitr::knit(print(conn <- file(.(full.path.Rmd)))     , output = stdout(), quiet = TRUE)); close(conn)
        fun(knitr::knit(print(conn <- file(.(full.path.Rmd.uri))) , output = stdout(), quiet = TRUE)); close(conn)
    }


    ## 'compiler::loadcmp' cannot handle file URIs nor connections
    if (requireNamespace("compiler", quietly = TRUE)) {
        base.path.Rc  <- "test.Rc"
        full.path.Rc  <- this.path::sys.here(base.path.Rc)
        short.path.Rc <- file.path(basename(dirname(full.path.Rc)), base.path.Rc)


        on.exit(unlink(full.path.Rc), add = TRUE)
        compiler::cmpfile(full.path.R, full.path.Rc)


        setwd(base.path.dir)
        fun(compiler::loadcmp(.(base.path.Rc) , envir = environment(), chdir = FALSE))
        fun(compiler::loadcmp(.(base.path.Rc) , envir = environment(), chdir = TRUE ))
        setwd(short.path.dir)
        fun(compiler::loadcmp(.(short.path.Rc), envir = environment(), chdir = FALSE))
        fun(compiler::loadcmp(.(short.path.Rc), envir = environment(), chdir = TRUE ))
        setwd(full.path.dir)
        fun(compiler::loadcmp(.(full.path.Rc) , envir = environment(), chdir = FALSE))
        fun(compiler::loadcmp(.(full.path.Rc) , envir = environment(), chdir = TRUE ))
    }


    ## 'box::use' cannot handle file URIs nor connections nor absolute paths
    if (requireNamespace("box", quietly = TRUE)) {
        setwd(base.path.dir); box::set_script_path(base.path.R)
        fun(box::use(module = ./.(as.symbol(sub("\\.R$", "", base.path.R))))); box::unload(module)
        setwd(short.path.dir); box::set_script_path(base.path.dir)
        fun(box::use(module = ./.(as.symbol(dirname(short.path.R)))/.(as.symbol(sub("\\.R$", "", basename(short.path.R)))))); box::unload(module)
    }


    ## 'box::use' cannot handle file URIs nor connections
    if (requireNamespace("plumber", quietly = TRUE)) {
        setwd(base.path.dir)
        fun(plumber::plumb(.(base.path.R)))
        setwd(short.path.dir)
        fun(plumber::plumb(.(short.path.R)))
        setwd(full.path.dir)
        fun(plumber::plumb(.(full.path.R)))


        entrypoint.R <- file.path(full.path.dir, "entrypoint.R")
        on.exit(unlink(entrypoint.R), add = TRUE)
        writeLines(c(
            readLines(full.path.R),
            "plumber::Plumber$new()"
        ), entrypoint.R)
        fun(plumber::plumb(dir = full.path.dir))
    }


    invisible()
})
