local({
    owd <- getwd()
    if (is.null(owd)) {
        message("cannot 'chdir' as current directory is unknown")
        return(invisible())
    } else on.exit(setwd(owd), add = TRUE)


    ## test for 3 specific cases of sourcing
    ## * sourcing a file by specifying its basename
    ## * sourcing a file by specifying its absolute path
    ## * sourcing a file by specifying one of its relative paths
    abs.path.R <- tempfile("test", fileext = ".R")
    on.exit(unlink(abs.path.R), add = TRUE)
    this.path:::.writeCode({
        n <- this.path:::.getframenumber()
        if (is.na(n)) stop("invalid traceback")
        sym <- ".this.path::document.context"
        frame <- sys.frame(n)
        if (!exists(sym, envir = frame, inherits = FALSE))
            sym <- paste0(sym, "s")
        stopifnot(bindingIsLocked(sym, frame))
        cat("\n> getwd()\n")
        print(getwd())
        cat("\n> ", paste(deparse(call("dynGet", sym)), collapse = "\n+ "), "\n", sep = "")
        print(frame[[sym]])
        cat("\n> this.path::sys.path(original = TRUE)\n")
        print(this.path::sys.path(original = TRUE))
        cat("\n> this.path::sys.path(for.msg = TRUE)\n")
        print(this.path::sys.path(for.msg = TRUE))
        cat("\n> sys.path(verbose = TRUE)\n")
        stopifnot(identical(
            print(this.path::sys.path(verbose = TRUE)),
            getOption("this.path::sys.path() expectation")
        ))
        cat("\n> this.path::sys.path(original = TRUE)\n")
        print(this.path::sys.path(original = TRUE))
        cat("\n> this.path::sys.path(for.msg = TRUE)\n")
        print(this.path::sys.path(for.msg = TRUE))
    }, file = abs.path.R)
    abs.path.R <- normalizePath(abs.path.R, "/", TRUE)
    abs.path.dir <- normalizePath(R.home(), "/", TRUE)
    basename.R <- this.path::basename2(abs.path.R)
    basename.dir <- this.path::dirname2(abs.path.R)


    make.rel.path.and.dir <- function(file) {
        x <- this.path::path.split.1(file)
        n <- length(x)
        if (n < 3L) {
            c(this.path::dirname2(file), this.path::basename2(file))
        } else {
            i <- n < seq_len(n) + max(2L, n%/%2L)
            c(this.path::path.unsplit(x[!i]), this.path::path.unsplit(x[i]))
        }
    }
    tmp <- make.rel.path.and.dir(abs.path.R)
    rel.path.dir <- tmp[[1L]]
    rel.path.R <- tmp[[2L]]
    rm(tmp)


    replace.backslash <- if (.Platform$OS.type == "windows") {
        function(x) chartr("\\", "/", x)
    } else {
        function(x) x
    }


    ## for 'source' and 'debugSource' specifically,
    ## try sourcing a file URI
    as.file.uri <- function(path) {
        if (!length(path))
            return(character())
        if (!is.character(path))
            path <- as.character(path)
        if (.Platform$OS.type == "windows") {
            ## on Windows we have file:///C:/path/to/file or similar
            path <- replace.backslash(path)
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
    basename.R.uri <- as.file.uri(basename.R)
    rel.path.R.uri <- as.file.uri(rel.path.R)
    abs.path.R.uri <- as.file.uri(abs.path.R)


    fun <- function(expr, envir = parent.frame(),
        bquote.envir = envir, eval.envir = envir)
    {
        if (!is.environment(envir))
            stop("not an environment", domain = "R")
        expr <- call("bquote", substitute(expr), as.symbol("bquote.envir"))
        expr <- eval(expr)
        dep <- deparse(expr)
        cat("\n\n\n\n\n\n\n\n\n\n")
        cat("\n> getwd()\n")
        print(getwd())
        cat("\n> ")
        cat(dep, sep = "\n+ ")
        eval(expr, eval.envir)
    }


    oopt <- options(`this.path::sys.path() expectation` = normalizePath(abs.path.R, "/", TRUE))
    on.exit(options(oopt), add = TRUE)


    ## try using source in all possible manners
    setwd(basename.dir)
    fun(source(.(basename.R)                         , local = TRUE, chdir = FALSE))               ## from a  basename                          without changing directory
    fun(source(.(basename.R)                         , local = TRUE, chdir = TRUE ))               ## from a  basename                          with    changing directory (shouldn't do anything)
    fun(source(.(basename.R.uri)                     , local = TRUE))                              ## from a  basename file URI
    fun(source(print(conn <- file(.(basename.R)))    , local = TRUE))               ; close(conn)  ## from a  basename connection
    setwd(rel.path.dir)
    fun(source(.(rel.path.R)                         , local = TRUE, chdir = FALSE))               ## from a  relative path                     without changing directory
    fun(source(.(rel.path.R)                         , local = TRUE, chdir = TRUE ))               ## from a  relative path                     with    changing directory
    fun(source(.(rel.path.R.uri)                     , local = TRUE))                              ## from a  relative path file URI
    fun(source(print(conn <- file(.(rel.path.R)))    , local = TRUE))               ; close(conn)  ## from a  relative path connection
    setwd(abs.path.dir)
    fun(source(.(abs.path.R)                         , local = TRUE, chdir = FALSE))               ## from an absolute path                     without changing directory
    fun(source(.(abs.path.R)                         , local = TRUE, chdir = TRUE ))               ## from an absolute path                     with    changing directory
    fun(source(.(abs.path.R.uri)                     , local = TRUE))                              ## from a  absolute path file URI
    fun(source(print(conn <- file(.(abs.path.R)))    , local = TRUE))               ; close(conn)  ## from an absolute path connection


    ## 'sys.source' cannot handle file URIs nor connections
    setwd(basename.dir)
    fun(sys.source(.(basename.R), envir = environment(), chdir = FALSE))
    fun(sys.source(.(basename.R), envir = environment(), chdir = TRUE ))
    setwd(rel.path.dir)
    fun(sys.source(.(rel.path.R), envir = environment(), chdir = FALSE))
    fun(sys.source(.(rel.path.R), envir = environment(), chdir = TRUE ))
    setwd(abs.path.dir)
    fun(sys.source(.(abs.path.R), envir = environment(), chdir = FALSE))
    fun(sys.source(.(abs.path.R), envir = environment(), chdir = TRUE ))


    ## 'debugSource' cannot handle connections
    if (.Platform$GUI == "RStudio") {
        debugSource <- get("debugSource", "tools:rstudio", inherits = FALSE)
        setwd(basename.dir)
        fun(debugSource(.(basename.R)    ))
        fun(debugSource(.(basename.R.uri)))
        setwd(rel.path.dir)
        fun(debugSource(.(rel.path.R)    ))
        fun(debugSource(.(rel.path.R.uri)))
        setwd(abs.path.dir)
        fun(debugSource(.(abs.path.R)    ))
        fun(debugSource(.(abs.path.R.uri)))
    }


    ## 'testthat::source_file' cannot handle file URIs nor connections
    if (requireNamespace("testthat", quietly = TRUE)) {
        setwd(basename.dir)
        fun(testthat::source_file(.(basename.R), env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(basename.R), env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(basename.R), env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(basename.R), env = environment(), chdir = TRUE , wrap = TRUE ))
        setwd(rel.path.dir)
        fun(testthat::source_file(.(rel.path.R), env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(rel.path.R), env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(rel.path.R), env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(rel.path.R), env = environment(), chdir = TRUE , wrap = TRUE ))
        setwd(abs.path.dir)
        fun(testthat::source_file(.(abs.path.R), env = environment(), chdir = FALSE, wrap = FALSE))
        fun(testthat::source_file(.(abs.path.R), env = environment(), chdir = FALSE, wrap = TRUE ))
        fun(testthat::source_file(.(abs.path.R), env = environment(), chdir = TRUE , wrap = FALSE))
        fun(testthat::source_file(.(abs.path.R), env = environment(), chdir = TRUE , wrap = TRUE ))
    }


    ## 'knitr::knit' cannot handle file URIs
    if (requireNamespace("knitr", quietly = TRUE)) {
        basename.Rmd <- basename.R; this.path::ext(basename.Rmd) <- ".Rmd"
        rel.path.Rmd <- rel.path.R; this.path::ext(rel.path.Rmd) <- ".Rmd"
        abs.path.Rmd <- abs.path.R; this.path::ext(abs.path.Rmd) <- ".Rmd"


        on.exit(unlink(abs.path.Rmd), add = TRUE)
        writeLines(c(
            "```{r}",
            ## remove expressions starting with 'cat'
            {
                exprs <- parse(abs.path.R)
                exprs <- exprs[!vapply(exprs, function(expr) {
                    is.call(expr) && identical(expr[[1L]], as.symbol("cat"))
                }, NA, USE.NAMES = FALSE)]
                this.path:::.writeCode(exprs, NULL)
            },
            "```"
        ), abs.path.Rmd)


        options(`this.path::sys.path() expectation` = normalizePath(abs.path.Rmd, "/", TRUE))
        setwd(basename.dir)
        fun(knitr::knit(.(basename.Rmd)                     , output = stdout(), quiet = TRUE))
        fun(knitr::knit(print(conn <- file(.(basename.Rmd))), output = stdout(), quiet = TRUE)); close(conn)
        setwd(rel.path.dir)
        fun(knitr::knit(.(rel.path.Rmd)                     , output = stdout(), quiet = TRUE))
        fun(knitr::knit(print(conn <- file(.(rel.path.Rmd))), output = stdout(), quiet = TRUE)); close(conn)
        setwd(abs.path.dir)
        fun(knitr::knit(.(abs.path.Rmd)                     , output = stdout(), quiet = TRUE))
        fun(knitr::knit(print(conn <- file(.(abs.path.Rmd))), output = stdout(), quiet = TRUE)); close(conn)
    }


    ## 'compiler::loadcmp' cannot handle file URIs nor connections
    if (requireNamespace("compiler", quietly = TRUE)) {
        basename.Rc <- basename.R; this.path::ext(basename.Rc) <- ".Rc"
        rel.path.Rc <- rel.path.R; this.path::ext(rel.path.Rc) <- ".Rc"
        abs.path.Rc <- abs.path.R; this.path::ext(abs.path.Rc) <- ".Rc"


        on.exit(unlink(abs.path.Rc), add = TRUE)
        compiler::cmpfile(abs.path.R, abs.path.Rc)


        options(`this.path::sys.path() expectation` = normalizePath(abs.path.Rc, "/", TRUE))
        setwd(basename.dir)
        fun(compiler::loadcmp(.(basename.Rc), envir = environment(), chdir = FALSE))
        fun(compiler::loadcmp(.(basename.Rc), envir = environment(), chdir = TRUE ))
        setwd(rel.path.dir)
        fun(compiler::loadcmp(.(rel.path.Rc), envir = environment(), chdir = FALSE))
        fun(compiler::loadcmp(.(rel.path.Rc), envir = environment(), chdir = TRUE ))
        setwd(abs.path.dir)
        fun(compiler::loadcmp(.(abs.path.Rc), envir = environment(), chdir = FALSE))
        fun(compiler::loadcmp(.(abs.path.Rc), envir = environment(), chdir = TRUE ))
    }


    ## 'box::use' cannot handle file URIs nor connections nor absolute paths
    if (requireNamespace("box", quietly = TRUE)) {
        options(`this.path::sys.path() expectation` = normalizePath(abs.path.R, "/", TRUE))
        setwd(basename.dir); box::set_script_path(this.path::path.join(basename.dir, "."))
        fun(box::use(module = ./.(as.symbol(sub("\\.R$", "", basename.R))))); box::unload(module)
        if (!this.path:::.is_abs_path(rel.path.R)) {
            tmp.fun <- function(x) {
                n <- length(x)
                if (n > 1L)
                    call("/", tmp.fun(x[-n]), as.symbol(x[[n]]))
                else as.symbol(x[[1L]])
            }
            tmp <- tmp.fun(c(".", this.path::path.split.1(sub("\\.R$", "", rel.path.R))))
            setwd(rel.path.dir); box::set_script_path(this.path::path.join(rel.path.dir, "."))
            fun(box::use(module = .(tmp))); box::unload(module)
            rm(tmp, tmp.fun)
        }
    }


    ## 'shiny::runApp'
    if (requireNamespace("shiny", quietly = TRUE)) {
        shinytmp <- tempfile("shinytmp", tmpdir = basename.dir)
        shinytmp <- replace.backslash(shinytmp)
        on.exit(unlink(shinytmp, recursive = TRUE, force = TRUE), add = TRUE)
        dir.create(shinytmp)
        file <- this.path::path.join(shinytmp, "app.R")
        writeLines(c(
            readLines(abs.path.R),
            "stop(structure(list(message = \"\", call = NULL), class = c(\"thispath.tests.R.catch.this.error\", \"error\", \"condition\")))"
        ), file)
        options(`this.path::sys.path() expectation` = normalizePath(file, "/", TRUE))
        rm(file)
        abs.path.app.R <- normalizePath(shinytmp, "/", TRUE)
        abs.path.app.dir <- abs.path.dir
        basename.app.R <- "."
        basename.app.dir <- abs.path.app.R
        tmp <- make.rel.path.and.dir(abs.path.app.R)
        rel.path.app.dir <- tmp[[1L]]
        rel.path.app.R <- tmp[[2L]]
        rm(tmp)
        setwd(basename.app.dir)
        this.path::tryCatch3({
            fun(shiny::runApp(.(basename.app.R)))
        }, thispath.tests.R.catch.this.error = )
        setwd(rel.path.app.dir)
        this.path::tryCatch3({
            fun(shiny::runApp(.(rel.path.app.R)))
        }, thispath.tests.R.catch.this.error = )
        setwd(abs.path.app.dir)
        this.path::tryCatch3({
            fun(shiny::runApp(.(abs.path.app.R)))
        }, thispath.tests.R.catch.this.error = )
    }


    ## 'plumber::plumb' cannot handle file URIs nor connections
    if (requireNamespace("plumber", quietly = TRUE)) {
        options(`this.path::sys.path() expectation` = normalizePath(abs.path.R, "/", TRUE))
        setwd(basename.dir)
        fun(plumber::plumb(.(basename.R)))
        setwd(rel.path.dir)
        fun(plumber::plumb(.(rel.path.R)))
        setwd(abs.path.dir)
        fun(plumber::plumb(.(abs.path.R)))


        entrypoint.R <- this.path::path.join(basename.dir, "entrypoint.R")
        on.exit(unlink(entrypoint.R), add = TRUE)
        writeLines(c(
            readLines(abs.path.R),
            "plumber::Plumber$new()"
        ), entrypoint.R)
        options(`this.path::sys.path() expectation` = normalizePath(entrypoint.R, "/", TRUE))
        setwd(basename.dir)
        fun(plumber::plumb())
        setwd(rel.path.dir)
        fun(plumber::plumb(dir = .(dirname(rel.path.R))))
        setwd(abs.path.dir)
        fun(plumber::plumb(dir = .(dirname(abs.path.R))))
    }


    ## 'utils::Sweave' cannot handle file URIs nor connections
    if (requireNamespace("utils", quietly = TRUE)) {
        basename.Rnw <- basename.R; this.path::ext(basename.Rnw) <- ".Rnw"
        rel.path.Rnw <- rel.path.R; this.path::ext(rel.path.Rnw) <- ".Rnw"
        abs.path.Rnw <- abs.path.R; this.path::ext(abs.path.Rnw) <- ".Rnw"


        on.exit(unlink(abs.path.Rnw), add = TRUE)
        writeLines(c(
            "\\documentclass{article}",
            "",
            "\\begin{document}",
            "",
            "<<>>=",
            ## remove expressions starting with 'cat'
            {
                exprs <- parse(abs.path.R)
                exprs <- exprs[!vapply(exprs, function(expr) {
                    is.call(expr) && identical(expr[[1L]], as.symbol("cat"))
                }, NA, USE.NAMES = FALSE)]
                this.path:::.writeCode(exprs, NULL)
            },
            "@",
            "",
            "\\end{document}"
        ), abs.path.Rnw)


        options(`this.path::sys.path() expectation` = normalizePath(abs.path.Rnw, "/", TRUE))
        setwd(basename.dir)
        outputname <- fun(utils::Sweave(.(basename.Rnw)))
        writeLines(readLines(outputname))
        unlink(outputname)


        tmpdir <- tempfile("dir")
        on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE))
        dir.create(tmpdir)
        setwd(tmpdir)
        writeLines(readLines(fun(utils::Sweave(.(this.path::path.join("..", basename.Rnw))))))
    }


    invisible()
})


local({
    FILE.R <- tempfile(fileext = ".R")
    on.exit(unlink(FILE.R))
    this.path:::.writeCode({
        stopifnot(identical(
            this.path::this.path(),
            getOption("this.path::this.path() expectation")
        ))
    }, FILE.R)
    oopt <- options(
        `this.path::this.path() expectation` = normalizePath(FILE.R, "/", TRUE),
        keep.source = TRUE
    )
    on.exit(options(oopt), add = TRUE)
    eval(
        parse(FILE.R),
        structure(list2env(list(.packageName = FILE.R), parent = .BaseNamespaceEnv), path = FILE.R)
    )
    eval(parse(FILE.R))
})


local({
    FILE.R <- tempfile(fileext = ".R")
    on.exit(unlink(FILE.R))
    this.path:::.writeCode({
        list(
            this.path::src.path(original = TRUE),
            this.path::src.path(original = NA),
            this.path::src.path(),
            this.path::src.path(original = TRUE),
            this.path::src.path(original = NA)
        )
    }, FILE.R)
    oopt <- options(keep.source = TRUE)
    on.exit(options(oopt), add = TRUE)
    stopifnot(identical(
        eval(parse(FILE.R)),
        list(FILE.R, normalizePath(FILE.R, "/", TRUE))[c(1L, 1L, 2L, 1L, 2L)]
    ))
})


local({
    FILE1.R <- tempfile(pattern = "file1_", fileext = ".R")
    on.exit(unlink(FILE1.R), add = TRUE)
    this.path:::.writeCode({
        fun <- function(x) x
        fun1 <- function() fun(this.path::src.path())
    }, FILE1.R)
    source(FILE1.R, environment(), keep.source = TRUE)


    FILE2.R <- tempfile(pattern = "file2_", fileext = ".R")
    on.exit(unlink(FILE2.R), add = TRUE)
    this.path:::.writeCode({
        fun2 <- function() fun(this.path::src.path())
    }, FILE2.R)
    source(FILE2.R, environment(), keep.source = TRUE)


    ## it might seem weird to use eval(expression())
    ## it is just to prevent the expressions from having source references
    stopifnot(identical(eval(expression(fun1())), normalizePath(FILE1.R, "/", TRUE)))
    stopifnot(identical(eval(expression(fun2())), normalizePath(FILE2.R, "/", TRUE)))


    FILE3.R <- tempfile("file3_", fileext = ".R")
    on.exit(unlink(FILE3.R), add = TRUE)
    this.path:::.writeCode({
        x <- list(fun1(), fun2(), fun(this.path::src.path()))
    }, FILE3.R)
    source(FILE3.R, environment(), keep.source = TRUE)
    stopifnot(identical(x, as.list(normalizePath(c(FILE1.R, FILE2.R, FILE3.R), "/", TRUE))))
})
