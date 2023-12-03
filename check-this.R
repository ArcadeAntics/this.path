local({
    if (!file.exists(this.path::here("tools", "maintainers-copy")))
        stop("unable to 'check.this()', not the maintainer's copy")
    # essentials:::.update.DESCRIPTION.Date()
    essentials:::check.this(  ## this.path
        special = TRUE,

        # INSTALL = FALSE, # html = TRUE, latex = TRUE,

        check = FALSE, no.stop.on.test.error = TRUE,
        as.cran = TRUE, `_R_CHECK_CRAN_INCOMING_` = TRUE,

        chdir = TRUE
    )
})


local({  ## for submitting to R Mac Builder https://mac.r-project.org/macbuilder/submit.html
    FILE <- "./tools/for-r-mac-builder"
            if (!file.create(FILE)) stop(sprintf("unable to create file '%s'", FILE))
    on.exit(if (!file.remove(FILE)) stop(sprintf("unable to remove file '%s'", FILE)))
    essentials:::check.this(INSTALL = FALSE, check = FALSE, chdir = TRUE)
})


local({  ## for submitting to CRAN https://cran.r-project.org/submit.html
    upcoming.CRAN.version <- "2.3.0"
    if (!file.exists(this.path::here("tools", "maintainers-copy")))
        stop("unable to 'check.this()', not the maintainer's copy")





    DESCRIPTION.dcf <- this.path::here("DESCRIPTION")
    DESCRIPTION <- read.dcf(DESCRIPTION.dcf)
    if (nrow(DESCRIPTION) != 1L)
        stop("contains a blank line", call. = FALSE)
    DESCRIPTION <- DESCRIPTION[1L, ]


    ## re-read the file, providing 'keep.white' this time
    DESCRIPTION <- read.dcf(DESCRIPTION.dcf, keep.white = names(DESCRIPTION))
    if (nrow(DESCRIPTION) != 1L)
        stop("contains a blank line", call. = FALSE)
    DESCRIPTION <- DESCRIPTION[1L, ]


    DESCRIPTION[["Version"]] <- upcoming.CRAN.version
    temp.DESCRIPTION.dcf <- tempfile(fileext = ".dcf")
    if (!file.copy(DESCRIPTION.dcf, temp.DESCRIPTION.dcf, overwrite = TRUE, copy.date = TRUE))
        stop(sprintf("unable to copy file '%s' to '%s'", DESCRIPTION.dcf, temp.DESCRIPTION.dcf))
    on.exit({
        if (!file.copy(temp.DESCRIPTION.dcf, DESCRIPTION.dcf, overwrite = TRUE, copy.date = TRUE))
            stop(sprintf("unable to copy file '%s' to '%s'", temp.DESCRIPTION.dcf, DESCRIPTION.dcf))
    }, add = TRUE, after = FALSE)
    write.dcf(t(DESCRIPTION), DESCRIPTION.dcf, useBytes = !l10n_info()[["UTF-8"]],
        keep.white = names(DESCRIPTION))





    info.dcf <- this.path::here("tools", "info.dcf")
    info <- read.dcf(info.dcf)
    if (nrow(info) != 1L)
        stop("contains a blank line", call. = FALSE)
    info <- info[1L, ]


    ## re-read the file, providing 'keep.white' this time
    info <- read.dcf(info.dcf, keep.white = names(info))
    if (nrow(info) != 1L)
        stop("contains a blank line", call. = FALSE)
    info <- info[1L, ]


    info[["devel"]] <- "FALSE"
    temp.info.dcf <- tempfile(fileext = ".dcf")
    if (!file.copy(info.dcf, temp.info.dcf, overwrite = TRUE, copy.date = TRUE))
        stop(sprintf("unable to copy file '%s' to '%s'", info.dcf, temp.info.dcf))
    on.exit({
        if (!file.copy(temp.info.dcf, info.dcf, overwrite = TRUE, copy.date = TRUE))
            stop(sprintf("unable to copy file '%s' to '%s'", temp.info.dcf, info.dcf))
    }, add = TRUE, after = FALSE)
    write.dcf(t(info), info.dcf, useBytes = !l10n_info()[["UTF-8"]],
        keep.white = names(info))





    essentials:::.update.DESCRIPTION.Date()
    essentials:::check.this(INSTALL = FALSE, check = FALSE, chdir = TRUE)
})


local({
    sourcelike <- function(file, envir = .GlobalEnv) {
        ofile <- file
        if (file.exists(file)) {
        } else if (file.exists(file <- path.join("~", ofile))) {
        } else stop("file not found")
        file <- set.sys.path(file, ofile = ofile, Function = "sourcelike")
        local.file <- sys.path(for.msg = TRUE, local = TRUE)
        lines <- readLines(file, warn = FALSE)
        srcfile <- srcfilecopy(file, lines, file.mtime(local.file),
            isFile = !is.na(local.file))
        exprs <- parse(text = lines, srcfile = srcfile)
        for (i in seq_along(exprs)) eval(exprs[i], envir)
        invisible()
    }


    sourcelike("test66.R", environment())
    sourcelike("test67.R", environment())
})


local({  ## testing this.path() with source(gzcon())
    FILE <- tempfile(fileext = ".R")
    on.exit(unlink(FILE), add = TRUE, after = FALSE)
    writeLines(c(
        "sys.frame(.External2(this.path:::.C_getframenumber))$ofile",
        "this.path::this.path(original = TRUE)",
        "this.path::this.path()"
    ), FILE)
    conn1 <- file(this.path::relpath(FILE))
    on.exit(close(conn1), add = TRUE, after = FALSE)
    source(conn1, echo = TRUE)
    conn2 <- gzcon(file(this.path::relpath(FILE), "rb"))
    on.exit(close(conn2), add = TRUE, after = FALSE)
    source(conn2, echo = TRUE)
})


local({  ## testing relpath() and rel2here()
    path <- c(
        NA,
        "",
        ".",
        essentials::f.str("%{Sys.getenv('USERPROFILE')}s\\Documents\\\u{03b4}.R"),
        essentials::f.str("//LOCALHOST/C$/Users/%{Sys.info()[['user']]}s/Documents/this.path/inst/extdata/untitled_msvcrt.txt")
    )
    owd <- getwd()
    if (is.null(owd))
        stop("cannot 'chdir' as current directory is unknown")
    on.exit(setwd(owd))
    setwd("~")
    oopt <- options(width = 10L)
    on.exit(options(oopt), add = TRUE, after = FALSE)
    withAutoprint({
        path
        getwd()
        this.path::here()
        this.path::relpath(path)
        this.path::rel2here(path)
        this.path::rel2proj(path)
        this.path::relpath(path2 <- c(path, "A:\\Users\\iris\\Documents"))
        this.path::rel2here(path2)
        this.path::rel2proj(path2)
    }, echo = TRUE, spaced = TRUE, verbose = FALSE,
        max.deparse.length = Inf, width.cutoff = 60L)
})


# unix.getFinalPathName <- function (path)
# {
#     if (!is.character(path))
#         stop("a character vector argument expected", domain = "R")
#     path <- this.path:::normpath(path)
#     path.unsplit(lapply(path.split(path), function(p) {
#         failure <- TRUE
#         tryCatch({
#             path <- normalizePath(p[[1L]], "/", TRUE)
#             failure <- FALSE
#         }, error = function(e) {
#             p <- p[p != "."]
#             while (i <- match("..", p, 0L)) {
#                 p <- if (i == 2L)
#                     p[-2L]
#                 else p[-i + 0L:1L]
#             }
#             p <<- p
#         })
#         if (failure) return(p)
#         for (p in p[-1L]) {
#             print(path)
#             if (p == ".") {
#             }
#             else if (p == "..")
#                 path <- dirname2(path)
#             else path <- normalizePath(path.unsplit(c(path, p)), "/", FALSE)
#         }
#         path
#     }))
# }
#
#
# unix.getFinalPathName("C:/testing/./..")


# realpath2 <- function (path)
# {
#     vapply(path.split(path), function(p) {
#         path <- normalizePath(p[[1L]], "/", TRUE)
#         for (p in p[-1L]) {
#             path <- normalizePath(path.unsplit(c(path, p)), "/", FALSE)
#         }
#         path
#     }, FUN.VALUE = "")
# }
# path <- paste0("C:/Users/", Sys.info()[["user"]], "/Documents/this.path/symlink/../Downloads")
# realpath2(path)
# normalizePath(path, "/", FALSE)


local({
    files <- list.files(all.files = TRUE, full.names = TRUE, recursive = TRUE, include.dirs = FALSE, no.. = TRUE)
    files <- files[!startsWith(files, "./.git/")]
    files <- files[!startsWith(files, "./.Rproj.user/")]
    files <- files[!startsWith(files, "./this.path.Rcheck/")]
    files <- grep("^\\./this\\.path_([[:digit:]]+[.-]){1,}[[:digit:]]+(\\.tar\\.gz|\\.zip|\\.tgz)",
        files, value = TRUE, invert = TRUE)
    Rfiles <- files[grepl("(?i)\\.R$", basename(files))]
    Rdfiles <- files[grepl("(?i)\\.Rd$", basename(files))]
    files
    Rfiles
    Rdfiles


    x <- this.path:::.readFiles(files)
    x <- grep("set\\.sys\\.path\\.jupyter", x, perl = TRUE, value = TRUE)
    x <- x |> names() |> print(quote = FALSE, width = 10)
    x |> file.edit()


    ## ^.{0,62}\\$|^.{63,65535} +\\$


    x <- this.path:::.readFiles(Rfiles)
    x <- grep("utils::", x, value = TRUE)
    x <- x |> names() |> print(quote = FALSE, width = 10)
    x |> file.edit()


    x <- this.path:::.readFiles(Rdfiles)
    x <- grep(r"(\\Emacs|\\Jupyter|\\Python|\\radian|\\RStudio|\\VSCode)", x, value = TRUE)
    x <- x |> names() |> print(quote = FALSE, width = 10)
    x |> file.edit()
})


local({
    ## read the text from this URL
    args1 <- c("curl", "--silent", "https://raw.githubusercontent.com/r-lib/vroom/master/inst/extdata/mtcars.csv")
    ## filter out the first row (the column names of the table)
    ## and rows containing "merc" case insensitive
    args2 <- c("perl", "-ne", "use English; print if $INPUT_LINE_NUMBER == 1 || /merc/i")
    command1 <- paste(shQuote(args1), collapse = " ")
    command2 <- paste(shQuote(args2), collapse = " ")
    if (.Platform$OS.type == "windows") {
        command1 <- shQuote(command1, "cmd2")
        command2 <- shQuote(command2, "cmd2")
    }
    command <- paste(command1, "|", command2)
    writeLines(command)
    utils::read.csv(print(pipe(command)))
})


withwd <- function (...)
{
    if (nargs() != 2L)
        stop(gettextf("%d arguments passed to %s which requires %s", nargs(), "withwd", "2"))
    ..1  ## force the promise
    owd <- getwd()
    if (is.null(owd))
        stop("cannot 'chdir' as current directory is unknown", domain = "R")
    on.exit(setwd(owd))
    setwd(..1)
    ..2
}


withclose <- function (...)
{
    if (nargs() != 2L)
        stop(gettextf("%d arguments passed to %s which requires %s", nargs(), "withclose", "2"))
    ..1  ## force the promise
    on.exit(close(..1))
    ..2
}


withunlink <- function (...)
{
    if (nargs() < 2L)
        stop(gettextf("%d arguments passed to %s which requires %s", nargs(), "withunlink", "at least 2"))
    for (n in seq_len(nargs() - 1L)) ...elt(n)  ## force the promise(s)
    rm(n)
    call <- as.call(c(list(as.symbol("unlink")), `names<-`(lapply(paste0("..", seq_len(nargs() - 1L)), as.symbol), ...names()[-nargs()])))
    if (!"x" %in% names(match.call(unlink, call)))
        stop(simpleError(gettextf("argument \"%s\" is missing, with no default",
            "x", domain = "R"), call))
    eval(call("delayedAssign", "call", call("on.exit", call)))
    call
    rm(call)
    ...elt(nargs())
}


withsink <- function (...)
{
    if (nargs() < 2L)
        stop(gettextf("%d arguments passed to %s which requires %s", nargs(), "withsink", "at least 2"))
    for (n in seq_len(nargs() - 1L)) ...elt(n)  ## force the promise(s)
    rm(n)
    call <- as.call(c(list(as.symbol("sink")), `names<-`(lapply(paste0("..", seq_len(nargs() - 1L)), as.symbol), ...names()[-nargs()])))
    call2 <- match.call(sink, call)
    if (i <- match("file", names(call2), 0L))
        call2 <- call2[-i]
    else stop(simpleError(gettextf("argument \"%s\" is missing", "file", domain = "R"), call))
    rm(i)
    eval(call("delayedAssign", "call", call))
    eval(call("delayedAssign", "call2", call("on.exit", call2)))
    call
    call2
    rm(call, call2)
    ...elt(nargs())
}


local({


    SINKFILE <- tempfile(fileext = ".txt")
    writeLines(SINKFILE)
    withclose(SINKCONN <- file(SINKFILE, "w"), {
        withsink(SINKCONN, {
            withsink(SINKCONN, type = "message", {
                bsubstitute <- function (expr, envir = parent.frame(),
                                         enclos = if (is.list(envir) || is.pairlist(envir))
                                                      parent.frame() else baseenv(),
                                         splice = TRUE, evaluated = FALSE)
                {
                    if (evaluated)
                        expr
                    else expr <- substitute(expr)
                    envir <- essentials:::as.env(envir, enclos)
                    splice <- if (splice) TRUE else FALSE
                    dot <- as.symbol(".")
                    dotdot <- as.symbol("..")
                    bracket <- as.symbol("{")
                    unquote <- function(e) {
                        if (is.pairlist(e))
                            as.pairlist(lapply(e, unquote))
                        else if (is.call(e)) {
                            if (identical(e[[1L]], dot))
                                eval(e[[2L]], envir)
                            else if (splice) {
                                if (identical(e[[1L]], dotdot))
                                    stop("can only splice inside a call", call. = FALSE)
                                else as.call(unquote.list(e))
                            }
                            else as.call(lapply(e, unquote))
                        }
                        else e
                    }
                    is.splice.macro <- function(e) is.call(e) && identical(e[[1L]], dotdot)
                    unquote.list <- function(e) {
                        p <- Position(is.splice.macro, e, nomatch = 0L)
                        if (p) {
                            n <- length(e)
                            head <- if (p == 1L)
                                NULL
                            else e[1:(p - 1L)]
                            tail <- if (p == n)
                                NULL
                            else e[(p + 1L):n]
                            mexp <- eval(e[[p]][[2L]], envir)
                            if (is.vector(mexp) || is.expression(mexp)) {
                            }
                            else if (is.call(mexp) && identical(e[[1L]], bracket))
                                mexp <- as.list(mexp)[-1L]
                            else stop("can only splice vectors")
                            c(lapply(head, unquote), mexp, as.list(unquote.list(tail)))
                        }
                        else lapply(e, unquote)
                    }
                    unquote(expr)
                }


                withunlink(FILE <- tempfile(fileext = ".R"), force = TRUE, expand = FALSE, {
                    this.path:::.write.code(file = FILE, {
                        # withAutoprint must be the first function called
                        withAutoprint({
                            n <- .External2(this.path:::.C_getframenumber)
                            if (is.na(n)) stop("invalid traceback")
                            stopifnot(bindingIsLocked(
                                sym <- ".this.path::document.context",
                                frame <- sys.frame(n)
                            ))
                            getwd()
                            frame[[sym]]
                            ## don't use as.list yet, will force the promises
                            this.path::sys.path(for.msg = TRUE)
                            this.path::sys.path(original = TRUE, for.msg = TRUE)
                            try(writeLines(sQuote(this.path::sys.path(verbose = TRUE))))
                            this.path::sys.path(for.msg = TRUE)
                            this.path::sys.path(original = TRUE, for.msg = TRUE)
                            try(Encoding(this.path::sys.path(original = TRUE)))
                            try(Encoding(this.path::sys.path(original = FALSE)))
                            frame[[sym]]
                            try(this.path::sys.path())
                        }, spaced = TRUE)
                    })


                    withunlink(ZIPFILE <- tempfile(fileext = ".zip"), force = TRUE, expand = FALSE, {
                        withwd(this.path::dirname2(FILE), {
                            utils::zip(ZIPFILE, this.path::basename2(FILE), flags = "-r9Xq")
                        })


                        file <- this.path::rel2here(FILE)
                        if (.Platform$OS.type == "windows")
                            file <- chartr("/", "\\", file)


                        fun <- function(expr) {
                            sexpr <- substitute(expr)
                            sexpr <- bsubstitute(sexpr, parent.frame(), evaluated = TRUE)
                            dep <- deparse1(sexpr, "\n", 60L)
                            dep <- gsub("\n", "\n+ ", dep, fixed = TRUE, useBytes = TRUE)
                            dep <- paste0("> ", dep)
                            cat("\n\n\n\n\n\n\n\n\n\n", dep, "\n", sep = "")
                            eval(sexpr, parent.frame())
                        }


                        fun(source(.(file), local = new.env(), chdir = FALSE))
                        fun(source(.(file), local = new.env(), chdir = TRUE))


                        withclose(conn <- unz(ZIPFILE, basename(FILE)), {
                            fun(try(source(print(conn), local = new.env())))
                        })


                        withclose(conn <- gzcon(file(file, "rb", encoding = "")), {
                            fun(source(print(conn), local = new.env()))
                        })


                        fun(sys.source(.(file), envir = new.env(), chdir = FALSE))
                        fun(sys.source(.(file), envir = new.env(), chdir = TRUE))


                        if (.Platform$GUI == "RStudio") {
                            withunlink(FILE2 <- tempfile("fil\u{00E9}", fileext = ".R"), force = TRUE, expand = FALSE, {
                                file.copy(FILE, FILE2, copy.date = TRUE)
                                file2 <- this.path::rel2here(FILE2)
                                if (.Platform$OS.type == "windows")
                                    file2 <- chartr("/", "\\", file2)
                                file2 <- iconv(file2, "UTF-8", "latin1")
                                fun(debugSource(.(file), local = new.env()))
                                fun(debugSource(.(file2), local = new.env()))
                            })
                        }


                        fun(testthat::source_file(.(file), env = new.env(), chdir = FALSE, wrap = FALSE))
                        fun(testthat::source_file(.(file), env = new.env(), chdir = TRUE, wrap = FALSE))


                        withunlink(FILE3 <- tempfile(fileext = ".Rmd"), force = TRUE, expand = FALSE, {
                            writeLines(con = FILE3, c(
                                "```{r}",
                                unlist(lapply(parse(FILE)[[c(1L, 2L)]][-1], deparse)),
                                "```"
                            ))


                            withunlink(FILE4 <- tempfile(fileext = ".md"), force = TRUE, expand = FALSE, {
                                file3 <- this.path::rel2here(FILE3)
                                if (.Platform$OS.type == "windows")
                                    file3 <- chartr("/", "\\", file3)
                                fun(knitr::knit(.(file3), FILE4, quiet = TRUE))
                                this.path:::.cat.file(FILE4, number = TRUE, squeeze.blank = TRUE, show.tabs = TRUE)


                                withwd(this.path::dirname2(FILE3), {
                                    utils::zip(ZIPFILE, this.path::basename2(FILE3), flags = "-r9Xq")
                                })


                                withclose(conn <- unz(ZIPFILE, basename(FILE3)), {
                                    fun(try(knitr::knit(print(conn), FILE4, quiet = TRUE)))
                                })
                                this.path:::.cat.file(FILE4, number = TRUE, squeeze.blank = TRUE, show.tabs = TRUE)


                                withclose(conn4 <- gzcon(file(file3, "rb", encoding = "")), {
                                    fun(knitr::knit(print(conn4), FILE4, quiet = TRUE))
                                })
                                this.path:::.cat.file(FILE4, number = TRUE, squeeze.blank = TRUE, show.tabs = TRUE)
                            })
                        })


                        sourcelike <- function(pathname, envir = parent.frame()) {
                            if (!(is.character(pathname) && file.exists(pathname)))
                                stop(gettextf("'%s' is not an existing file", pathname, domain = "R-base"))
                            exprs <- parse(n = -1, file = pathname, srcfile = NULL, keep.source = FALSE)
                            for (i in seq_along(exprs)) eval(exprs[i], envir)
                        }


                        fun(wrap.source(sourcelike(.(FILE)), character.only = TRUE, file.only = TRUE))


                        box.use <- function(file) {
                            file  ## force the promises


                            ## we have to use box::set_script_path() because {box}
                            ## does not allow us to import a module by its path
                            script_path <- box::script_path()
                            on.exit(box::set_script_path(script_path))
                            box::set_script_path(normalizePath(file, "/", TRUE))


                            eval(call("delayedAssign", "expr", bquote(
                                box::use(module = ./.(as.symbol(
                                    this.path::removeext(this.path::basename2(file))
                                )))
                            )))
                            expr
                            box::unload(module)
                        }


                        fun(box.use(.(file)))


                        cmpfile.loadcmp <- function(file, envir = parent.frame()) {
                            OUTFILE <- file
                            ext(OUTFILE) <- ".Rc"
                            on.exit(unlink(OUTFILE))
                            compiler::cmpfile(file, OUTFILE)
                            compiler::loadcmp(OUTFILE, envir)
                        }


                        fun(cmpfile.loadcmp(.(file)))


                        sourcelike2 <- function(file, envir = parent.frame()) {
                            this.path::set.sys.path(file, Function = c("sourcelike2", "environment", format(environment(sys.function()))))
                            exprs <- parse(n = -1, file = file, srcfile = NULL, keep.source = FALSE)
                            for (i in seq_along(exprs)) eval(exprs[i], envir)
                        }


                        fun(sourcelike2(.(file)))
                    })
                })
            })
        })
    })


    file.edit <- get0("file.edit", globalenv(), mode = "function", ifnotfound = utils::file.edit)
    file.edit(SINKFILE)
    invisible(SINKFILE)
})


local({
    withunlink(FILE <- tempfile(fileext = ".R"), {
        writeLines("this.path()", FILE)
        withunlink(ZIPFILE <- tempfile(fileext = ".zip"), {
            withwd(this.path::dirname2(FILE), utils::zip(ZIPFILE, this.path::basename2(FILE), flags = "-r9Xq"))
            withclose(conn <- unz(ZIPFILE, this.path::basename2(FILE)), source(conn))
        })
    })
})


# enumerate <- function (x, from = 1L)
# list(seq.int(from, along.with = x), x)
#
#
# mfor(i, xx, enumerate(letters), {
#     print(i)
#     print(xx)
# })





# x <- list(FALSE, 0L, 0, 0i, "", as.symbol("a"), quote(0L + 0L), ~FALSE)
# do.call(rbind, c(x[-6], list(deparse.level = 2)), quote = TRUE)
# (y <- do.call(rbind, c(lapply(x[-6], quoteExpression), list(deparse.level = 2))))
# methods::setClass("S4name", contains = "name")
# x <- methods::new("S4name", quote(a))





# .regexps <- list()
# .regexps$hexadecimal <- paste0(
#     "([-+])?",
#     "0[xX]",
#     "(",
#             "[[:xdigit:]]+(\\.[[:xdigit:]]*)?",
#         "|",
#             "\\.[[:xdigit:]]+",
#     ")",
#     "([Pp]([-+]?[[:digit:]]+))?"
# )
# .regexps$decimal <- paste0(
#     "(",
#             "[[:digit:]]+(\\.[[:digit:]]*)?",
#         "|",
#             "\\.[[:digit:]]+",
#     ")",
#     "([Ee]([-+]?[[:digit:]]+))?"
# )
# .regexps$numeric <- paste0(
#     "(",
#             .regexps$hexadecimal,
#         "|",
#             .regexps$decimal,
#     ")"
# )
#
#
# num.choices <- list(
#     sign  = c("", "-", "+"),
#     start = c("0x", "0X"),
#     num   = c("9AB", "9AB.", "9.AB", ".9AB")
# )
# exp.choices <- list(
#     start = c("P", "p"),
#     sign  = c("", "-", "+"),
#     num   = c("123")
# )
# combinations <- function (x, lex.order = FALSE)
# {
#     lens <- lengths(x)
#     length.out <- prod(lens)
#     if (length.out <= 0L)
#         return(list())
#     each <- if (lex.order)
#         rev(cumprod(c(1L, rev(lens)[-length(lens)])))
#     else    cumprod(c(1L,      lens[-length(lens)]))
#     essentials::plapply(list(
#         x = x,
#         each = each
#     ), base::rep, length.out = length.out)
# }
# x <- combinations(num.choices)
# y <- combinations(num.choices, lex.order = TRUE)
# essentials::psapply(x, paste0, USE.NAMES = FALSE)
# essentials::psapply(y, paste0, USE.NAMES = FALSE)
#
#
# num.choices <- essentials::pvapply(combinations(num.choices), paste0, FUN.VALUE = "", USE.NAMES = FALSE)
# exp.choices <- essentials::pvapply(combinations(exp.choices), paste0, FUN.VALUE = "", USE.NAMES = FALSE)
#
#
# choices <- list(
#     num.choices = num.choices,
#     exp.choices = c("", exp.choices)  # the exponent is optional
# )
# choices <- essentials::pvapply(combinations(choices), paste0, FUN.VALUE = "", USE.NAMES = FALSE)
#
#
# all(grepl(paste0("^(", .regexps$hexadecimal, ")$"), choices))
