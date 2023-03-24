essentials:::check.this(  # this.path
    special = TRUE,

    check = TRUE, as.cran = TRUE,

    chdir = TRUE
)


if (FALSE) {  # for submitting to R Mac Builder https://mac.r-project.org/macbuilder/submit.html
    essentials:::check.this(
        INSTALL = FALSE,

        check = FALSE,

        chdir = TRUE
    )
}


local({  # testing this.path() with source(gzcon())
    FILE <- tempfile(fileext = ".R")
    on.exit(unlink(FILE), add = TRUE, after = FALSE)
    writeLines(c(
        "sys.frame(-3)$ofile",
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


local({  # testing relpath() and rel2here()
    path <- c(
        NA,
        "",
        ".",
        paste0(Sys.getenv("USERPROFILE"), "\\Documents\\\u03b4.R"),
        paste0("//LOCALHOST/C$/Users/", Sys.info()[["user"]], "/Documents/this.path/inst/extdata/untitled_msvcrt.txt")
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
        this.path::relpath(path2 <- c(path, "A:\\Users\\documents"))
        this.path::rel2here(path2)
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
    files
})


local({
    # read the text from this URL
    args1 <- c("curl", "--silent", "https://raw.githubusercontent.com/r-lib/vroom/master/inst/extdata/mtcars.csv")
    # filter out the first row (the column names of the table)
    # and rows containing "merc" case insensitive
    args2 <- c("perl", "-ne", "use English; print if $INPUT_LINE_NUMBER == 1 || /merc/i")
    command1 <- paste(shQuote(args1), collapse = " ")
    command2 <- paste(shQuote(args2), collapse = " ")
    if (.Platform$OS.type == "windows") {
        command1 <- shQuote(command1, "cmd2")
        command2 <- shQuote(command2, "cmd2")
    }
    command <- paste(command1, "|", command2)
    writeLines(command)
    utils::read.csv(pipe(command))
})


local({


    SINKFILE <- tempfile(fileext = ".txt")
    SINKCONN <- file(SINKFILE, "w")
    on.exit(close(SINKCONN), add = TRUE, after = FALSE)
    sink(SINKCONN)
    on.exit(sink(), add = TRUE, after = FALSE)
    sink(SINKCONN, type = "message")
    on.exit(sink(type = "message"), add = TRUE, after = FALSE)


    .this.path <- this.path:::.this.path
    debugSource <- this.path:::debugSource


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


    FILE <- tempfile(fileext = ".R")
    on.exit(unlink(FILE, force = TRUE, expand = FALSE), add = TRUE, after = FALSE)


    this.path:::write.code(file = FILE, {
        withAutoprint({
            print(n <- .this.path(get.frame.number = TRUE))
            if (is.na(n))
                stop("invalid traceback")
            if (!(identical(sys.function(n), source                ) ||
                  identical(sys.function(n), sys.source            ) ||
                  identical(sys.function(n), debugSource           ) ||
                  identical(sys.function(n), testthat::source_file ) ||
                  identical(sys.function(n), knitr::knit           ) ||
                  identical(sys.function(n), this.path::wrap.source)))
            {
                n <- n + 1L
            }

            bindings <- grep("^\\.this\\.path::", print(sort(names(frame <- sys.frame(n)))), value = TRUE)
            stopifnot(vapply(bindings, bindingIsLocked, frame, FUN.VALUE = NA))
            getwd()
            try(this.path:::PRINFO(".this.path::file", frame, inherits = FALSE))
            # don't use as.list yet, will force the promises
            .this.path(for.msg = TRUE)
            .this.path(original = TRUE, for.msg = TRUE)
            try(writeLines(sQuote(.this.path(verbose = TRUE))))
            .this.path(for.msg = TRUE)
            .this.path(original = TRUE, for.msg = TRUE)
            try(Encoding(.this.path(original = TRUE)))
            try(Encoding(.this.path(original = FALSE)))
            try(this.path:::PRINFO(".this.path::file", frame, inherits = FALSE))
            sapply(bindings, get, envir = frame, inherits = FALSE, simplify = FALSE)
            try(this.path::this.path())
        }, spaced = TRUE)
    })


    ZIPFILE <- tempfile(fileext = ".zip")
    on.exit(unlink(ZIPFILE, force = TRUE, expand = FALSE), add = TRUE, after = FALSE)


    owd <- getwd()
    on.exit(setwd(owd), add = TRUE, after = FALSE)
    setwd(dirname(FILE))
    utils::zip(ZIPFILE, basename(FILE), flags = "-r9Xq")
    setwd(owd)


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


    conn <- unz(ZIPFILE, basename(FILE))
    on.exit(close(conn), add = TRUE, after = FALSE)


    fun(try(source(print(conn), local = new.env())))


    conn2 <- gzcon(file(file, "rb"))
    on.exit(close(conn2), add = TRUE, after = FALSE)
    fun(source(print(conn2), local = new.env()))


    fun(sys.source(.(file), envir = new.env(), chdir = FALSE))
    fun(sys.source(.(file), envir = new.env(), chdir = TRUE))


    if (is.function(debugSource)) {
        FILE2 <- tempfile("fil\u{00E9}", fileext = ".R")
        on.exit(unlink(FILE2, force = TRUE, expand = FALSE), add = TRUE, after = FALSE)
        file.copy(FILE, FILE2, copy.date = TRUE)
        file2 <- this.path::rel2here(FILE2)
        if (.Platform$OS.type == "windows")
            file2 <- chartr("/", "\\", file2)
        file2 <- iconv(file2, "UTF-8", "latin1")
        fun(debugSource(.(file), local = new.env()))
        fun(debugSource(.(file2), local = new.env()))
    }


    fun(testthat::source_file(.(file), env = new.env(), chdir = FALSE, wrap = FALSE))
    fun(testthat::source_file(.(file), env = new.env(), chdir = TRUE, wrap = FALSE))


    FILE3 <- tempfile(fileext = ".Rmd")
    on.exit(unlink(FILE3, force = TRUE, expand = FALSE), add = TRUE, after = FALSE)
    writeLines(con = FILE3, c(
        "```{r}",
        unlist(lapply(parse(FILE)[[c(1L, 2L)]][-1], deparse)),
        "```"
    ))


    FILE4 <- tempfile(fileext = ".md")
    on.exit(unlink(FILE4, force = TRUE, expand = FALSE), add = TRUE, after = FALSE)


    file3 <- this.path::rel2here(FILE3)
    if (.Platform$OS.type == "windows")
        file3 <- chartr("/", "\\", file3)
    fun(knitr::knit(.(file3), FILE4, quiet = TRUE))
    this.path:::cat.file(FILE4, number = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE)


    owd2 <- getwd()
    on.exit(setwd(owd2), add = TRUE, after = FALSE)
    setwd(dirname(FILE3))
    utils::zip(ZIPFILE, basename(FILE3), flags = "-r9Xq")
    setwd(owd2)


    conn3 <- unz(ZIPFILE, basename(FILE3))
    on.exit(close(conn3), add = TRUE, after = FALSE)


    fun(try(knitr::knit(print(conn3), FILE4, quiet = TRUE)))
    this.path:::cat.file(FILE4, number = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE)


    conn4 <- gzcon(file(file3, "rb"))
    on.exit(close(conn4), add = TRUE, after = FALSE)
    fun(knitr::knit(print(conn4), FILE4, quiet = TRUE))
    this.path:::cat.file(FILE4, number = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE)


    sourcelike <- function(pathname, envir = parent.frame()) {
        if (!(is.character(pathname) && file.exists(pathname)))
            stop(gettextf("'%s' is not an existing file", pathname, domain = "R-base"))
        exprs <- parse(n = -1, file = pathname, srcfile = NULL, keep.source = FALSE)
        for (i in seq_along(exprs)) eval(exprs[i], envir)
    }


    fun(wrap.source(sourcelike(.(FILE)), character.only = TRUE, file.only = TRUE))


    sourcelike2 <- function(file, envir = parent.frame()) {
        this.path::inside.source(file)
        exprs <- parse(n = -1, file = file, srcfile = NULL, keep.source = FALSE)
        for (i in seq_along(exprs)) eval(exprs[i], envir)
    }


    fun(sourcelike2(.(file)))


    file.edit <- get0("file.edit", globalenv(), mode = "function", ifnotfound = utils::file.edit)
    file.edit(SINKFILE)
    invisible(SINKFILE)
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
