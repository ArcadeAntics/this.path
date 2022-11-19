essentials:::check.this(  # this.path
    special = TRUE,

    check = TRUE, as.cran = TRUE,

    chdir = TRUE
)


# evalq({
#     names(which(c(
#         os.unix = os.unix, os.windows = os.windows,
#         gui.aqua = gui.aqua, gui.rgui = gui.rgui, gui.rstudio = gui.rstudio,
#             gui.tk = gui.tk, gui.vscode = gui.vscode,
#         os.unix.in.shell = os.unix.in.shell, os.windows.in.shell = os.windows.in.shell,
#             in.shell = in.shell,
#         unrecognized.manner = unrecognized.manner,
#         ucrt = ucrt, utf8 = utf8
#     )))
# }, getNamespace("this.path"))





SINKFILE <- tempfile(fileext = ".txt")
SINKCON <- file(SINKFILE, "w")
sink(SINKCON)
sink(SINKCON, type = "message")
local({
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

            bindings <- grep("^\\._this\\.path::.+_\\.$", print(sort(names(frame <- sys.frame(n)))), value = TRUE)
            stopifnot(vapply(bindings, bindingIsLocked, frame, FUN.VALUE = NA))
            getwd()
            try(this.path:::PRINFO("._this.path::file_.", frame, inherits = FALSE))
            # don't use as.list yet, will force the promises
            .this.path(for.msg = TRUE)
            .this.path(original = TRUE, for.msg = TRUE)
            try(writeLines(sQuote(.this.path(verbose = TRUE))))
            .this.path(for.msg = TRUE)
            .this.path(original = TRUE, for.msg = TRUE)
            try(Encoding(.this.path(original = TRUE)))
            try(Encoding(.this.path(original = FALSE)))
            try(this.path:::PRINFO("._this.path::file_.", frame, inherits = FALSE))
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


    file <- this.path::as.rel.path(FILE, ".")
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


    con <- unz(ZIPFILE, basename(FILE))
    on.exit(close(con), add = TRUE, after = FALSE)


    fun(try(source(print(con), local = new.env())))


    con2 <- gzcon(file(file, "rb"))
    on.exit(close(con2), add = TRUE, after = FALSE)
    fun(source(print(con2), local = new.env()))


    fun(sys.source(.(file), envir = new.env(), chdir = FALSE))
    fun(sys.source(.(file), envir = new.env(), chdir = TRUE))


    if (is.function(debugSource)) {
        FILE2 <- tempfile("fil\u{00E9}", fileext = ".R")
        on.exit(unlink(FILE2, force = TRUE, expand = FALSE), add = TRUE, after = FALSE)
        file.copy(FILE, FILE2, copy.date = TRUE)
        file2 <- this.path::as.rel.path(FILE2, ".")
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


    file3 <- this.path::as.rel.path(FILE3, ".")
    if (.Platform$OS.type == "windows")
        file3 <- chartr("/", "\\", file3)
    fun(knitr::knit(.(file3), FILE4, quiet = TRUE))
    this.path:::cat.file(FILE4, number = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE)


    owd <- getwd()
    on.exit(setwd(owd), add = TRUE, after = FALSE)
    setwd(dirname(FILE3))
    utils::zip(ZIPFILE, basename(FILE3), flags = "-r9Xq")
    setwd(owd)


    con3 <- unz(ZIPFILE, basename(FILE3))
    on.exit(close(con3), add = TRUE, after = FALSE)


    fun(try(knitr::knit(print(con3), FILE4, quiet = TRUE)))
    this.path:::cat.file(FILE4, number = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE)


    con4 <- gzcon(file(file3, "rb"))
    on.exit(close(con4), add = TRUE, after = FALSE)
    fun(knitr::knit(print(con4), FILE4, quiet = TRUE))
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
})
sink(type = "message")
sink()
close(SINKCON)
# this.path:::cat.file(SINKFILE)
essentials::file.open(SINKFILE)





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
