asArgs <- function (...)
{
    n <- nargs()


    # if no arguments are provided, return character(0)
    if (!n) return(character())


    # optimize the most common case:
    # exactly one argument is supplied being:
    # * NULL
    # * atomic object without class
    # * zero-length object without class
    else if (n == 1L) {
        if (is.null(..1)) return(character())
        else if (is.object(..1)) {}
        else if (is.numeric(..1) || is.complex(..1)) return(format.default(..1, trim = TRUE, digits = 17L, drop0trailing = TRUE))
        else if (is.raw(..1)) return(sprintf("0x%02x", as.integer(..1)))
        else if (is.logical(..1) || is.character(..1)) {
            value <- as.character(..1)
            if (anyNA(value))
                value[is.na(value)] <- "NA"
            return(value)
        }
        else if (!length(..1)) return(character())
    }


    value <- rapply(list(...), function(xx) {


        if (is.null(xx)) character()


        # if the object has a class, use its `as.character` method
        # for classes "factor" and "POSIXt", do not method dispatch
        else if (is.object(xx)) {
            if (inherits(xx, "factor")) as.character.factor(xx)


            # we will lose information smaller than 1 microsecond,
            # but what can you do?
            else if (inherits(xx, "POSIXct"))
                format.POSIXct(xx, format = "%Y-%m-%d %H:%M:%OS6")
            else if (inherits(xx, "POSIXlt"))
                format.POSIXlt(xx, format = "%Y-%m-%d %H:%M:%OS6")
            else as.character(xx)
        }


        # for numeric / / complex
        # * keep all digits
        # * remove trailing 0
        #
        # this means numbers will be accurate when converted back
        #
        # x <- c(pi, exp(1))
        # as.numeric(as.character(x)) == x
        # as.numeric(this.path:::asArgs(x)) == x
        #
        # x <- complex(real = beta(5, 6), imaginary = besselI(5, 1))
        # as.complex(as.character(x)) == x
        # as.complex(this.path:::asArgs(x)) == x
        else if (is.numeric(xx) || is.complex(xx))
            format.default(xx, trim = TRUE, digits = 17L, drop0trailing = TRUE)


        # for raw, format as usual, but put "0x" at the start
        #
        # x <- as.raw(0:255)
        # as.raw(as.character(x)) == x
        # as.raw(this.path:::asArgs(x)) == x
        else if (is.raw(xx))
            sprintf("0x%02x", as.integer(xx))


        # treat a pairlist like a list
        else if (is.pairlist(xx))
            asArgs(as.list(xx))


        # turn into character
        else as.character(xx)
    }, how = "replace")
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
    value[is.na(value)] <- "NA"
    return(value)
}


as.comment <- function (x, comment.char = "",
    comchar = nchar(comment.char, type = "bytes", keepNA = FALSE))
{
    if (comchar && nzchar(x <- paste(x, collapse = "\n"))) {
        x <- regmatches(x, gregexpr("\n", x, fixed = TRUE),
            invert = TRUE)[[1L]]
        paste(comment.char, x, collapse = "\n", recycle0 = TRUE)
    }
    else ""
}


splitter.inator <- function (x, sep = "",
    sepchar = nchar(sep, type = "bytes", keepNA = FALSE),
    w = nchar(x, type = "width"))
{
    if (!length(x))
        return(character())
    cumw <- cumsum(w) + seq.int(0L, by = sepchar, along.with = x)
    i <- seq_len(min(10L, max(1L, sum(cumw <= 80L))))
    c(paste(x[i], collapse = sep),
        splitter.inator(x[-i], sep = sep, sepchar = sepchar, w = w[-i]))
}





format4scan <- function (x, sep = "", comment.char = "", nlines.between.comment.and.args = 0,
    nlines.between.args = 2)
{
    if (is.character(sep) || is.null(sep)) {
        if (length(sep) == 0) sepchar <- FALSE
        else {
            sep <- sep[[1L]]
            sepchar <- nchar(sep, type = "bytes", keepNA = FALSE)
            if (sepchar > 1)
                stop("invalid 'sep' value: must be one byte")
        }
    }
    else stop("invalid 'sep' argument")


    if (!is.character(comment.char) || length(comment.char) != 1)
        stop("invalid 'comment.char' argument")
    comchar <- nchar(comment.char, type = "bytes", keepNA = FALSE)
    if (comchar > 1)
        stop("invalid 'comment.char' argument")


    fun <- function(xx) {


        com <- as.comment(comment(xx), comment.char = comment.char, comchar = comchar)
        xx <- asArgs(xx)
        if (any(Encoding(xx) == "bytes"))
            stop("strings with \"bytes\" encoding is not allowed")


        xx <- if (sepchar)
            paste0(
                "\"",
                gsub("\"", "\"\"", encodeString(enc2utf8(xx)), fixed = TRUE, useBytes = TRUE),
                "\"",
                recycle0 = TRUE)
        else encodeString(enc2utf8(xx), quote = "\"")
        xx <- paste(splitter.inator(xx, sep = sep, sepchar = sepchar), collapse = "\n")


        paste(com, xx, sep = if (nzchar(com) && nzchar(xx))
            paste0("\n", strrep("\n", nlines.between.comment.and.args))
        else "")
    }


    if (is.list(x)) {
        value <- c(as.comment(comment(x)), vapply(x, fun, ""))
        paste(value[nzchar(value)], collapse = paste0("\n", strrep("\n",
            nlines.between.args)))
    }
    else fun(x)
}


writeArgs <- function (x, file = tempfile("args"), comments = TRUE,
    nlines.between.comment.and.args = 0, nlines.between.args = 2,
    at = TRUE)
{
    text <- format4scan(x, sep = ",", comment.char = if (comments) "#" else "",
        nlines.between.comment.and.args = nlines.between.comment.and.args,
        nlines.between.args = nlines.between.args)
    if (is.null(file)) {
        text
    }
    else if (is.character(file)) {
        if (nzchar(file)) {
            file <- normalizePath(file, mustWork = FALSE)
            writeLines(text, file, useBytes = TRUE)
            if (at)
                paste0("@", file)
            else file
        }
        else {
            writeLines(text, useBytes = TRUE)
            invisible(text)
        }
    }
    else writeLines(text, file, useBytes = TRUE)
 }


readArgs <- function (file)
{
    scan(file = file, what = "", sep = ",", quote = "\"",
        na.strings = NULL, quiet = TRUE, comment.char = "#",
        allowEscapes = TRUE, encoding = "UTF-8")
}





format4parse <- function (x, sep = " ; ", comment.char = "#", nlines.between.comment.and.args = 0,
    nlines.between.args = 2)
{
    if (is.character(sep) || is.null(sep)) {
        if (length(sep) == 0) sepchar <- FALSE
        else {
            sep <- sep[[1L]]
            sepchar <- nchar(sep, type = "bytes", keepNA = FALSE)
        }
    }
    else stop("invalid 'sep' argument")


    if (!is.character(comment.char) || length(comment.char) != 1)
        stop("invalid 'comment.char' argument")
    comchar <- nchar(comment.char, type = "bytes", keepNA = FALSE)


    fun <- function(xx) {


        com <- as.comment(comment(xx), comment.char = comment.char, comchar = comchar)
        xx <- asArgs(xx)
        if (any(Encoding(xx) == "bytes"))
            stop("strings with \"bytes\" encoding is not allowed")


        xx <- encodeString(enc2utf8(x), quote = "\"")
        xx <- paste(splitter.inator(xx, sep = sep, sepchar = sepchar), collapse = "\n")


        paste(com, xx, sep = if (nzchar(com) && nzchar(xx))
            paste0("\n", strrep("\n", nlines.between.comment.and.args))
        else "")
    }


    if (is.list(x)) {
        value <- c(as.comment(comment(x)), vapply(x, fun, ""))
        paste(value[nzchar(value)], collapse = paste0("\n", strrep("\n",
            nlines.between.args)))
    }
    else fun(x)
}


writeArgs2 <- function (x, file = tempfile("args"), sep = TRUE, comments = TRUE,
    nlines.between.comment.and.args = 0, nlines.between.args = 2,
    at = TRUE)
{
    text <- format4parse(x, sep = if (sep) " ; " else "\n",
        comment.char = if (comments) "#" else "",
        nlines.between.comment.and.args = nlines.between.comment.and.args,
        nlines.between.args = nlines.between.args)
    if (is.null(file)) {
        text
    }
    else if (is.character(file)) {
        if (nzchar(file)) {
            file <- normalizePath(file, mustWork = FALSE)
            writeLines(text, file, useBytes = TRUE)
            if (at)
                paste0("@", file)
            else file
        }
        else {
            writeLines(text, useBytes = TRUE)
            invisible(text)
        }
    }
    else writeLines(text, file, useBytes = TRUE)
}


readArgs2 <- function (file)
{
    value <- parse(file = file, keep.source = FALSE, encoding = "UTF-8")
    if (any(vapply(value, typeof, "") != "character"))
        stop("invalid 'file', does not contain exclusively strings")
    as.character(value)
}




# x <- rep(list(letters), 3)
# comment(x[[1]]) <- "test"
# comment(x[[3]]) <- "more"
# comment(x) <- "what"
# cat(format4scan(x, sep = ",", comment.char = "#"))





# fun <- function(n) 1:(n - 1)
# x <- as.character(str2expression(sprintf(r"("\x%X")", fun(256))))
# grep("[[:alpha:]]", x, value = TRUE)
# a <- as.character(str2expression(sprintf(r"("\u{%X}")", fun(2^16))))
# grep("[[:alpha:]]", a, value = TRUE)
# FILE <- writeArgs(x, at = FALSE)
# y <- scan(FILE, what = "", sep = ",", quote = "\"", na.strings = NULL,
#     quiet = TRUE, comment.char = "#", allowEscapes = TRUE, encoding = "UTF-8")
# which(x != y)
# as.raw(which(x != y))
#
#
# names(x) <- seq_along(x)
# for (n in names(x)) {
#     invisible(Rterm(
#         c("--no-echo", "--no-restore"),
#         exprs = substitute(commandArgs(TRUE)),
#         args = x[n],
#         quiet = TRUE
#     ))
# }





# this.path::Rscript(exprs = strsplit(this.path::dedent(r"----{
#     x <- "\u{03C3}"
#     y <- "s"
#     print(x)
#     print(y)
#     x == y
# }----"), "\n")[[1L]], exprs.literal = TRUE)


if (FALSE)
{
    oargs <- essentials::ASCII(plot = FALSE)

    oargs <- c(
        essentials::ASCII(plot = FALSE),
        local({
            temp <- sprintf("%04x", c(1:55295, 57344:65533))
            `names<-`(as.character(str2expression(paste0("\"\\u", temp, "\""))), temp)
        }),
        local({
            temp <- sprintf("%08x", 65536:1114111)
            `names<-`(as.character(str2expression(paste0("\"\\U", temp, "\""))), temp)
        })
    )
    oargs <- oargs[1:63740]
    oargs <- oargs[1:1000]

    # oargs <- "\xe7"
    # Encoding(oargs) <- "latin1"


    FILE2 <- tempfile("args")
    this.path:::writeArgs2(oargs, FILE2)
    args <- this.path:::readArgs2(FILE2)
    if (length(oargs) != length(args))
        stop("problem")
    i <- which(args != oargs)
    print(cbind(oargs[i], args[i]), quote = FALSE)


    FILE <- tempfile("args")
    this.path:::writeArgs(oargs, FILE)
    args <- this.path:::readArgs(FILE)
    if (length(oargs) != length(args))
        stop("problem")
    i <- which(args != oargs)
    print(cbind(oargs[i], args[i]), quote = FALSE)
}
