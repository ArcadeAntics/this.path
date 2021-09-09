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


splitter.inator <- function (x, sep = " ",
    sepchar = nchar(sep, type = "bytes", keepNA = FALSE),
    w = nchar(x, type = "width"))
{
    if (identical(sep, "\n"))
        return(x)
    value <- character()
    while (len <- length(x)) {
        cumw <- if (len > 40L)
            cumsum(w[seq_len(40L)]) + seq.int(0L, by = sepchar, length.out = 40L)
        else cumsum(w)              + seq.int(0L, by = sepchar, length.out = len)
        i <- seq_len(min(10L, max(1L, sum(cumw <= 80L))))
        value <- c(value, paste(x[i], collapse = sep))
        x <- x[-i]
        w <- w[-i]
    }
    return(value)
}





format4scan <- function (x, sep = " ", comment.char = "", nlines.between.comment.and.args = 0,
    nlines.between.args = 2)
{
    if (is.character(sep) || is.null(sep)) {
        if (length(sep) == 0) {
            sep <- " "
            sepchar <- FALSE
        }
        else {
            sep <- sep[[1L]]
            sepchar <- nchar(sep, type = "bytes", keepNA = FALSE)
            if (!sepchar)
                sep <- " "
            else if (sepchar > 1)
                stop("invalid 'sep' value: must be one byte")
        }
    }
    else stop("invalid 'sep' argument")
    qstring <- if (sepchar) "\"\"" else "\\\""


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


        xx <- paste0("\"", gsub("\"",
            qstring, enc2utf8(xx), fixed = TRUE), "\"",
            recycle0 = TRUE)
        xx <- paste(splitter.inator(xx, sep), collapse = "\n")


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


format4parse <- function (x, comment.char = "#", nlines.between.comment.and.args = 0,
    nlines.between.args = 2)
{
    if (!is.character(comment.char) || length(comment.char) != 1)
        stop("invalid 'comment.char' argument")
    comchar <- nchar(comment.char, type = "bytes", keepNA = FALSE)


    fun <- function(xx) {


        com <- as.comment(comment(xx), comment.char = comment.char, comchar = comchar)
        xx <- asArgs(xx)
        if (any(Encoding(xx) == "bytes"))
            stop("strings with \"bytes\" encoding is not allowed")


        xx <- paste(encodeString(enc2utf8(xx), quote = "\""), collapse = "\n")


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





setReadWriteArgsMethod <- function (condition, read, write)
{
    readWriteArgs[[length(readWriteArgs) + 1L]] <<- list(
        condition = match.fun(condition),
        read      = match.fun(read),
        write     = match.fun(write)
    )
    invisible()
}
environment(setReadWriteArgsMethod) <- list2env(list(readWriteArgs = list()))


# *.csv file, comma separated value file
setReadWriteArgsMethod(
    condition = function(file) {
        grepl("\\.csv$", file, ignore.case = TRUE)
    },
    read      = function(file) {
        scan(file = file, what = "", sep = ",", quote = "\"",
            dec = ".", na.strings = NULL, quiet = TRUE,
            comment.char = "", encoding = "UTF-8")
    },
    write     = function(x, comments = TRUE,
        nlines.between.comment.and.args = 0, nlines.between.args = 2) {
        format4scan(x, sep = ",", comment.char = "",
            nlines.between.comment.and.args = nlines.between.comment.and.arg,
            nlines.between.args = nlines.between.args)
    }
)


# *.pyargs file, python arguments file
setReadWriteArgsMethod(
    condition = function(file) {
        grepl("\\.pyargs$", file, ignore.case = TRUE)
    },
    read      = function(file) {
        readLines(file, warn = FALSE, encoding = "UTF-8")
    },
    write     = function(x, comments = TRUE,
        nlines.between.comment.and.args = 0, nlines.between.args = 2) {
        x <- asArgs(x)
        if (any(Encoding(x) == "bytes"))
            stop("strings with \"bytes\" encoding is not allowed")
        paste(enc2utf8(x), collapse = "\n")
    }
)


# *.Rargs file, R arguments file
setReadWriteArgsMethod(
    condition = function(file) {
        grepl("\\.Rargs$", file, ignore.case = TRUE)
    },
    read      = function(file) {
        value <- parse(file = file, keep.source = FALSE, encoding = "UTF-8")
        if (any(vapply(value, typeof, "") != "character"))
            stop("invalid 'file', does not contain exclusively strings")
        as.character(value)
    },
    write     = function(x, comments = TRUE,
        nlines.between.comment.and.args = 0, nlines.between.args = 2) {
        format4parse(x, comment.char = if (comments) "#" else "",
            nlines.between.comment.and.args = nlines.between.comment.and.args,
            nlines.between.args = nlines.between.args)
    }
)


# *.tsv file, tab separated value file
setReadWriteArgsMethod(
    condition = function(file) {
        grepl("\\.tsv$", file, ignore.case = TRUE)
    },
    read      = function(file) {
        scan(file = file, what = "", sep = "\t", quote = "\"",
            dec = ".", na.strings = NULL, quiet = TRUE,
            comment.char = "", encoding = "UTF-8")
    },
    write     = function(x, comments = TRUE,
        nlines.between.comment.and.args = 0, nlines.between.args = 2) {
        format4scan(x, sep = "\t", comment.char = "",
            nlines.between.comment.and.args = nlines.between.comment.and.args,
            nlines.between.args = nlines.between.args)
    }
)


environment(setReadWriteArgsMethod)$readWriteArgsdefault <- list(
    read  = function(file) {
        scan(file = file, what = "", sep = "", quote = "\"'",
            dec = ".", na.strings = NULL, quiet = TRUE,
            comment.char = "#", encoding = "UTF-8")
    },
    write = function(x, comments = TRUE,
        nlines.between.comment.and.args = 0, nlines.between.args = 2) {
        format4scan(x, sep = "", comment.char = if (comments) "#" else "",
            nlines.between.comment.and.args = nlines.between.comment.and.args,
            nlines.between.args = nlines.between.args)
    }
)





selectReadWriteArgsMethod <- function (file)
{
    if (!is.character(file))
        return(readWriteArgsdefault)
    for (method in readWriteArgs) {
        if (method$condition(file))
            return(method)
    }
    return(readWriteArgsdefault)
}
environment(selectReadWriteArgsMethod) <- environment(setReadWriteArgsMethod)




writeArgs <- function (x, file = tempfile(pattern = pattern, fileext = fileext),
    pattern = "args", fileext = ".Rargs", comments = TRUE,
    nlines.between.comment.and.args = 0, nlines.between.args = 2,
    at = TRUE)
{
    text <- selectReadWriteArgsMethod(file)$write(x, comments = comments,
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
selectReadWriteArgsMethod(file)$read(file)
