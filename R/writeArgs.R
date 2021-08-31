asArgs <- function (...)
{
    # optimize the most common case:
    # exactly one argument is supplied being:
    # * NULL
    # * atomic object without class
    # * zero-length object without class
    if (...length() == 1L) {
        if (is.null(..1)) return(character())
        else if (is.object(..1)) {}
        else if (is.numeric(..1) || is.complex(..1)) return(format.default(..1, trim = TRUE, digits = 17L, drop0trailing = TRUE))
        else if (is.raw(..1)) return(sprintf("0x%02x", as.integer(..1)))
        else if (is.logical(..1) || is.character(..1)) {
            value <- if (is.logical(..1))
                as.character(..1)
            else ..1
            if (anyNA(value))
                value[is.na(value)] <- "NA"
            return(value)
        }
        else if (!length(..1)) return(character())
    }


    value <- rapply(list(...), function(xx) {


        if (is.null(xx))
            character()


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
    })
    attributes(value) <- NULL
    value[is.na(value)] <- "NA"
    return(value)
}


format4scan <- function (x, sep = "", comment.char = "", nlines.between.comment.and.args = 0,
    nlines.between.args = 2)
{
    if (is.character(sep) || is.null(sep)) {
        if (length(sep) == 0) sepchar <- FALSE
        else {
            sc <- sep[[1L]]
            sepchar <- nchar(sc, type = "bytes", keepNA = FALSE)
            if (sepchar > 1)
                stop("invalid 'sep' value: must be one byte")
        }
    }
    else stop("invalid 'sep' argument")


    if (!is.character(comment.char) || length(comment.char) != 1)
        stop("invalid 'comment.char' argument")
    comchar <- FALSE
    nc <- nchar(comment.char, type = "bytes", keepNA = FALSE)
    if (nc > 1)
        stop("invalid 'comment.char' argument")
    else if (nc == 1)
        comchar <- TRUE


    as.comment <- function(com) {
        if (comchar && nzchar(com <- paste(com, collapse = "\n"))) {
            com <- regmatches(com, gregexpr("\n", com, fixed = TRUE),
                invert = TRUE)[[1L]]
            paste(comment.char, com, collapse = "\n", recycle0 = TRUE)
        }
        else ""
    }
    nchar2 <- function(x) {
        value <- integer(length(x))
        bytes <- Encoding(x) == "bytes"
        value[!bytes] <- nchar(x[!bytes], type = "width")
        value[bytes] <- nchar(x[bytes], type = "bytes")
        return(value)
    }
    splitter.inator <- function(x) {
        if (!length(x)) return(NULL)
        cumw <- cumsum(nchar2(x)) + 0L:(length(x) - 1L) * sepchar
        n <- min(10L, max(1L, sum(cumw <= 80L)))
        c(paste(x[seq_len(n)], collapse = sc), splitter.inator(x[-seq_len(n)]))
    }
    encodeString2 <- function(x) {
        if (!length(x))
            return(character())
        x <- gsub("\\"  , "\\\\" , x, fixed = TRUE, useBytes = TRUE)
        x <- gsub("\a"  , "\\a"  , x, fixed = TRUE, useBytes = TRUE)
        x <- gsub("\b"  , "\\b"  , x, fixed = TRUE, useBytes = TRUE)
        x <- gsub("\t"  , "\\t"  , x, fixed = TRUE, useBytes = TRUE)
        x <- gsub("\n"  , "\\n"  , x, fixed = TRUE, useBytes = TRUE)
        x <- gsub("\v"  , "\\v"  , x, fixed = TRUE, useBytes = TRUE)
        x <- gsub("\f"  , "\\f"  , x, fixed = TRUE, useBytes = TRUE)
             gsub("\r"  , "\\r"  , x, fixed = TRUE, useBytes = TRUE)
    }


    fun <- function(xx) {


        com <- as.comment(comment(xx))
        xx <- asArgs(xx)
        bytes <- Encoding(xx) == "bytes"


        if (sepchar) {
            xx[!bytes] <- encodeString(enc2utf8(xx[!bytes]))
            xx[bytes] <- encodeString2(xx[bytes])
            xx        <- paste0("\"", gsub("\"", "\"\"", xx,
                fixed = TRUE, useBytes = TRUE), "\"", recycle0 = TRUE)
        }
        else {
            xx[!bytes] <- encodeString(enc2utf8(xx[!bytes]), quote = "\"")
            xx[bytes] <- paste0("\"", gsub("\"", "\\\"", encodeString2(xx[bytes]),
                fixed = TRUE, useBytes = TRUE), "\"", recycle0 = TRUE)
        }
        xx <- paste(splitter.inator(xx), collapse = "\n")


        paste(com, xx, sep = if (nzchar(com) && nzchar(xx))
            paste0("\n", strrep("\n", nlines.between.comment.and.args))
        else "")
    }
    if (is.list(x)) {
        value <- c(as.comment(comment(x)), vapply(x, "fun", ""))
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
    else if (nzchar(file)) {
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


writeArgs2 <- function (x, file = tempfile("args"), at = TRUE)
{
    text <- asArgs(x)
    if (any(Encoding(text) == "bytes"))
        stop("strings with \"bytes\" encoding is not allowed")
    text <- encodeString(enc2utf8(text), quote = "\"")
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


if (FALSE) {
    FILE <- tempfile("args")

    oargs <- ASCII(plot = FALSE)

    oargs <- c(
        local({
            temp <- sprintf("%04x", c(1:55295, 57344:65535))
            `names<-`(as.character(str2expression(paste0("\"\\u", temp, "\""))), temp)
        }),
        local({
            temp <- sprintf("%08x", 65536:1114111)
            `names<-`(as.character(str2expression(paste0("\"\\U", temp, "\""))), temp)
        })
    )


    this.path:::writeArgs2(oargs, FILE)
    args <- parse(FILE, keep.source = FALSE, encoding = "UTF-8")
    if (any(vapply(args, typeof, "") != "character"))
        stop("invalid 'file', does not contain exclusively strings")
    args <- as.character(args)
    if (length(oargs) != length(args))
        stop("problem")
    data.frame(oargs, args)[args != oargs, , drop = FALSE]


    FILE <- tempfile("args")
    this.path:::writeArgs(oargs, FILE)
    args <- scan(FILE, what = "", sep = ",",
        quote = "\"", na.strings = NULL, quiet = TRUE,
        comment.char = "#", allowEscapes = TRUE, encoding = "UTF-8")
    which(args != oargs)


}
