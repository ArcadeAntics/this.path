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
        if (is.null(..1))
            return(character())
        else if (is.object(..1)) {
        }
        else if (is.numeric(..1) || is.complex(..1))
            return(format.default(..1, trim = TRUE, digits = 17L,
                decimal.mark = ".", drop0trailing = TRUE))
        else if (is.raw(..1))
            return(sprintf("0x%02x", as.integer(..1)))
        else if (is.logical(..1) || is.character(..1)) {
            value <- as.character(..1)
            if (anyNA(value)) value[is.na(value)] <- "NA"
            return(value)
        }
        else if (!length(..1))
            return(character())
    }


    value <- rapply(list(...), function(xx) {


        if (is.null(xx)) character()


        # if the object has a class, use its `as.character` method
        # for classes "factor" and "POSIXt", do not method dispatch
        else if (is.object(xx)) {
            if (inherits(xx, "factor"))
                as.character.factor(xx)


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
            format.default(xx, trim = TRUE, digits = 17L,
                decimal.mark = ".", drop0trailing = TRUE)


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
    if (anyNA(value)) value[is.na(value)] <- "NA"
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





format4scan <- function (x, sep = " ", quote = "\"'", comment.char = "",
    allowEscapes = FALSE, nlines.between.comment.and.args = 0,
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


    if (is.character(quote)) {
        quote <- quote[[1L]]
        if (nzchar(quote))
            quote <- substr(as.character(as.symbol(quote)), 1L, 1L)
    }
    else if (is.null(quote))
        quote <- ""
    else stop(gettext("invalid quote symbol set"))


    qstring <- if (sepchar) strrep(quote, 2L) else paste0("\\", quote)


    if (!is.character(comment.char) || length(comment.char) != 1)
        stop("invalid 'comment.char' argument")
    comchar <- nchar(comment.char, type = "bytes", keepNA = FALSE)
    if (comchar > 1)
        stop("invalid 'comment.char' argument")


    allowEscapes <- as.logical(allowEscapes)[1L]
    if (is.na(allowEscapes))
        stop(gettextf("invalid '%s' argument", "allowEscapes"))


    fun <- function(xx) {


        com <- as.comment(comment(xx), comment.char = comment.char, comchar = comchar)
        xx <- asArgs(xx)
        if (any(Encoding(xx) == "bytes"))
            stop("strings with \"bytes\" encoding is not allowed")


        xx <- enc2utf8(xx)
        if (allowEscapes) {
            xx <- gsub("\\"  , "\\\\" , xx, fixed = TRUE)
            xx <- gsub("\001", "\\001", xx, fixed = TRUE)
            xx <- gsub("\002", "\\002", xx, fixed = TRUE)
            xx <- gsub("\003", "\\003", xx, fixed = TRUE)
            xx <- gsub("\004", "\\004", xx, fixed = TRUE)
            xx <- gsub("\005", "\\005", xx, fixed = TRUE)
            xx <- gsub("\006", "\\006", xx, fixed = TRUE)
            xx <- gsub("\a"  , "\\a"  , xx, fixed = TRUE)
            xx <- gsub("\b"  , "\\b"  , xx, fixed = TRUE)
            xx <- gsub("\t"  , "\\t"  , xx, fixed = TRUE)
            xx <- gsub("\n"  , "\\n"  , xx, fixed = TRUE)
            xx <- gsub("\v"  , "\\v"  , xx, fixed = TRUE)
            xx <- gsub("\f"  , "\\f"  , xx, fixed = TRUE)
            xx <- gsub("\r"  , "\\r"  , xx, fixed = TRUE)
            xx <- gsub("\016", "\\016", xx, fixed = TRUE)
            xx <- gsub("\017", "\\017", xx, fixed = TRUE)
            xx <- gsub("\020", "\\020", xx, fixed = TRUE)
            xx <- gsub("\021", "\\021", xx, fixed = TRUE)
            xx <- gsub("\022", "\\022", xx, fixed = TRUE)
            xx <- gsub("\023", "\\023", xx, fixed = TRUE)
            xx <- gsub("\024", "\\024", xx, fixed = TRUE)
            xx <- gsub("\025", "\\025", xx, fixed = TRUE)
            xx <- gsub("\026", "\\026", xx, fixed = TRUE)
            xx <- gsub("\027", "\\027", xx, fixed = TRUE)
            xx <- gsub("\030", "\\030", xx, fixed = TRUE)
            xx <- gsub("\031", "\\031", xx, fixed = TRUE)
            xx <- gsub("\032", "\\032", xx, fixed = TRUE)
            xx <- gsub("\033", "\\033", xx, fixed = TRUE)
            xx <- gsub("\034", "\\034", xx, fixed = TRUE)
            xx <- gsub("\035", "\\035", xx, fixed = TRUE)
            xx <- gsub("\036", "\\036", xx, fixed = TRUE)
            xx <- gsub("\037", "\\037", xx, fixed = TRUE)
            xx <- gsub("\177", "\\177", xx, fixed = TRUE)
        }
        if (nzchar(quote))
            xx <- paste0(
                quote,
                gsub(quote, qstring, xx, fixed = TRUE),
                quote,
                recycle0 = TRUE
            )
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





scan2 <- function (..., sep = "", quote = if (identical(sep, "\n")) "" else "'\"",
    comment.char = "", allowEscapes = FALSE)
{
    scan(..., what = "", sep = sep, quote = quote, dec = ".",
        na.strings = NULL, quiet = TRUE, comment.char = comment.char,
        allowEscapes = allowEscapes, encoding = "UTF-8")
}


has.ext <- function (file, fileext, compression = FALSE, ignore.case = TRUE)
{
    grepl(
        paste0(
            ".",
            fileext,
            ifelse(compression, "(\\.([gG][zZ]|[bB][zZ]2|[xX][zZ]))?", ""),
            "$",
            recycle0 = TRUE
        ),
        basename(file),
        ignore.case = ignore.case
    )
}





setReadWriteArgsMethod <- function (name, condition, read, write)
{
    name <- as.character(as.symbol(name))
    if (name %in% lockedMethods)
        stop(gettextf("unable to overwrite read/writeArgs method '%s'",
            name))
    methods[[name]] <<- list(
        condition = match.fun(condition),
        read      = match.fun(read),
        write     = match.fun(write)
    )
    invisible()
}
environment(setReadWriteArgsMethod) <- list2env(list(
    methods       = list(),
    lockedMethods = character(),
    default       = list(
        read  = function(file) {
            scan2(file = file, sep = "", quote = "\"'", comment.char = "#")
        },
        write = function(x, comments = TRUE,
            nlines.between.comment.and.args = 0,
            nlines.between.args = 2) {
            format4scan(x, sep = "", comment.char = if (comments) "#" else "",
                nlines.between.comment.and.args = nlines.between.comment.and.args,
                nlines.between.args = nlines.between.args)
        }
    )
))


# *.Rargs file, R arguments file
setReadWriteArgsMethod(
    name      = "Rargs",
    condition = function(file) has.ext(file, "\\.Rargs", compression = TRUE),
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


# *.pyargs file, python arguments file
setReadWriteArgsMethod(
    name      = "pyargs",
    condition = function(file) has.ext(file, "\\.pyargs", compression = TRUE),
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


# *.csv file, comma separated value file
setReadWriteArgsMethod(
    name      = "csv",
    condition = function(file) has.ext(file, "\\.csv", compression = TRUE),
    read      = function(file) {
        scan2(file = file, sep = ",", quote = "\"", comment.char = "")
    },
    write     = function(x, comments = TRUE,
        nlines.between.comment.and.args = 0, nlines.between.args = 2) {
        format4scan(x, sep = ",", comment.char = "",
            nlines.between.comment.and.args = nlines.between.comment.and.arg,
            nlines.between.args = nlines.between.args)
    }
)


# *.tsv file, tab separated value file
setReadWriteArgsMethod(
    name      = "tsv",
    condition = function(file) has.ext(file, "\\.(tsv|tab)", compression = TRUE),
    read      = function(file) {
        scan2(file = file, sep = "\t", quote = "\"", comment.char = "")
    },
    write     = function(x, comments = TRUE,
        nlines.between.comment.and.args = 0, nlines.between.args = 2) {
        format4scan(x, sep = "\t", comment.char = "",
            nlines.between.comment.and.args = nlines.between.comment.and.args,
            nlines.between.args = nlines.between.args)
    }
)





environment(setReadWriteArgsMethod)$lockedMethods <-
    names(environment(setReadWriteArgsMethod)$methods)





selectReadWriteArgsMethod <- function (file, name = NULL)
{
    if (is.null(file) || !nzchar(file))
        return(if (is.null(name))
            default
        else methods[[match.arg(name, names(methods))]])
    for (method in methods) {
        if (method$condition(file))
            return(method)
    }
    return(default)
}
environment(selectReadWriteArgsMethod) <- environment(setReadWriteArgsMethod)





writeArgs <- function (x, file = tempfile(pattern = pattern, fileext = fileext),
    pattern = "args", fileext = ".Rargs", comments = TRUE,
    nlines.between.comment.and.args = 0, nlines.between.args = 2,
    at = TRUE, name = NULL)
{
    text <- selectReadWriteArgsMethod(file, name)$write(x, comments = comments,
        nlines.between.comment.and.args = nlines.between.comment.and.args,
        nlines.between.args = nlines.between.args)
    if (is.null(file)) {
        text
    }
    else if (nzchar(file)) {
        fun <- if (grepl("^(ftp|http|https|file)://", file)) base::file
        else {
            file <- normalizePath(file, mustWork = FALSE)
            if (grepl(".\\.gz$", basename(file), ignore.case = TRUE))
                gzfile
            else if (grepl(".\\.bz2$", basename(file), ignore.case = TRUE))
                bzfile
            else if (grepl(".\\.xz$", basename(file), ignore.case = TRUE))
                xzfile
            else base::file
        }
        con <- fun(file, "w")
        on.exit(close(con))
        writeLines(text, con, useBytes = TRUE)
        if (at)
            paste0("@", file)
        else file
    }
    else {
        cat(text, file = stdout(), sep = "\n")
        invisible(text)
    }
}


readArgs <- function (file, name = NULL)
selectReadWriteArgsMethod(file, name)$read(file)





if (FALSE)
{
    X <- c(
        essentials::ASCII(plot = FALSE),
        str2expression(sprintf("\"\\u%04x\"", c(256:55295, 57344:65533)))
    )
    X <- this.path:::asArgs(X)
    Y <- setdiff(X, c("\b", "\f", "\r", "\033"))
    Z <- setdiff(X, c("\n", "\r"))
    this.path::writeArgs(X, file = "", name = "Rargs")
    this.path::writeArgs(Y, file = "", name = "pyargs")
    this.path::writeArgs(Y, file = "", name = "csv")
    this.path::writeArgs(Y, file = "", name = "tsv")


    FILES <- tempfile(fileext = paste0(c(".Rargs", ".pyargs", ".csv", ".tsv", ""), ".gz"))
    lapply(FILES, this.path::writeArgs, x = X)
    lapply(FILES, function(...) all.equal(this.path::readArgs(...), X))
    lapply(FILES, this.path::writeArgs, x = Z)
    lapply(FILES, function(...) all.equal(this.path::readArgs(...), Z))
}
