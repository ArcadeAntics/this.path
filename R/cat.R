.cat.condition <- function (c, outfile = stderr())
{
    # c <- simpleError("testing th\U{0001D11E}s\n out", quote(5 + 6)); warning("comment out this later")


    deparse1s <- function(call) deparse(call, backtick = TRUE, nlines = 1L)
    wd <- function(x) {
        value <- nchar(x, "width", allowNA = TRUE)
        if (any(i <- is.na(value)))
            value[i] <- nchar(x[i], "bytes")
        value
    }
    strlen <- function(x) nchar(x, "bytes")
    strchr <- function(text, pattern) regexpr(pattern, text, fixed = TRUE, useBytes = TRUE)
    snprintf <- function(n, fmt, ..., printTrunc = FALSE) {
        buf <- sprintf(fmt = fmt, ...)
        if (strlen(buf) >= n) {
            buf <- substr(buf, 1L, sum(cumsum(strlen(strsplit(buf, NULL)[[1L]])) < n))
            if (printTrunc) {
                msg <- gettext("[... truncated]", domain = "R")
                if (strlen(buf) + 1L + strlen(msg) < BUFSIZE) {
                    buf <- rawToChar(c(charToRaw(buf), charToRaw(" "), charToRaw(msg)))
                }
            }
        }
        buf
    }
    strncat <- function(n, x, y) {
        buf <- rawToChar(c(charToRaw(x), charToRaw(y)))
        if (strlen(buf) > n)
            buf <- substr(buf, 1L, sum(cumsum(strlen(strsplit(buf, NULL)[[1L]])) <= n))
        buf
    }
    LONGWARN <- 75L
    BUFSIZE <- 8192L
    R_WarnLength <- 1000L
    if (inherits(c, "warning")) {
        call <- conditionCall(c)
        format <- "%s"
        ap <- conditionMessage(c)
        dcall <- if (!is.null(call)) {
            deparse1s(call)
        } else ""
        psize <- min(BUFSIZE, R_WarnLength + 1)
        buf <- snprintf(psize, format, ap, printTrunc = TRUE)
        buf <- c(
            if (nzchar(dcall)) {
                c(
                    gettextf("Warning in %s :", dcall, domain = "R"),
                    if (!(( .mbcslocale && 18L + wd(dcall) + wd(buf) <= LONGWARN) ||
                          (!.mbcslocale && 18L + strlen(dcall) + strlen(buf) <= LONGWARN)))
                        "\n "
                )
            } else gettext("Warning:", domain = "R"),
            sprintf(" %s\n", buf)
        )
    }
    else if (inherits(c, "error")) {
        call <- conditionCall(c)
        format <- "%s"
        ap <- conditionMessage(c)
        msg_len <- min(BUFSIZE, R_WarnLength) + 1
        if (!is.null(call)) {
            head <- gettext("Error in ", domain = "R", trim = FALSE); tail <- "\n  "
            dcall <- deparse1s(call)
            tmp2 <- snprintf(BUFSIZE, "%s", head)
            tmp <- snprintf(max(msg_len - strlen(head), 0L), format, ap)
            if (strlen(tmp2) + strlen(tail) + strlen(tmp) < BUFSIZE) {
                errbuf <- snprintf(BUFSIZE, gettext("Error in %s : ", domain = "R", trim = FALSE), dcall)
                if (.mbcslocale) {
                    msgline1 <- strchr(tmp, "\n")
                    msgline1 <- if (msgline1 != -1L) wd(rawToChar(charToRaw(tmp)[seq_len(msgline1 - 1L)])) else wd(tmp)
                    if (14L + strlen(dcall) + msgline1 > LONGWARN)
                        errbuf <- strncat(BUFSIZE, errbuf, tail)
                } else {
                    msgline1 <- strchr(tmp, "\n")
                    msgline1 <- if (msgline1 != -1L) msgline1 - 1L else strlen(tmp)
                    if (14L + strlen(dcall) + msgline1 > LONGWARN)
                        errbuf <- strncat(BUFSIZE, errbuf, tail)
                }
                errbuf <- strncat(BUFSIZE, errbuf, tmp)
            } else {
                errbuf <- sprintf("%s", gettext("Error: ", domain = "R", trim = FALSE))
                errbuf <- strncat(BUFSIZE, errbuf, tmp)
            }
        } else {
            errbuf <- sprintf("%s", gettext("Error: ", domain = "R", trim = FALSE))
            errbuf <- strncat(Inf, errbuf, snprintf(max(msg_len - strlen(errbuf), 0L), format, ap))
        }
        nc <- strlen(errbuf)
        if (nc > BUFSIZE - 1L - (.R_MB_CUR_MAX - 1L)) {
            end <- min(nc + 1L, (BUFSIZE + 1L) - 4L)
            if (nc > end - 1L)
                errbuf <- strncat(end - 1L, errbuf, "")
            errbuf <- strncat(BUFSIZE, errbuf, "...\n")
        } else {
            if (!endsWith(errbuf, "\n")) {
                errbuf <- strncat(BUFSIZE, errbuf, "\n")
                nc <- nc + 1L
            }
        }
        buf <- errbuf
    }
    else buf <- conditionMessage(c)
    if (is.null(outfile))
        paste(buf, collapse = "")
    else {
        cat(buf, file = outfile, sep = "")
        invisible(paste(buf, collapse = ""))
    }
}


.cat.file <- function (file, show.all = FALSE, number.nonblank = FALSE, show.ends = FALSE,
    number = FALSE, squeeze.blank = FALSE, show.tabs = FALSE,
    show.nonprinting = FALSE, outfile = stdout(), show.command = FALSE)
{
    if (show.command) {
        flags <- ""
        if (number.nonblank <- if (number.nonblank) TRUE else FALSE) {
            flags <- paste0(flags, "b")
        } else if (number <- if (number) TRUE else FALSE)
            flags <- paste0(flags, "n")
        if (squeeze.blank <- if (squeeze.blank) TRUE else FALSE)
            flags <- paste0(flags, "s")
        if (show.all <- if (show.all) TRUE else FALSE) {
            flags <- paste0(flags, "A")
        } else {
            if (show.nonprinting <- if (show.nonprinting) TRUE else FALSE)
                flags <- paste0(flags, "v")
            if (show.ends <- if (show.ends) TRUE else FALSE)
                flags <- paste0(flags, "E")
            if (show.tabs <- if (show.tabs) TRUE else FALSE)
                flags <- paste0(flags, "T")
        }
        if (nzchar(flags))
            flags <- paste0(" -", flags)
        cat(sprintf("\n$ cat%s %s\n", flags, shQuote(file)))
    }
    if (show.all) {
        show.nonprinting <- TRUE
        show.ends <- TRUE
        show.tabs <- TRUE
    }
    Lines <- readLines(file, warn = FALSE)
    # Lines <- strsplit(rawToChar(as.raw(1:255)), "", useBytes = TRUE)[[1L]]
    if (show.tabs)
        Lines <- gsub("\t", "^I", Lines, fixed = TRUE, useBytes = TRUE)
    if (show.nonprinting) {
        tab <- charToRaw("\t")
        Lines <- vapply(Lines, function(Line) {
            bytes <- charToRaw(Line)
            if (all(bytes >= 32L & bytes < 127L | bytes == tab))
                return(Line)
            paste(vapply(bytes, function(byte) {
                if (byte >= 32L) {
                    if (byte < 127L) {
                        rawToChar(byte)
                    }
                    else if (byte == 127L) {
                        "^?"
                    }
                    else paste0("M-", {
                        if (byte >= 128L + 32L) {
                            if (byte < 128L + 127L) {
                                rawToChar(as.raw(as.integer(byte) - 128L))
                            }
                            else {
                                "^?"
                            }
                        }
                        else {
                            paste0("^", rawToChar(as.raw(as.integer(byte) - 128L + 64L)))
                        }
                    })
                }
                else if (byte == tab)
                    "\t"
                else {
                    paste0("^", rawToChar(as.raw(as.integer(byte) + 64L)))
                }
            }, "", USE.NAMES = FALSE), collapse = "")
        }, "", USE.NAMES = FALSE)
    }
    if (squeeze.blank) {
        if (length(Lines)) {
            nonblank <- nzchar(Lines)
            Lines <- Lines[c(TRUE, nonblank[-1L] | nonblank[-length(nonblank)])]
        }
    }
    if (number.nonblank) {
        nonblank <- nzchar(Lines)
        n <- sum(nonblank)
        Lines[nonblank] <- sprintf("%6d\t%s", seq_len(n), Lines[nonblank])
    }
    else if (number) {
        n <- length(Lines)
        Lines <- sprintf("%6d\t%s", seq_len(n), Lines)
    }
    if (show.ends)
        Lines <- sprintf("%s$", Lines)
    if (is.null(outfile))
        Lines
    else {
        writeLines(Lines, outfile, useBytes = !.utf8locale)
        invisible(Lines)
    }
}


if (FALSE) {


FILE <- tempfile(fileext = ".txt")
writeLines(
"testing this out



\t



\t

5 + \u03b4", FILE, useBytes = TRUE)


## number ----


cat.file(FILE)
system2("cat", shQuote(c(FILE)))


cat.file(FILE, number.nonblank = TRUE)
system2("cat", shQuote(c(FILE, "--number-nonblank")))


cat.file(FILE, number = TRUE)
system2("cat", shQuote(c(FILE, "--number")))


## show-ends ----


cat.file(FILE, show.ends = TRUE)
system2("cat", shQuote(c(FILE, "--show-ends")))


cat.file(FILE, show.ends = TRUE, number.nonblank = TRUE)
system2("cat", shQuote(c(FILE, "--show-ends", "--number-nonblank")))


cat.file(FILE, show.ends = TRUE, number = TRUE)
system2("cat", shQuote(c(FILE, "--show-ends", "--number")))


## squeeze-blank ----


cat.file(FILE, squeeze.blank = TRUE)
system2("cat", shQuote(c(FILE, "--squeeze-blank")))


## show-tabs ----


cat.file(FILE, show.tabs = TRUE)
system2("cat", shQuote(c(FILE, "--show-tabs")))


## show-nonprinting ----


cat.file(FILE, show.nonprinting = TRUE)
system2("cat", shQuote(c(FILE, "--show-nonprinting")))


## show-all ----


cat.file(FILE, show.all = TRUE)
system2("cat", shQuote(c(FILE, "--show-all")))


cat.file(FILE, show.all = TRUE, squeeze.blank = TRUE)
system2("cat", shQuote(c(FILE, "--show-all", "--squeeze-blank")))





unlink(FILE)


}
