cat.file <- function (file, show.all = FALSE, number.nonblank = FALSE, show.ends = FALSE,
    number = FALSE, squeeze.blank = FALSE, show.tabs = FALSE,
    show.nonprinting = FALSE, outfile = stdout(), print.command = FALSE)
{
    if (print.command) {
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
        writeLines(Lines, outfile, useBytes = !utf8)
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


# number ----


cat.file(FILE)
system2("cat", shQuote(c(FILE)))


cat.file(FILE, number.nonblank = TRUE)
system2("cat", shQuote(c(FILE, "--number-nonblank")))


cat.file(FILE, number = TRUE)
system2("cat", shQuote(c(FILE, "--number")))


# show-ends ----


cat.file(FILE, show.ends = TRUE)
system2("cat", shQuote(c(FILE, "--show-ends")))


cat.file(FILE, show.ends = TRUE, number.nonblank = TRUE)
system2("cat", shQuote(c(FILE, "--show-ends", "--number-nonblank")))


cat.file(FILE, show.ends = TRUE, number = TRUE)
system2("cat", shQuote(c(FILE, "--show-ends", "--number")))


# squeeze-blank ----


cat.file(FILE, squeeze.blank = TRUE)
system2("cat", shQuote(c(FILE, "--squeeze-blank")))


# show-tabs ----


cat.file(FILE, show.tabs = TRUE)
system2("cat", shQuote(c(FILE, "--show-tabs")))


# show-nonprinting ----


cat.file(FILE, show.nonprinting = TRUE)
system2("cat", shQuote(c(FILE, "--show-nonprinting")))


# show-all ----


cat.file(FILE, show.all = TRUE)
system2("cat", shQuote(c(FILE, "--show-all")))


cat.file(FILE, show.all = TRUE, squeeze.blank = TRUE)
system2("cat", shQuote(c(FILE, "--show-all", "--squeeze-blank")))





unlink(FILE)


}
