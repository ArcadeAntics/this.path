cat.condition <- function (c)
{
    # c <- simpleError("testing th\U{0001D11E}s\n out", quote(5 + 6))
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
        cat(
            if (nzchar(dcall)) {
                c(
                    gettextf("Warning in %s :", dcall, domain = "R"),
                    if (!(( mbcslocale && 18L + wd(dcall) + wd(buf) <= LONGWARN) ||
                          (!mbcslocale && 18L + strlen(dcall) + strlen(buf) <= LONGWARN)))
                        "\n "
                )
            } else gettext("Warning:", domain = "R"),
            sprintf(" %s\n", buf),
            file = stderr(), sep = "")
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
                if (mbcslocale) {
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
        if (nc > BUFSIZE - 1L - (R_MB_CUR_MAX - 1L)) {
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
        cat(errbuf, file = stderr())
    }
    else cat(conditionMessage(c), file = stderr(), sep = "")
}
