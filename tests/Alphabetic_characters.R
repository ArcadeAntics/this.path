x <- as.character(str2expression(sprintf("\"\\x%02X\"", 33:255)))
Encoding(x) <- "latin1"
x <- enc2utf8(x)
x <- c(x, "\u{03C3}", "\u{03B4}", "\u{7B90}", "\u{5316}", "\u{5B57}")


fun <- function (...)
{
    x <- c(...)
    y <- encodeString(x, quote = "\"")
    nc <- nchar(y, type = "width")
    y <- paste0(y, strrep(" ", max(0L, nc) - nc))
    i <- grepl("^[[:alpha:]]*$", x)
    cat(y[ i], fill = TRUE, labels = c("alpha:",
        format(as.character(seq_len(sum( i))), justify = "left", width = 6)))
    cat("\n")
    cat(y[!i], fill = TRUE, labels = c("not  :",
        format(as.character(seq_len(sum(!i))), justify = "left", width = 6)))
    cat("\n")
}


fun2 <- function (..., LANGUAGE = list.dirs(system.file(package = "translations"), full.names = FALSE, recursive = FALSE))
{
    oLANGUAGE <- Sys.getenv("LANGUAGE", unset = NA)
    if (is.na(oLANGUAGE))
        on.exit(Sys.unsetenv("LANGUAGE"))
    else on.exit(Sys.setenv(LANGUAGE = oLANGUAGE))
    for (LANGUAGE in LANGUAGE) {
        Sys.setenv(LANGUAGE = LANGUAGE)
        cat("LANGUAGE=", LANGUAGE, "\n\n", sep = "")
        fun(...)
    }
}


fun2(x)
