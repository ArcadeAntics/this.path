# deaccent <- function (x)
# {
#     for (nm in names(deaccent.patterns)) {
#         x <- gsub(deaccent.patterns[[nm]], nm, x)
#     }
#     x
# }
# deaccent.patterns <- c(
#     f = "\x83",
#     S = "\x8A",
#     Z = "\x8E",
#     s = "\x9A",
#     z = "\x9E",
#     Y = "\x9F|\xDD",
#     A = "\xC0|\xC1|\xC2|\xC3|\xC4|\xC5",
#     C = "\xC7",
#     E = "\xC8|\xC9|\xCA|\xCB",
#     I = "\xCC|\xCD|\xCE|\xCF",
#     N = "\xD1",
#     O = "\xD2|\xD3|\xD4|\xD5|\xD6|\xD8",
#     U = "\xD9|\xDA|\xDB|\xDC",
#     a = "\xE0|\xE1|\xE2|\xE3|\xE4|\xE5",
#     c = "\xE7",
#     e = "\xE8|\xE9|\xEA|\xEB",
#     i = "\xEC|\xED|\xEE|\xEF",
#     n = "\xF1",
#     o = "\xF2|\xF3|\xF4|\xF5|\xF6|\xF8",
#     u = "\xF9|\xFA|\xFB|\xFC",
#     y = "\xFD|\xFF"
# )
# Encoding(deaccent.patterns) <- "latin1"


dedent <- function (x, strip = TRUE)
{
    fun <- function(xx) {


        # the pattern we are looking for is any number of space and tab
        # characters, followed by a non-space character, followed by any number
        # of characters
        pattern <- "^([ \t]*)\\S.*$"
        if (any(i <- grepl(pattern, xx))) {
            starts <- sub(pattern, "\\1", xx[i])
            start <- starts[[1L]]
            n <- nchar(start)
            while (!all(startsWith(starts, start)))
                start <- substr(start, 1, n <- n - 1)
            if (n)
                xx[i] <- substr(xx[i], n + 1L, 1000000L)
        }
        paste(xx, collapse = "\n")
    }
    if (!is.character(x))
        x <- as.character(x)
    value <- x
    if (strip)
        value <- gsub("^[ \t]*\n|\n[ \t]*$", "", value)
    value <- regmatches(value, gregexpr("\n", value, fixed = TRUE), invert = TRUE)
    value <- vapply(value, "fun", "", USE.NAMES = FALSE)
    dim(value) <- dim(x)
    dimnames(value) <- dimnames(x)
    names(value) <- names(x)
    return(value)
}
