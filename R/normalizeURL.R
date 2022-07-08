extra.whitespace.pattern <- "^(\n|\r)+|(\t|\n|\r| )+$"


.normalizeURL <- function (path)
{
    # x <- "https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R"
    # print(c(x, this.path:::.normalizeURL(x)))
    # source(x)


    vapply(path.split.URL(path), function(x) {
        x <- x[x != "."]
        while (i <- match("..", x, nomatch = 0L)) {
            x <- if (i >= 3L)
                x[-i + 0L:1L]
            else x[-i]
        }
        x[-1L] <- gsub(extra.whitespace.pattern, "", x[-1L])
        paste(x, collapse = "/")
    }, FUN.VALUE = "")
}
