print.ThisPathInfo <- function (x, ..., quote = TRUE)
.External2(.C_printThisPathInfo, x, quote)


format.ThisPathInfo <- function (x, ...)
{
    value <- NULL
    conn <- textConnection("value", "w", local = TRUE)
    on.exit(close(conn))
    sink(conn)
    on.exit(sink(), add = TRUE, after = FALSE)
    print.ThisPathInfo(x = x, ...)
    on.exit()
    sink()
    close(conn)
    value
}


as.character.ThisPathInfo <- function (x, ...)
paste(format.ThisPathInfo(x = x, ...), collapse = "\n")
