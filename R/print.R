.PrintValueEnv <- function (x, envir = parent.frame())
.External2(.C_PrintValueEnv, x, envir)


.maybePrintValueEnv <- function (x, envir = parent.frame())
{
    if (withVisible(x)$visible)
        .External2(.C_PrintValueEnv, x, envir)
    else invisible(x)
}


print.ThisPathDocumentContext <- function (x, ..., quote = TRUE)
.External2(.C_printThisPathDocumentContext, x, quote)


if (getRversion() >= "3.5.0") {


format.ThisPathDocumentContext <- function (x, ...)
{
    value <- NULL
    conn <- textConnection("value", "w", local = TRUE)
    on.exit(close(conn))
    sink(conn)
    on.exit(sink(), add = TRUE, after = FALSE)
    print.ThisPathDocumentContext(x = x, ...)
    on.exit()
    sink()
    close(conn)
    value
}


} else {


format.ThisPathDocumentContext <- function (x, ...)
{
    value <- NULL
    conn <- textConnection("value", "w", local = TRUE)
    sink(conn)
    on.exit(sink())
    on.exit(close(conn), add = TRUE)
    print.ThisPathDocumentContext(x = x, ...)
    on.exit()
    sink()
    close(conn)
    value
}


}


as.character.ThisPathDocumentContext <- function (x, ...)
paste(format.ThisPathDocumentContext(x = x, ...), collapse = "\n")
