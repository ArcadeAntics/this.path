all.char <- function ()
{
    c(
        local({
            nms <- sprintf("%02x", strtoi("01", 16):strtoi("ff", 16))
            value <- as.character(str2expression(sprintf("\"\\x%s\"", nms)))
            Encoding(value) <- "latin1"
            value <- enc2utf8(value)
            names(value) <- nms
            value
        }),
        local({
            nms <- sprintf("%04x", c(
                strtoi("0100", 16):strtoi("d7ff", 16),
                strtoi("e000", 16):strtoi("fffd", 16)
            ))
            value <- as.character(str2expression(sprintf("\"\\u%s\"", nms)))
            names(value) <- nms
            value
        }),
        local({
            nms <- sprintf("%08x", strtoi("010000", 16):strtoi("10ffff", 16))
            value <- as.character(str2expression(sprintf("\"\\U%s\"", nms)))
            names(value) <- nms
            value
        })
    )
}


sys.source("./inst/extdata/main.R", environment(),
    toplevel.env = getOption("topLevelEnvironment", as.environment(environment())))
