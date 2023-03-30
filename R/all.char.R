all.char <- function ()
{
    c(
        local({
            value <- vapply(as.raw(1:255), rawToChar, "")
            Encoding(value) <- "latin1"
            value <- enc2utf8(value)
            names(value) <- sprintf("0x%02x", 1:255)
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


if (getRversion() >= "3.4.0") {
    sys.source("./inst/extdata/main.R", environment(), toplevel.env = getOption("topLevelEnvironment"))
} else {
    sys.source("./inst/extdata/main.R", environment())
}


tmp <- function(expr) {
    if (typeof(expr) == "closure") {
        formals(expr) <- tmp(formals(expr))
        body(expr) <- tmp(body(expr))
        expr
    }
    else if (is.pairlist(expr)) {
        as.pairlist(lapply(expr, tmp))
    }
    else if (is.call(expr)) {
        if (identical(expr, `.Platform$OS.type == "windows"`))
            os.windows
        else if (identical(expr, `identical(R.version[["crt"]], "ucrt")`))
            ucrt
        else as.call(lapply(expr, tmp))
    }
    else expr
}
evalq(envir = environment(tmp) <- new.env(), {
    `.Platform$OS.type == "windows"` <- quote(.Platform$OS.type == "windows")
    os.windows <- quote(os.windows)
    `identical(R.version[["crt"]], "ucrt")` <- quote(identical(R.version[["crt"]], "ucrt"))
    ucrt <- quote(ucrt)
})


languageEnvvars <- tmp(languageEnvvars)
Sys.putenv <- tmp(Sys.putenv)


rm(tmp)
