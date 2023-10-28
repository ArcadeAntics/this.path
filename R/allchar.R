## ensure the functions always have a source reference
oopt <- options(keep.source = TRUE)
eval(parse(text = '
.isASCII <- function (x)
vapply(x, function(xx) all(charToRaw(xx) <= 0x7f), NA)


.all.char <- function ()
{
    c({
        codes <- 0x01:0x7f
        value <- rawToChar(as.raw(codes), multiple = TRUE)
        names(value) <- sprintf("0x%02x", codes)
        value
    }, {
        codes <- 0x80:0xff
        value <- rawToChar(as.raw(codes), multiple = TRUE)
        Encoding(value) <- "latin1"
        tmp <- enc2utf8(value)
        i <- .isASCII(tmp)
        value[!i] <- tmp[!i]
        value[i] <- iconv(value[i], "latin1", "UTF-8")
        names(value) <- sprintf("0x%02x", codes)
        value
    }, {
        names <- sprintf("%04x", c(
            strtoi("0100", 16):strtoi("d7ff", 16),
            strtoi("e000", 16):strtoi("fffd", 16)
        ))
        value <- as.character(str2expression(sprintf("\\"\\\\u%s\\"", names)))
        names(value) <- names
        value
    }, {
        names <- sprintf("%08X", strtoi("010000", 16):strtoi("10FFFF", 16))
        value <- as.character(str2expression(sprintf("\\"\\\\U%s\\"", names)))
        names(value) <- names
        value
    })
}
'))
options(oopt)
rm(oopt)


if (getRversion() >= "3.4.0") {
    sys.source("./inst/extdata/main.R", environment(), toplevel.env = getOption("topLevelEnvironment"))
} else {
    sys.source("./inst/extdata/main.R", environment())
}


tmp <- evalq(envir = new.env(), {
    `.Platform$OS.type == "windows"` <- quote(.Platform$OS.type == "windows")
    .os.windows <- quote(.os.windows)
    `identical(R.version[["crt"]], "ucrt")` <- quote(identical(R.version[["crt"]], "ucrt"))
    .ucrt <- quote(.ucrt)
    `!is.character(LANGUAGE) || length(LANGUAGE) != 1L` <- quote(!is.character(LANGUAGE) || length(LANGUAGE) != 1L)
    `!.IS_SCALAR_STR(LANGUAGE)` = quote(!.IS_SCALAR_STR(LANGUAGE))
function (expr)
{
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
            .os.windows
        else if (identical(expr, `identical(R.version[["crt"]], "ucrt")`))
            .ucrt
        else if (identical(expr, `!is.character(LANGUAGE) || length(LANGUAGE) != 1L`))
            `!.IS_SCALAR_STR(LANGUAGE)`
        else as.call(lapply(expr, tmp))
    }
    else expr
}
})


.language.envvars <- tmp(.language.envvars)
Sys.putenv <- tmp(Sys.putenv)


rm(tmp)
