if (getRversion() >= "3.4.0") {
    sys.source(
        "./inst/extdata/main.R", environment(),
        keep.source = !is.null(attr(function() NULL, "srcref")),
        toplevel.env = getOption("topLevelEnvironment")
    )
} else {
    sys.source(
        "./inst/extdata/main.R", environment(),
        keep.source = !is.null(attr(function() NULL, "srcref"))
    )
}


tmp <- evalq(envir = new.env(), {
    `.Platform$OS.type == "windows"` <- quote(.Platform$OS.type == "windows")
    .OS_windows <- quote(.OS_windows)
    `identical(R.version[["crt"]], "ucrt")` <- quote(identical(R.version[["crt"]], "ucrt"))
    .ucrt <- quote(.ucrt)
    `!is.character(LANGUAGE) || length(LANGUAGE) != 1L` <- quote(!is.character(LANGUAGE) || length(LANGUAGE) != 1L)
    `!.IS_SCALAR_STR(LANGUAGE)` = quote(!.IS_SCALAR_STR(LANGUAGE))
function (expr)
{
    if (typeof(expr) == "closure") {
        at <- attributes(expr)
        formals(expr) <- tmp(formals(expr))
        body(expr) <- tmp(body(expr))
        if (!is.null(at))
            attributes(expr) <- at
        expr
    }
    else if (is.pairlist(expr)) {
        as.pairlist(lapply(expr, tmp))
    }
    else if (is.call(expr)) {
        if (identical(expr, `.Platform$OS.type == "windows"`))
            .OS_windows
        else if (identical(expr, `identical(R.version[["crt"]], "ucrt")`))
            .ucrt
        else if (identical(expr, `!is.character(LANGUAGE) || length(LANGUAGE) != 1L`))
            `!.IS_SCALAR_STR(LANGUAGE)`
        else as.call(lapply(expr, tmp))
    }
    else expr
}
})


.language_envvars <- tmp(.language_envvars)
Sys.putenv <- tmp(Sys.putenv)


rm(tmp)
