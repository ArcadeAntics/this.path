# when you open Rgui for Windows and try to run code from a
# script (involving this.path), the path will be pulled from
# the name of the window handle (which should be of the form
# %(path) - %(R Editor)
#
# in different languages, the string %(R Editor) is
# translated to different sets of characters. what we do
# here is go through all languages to figure out how that
# string translates.
#
# we loop through each language and:
# * start an Rgui session
# * open an existing script
# * grab the translation of "R Editor"
# * open a new script
# * grab the untitled name
#
# we turn this into a regular expression, then send it to
# a file to be saved, then read back in later when needed


stopifnot(.Platform$OS.type == "windows")


main <- function ()
{
    stopifnot(.Platform$OS.type == "windows")


    loadNamespace("this.path")
    loadNamespace("essentials", versionCheck = list(op = ">=", version = "0.2.0"))


    # this process needs to run in order to continue
    tryCatch({
        essentials::python(command = "import pyautogui", mustWork = TRUE, quiet = TRUE)
    }, error = function(c) {
        warning("python or python module 'pyautogui' is not available:\n try 'pip install pyautogui'")
        stop(c)
    })


    envir <- new.env(parent = baseenv())
    source(this.path::here("select_screen_res.R"), envir)
    py <- envir$select.screen.res()$file


    FILES <- tempfile(fileext = c(".RData", ".rds", ".py", ".R"))
    on.exit(unlink(FILES))
    tmpRData   <- FILES[[1L]]
    tmpRobject <- FILES[[2L]]
    tmppy      <- FILES[[3L]]
    tmpR       <- FILES[[4L]]
    file.create(tmpR)


    file.copy(py, tmppy)


    x <- character()


    saveRDS(x, tmpRobject)


    fun <- function() NULL
    body(fun) <- bquote({
        text <- names(utils::getWindowsHandles())[[2L]]
        if (Encoding(text) == "unknown") {
            loc <- utils::localeToCharset()[1L]
            Encoding(text) <- switch(loc, `UTF-8` = "UTF-8",
                `ISO8859-1` = "latin1", "unknown")
        }
        saveRDS(c(readRDS(.(tmpRobject)), enc2utf8(text)), .(tmpRobject))
    })
    FUN <- fun
    Q <- function(...) q(...)
    # we want to save fun, but save FUN in case caps lock is on
    # same with q, but save Q in case caps lock is on
    save(fun, FUN, Q, file = tmpRData)


    options <- c("--vanilla", paste0("--workspace=", tmpRData), "R_DEFAULT_PACKAGES=NULL")
    suffix <- {
        if (identical(R.version[["crt"]], "ucrt"))
            "ucrt"
        else "not_ucrt"
    }
    apk <- file.path(R.home("bin"), "Rgui.exe")
    quiet <- !getOption("verbose")
    for (language in rownames(this.path:::languages)) {
        n <- length(readRDS(tmpRobject))
        essentials:::Rgui(
            options = c(
                options,
                switch(suffix, ucrt = {
                    paste0(c("LANG=", "LANGUAGE="), language)
                }, not_ucrt = {
                    this.path:::languageEnvvars(language)
                }, stop("invalid 'suffix'; should never happen, please report!"))
            ),
            wait = FALSE, mustWork = TRUE, quiet = quiet,
            name = apk
        )
        Sys.sleep(0.2)
        essentials::python(file = tmppy, args = tmpR, mustWork = TRUE, quiet = quiet)
        stopifnot(length(readRDS(tmpRobject)) == n + 2L)
    }
    x <- readRDS(tmpRobject)
    if (length(x) < 2L)
        stop("invalid 'x'; should never happen, please report!")



    open.script <- x[c(TRUE, FALSE)]
    untitled.script <- x[c(FALSE, TRUE)]


    if (any(i <- !startsWith(open.script, paste0(tmpR, " - "))))
        stop(ngettext(sum(i),
            "incorrect prefix: ",
            "incorrect prefixes:\n  "),
            paste(open.script[i], collapse = "\n  "))
    open.script <- substring(open.script, 1L + nchar(tmpR) + 3L)
    open.script <- unique(open.script)
    open.script <- essentials::regexencode(open.script)
    open.script <- paste(open.script, collapse = "|")
    open.script <- paste0("(.+) - (", open.script, ")")
    writeLines(open.script, this.path::here(paste0("r_editor_regexp_", suffix, ".txt")), useBytes = TRUE)


    untitled.script <- unique(untitled.script)
    writeLines(untitled.script, this.path::here(paste0("untitled_", suffix, ".txt")), useBytes = TRUE)


    invisible()
}


main()
