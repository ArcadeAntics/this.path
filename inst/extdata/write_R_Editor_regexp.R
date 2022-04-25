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


main <- function ()
{
    stopifnot(.Platform$OS.type == "windows")


    loadNamespace("this.path")
    loadNamespace("essentials", versionCheck = list(op = ">=", version = "0.2.0"))


    # this process needs to run in order to continue
    tryCatch({
        essentials::python(command = "import pyautogui, sys", mustWork = TRUE, quiet = TRUE)
    }, error = function(c) {
        warning("python or python module 'pyautogui' is not available:\n try 'pip install pyautogui'")
        stop(c)
    })


    FILES <- tempfile(fileext = c(".RData", ".rds", ".py", ".R"))
    on.exit(unlink(FILES))
    tmpRData   <- FILES[[1L]]
    tmpRobject <- FILES[[2L]]
    tmppy      <- FILES[[3L]]
    tmpR       <- FILES[[4L]]
    file.create(tmpR)


    writeLines(essentials::dedent("
        import pyautogui, sys
        pyautogui.PAUSE = 0.2


        pyautogui.leftClick(  17,   59)  # 'Open script' button
        pyautogui.write(sys.argv[1] + '\\n')
        pyautogui.leftClick( 323,  308)  # Select R Console
        pyautogui.write('fun()\\n')


        pyautogui.leftClick(  15,   32)  # 'File' button
        pyautogui.leftClick( 109,   77)  # 'New script' button
        pyautogui.leftClick( 323,  308)  # Select R Console
        pyautogui.write('fun()\\n')


        pyautogui.write('quit()\\n')
    "), tmppy)


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
    save(fun, file = tmpRData)


    options <- c("--vanilla", paste0("--workspace=", tmpRData), "R_DEFAULT_PACKAGES=NULL")
    suffix <- {
        if (getRversion() < "4.2.0")
            "lt_4.2.0"
        else "ge_4.2.0"
    }
    apk <- file.path(R.home("bin"), "Rgui.exe")
    quiet <- !getOption("verbose")
    for (language in rownames(this.path:::languages)) {
        n <- length(readRDS(tmpRobject))
        essentials:::Rgui(
            options = c(
                options,
                switch(suffix, lt_4.2.0 = {
                    this.path:::languageEnvvars(language)
                }, ge_4.2.0 = {
                    paste0(c("LANG=", "LANGUAGE="), language)
                })
            ),
            wait = FALSE, mustWork = TRUE, quiet = quiet,
            name = apk
        )
        Sys.sleep(0.2)
        essentials::python(file = tmppy, args = tmpR, mustWork = TRUE, quiet = quiet)
        stopifnot(length(readRDS(tmpRobject)) == n + 2L)
    }
    x <- readRDS(tmpRobject)


    open.script <- x[seq.int(1L, length(x), 2L)]
    untitled.script <- x[seq.int(2L, length(x), 2L)]


    if (any(i <- !startsWith(open.script, paste0(tmpR, " - "))))
        stop(ngettext(sum(i),
            "incorrect prefix: ",
            "incorrect prefixes:\n  "),
            paste(open.script[i], collapse = "\n  "))
    open.script <- substring(open.script, nchar(tmpR) + 3L + 1L)
    open.script <- unique(open.script)
    open.script <- essentials:::regexQuote(open.script)
    open.script <- paste(open.script, collapse = "|")
    open.script <- paste0("(.+) - (", open.script, ")")
    writeLines(open.script, this.path::here(paste0("r_editor_regexp_", suffix, ".txt")), useBytes = TRUE)


    untitled.script <- unique(untitled.script)
    writeLines(untitled.script, this.path::here(paste0("untitled_", suffix, ".txt")), useBytes = TRUE)


    invisible()
}


main()
