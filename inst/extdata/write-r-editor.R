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


    FILES <- tempfile(fileext = c(".RData", ".rds", ".py", ".R"))
    on.exit(unlink(FILES))
    tmpRData   <- FILES[[1L]]
    tmpRobject <- FILES[[2L]]
    tmppy      <- FILES[[3L]]
    tmpR       <- FILES[[4L]]
    file.create(tmpR)
    writeLines(c(
        "import pyautogui",


        # ensure RGui is the active window before typing anything
        "if not pyautogui.getActiveWindow().title.startswith('RGui'):",
        "    raise ValueError('RGui is not the active window')\n",


        "pyautogui.write('e\\n')"
    ), tmppy)


    x <- character()


    saveRDS(x, tmpRobject)


    eval(call("delayedAssign", "e", bquote({
        utils::file.edit(.(tmpR))
        text <- names(utils::getWindowsHandles())[[1L]]
        if (Encoding(text) == "unknown") {
            loc <- utils::localeToCharset()[1L]
            Encoding(text) <- switch(loc, `UTF-8` = "UTF-8",
                `ISO8859-1` = "latin1", "unknown")
        }
        saveRDS(c(readRDS(.(tmpRobject)), enc2utf8(text)), .(tmpRobject))
        utils::file.edit("")
        text <- names(utils::getWindowsHandles())[[1L]]
        if (Encoding(text) == "unknown") {
            loc <- utils::localeToCharset()[1L]
            Encoding(text) <- switch(loc, `UTF-8` = "UTF-8",
                `ISO8859-1` = "latin1", "unknown")
        }
        saveRDS(c(readRDS(.(tmpRobject)), enc2utf8(text)), .(tmpRobject))
        quit(save = "no")
    })))
    delayedAssign("E", e)


    # we want to save 'e', but save 'E' in case caps lock is on
    save(e, E, file = tmpRData, eval.promises = FALSE)
    rm(e, E)


    options <- c("--vanilla", paste0("--workspace=", tmpRData), "R_DEFAULT_PACKAGES=NULL")
    suffix <- {
        if (identical(R.version[["crt"]], "ucrt"))
            "ucrt"
        else "not-ucrt"
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
                }, `not-ucrt` = {
                    this.path:::languageEnvvars(language)
                }, stop("invalid 'suffix'; should never happen, please report!"))
            ),
            wait = FALSE, mustWork = TRUE, quiet = quiet,
            name = apk
        )
        Sys.sleep(0.2)
        essentials::python(file = tmppy, mustWork = TRUE, quiet = quiet)
        Sys.sleep(0.2)
        stopifnot(length(readRDS(tmpRobject)) == n + 2L)
    }
    x <- readRDS(tmpRobject)
    if (length(x) < 2L)
        stop("invalid 'x'; should never happen, please report!")



    r.editor <- x[c(TRUE, FALSE)]
    untitled <- x[c(FALSE, TRUE)]


    useBytes <- !identical(utils::localeToCharset()[1L], "UTF-8")


    if (any(i <- !startsWith(r.editor, paste0(tmpR, " - "))))
        stop(ngettext(sum(i),
            "incorrect prefix: ",
            "incorrect prefixes:\n  "),
            paste(r.editor[i], collapse = "\n  "))
    r.editor <- substring(r.editor, 1L + nchar(tmpR))
    r.editor <- unique(r.editor)
    writeLines(r.editor, this.path::here(paste0("r-editor_", suffix, ".txt")), useBytes = useBytes)


    untitled <- unique(untitled)
    writeLines(untitled, this.path::here(paste0("untitled_", suffix, ".txt")), useBytes = useBytes)


    invisible(list(r.editor = r.editor, untitled = untitled))
}


main()
