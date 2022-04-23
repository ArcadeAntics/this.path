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


num.RGui.sessions <- function ()
length(utils::getWindowsHandles("all", "^RGui", minimized = TRUE))


main <- function ()
{
    stopifnot(.Platform$OS.type == "windows")


    FILES <- tempfile(fileext = c(".RData", ".rds", ".py", ".R"))
    on.exit(unlink(FILES))
    tmpRData   <- FILES[[1L]]
    tmpRobject <- FILES[[2L]]
    tmppy      <- FILES[[3L]]
    tmpR       <- FILES[[4L]]
    file.create(tmpR)


    oenv <- essentials::envvars("R_EDITOR_REGEXP_TMP_R_SCRIPT" = tmpR)
    on.exit(essentials::envvars(oenv), add = TRUE)


    writeLines(essentials::dedent(r"{
        import os, pyautogui
        pyautogui.PAUSE = 0.2


        pyautogui.leftClick(  17,   59)  # 'Open script' button
        pyautogui.write(os.environ["R_EDITOR_REGEXP_TMP_R_SCRIPT"] + "\n")
        pyautogui.leftClick( 323,  308)  # Select R Console
        pyautogui.write("fun()\n")


        pyautogui.leftClick(  15,   32)  # 'File' button
        pyautogui.leftClick( 109,   77)  # 'New script' button
        pyautogui.leftClick( 323,  308)  # Select R Console
        pyautogui.write("fun()\n")


        pyautogui.write("quit()\n")
    }"), tmppy)


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
    save(list = "fun", file = tmpRData)


    options <- c("--vanilla", paste0("--workspace=", tmpRData), "R_DEFAULT_PACKAGES=NULL")
    which <- {
        if (getRversion() < "4.2.0")
            "lt_4.2.0"
        else "ge_4.2.0"
    }
    apk <- file.path(R.home("bin"), "Rgui.exe")
    for (language in rownames(this.path:::languages)) {
        n <- num.RGui.sessions()
        essentials:::Rgui(
            options = c(
                options,
                switch(which, lt_4.2.0 = {
                    this.path:::languageEnvvars(language)
                }, ge_4.2.0 = {
                    paste0(c("LANG=", "LANGUAGE="), language)
                })
            ),
            wait = FALSE, mustWork = TRUE, quiet = TRUE,
            name = apk
        )
        Sys.sleep(0.2)
        essentials::python(file = tmppy, mustWork = TRUE, quiet = TRUE)


        # wait until the RGui process is finished before moving to the next one.
        # we assume the user is not opening more windows of RGui in the
        # meantime, which is a fair assumption
        while (num.RGui.sessions() > n) NULL
    }
    x <- readRDS(tmpRobject)


    open.script <- x[seq.int(1, length(x), 2)]
    untitled.script <- x[seq.int(2, length(x), 2)]


    if (any(i <- !startsWith(open.script, paste0(tmpR, " - "))))
        stop(ngettext(sum(i),
            "incorrect prefix: ",
            "incorrect prefixes:\n  "),
            paste(open.script[i], collapse = "\n  "))
    open.script <- substring(open.script, nchar(tmpR) + 3 + 1)
    open.script <- unique(open.script)
    open.script <- essentials:::regexQuote(open.script)
    open.script <- paste(open.script, collapse = "|")
    open.script <- paste0("(.+) - (", open.script, ")")
    writeLines(open.script, this.path::here(paste0("r_editor_regexp_", which, ".txt")), useBytes = TRUE)


    untitled.script <- unique(untitled.script)
    writeLines(untitled.script, this.path::here(paste0("untitled_", which, ".txt")), useBytes = TRUE)


    invisible()
}


main()
