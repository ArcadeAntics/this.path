# when you open Rgui for Windows and try to run code from a
# script (involving this.path), the path will be pulled from
# the name of the window handle (which should be of the form
# %(path) - %(R Editor)
#
# in different languages and locales, the string %(R Editor)
# is translated to different sets of characters. what we do
# here is go through all languages and corresponding locales
# to figure out how that string translates.
#
# we loop through each language and:
# * start an Rgui session
# * open a new script
# * grab the translation of "R Editor" from the name of the
#       R Editor
#
# we use package 'Rautogui' to automate this process.
# I hope to upload 'Rautogui' to CRAN at some point soon
#
# we turn this into a regular expression, then send it to
# a file to be saved, then read back in later when needed


num.RGui.sessions <- function ()
length(utils::getWindowsHandles("all", "^RGui", minimized = TRUE))


main <- function ()
{
    stopifnot(.Platform$OS.type == "windows")


    tmpRData <- tempfile(fileext = ".RData")
    FILE <- tempfile(fileext = ".rds")
    on.exit(unlink(c(tmpRData, FILE)))


    x <- character()


    saveRDS(x, FILE)


    essentials::delayedAssign2("expr", bquote({
        text <- names(utils::getWindowsHandles())[[2L]]
        if (Encoding(text) == "unknown") {
            loc <- utils::localeToCharset()[1L]
            Encoding(text) <- switch(loc, `UTF-8` = "UTF-8",
                `ISO8859-1` = "latin1", "unknown")
        }
        text <- enc2utf8(text)
        text <- sub("^.* - (.*)$", "\\1", text)
        saveRDS(c(readRDS(.(FILE)), text), .(FILE))
        quit()
    }), evaluated = TRUE)
    save(list = "expr", file = tmpRData, eval.promises = FALSE)


    options <- c("--no-save", paste0("--workspace=", tmpRData),
        "--no-site-file", "--no-init-file", "--no-environ",
        "R_DEFAULT_PACKAGES=NULL")
    Rautogui::setPAUSE(0.1)
    for (language in rownames(this.path:::languages)) {
        n <- num.RGui.sessions()
        essentials:::Rgui(
            options = c(
                options,
                this.path:::languageEnvvars(language)
            ),
            wait = FALSE, quiet = TRUE
        )
        Sys.sleep(0.2)
        Rautogui::leftClick(  15,   32)
        Rautogui::leftClick( 109,   77)
        Rautogui::leftClick( 340,  313)
        Rautogui::type("expr\n")


        # wait until the RGui process is finished before moving to the next one.
        # we assume the user is not opening more windows of RGui in the
        # meantime, which is a fair assumption
        while (num.RGui.sessions() > n) NULL
    }
    x <- readRDS(FILE)


    x <- unique(x)
    x <- essentials:::regexQuote(x)
    x <- paste(x, collapse = "|")
    x <- paste0("(.+) - (", x, ")")
    x <- encodeString(enc2utf8(x), quote = "\"")
    writeLines(x, this.path::here("R_Editor_regexp.R"), useBytes = TRUE)
}


main()
