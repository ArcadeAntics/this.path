# when you open Rgui for windows and try to run code from a
# script (involving this.path), the path will be pulled from
# the name of the window handle (which should be of the form
# %(path) - %(R Editor)
#
# in different languages and locales, the string R Editor
# is translated to different sets of characters. what we do
# here is go through all languages and corresponding locales
# to figure out how that string translates.
#
# we loop through each language and:
# * start and Rgui session
# * open a new script
# * grab the translation of "R Editor" from the name of the
#       R Editor
#
# we use package 'rMouse' to automate this process.
#
# we turn this into a regular expression, then send it to
# a file to be saved, then read back in later when needed


if (.Platform$OS.type != "windows")
    stop("not windows")


tmpRData <- tempfile(fileext = ".RData")
FILE <- tempfile(fileext = ".rds")
tryCatch({


    x <- character()


    saveRDS(x, FILE)


    this.path::delayedAssign2("expr", substitute({
        text <- names(utils::getWindowsHandles())[[2L]]
        if (Encoding(text) == "unknown")
            Encoding(text) <- "latin1"
        saveRDS(c(readRDS(FILE), sub("^.* - (.*)$", "\\1", enc2utf8(text))), FILE)
        q()
    }, list(FILE = FILE)), evaluated = TRUE)
    save(list = "expr", file = tmpRData, eval.promises = FALSE)


    options <- c("--no-save", paste0("--workspace=", tmpRData),
        "--no-site-file", "--no-init-file", "--no-environ")
    rMouse::setAutoDelay(100)
    for (language in rownames(this.path:::languages)) {
        this.path:::Rgui(c(options, this.path:::languageEnvvars(language)), wait = FALSE)
        rMouse::move( 15,  32)
        rMouse::left()
        rMouse::move(109,  77)
        rMouse::left()
        rMouse::move(340, 313)
        rMouse::left()
        rMouse::type("expr\n")
    }
    x <- readRDS(FILE)


}, finally = unlink(c(tmpRData, FILE)))


x <- unique(x)
x <- this.path:::regexQuote(x)
x <- paste(x, collapse = "|")
x <- paste0("(", x, ")")
x <- paste0("(.+) - ", x)
x <- encodeString(enc2utf8(x), quote = "\"")
writeLines(x, this.path::here("R_Editor_regexp.R"), useBytes = TRUE)
