if (.Platform$OS.type != "windows")
    stop("not windows")


tmpRData <- tempfile(fileext = ".RData")
FILE <- tempfile(fileext = ".rds")
tryCatch({


    x <- matrix(dimnames = list(NULL, c(
        "LANGUAGE", "R Editor")), data = c(character()
    ), nrow = 0L, ncol = 2L, byrow = TRUE)


    saveRDS(x, FILE)


    this.path::delayedAssign2("expr", substitute({
        text <- names(utils::getWindowsHandles())[[2L]]
        if (Encoding(text) == "unknown")
            Encoding(text) <- "latin1"
        text <- sub("^.* - (.*)$", "\\1", enc2utf8(text))
        text <- c(essentials::getEnvvar("LANGUAGE"), text)
        x <- readRDS(FILE)
        x <- rbind(x, text, deparse.level = 0)
        saveRDS(x, FILE)
        q()
    }, list(FILE = FILE)), evaluated = TRUE)
    save(list = "expr", file = tmpRData, eval.promises = FALSE)


    options <- c("--no-save", paste0("--workspace=", tmpRData),
        "--no-site-file", "--no-init-file", "--no-environ")
    rMouse::setAutoDelay(10)
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


# FILE <- this.path::here("R_Editor_translations.rds")
# if (file.exists(FILE)) {
#     y <- readRDS(FILE)
#     y <- y[!y[, "LANGUAGE"] %in% x[, "LANGUAGE"], , drop = FALSE]
#     x <- rbind(x, y)
#     x <- x[order(x[, "LANGUAGE"]), , drop = FALSE]
# }
# saveRDS(x, FILE)
#
#
# sys.source(this.path::here("save_R_Editor_regexp.R"), environment())
