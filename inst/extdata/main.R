.languages <- matrix(dimnames = list(NULL, c(
    "LANGUAGE", "codepage", "Format: (* Custom Locale)"    , "locale"                      )), data = c(
    "da"      , "865"     , "Danish (Denmark)"             , "Danish_Denmark"              ,
    "de"      , "1252"    , "German (Germany)"             , "German_Germany"              ,
    "en"      , "1252"    , "English (World)"              , "English_World"               ,
    "en@quot" , "1252"    , "English (World)"              , "English_World"               ,
    "en_GB"   , "1252"    , "English (United Kingdom)"     , "English_United Kingdom"      ,
    "es"      , "1252"    , "Spanish (Spain)"              , "Spanish_Spain"               ,
    "fa"      , "1256"    , "Persian (Iran)"               , "Persian_Iran"                ,
    "fr"      , "1252"    , "French (France)"              , "French_France"               ,
    "it"      , "1252"    , "Italian (Italy)"              , "Italian_Italy"               ,
    "ja"      , "932"     , "Japanese (Japan)"             , "Japanese_Japan"              ,
    "ko"      , "949"     , "Korean (Korea)"               , "Korean_Korea"                ,
    "lt"      , "1257"    , "Lithuanian (Lithuania)"       , "Lithuanian_Lithuania"        ,
    "Meta"    , ""        , ""                             , ""                            ,
    "nn"      , "865"     , "Norwegian Nynorsk (Norway)"   , "Norwegian-Nynorsk_Norway"    ,
    "pl"      , "1250"    , "Polish (Poland)"              , "Polish_Poland"               ,
    "pt_BR"   , "860"     , "Portuguese (Brazil)"          , "Portuguese_Brazil"           ,
    "ru"      , "1251"    , "Russian (Russia)"             , "Russian_Russia"              ,
    "tr"      , "1254"    , "Turkish (Turkey)"             , "Turkish_Turkey"              ,
    "zh_CN"   , "936"     , "Chinese (Simplified, China)"  , "Chinese (Simplified)_China"  ,
    "zh_TW"   , "950"     , "Chinese (Traditional, Taiwan)", "Chinese (Traditional)_Taiwan"
), ncol = 4L, byrow = TRUE)
rownames(.languages) <- .languages[, "LANGUAGE"]


.codepages <- .languages[, "codepage"]
.locales <- .languages[, "locale"]


.language_envvars <- function (LANGUAGE = Sys.getenv("LANGUAGE"), utf8 = identical(R.version[["crt"]], "ucrt"))
{
    if (!is.character(LANGUAGE) || length(LANGUAGE) != 1L)
        stop(gettextf("'%s' must be a character string",
            "LANGUAGE", domain = "R"), domain = NA)
    if (.Platform$OS.type == "windows") {
        if (!nzchar(LANGUAGE))
            return(c("LANGUAGE=", "LC_ALL="))
        LANGUAGE <- match.arg(LANGUAGE, c(rownames(.languages), NA))
        if (is.na(LANGUAGE))
            return(c("LANGUAGE=", "LC_ALL="))
        paste0(
            c("LANGUAGE=", "LC_ALL="),
            c(LANGUAGE, .locales[[LANGUAGE]]),
            if (nzchar(.locales[[LANGUAGE]]))
                c("", if (utf8) ".utf8" else paste0(".", .codepages[[LANGUAGE]]))
        )
    } else {
        if (!nzchar(LANGUAGE))
            return("LANGUAGE=")
        LANGUAGE <- match.arg(LANGUAGE, c(rownames(.languages), NA))
        if (is.na(LANGUAGE))
            return("LANGUAGE=")
        paste0("LANGUAGE=", LANGUAGE)
    }
}


Sys.putenv <- function (x)
{
    x <- as.character(x)
    m <- regexpr("=", x, fixed = TRUE, useBytes = TRUE)
    y <- regmatches(x, m, invert = TRUE)
    if (any(invalid <- lengths(y) != 2L)) {
        stop(ngettext(sum(invalid), "invalid environment variable:\n",
                                    "invalid environment variables:\n"),
             paste(utils::capture.output(x[invalid]), collapse = "\n"))
    }
    y <- .mapply(`Encoding<-`, list(y, Encoding(x)), NULL)
    args <- lapply(y, `[[`, 2L)
    names(args) <- vapply(y, `[[`, 1L, FUN.VALUE = "", USE.NAMES = FALSE)
    do.call("Sys.setenv", args)
}


if (sys.nframe() == 0L && !nzchar(Sys.getenv("R_PACKAGE_NAME"))) {
    stopifnot(.Platform$OS.type == "windows")


    main <- function() {


        testing <- FALSE
        # testing <- TRUE; warning("comment out 'testing <- TRUE' later", immediate. = TRUE)


        ## there was a time when I was doing something along the lines:
        ##
        ## ```
        ## for (language in rownames(.languages)) {
        ##     Sys.putenv(.language_envvars(language))
        ##     gettext("Untitled", domain = "RGui")
        ##     gettext("R Editor", domain = "RGui")
        ## }
        ## ```
        ##
        ## but it doesn't work well for Microsoft Visual C++ Runtime
        ## because the messages usually get mistranslated in the windows titles
        ## but they don't with gettext()
        ##
        ## we want the exact windows titles, mistranslated or otherwise
        ##
        ## and it also doesn't work well for Universal C Runtime in Lithuanian


        stopifnot(.Platform$OS.type == "windows")
        stopifnot(bindtextdomain("RGui") != "")


        exe <- "Rscript.exe"
        path <- R.home("..")
        path <- normalizePath(path, "/", TRUE)
        path <- list.files(path, full.names = TRUE)
        path <- file.path(path, "bin", exe)
        path <- path[file.access(path, 1L) == 0L]
        path <- c(file.path(R.home("bin"), exe), path)
        path <- utils::shortPathName(path)


        # args <- c("--version")
        # args <- c("--default-packages=NULL", "--vanilla", "-e", "utils::capture.output")
        # args <- paste(shQuote(args), collapse = " ")
        # command <- paste(shQuote(path), args)
        # names(command) <- path
        # x <- lapply(command, system, intern = TRUE)
        # print(x, quote = FALSE, width = 10)
        # stop("comment this out later")


        FILE.R <- tempfile(fileext = ".R")
        on.exit(unlink(FILE.R))
        writeLines("writeLines(c(identical(R.version[['crt']], 'ucrt'), as.character(getRversion()), file.path(R.home('bin'), 'Rgui.exe')))", FILE.R)
        args <- c("--default-packages=NULL", "--vanilla", FILE.R)
        args <- paste(shQuote(args), collapse = " ")
        command <- paste(shQuote(path), args)
        x <- vapply(command, system, intern = TRUE,
            FUN.VALUE = character(3), USE.NAMES = FALSE)
        on.exit()
        unlink(FILE.R)
        x <- data.frame(
            ucrt    = as.logical(x[1L, ]),
            version = numeric_version(x[2L, ]),
            rgui    = utils::shortPathName(x[3L, ])
        )
        x <- x[!duplicated(x$version), , drop = FALSE]
        x <- x[order(x$version, decreasing = TRUE), , drop = FALSE]


        fun <- function(...) {
            nms <- as.list(substitute(list(...)))[-1L]
            x <- list(...)
            x <- lapply(x, as.logical)
            if (is.null(names(x))) {
                names(x) <- nms
            } else if (any(no.names <- !nzchar(names(x)))) {
                nms <- as.character(nms)
                names(x)[no.names] <- nms[no.names]
            }
            i <- vapply(x, function(xx) {
                if (!any(xx))
                    0L
                else which(xx)[[1L]]
            }, FUN.VALUE = 0L)
            if (any(j <- !i)) {
                msgs <- paste0(names(x), " unavailable")
                if (all(j)) {
                    stop(paste(msgs, collapse = "\n "))
                } else warning(paste(msgs[j], collapse = "\n"))
                i <- i[!j]
            }
            i
        }


        i <- fun(
            `Universal C Runtime`          =  x$ucrt,
            `Microsoft Visual C++ Runtime` = !x$ucrt
        )
        x <- x[i, , drop = FALSE]


        write_r_editor <- function(rgui, ucrt) {


            # rgui <- x$rgui[[1L]]; warning("comment this out later", immediate. = TRUE)
            # ucrt <- x$ucrt[[1L]]; warning("comment this out later", immediate. = TRUE)


            # rgui <- x$rgui[[2L]]; warning("comment this out later", immediate. = TRUE)
            # ucrt <- x$ucrt[[2L]]; warning("comment this out later", immediate. = TRUE)


            FILES <- tempfile(fileext = c(".txt", ".R", ".Rprofile"))
            on.exit(unlink(FILES))
            FILE.txt <- FILES[[1L]]
            FILE.R   <- FILES[[2L]]
            Rprofile <- FILES[[3L]]
            file.create(FILE.txt)
            file.create(FILE.R)


            local({
                conn <- file(Rprofile, "wb", encoding = "")
                on.exit(close(conn))
                writeLines(paste0("FILE.txt <- rawToChar(as.raw(c(", paste0(as.integer(charToRaw(FILE.txt)), "L", collapse = ", "), ")))"), conn, useBytes = TRUE)
                writeLines(paste0("FILE.R   <- rawToChar(as.raw(c(", paste0(as.integer(charToRaw(FILE.R  )), "L", collapse = ", "), ")))"), conn, useBytes = TRUE)
                writeLines('
                    .First <- function() {
                        options(error = function() {
                            quit(save = "no", status = 1L)
                        })
                        reg.finalizer(environment(), function(e) {
                            conn <- e$conn
                            if (!is.null(conn) && isOpen(conn))
                                close(conn)
                        }, onexit = TRUE)
                        fun <- function() {
                            text <- names(utils::getWindowsHandles())[[1L]]
                            if (Encoding(text) == "unknown") {
                                loc <- l10n_info()
                                Encoding(text) <- if (loc$`UTF-8`)
                                    "UTF-8"
                                else if (loc$`Latin-1`)
                                    "latin1"
                                else "unknown"
                            }
                            writeLines(text, conn, useBytes = TRUE)
                            writeLines(Encoding(text), conn, useBytes = TRUE)
                        }
                        conn <- file(FILE.txt, "ab", encoding = "")
                        utils::file.edit(FILE.R)
                        fun()
                        utils::file.edit("")
                        fun()
                        quit(save = "no")
                    }
                ', conn, useBytes = TRUE)
            })


            R_PROFILE_USER <- Sys.getenv("R_PROFILE_USER", NA)
            if (is.na(R_PROFILE_USER)) {
                on.exit(Sys.unsetenv("R_PROFILE_USER"), add = TRUE, after = FALSE)
            } else {
                on.exit(Sys.setenv(R_PROFILE_USER = R_PROFILE_USER), add = TRUE, after = FALSE)
            }
            Sys.setenv(R_PROFILE_USER = Rprofile)


            ## we want to provide --vanilla to enable factory-default settings
            ## for Rgui.exe
            ##
            ## --vanilla is a combination of --no-save, --no-restore,
            ##           --no-site-file, --no-init-file, --no-environ,
            ##           and --no-Rconsole
            ##
            ## however, we want to run the init file, so instead of --vanilla,
            ## use the other arguments except --no-init-file
            options <- c("R_DEFAULT_PACKAGES=NULL", "--no-save", "--no-restore",
                "--no-site-file", "--no-environ", "--no-Rconsole")


            n <- 0L
            for (language in rownames(.languages)) {
                args <- c(rgui, .language_envvars(language, ucrt), options)
                command <- paste(shQuote(args), collapse = " ")
                ans <- system(command)
                if (ans) {
                    if (ans == -1L) {
                        stop(gettextf("'%s' could not be run",
                            command,      domain = "R-base"), domain = NA)
                    } else {
                        stop(gettextf("'%s' execution failed with error code %d",
                            command, ans, domain = "R-base"), domain = NA)
                    }
                }
                n <- n + 4L
                lines <- local({
                    oopt <- options(warn = 2L)
                    on.exit(options(oopt))
                    readLines(FILE.txt, n = n + 1L, warn = TRUE)
                })
                stopifnot(length(lines) == n)
            }


            readLines2 <- function(path, divisor = 2L) {
                conn <- file(path, "rb", encoding = "")
                on.exit(close(conn))
                x <- readLines(conn)
                if (!length(x) || length(x) %% divisor)
                    stop("invalid 'x'; should never happen, please report!")
                encoding <- x[c(FALSE, TRUE)]
                x        <- x[c(TRUE, FALSE)]
                Encoding(x) <- encoding
                x
            }


            txt <- readLines2(FILE.txt, divisor = 4L)


            r.editor <- txt[c(TRUE, FALSE)]
            untitled <- txt[c(FALSE, TRUE)]


            dir <- "./inst/extdata"
            valid.dir <- endsWith(
                tryCatch(normalizePath(dir, "/", TRUE), error = function(e) ""),
                "/this.path/inst/extdata"
            )
            suffix <- if (ucrt) "ucrt" else "msvcrt"
            r.editor.path <- sprintf("%s/r-editor_%s.txt", dir, suffix)
            untitled.path <- sprintf("%s/untitled_%s.txt", dir, suffix)


            if (testing) {


                if (!valid.dir || !file.exists(r.editor.path)) {
                    warning("\n no file to compare \" - R Editor\" strings against! Window titles:\n\n",
                            paste(unique(r.editor), collapse = "\n"),
                            call. = FALSE, immediate. = TRUE, domain = NA)
                    cat("\n", file = stderr())
                } else if (
                    any(invalid <- !(vapply(r.editor, function(str) {
                        paste(charToRaw(str), collapse = "")
                    }, FUN.VALUE = "", USE.NAMES = FALSE) %in% vapply(readLines2(r.editor.path), function(str) {
                        paste(c(charToRaw(FILE.R), charToRaw(str)), collapse = "")
                    }, FUN.VALUE = "", USE.NAMES = FALSE)))
                ) {
                    stop(ngettext(sum(invalid), "invalid \" - R Editor\" string:\n",
                                                "invalid \" - R Editor\" strings:\n"),
                         paste(utils::capture.output(r.editor[invalid]), collapse = "\n"))
                } else {
                    cat("\nAll \" - R Editor\" strings are valid!\n", r.editor, sep = "\n")
                }


                if (!valid.dir || !file.exists(untitled.path)) {
                    warning("\n no file to compare \"Untitled - R Editor\" strings against! Window titles:\n\n",
                            paste(unique(untitled), collapse = "\n"),
                            call. = FALSE, immediate. = TRUE, domain = NA)
                    cat("\n", file = stderr())
                } else if (
                    any(invalid <- !(vapply(untitled, function(str) {
                        paste(charToRaw(str), collapse = "")
                    }, FUN.VALUE = "", USE.NAMES = FALSE) %in% vapply(readLines2(untitled.path), function(str) {
                        paste(charToRaw(str), collapse = "")
                    }, FUN.VALUE = "", USE.NAMES = FALSE)))
                ) {
                    stop(ngettext(sum(invalid), "invalid \"Untitled - R Editor\" string:\n",
                                                "invalid \"Untitled - R Editor\" strings:\n"),
                         paste(utils::capture.output(untitled[invalid]), collapse = "\n"))
                } else {
                    cat("\nAll \"Untitled - R Editor\" strings are valid!\n", untitled, sep = "\n")
                }


            } else {


                ## string comparisons often involve translating between encodings
                ## so we will do raw comparisons instead to avoid translations
                r.editor <- vapply(r.editor, function(str) {
                    bytes <- charToRaw(str)
                    ## all "R Editor" strings must start with this prefix
                    prefix <- c(charToRaw(FILE.R), charToRaw(" - "))
                    if (length(bytes) < length(prefix) ||
                        any(bytes[seq_along(prefix)] != prefix))
                    {
                        stop("invalid \" - R Editor\" string: ", str, domain = NA)
                    }
                    value <- rawToChar(bytes[seq_along(bytes) > length(prefix) - 3L])
                    Encoding(value) <- Encoding(str)
                    value
                }, FUN.VALUE = "", USE.NAMES = FALSE)


                r.editor <- unique(r.editor)
                untitled <- unique(untitled)


                if (valid.dir) {


                    writeLines2 <- function(x, path) {
                        ## save the text as its bytes without translation
                        ## plus its encoding
                        conn <- file(path, "wb", encoding = "")
                        on.exit(close(conn))
                        writeLines(rbind(x, Encoding(x)), conn, sep = "\r\n", useBytes = TRUE)
                    }


                    writeLines2(r.editor, r.editor.path)
                    writeLines2(untitled, untitled.path)


                } else {


                    warning("\n no directory in which to write \" - R Editor\" strings:\n\n",
                            paste(r.editor, collapse = "\n"),
                            call. = FALSE, immediate. = TRUE, domain = NA)
                    cat("\n", file = stderr())


                    warning("\n no directory in which to write \"Untitled - R Editor\" strings:\n\n",
                            paste(untitled, collapse = "\n"),
                            call. = FALSE, immediate. = TRUE, domain = NA)
                    cat("\n", file = stderr())


                }


            }


            invisible(list(r.editor = r.editor, untitled = untitled))
        }


        invisible(lapply(seq_len(nrow(x)), function(i) {
            write_r_editor(rgui = x$rgui[[i]], ucrt = x$ucrt[[i]])
        }))
    }


    main()
}
