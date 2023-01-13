languages <- matrix(dimnames = list(NULL, c(
    "LANGUAGE", "Format: (* Custom Locale)"    , "locale"                      )), data = c(
    "da"      , "Danish (Denmark)"             , "Danish_Denmark"              ,
    "de"      , "German (Germany)"             , "German_Germany"              ,
    "en"      , "English (World)"              , "English_World"               ,
    "en@quot" , "English (World)"              , "English_World"               ,
    "en_GB"   , "English (United Kingdom)"     , "English_United Kingdom"      ,
    "es"      , "Spanish (Spain)"              , "Spanish_Spain"               ,
    "fa"      , "Persian (Iran)"               , "Persian_Iran"                ,
    "fr"      , "French (France)"              , "French_France"               ,
    "it"      , "Italian (Italy)"              , "Italian_Italy"               ,
    "ja"      , "Japanese (Japan)"             , "Japanese_Japan"              ,
    "ko"      , "Korean (Korea)"               , "Korean_Korea"                ,
    "lt"      , "Lithuanian (Lithuania)"       , "Lithuanian_Lithuania"        ,
    "Meta"    , ""                             , ""                            ,
    "nn"      , "Norwegian Nynorsk (Norway)"   , "Norwegian-Nynorsk_Norway"    ,
    "pl"      , "Polish (Poland)"              , "Polish_Poland"               ,
    "pt_BR"   , "Portuguese (Brazil)"          , "Portuguese_Brazil"           ,
    "ru"      , "Russian (Russia)"             , "Russian_Russia"              ,
    "tr"      , "Turkish (Turkey)"             , "Turkish_Turkey"              ,
    "zh_CN"   , "Chinese (Simplified, China)"  , "Chinese (Simplified)_China"  ,
    "zh_TW"   , "Chinese (Traditional, Taiwan)", "Chinese (Traditional)_Taiwan"
), ncol = 3L, byrow = TRUE)
rownames(languages) <- languages[, "LANGUAGE"]


languageEnvvars <- function (LANGUAGE = Sys.getenv("LANGUAGE"), ucrt = identical(R.version[["crt"]], "ucrt"))
{
    if (.Platform$OS.type == "windows") {
        if (is.character(LANGUAGE) &&
            length(LANGUAGE) == 1L &&
            !is.na(LANGUAGE) &&
            LANGUAGE == "")
        {
            return(c("LANGUAGE=", "LC_ALL="))
        }
        LANGUAGE <- match.arg(LANGUAGE, c(rownames(languages), NA))
        if (is.na(LANGUAGE))
            return(c("LANGUAGE=", "LC_ALL="))
        paste0(
            c("LANGUAGE=", "LC_ALL="),
            c(LANGUAGE, languages[[LANGUAGE, "locale"]]),
            if (ucrt) c("", ".utf8") else ""
        )
    } else {
        if (is.character(LANGUAGE) &&
            length(LANGUAGE) == 1L &&
            !is.na(LANGUAGE) &&
            LANGUAGE == "")
        {
            return("LANGUAGE=")
        }
        LANGUAGE <- match.arg(LANGUAGE, c(rownames(languages), NA))
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
    args <- lapply(y, `[[`, 2L)
    names(args) <- vapply(y, `[[`, 1L, FUN.VALUE = "", USE.NAMES = FALSE)
    do.call("Sys.setenv", args)
}


if (!(isNamespace(environment()) &&
      getNamespaceName(environment()) == "this.path"))
{
    stopifnot(.Platform$OS.type == "windows")


    main <- function() {

        testing <- FALSE
        # testing <- TRUE


        # there was a time when I was doing something more along the lines:
        #
        # ```
        # for (language in rownames(languages)) {
        #     Sys.putenv(languageEnvvars(language))
        #     gettext("Untitled", domain = "RGui")
        #     gettext("R Editor", domain = "RGui")
        # }
        # ```
        #
        # but it doesn't work well for Microsoft Visual C++ Runtime
        # because the messages usually get mis-translated in the windows titles
        # but they don't with gettext()
        #
        # and it also doesn't work well for Universal C Runtime in Lithuanian


        stopifnot(.Platform$OS.type == "windows")
        stopifnot(bindtextdomain("RGui") != "")


        path <- R.home("..")
        path <- list.files(path, full.names = TRUE)
        path <- file.path(path, "bin", "Rscript.exe")
        path <- path[file.exists(path)]
        path <- c(file.path(R.home("bin"), "Rscript.exe"), path)


        FILE <- tempfile(fileext = ".R")
        on.exit(unlink(FILE), add = TRUE, after = FALSE)
        writeLines("writeLines(c(identical(R.version[['crt']], 'ucrt'), as.character(getRversion()), file.path(R.home('bin'), 'Rgui.exe')))", FILE)
        args <- c("--default-packages=NULL", "--vanilla", FILE)
        args <- paste(shQuote(args), collapse = " ")
        command <- paste(shQuote(path), args)
        x <- vapply(command, system, intern = TRUE,
            FUN.VALUE = character(3), USE.NAMES = FALSE)
        x <- data.frame(
            ucrt    = as.logical(x[1L, ]),
            version = as.numeric_version(x[2L, ]),
            rgui    = x[3L, ]
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


        # rgui <- x$rgui[[1L]]
        # ucrt <- x$ucrt[[1L]]


        # rgui <- x$rgui[[2L]]
        # ucrt <- x$ucrt[[2L]]


        write.r.editor <- function(rgui, ucrt) {


            FILES <- tempfile(fileext = c(".txt", ".R", ".Rprofile"))
            on.exit(unlink(FILES))
            tmptxt      <- FILES[[1L]]
            tmpR        <- FILES[[2L]]
            tmpRprofile <- FILES[[3L]]
            file.create(tmpR)


            local({
                conn <- file(tmpRprofile, "wb", encoding = "")
                on.exit(close(conn))
                writeLines(paste0("tmptxt <- rawToChar(as.raw(c(", paste0(as.integer(charToRaw(tmptxt)), "L", collapse = ", "), ")))"), conn, useBytes = TRUE)
                writeLines(paste0("tmpR   <- rawToChar(as.raw(c(", paste0(as.integer(charToRaw(tmpR  )), "L", collapse = ", "), ")))"), conn, useBytes = TRUE)
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
                        conn <- file(tmptxt, "ab", encoding = "")
                        utils::file.edit(tmpR)
                        text <- names(utils::getWindowsHandles())[[1L]]
                        if (Encoding(text) == "unknown") {
                            loc <- utils::localeToCharset()[1L]
                            Encoding(text) <- switch(loc, `UTF-8` = "UTF-8",
                                `ISO8859-1` = "latin1", "unknown")
                        }
                        writeLines(text, conn, useBytes = TRUE)
                        writeLines(Encoding(text), conn, useBytes = TRUE)
                        utils::file.edit("")
                        text <- names(utils::getWindowsHandles())[[1L]]
                        if (Encoding(text) == "unknown") {
                            loc <- utils::localeToCharset()[1L]
                            Encoding(text) <- switch(loc, `UTF-8` = "UTF-8",
                                `ISO8859-1` = "latin1", "unknown")
                        }
                        writeLines(text, conn, useBytes = TRUE)
                        writeLines(Encoding(text), conn, useBytes = TRUE)
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
            Sys.setenv(R_PROFILE_USER = tmpRprofile)


            # we want to provide --vanilla to enable factory-default settings
            # for Rgui.exe
            #
            # --vanilla is a combination of --no-save, --no-restore,
            #           --no-site-file, --no-init-file, --no-environ,
            #           and --no-Rconsole
            #
            # however, we want to run the init file, so instead of --vanilla,
            # use the other arguments except --no-init-file
            options <- c("R_DEFAULT_PACKAGES=NULL", "--no-save", "--no-restore",
                "--no-site-file", "--no-environ", "--no-Rconsole")


            n <- 0L
            for (language in rownames(languages)) {
                args <- c(rgui, options, languageEnvvars(language, ucrt = ucrt))
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
                    readLines(tmptxt, n = n + 1L, warn = TRUE)
                })
                stopifnot(length(lines) == n)
            }


            readLines2 <- function(path) {
                conn <- file(path, "rb", encoding = "")
                on.exit(close(conn))
                txt <- readLines(conn)
                if (length(txt) < 4L)
                    stop("invalid 'txt'; should never happen, please report!")
                encoding <- txt[c(FALSE, TRUE)]
                txt      <- txt[c(TRUE, FALSE)]
                Encoding(txt) <- encoding
                txt
            }


            txt <- local({
                txt <- readLines2(tmptxt)
                unknown <- Encoding(txt) == "unknown"
                if (any(unknown)) {
                    Encoding(txt)[unknown] <- if (ucrt) "UTF-8" else "latin1"
                }
                txt
            })


            r.editor <- txt[c(TRUE, FALSE)]
            untitled <- txt[c(FALSE, TRUE)]


            suffix <- if (ucrt) "ucrt" else "msvcrt"
            r.editor.path <- sprintf("./inst/extdata/r-editor_%s.txt", suffix)
            untitled.path <- sprintf("./inst/extdata/untitled_%s.txt", suffix)


            if (testing) {


                if (
                    any(invalid <- !(vapply(r.editor, function(str) {
                        paste(charToRaw(str), collapse = "")
                    }, FUN.VALUE = "", USE.NAMES = FALSE) %in% vapply(readLines2(r.editor.path), function(str) {
                        paste(c(charToRaw(tmpR), charToRaw(str)), collapse = "")
                    }, FUN.VALUE = "", USE.NAMES = FALSE)))
                ) {
                    stop(ngettext(sum(invalid), "invalid \" - R Editor\" string:\n",
                                                "invalid \" - R Editor\" strings:\n"),
                         paste(utils::capture.output(r.editor[invalid]), collapse = "\n"))
                } else {
                    cat("\nAll \" - R Editor\" strings are valid!\n")
                    print(r.editor)
                }


                if (
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
                    cat("\nAll \"Untitled - R Editor\" strings are valid!\n")
                    print(untitled)
                }


            } else {


                # string comparisons often involve translating between encodings
                # so we will do raw comparisons instead to avoid translations
                r.editor <- vapply(r.editor, function(str) {
                    bytes <- charToRaw(str)
                    # all "R Editor" strings must begin with this prefix
                    prefix <- c(charToRaw(tmpR), charToRaw(" - "))
                    if (length(bytes) < length(prefix) ||
                        any(bytes[seq_along(prefix)] != prefix))
                    {
                        stop("invalid \" - R Editor\" string: ", str, domain = NA)
                    }
                    value <- rawToChar(bytes[-seq_len(length(prefix) - 3L)])
                    Encoding(value) <- Encoding(str)
                    value
                }, FUN.VALUE = "", USE.NAMES = FALSE)


                r.editor <- unique(r.editor)
                untitled <- unique(untitled)


                writeLines2 <- function(txt, path) {
                    # save the text as its bytes without translation
                    # plus its encoding
                    conn <- file(path, "wb", encoding = "")
                    on.exit(close(conn))
                    writeLines(rbind(txt, Encoding(txt)), conn, useBytes = TRUE)
                }


                writeLines2(r.editor, r.editor.path)
                writeLines2(untitled, untitled.path)

            }


            invisible(list(r.editor = r.editor, untitled = untitled))
        }


        write.r.editor(rgui = x$rgui[[1L]], ucrt = x$ucrt[[1L]])
        write.r.editor(rgui = x$rgui[[2L]], ucrt = x$ucrt[[2L]])
    }


    main()
}





# relative.to <- this.path:::abspath("~/this.path/testing/code")
#
#
# path <- "C:\\Users\\andre\\Documents\\this.path\\testing\\this\\out"
# # path <- "C:Users\\andre\\Documents\\this.path\\testing\\this\\out"
# # path <- "test"
# # path <- "./."
# # path <- "\\\\LOCALHOST\\\\\\C$///////////////Users\\andre\\Documents\\this.path\\testing\\this\\out"
#
#
# normalizePath2 <- function (path, winslash = "\\")
# {
#     x <- path
#     p <- ""
#     delayedAssign("returnx", return(x))
#     repeat {
#         tryCatch2({
#             path <- normalizePath(path, winslash, TRUE)
#         }, error = function(e) {
#             b <- basename2(path)
#             if (b == "")
#                 returnx
#             if (b != ".")
#                 p <<- path.join(b, p)
#             path <<- dirname2(path)
#         }, else. = {
#             return(path.join(path, p))
#         })
#     }
# }
# environment(normalizePath2) <- getNamespace("this.path")
# normalizePath2 <- compiler::cmpfun(normalizePath2)
#
#
# normalizePath2(path)
# normalizePath2(relative.to)
#
#
# # path <- "C:\\Users\\andre\\Desktop"
#
#
#
#
# new.path.split.1 <- function (path)
# {
#     path <- chartr("\\", "/", path)
#     p <- character(0)
#     do({
#         p <- c(basename2(path), p)
#         path <- dirname2(path)
#     }) %until% (path == (dir <- dirname2(path)))
#     p <- c(path, p)
#     p <- p[p != "."]
#     if (length(p) <= 0L)
#         p <- "."
#     p
#     p[[1L]] <- gsub("(?<=[^/])/{2,}", "/", p[[1L]], perl = TRUE, useBytes = TRUE)
#     p[[1L]] <- sub("^(//[^/]+/+[^/]+)/$", "\\1", p[[1L]])
#     p
# }
# environment(new.path.split.1) <- getNamespace("this.path")
# new.path.split.1 <- compiler::cmpfun(new.path.split.1)
#
#
# new.path.split <- function (path, use.names = TRUE)
# {
#     if (!is.character(path))
#         stop(gettextf("invalid '%s' argument", "path", domain = "R"))
#     if (use.names) {
#         value <- lapply(path, new.path.split.1)
#         names(value) <- path
#         value
#     }
#     else lapply(path, new.path.split.1)
# }
# new.path.split <- compiler::cmpfun(new.path.split)
#
#
#
#
#
# xx           <- new.path.split.1(normalizePath2(path))
# relative.to2 <- new.path.split.1(normalizePath2(relative.to))
#
#
# len <- length(relative.to2)
# len2 <- length(xx)
# n <- min(len2, len)
# n <- match(FALSE, xx[seq_len(n)] == relative.to2[seq_len(n)], n + 1L) - 1L
# if (n == 0L) {
#     path
# } else {
#     value <- c(rep("..", len - n), xx[seq.int(n + 1L, length.out = len2 - n)])
#     if (length(value) <= 0L)
#         "."
#     else if (!(value[[1L]] %in% c(".", "..")))
#         paste(c(".", value), collapse = "/")
#     else paste(value, collapse = "/")
# }
