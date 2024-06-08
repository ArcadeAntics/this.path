.languages <- matrix(dimnames = list(NULL, c(
    "LANGUAGE", "Format: (* Custom Locale)"    , "locale.codepage"                                , "locale.utf8"                      )), data = c(
    "da"      , "Danish (Denmark)"             , "Danish_Denmark.865"                             , "Danish_Denmark.utf8"              ,
    "de"      , "German (Germany)"             , "German_Germany.1252"                            , "German_Germany.utf8"              ,
    "en"      , "English (World)"              , "English_World.1252"                             , "English_World.utf8"               ,
    "en@quot" , "English (World)"              , "English_World.1252"                             , "English_World.utf8"               ,
    "en_GB"   , "English (United Kingdom)"     , "English_United Kingdom.1252"                    , "English_United Kingdom.utf8"      ,
    "es"      , "Spanish (Spain)"              , "Spanish_Spain.1252"                             , "Spanish_Spain.utf8"               ,
    "fa"      , "Persian (Iran)"               , "Persian_Iran.1256"                              , "Persian_Iran.utf8"                ,
    "fr"      , "French (France)"              , "French_France.1252"                             , "French_France.utf8"               ,
    "it"      , "Italian (Italy)"              , "Italian_Italy.1252"                             , "Italian_Italy.utf8"               ,
    "ja"      , "Japanese (Japan)"             , "Japanese_Japan.932"                             , "Japanese_Japan.utf8"              ,
    "ko"      , "Korean (Korea)"               , "Korean_Korea.949"                               , "Korean_Korea.utf8"                ,
    "lt"      , "Lithuanian (Lithuania)"       , "Lithuanian_Lithuania.1257"                      , "Lithuanian_Lithuania.utf8"        ,
    "Meta"    , ""                             , ""                                               , ""                                 ,
    "nn"      , "Norwegian Nynorsk (Norway)"   , "Norwegian-Nynorsk_Norway.865"                   , "Norwegian Nynorsk_Norway.utf8"    ,
    "pl"      , "Polish (Poland)"              , "Polish_Poland.1250"                             , "Polish_Poland.utf8"               ,
    "pt_BR"   , "Portuguese (Brazil)"          , "Portuguese_Brazil.850"                          , "Portuguese_Brazil.utf8"           ,
    "ru"      , "Russian (Russia)"             , "Russian_Russia.1251"                            , "Russian_Russia.utf8"              ,
    "tr"      , "Turkish (T\u00FCrkiye)"       , `Encoding<-`("Turkish_T\xFCrkiye.1254", "latin1"), "Turkish_T\u00FCrkiye.utf8"        ,
    "zh_CN"   , "Chinese (Simplified, China)"  , "Chinese (Simplified)_China.936"                 , "Chinese (Simplified)_China.utf8"  ,
    "zh_TW"   , "Chinese (Traditional, Taiwan)", "Chinese (Traditional)_Taiwan.950"               , "Chinese (Traditional)_Taiwan.utf8"
), ncol = 4L, byrow = TRUE)
rownames(.languages) <- .languages[, "LANGUAGE"]


.language_envvars_list <- sapply(
    rownames(.languages),
    function(language) {
        sapply(
            c("codepage", "utf8"),
            function(x) {
                locale <- .languages[[language, paste0("locale.", x)]]
                c(
                    paste0("LANGUAGE=", language),
                    `Encoding<-`(paste0("LC_ALL=", `Encoding<-`(locale, "bytes")), Encoding(locale))
                )
            },
            simplify = FALSE
        )
    },
    simplify = FALSE
)


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
            c("LANGUAGE=", "LC_ALL=")
        else .language_envvars_list[[LANGUAGE]][[if (utf8) "utf8" else "codepage"]]
    } else {
        if (!nzchar(LANGUAGE))
            return("LANGUAGE=")
        LANGUAGE <- match.arg(LANGUAGE, c(rownames(.languages), NA))
        if (is.na(LANGUAGE))
            "LANGUAGE="
        else paste0("LANGUAGE=", LANGUAGE)
    }
}


Sys.putenv <- function (x)
{
    x <- as.character(x)
    m <- regexpr("=", x, fixed = TRUE, useBytes = TRUE)
    y <- regmatches(x, m, invert = TRUE)
    if (any(invalid <- lengths(y) != 2L)) {
        stop(
            ngettext(
                sum(invalid),
                "invalid environment variable:\n",
                "invalid environment variables:\n"
            ),
            paste(utils::capture.output(x[invalid]), collapse = "\n")
        )
    }
    y <- .mapply(`Encoding<-`, list(y, Encoding(x)), NULL)
    args <- lapply(y, `[[`, 2L)
    names(args) <- vapply(y, `[[`, "", 1L, USE.NAMES = FALSE)
    do.call("Sys.setenv", args)
}


.read_C_strings <- function (path, divisor = 1L)
{
    x <- readBin(path, "raw", file.size(path))
    if (!length(x))
        return(character())
    if (x[[length(x)]] != as.raw(0L))
        stop(sprintf("incomplete final string found on '%s'", path))
    if (length(nul <- which(x == as.raw(0L))) %% divisor)
        stop(sprintf("expected a multiple of %s strings on '%s'", divisor, path))
    x <- .mapply(
        function(to, length.out) {
            rawToChar(x[seq.int(to = to, length.out = length.out)])
        },
        list(
            to = nul - 1L,
            length.out = diff(c(0L, nul)) - 1L
        ),
        NULL
    )
    as.character(x)
}


.read_C_strings_with_encoding <- function (path, divisor = 1L)
{
    x <- .read_C_strings(path, divisor * 2L)
    if (length(x)) {
        encoding <- x[c(FALSE, TRUE)]
        x        <- x[c(TRUE, FALSE)]
        Encoding(x) <- encoding
    }
    x
}


if (sys.nframe() == 0L && !nzchar(Sys.getenv("R_PACKAGE_NAME"))) {
    stopifnot(.Platform$OS.type == "windows")


    main <- function() {


        testing <- FALSE
        # testing <- TRUE; warning("comment out 'testing <- TRUE' later", immediate. = TRUE)
        quiet_on_success <- testing


        tmpdir <- tempdir()
        if (!all(charToRaw(tmpdir) <= 0x7f)) {
            ## if the temporary directory does not consistent purely of ASCII
            ## characters, use a different directory
            tmpdirdir <- sprintf("%s\\tmp", Sys.getenv("SystemDrive"))
            if (!dir.exists(tmpdirdir)) {
                dir.create(tmpdirdir)
                on.exit(unlink(tmpdirdir, recursive = TRUE, force = TRUE, expand = FALSE), add = TRUE, after = FALSE)
            }
            tmpdir <- tempfile("Rtmp", tmpdirdir)
            tryCatch(dir.create(tmpdir),
            ## turn a warning into an error
            warning = stop
            )
            on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE, expand = FALSE), add = TRUE, after = FALSE)
        }


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
        stopifnot(identical(R.version[["crt"]], "ucrt"))
        stopifnot(getRversion() >= "4.2.0")
        stopifnot(l10n_info()[["UTF-8"]])


        exe <- "Rscript.exe"
        x <- Sys.getenv()
        path <- c(
            grep("(?i)^r_release$", names(x)),
            grep("(?i)^r_oldrel$", names(x)),
            grep("(?i)^r_[[:digit:]]+_[[:digit:]]+$", names(x))
        )
        path <- unique(path)
        path <- as.character(x[path])
        if (length(path)) {
            path <- file.path(path, exe)
            path <- path[file.access(path, 1L) == 0L]
            path <- c(file.path(R.home("bin"), exe), path)
        } else {
            path <- file.path(R.home(), "..")
            path <- normalizePath(path, "/", TRUE)
            path <- list.files(path, full.names = TRUE)
            path <- file.path(path, "bin", exe)
            path <- path[file.access(path, 1L) == 0L]
            path <- c(file.path(R.home("bin"), exe), path)
            path <- utils::shortPathName(path)
        }


        # args <- c("--version")
        # args <- c("--default-packages=NULL", "--vanilla", "-e", "utils::capture.output")
        # args <- paste(shQuote(args), collapse = " ")
        # command <- paste(shQuote(path), args)
        # names(command) <- path
        # x <- lapply(command, system, intern = TRUE)
        # print(x, quote = FALSE, width = 10)
        # stop("comment this out later")


        deparse_strings <- function(x) {
            if (!is.character(x))
                x <- as.character(x)
            vapply(
                x,
                function(x) {
                    if (nzchar(x))
                        sprintf(
                            "`Encoding<-`(rawToChar(as.raw(c(%s))), \"%s\")",
                            paste0(as.integer(charToRaw(x)), "L", collapse = ", "),
                            Encoding(x)
                        )
                    else "\"\""
                },
                ""
            )
        }


        get_R_info <- function(path) {
            tmpfiles <- tempfile(tmpdir = tmpdir, fileext = c(".R", ""))
            on.exit(unlink(tmpfiles), add = TRUE, after = FALSE)
            R_PROFILE_USER <- Sys.getenv("R_PROFILE_USER", NA)
            if (is.na(R_PROFILE_USER)) {
                on.exit(Sys.unsetenv("R_PROFILE_USER"), add = TRUE, after = FALSE)
            } else {
                on.exit(Sys.setenv(R_PROFILE_USER = R_PROFILE_USER), add = TRUE, after = FALSE)
            }
            Sys.setenv(R_PROFILE_USER = tmpfiles[[1L]])
            writeLines(
                sprintf(
                    '
                        .First <- function() {
                            conn <- file(%s, "ab", encoding = "")
                            on.exit(close(conn))
                            nul <- as.raw(0L)
                            writeBin(
                                c(
                                    charToRaw(if (identical(R.version[["crt"]], "ucrt")) "TRUE" else "FALSE"),
                                    nul,
                                    charToRaw(as.character(getRversion())),
                                    nul,
                                    charToRaw(R.home("bin")),
                                    nul,
                                    charToRaw(.Platform$r_arch),
                                    nul
                                ),
                                conn
                            )
                        }
                    ',
                    deparse_strings(tmpfiles[[2L]])
                ),
                tmpfiles[[1L]]
            )
            file.create(tmpfiles[[2L]])
            expected <- 4L
            n <- 0L
            x <- character()
            lapply(path, function(path) {
                args <- c(
                    path,
                    "--default-packages=NULL",
                    "--no-save",
                    "--no-restore",
                    "--no-site-file",
                    "--no-environ",
                    "--no-Rconsole",
                    "-e",
                    "NULL"
                )
                command <- paste(shQuote(args), collapse = " ")
                system(command, intern = TRUE)
                n <<- n + expected
                x <<- .read_C_strings(tmpfiles[[2L]])
                stopifnot(length(x) == n)
            })
            ## break into groups of 'expected'
            x <- split(x, seq.int(0L, along.with = x) %/% expected)
            x <- data.frame(
                ucrt      =       as.logical(vapply(x, `[[`, "", 1L)),
                version   = R_system_version(vapply(x, `[[`, "", 2L)),
                R_BIN_DIR =                  vapply(x, `[[`, "", 3L),
                r_arch    =                  vapply(x, `[[`, "", 4L)
            )
            x$major <- vapply(unclass(x$version), `[[`, 0L, 1L)
            x$minor <- vapply(unclass(x$version), `[[`, 0L, 2L)
            x$patch <- vapply(unclass(x$version), `[[`, 0L, 3L)
            x
        }
        x <- get_R_info(path)
        x$rgui <- file.path(x$R_BIN_DIR, "Rgui.exe")
        x <- x[x$version >= "2.15.0", , drop = FALSE]
        x <- x[order(x$version, decreasing = TRUE), , drop = FALSE]
        proto <- x[integer(), , drop = FALSE]
        x <- split(x, interaction(x$major, x$minor, drop = TRUE, lex.order = TRUE))
        x <- lapply(x, function(x) {
            if (nrow(x) == 1L)
                x
            else {
                x <- x[x[["version"]] == x[[1L, "version"]], , drop = FALSE]
                if (nrow(x) == 1L)
                    x
                else {
                    if (length(i <- which(x$r_arch == "x64")))
                        x[i[[1L]], , drop = FALSE]
                    else if (length(i <- which(x$r_arch != "i386")))
                        x[i[[1L]], , drop = FALSE]
                    else x[1L, , drop = FALSE]
                }
            }
        })
        x <- do.call("rbind", x)
        x <- rbind(proto, x)
        x <- x[order(x$version), , drop = FALSE]
        x$suffix <- paste(ifelse(x$ucrt, "ucrt", "msvcrt"), x$major, x$minor, sep = "_", recycle0 = TRUE)
        x$suffixes <- I(vector("list", nrow(x)))
        invisible(lapply(
            split(seq_len(nrow(x)), x$ucrt),
            function(i) {
                x$suffixes[i] <<- lapply(
                    seq_along(i),
                    function(j) rev(x[i[seq_len(j)], "suffix"])
                )
            }
        ))
        x$suffix <- NULL


        write_r_editor <- function(rgui, ucrt, suffixes) {


            # i <- 1L; rgui <- x[[i, "rgui"]]; ucrt <- x[[i, "ucrt"]]; suffixes <- x[[i, "suffixes"]]; warning("comment this out later", immediate. = TRUE)


            FILES <- tempfile(tmpdir = tmpdir, fileext = c(".dat", ".R", ".Rprofile", ".Renviron"))
            on.exit(unlink(FILES), add = TRUE, after = FALSE)
            FILE.dat <- FILES[[1L]]
            FILE.R   <- FILES[[2L]]
            Rprofile <- FILES[[3L]]
            Renviron <- FILES[[4L]]
            file.create(FILE.dat)
            file.create(FILE.R)


            local({
                conn <- file(Rprofile, "wb", encoding = "")
                on.exit(close(conn), add = TRUE, after = FALSE)
                writeLines(paste0("FILE.dat <- ", deparse_strings(FILE.dat)), conn, useBytes = TRUE)
                writeLines(paste0("FILE.R   <- ", deparse_strings(FILE.R  )), conn, useBytes = TRUE)
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
                            nul <- as.raw(0L)
                            writeBin(
                                c(
                                    charToRaw(text),
                                    nul,
                                    charToRaw(Encoding(text)),
                                    nul
                                ),
                                conn
                            )
                        }
                        conn <- file(FILE.dat, "ab", encoding = "")
                        utils::file.edit(FILE.R)
                        fun()
                        utils::file.edit("")
                        fun()
                        quit(save = "no")
                    }
                ', conn, useBytes = TRUE)
            })


            make_Renviron <- function(language, ucrt) {
                x <- .language_envvars(language, ucrt)
                x <- c(x, "R_DEFAULT_PACKAGES=NULL")
                conn <- file(Renviron, "wb", encoding = "")
                on.exit(close(conn), add = TRUE, after = FALSE)
                writeLines(x, conn, useBytes = TRUE)
                invisible()
            }


            R_PROFILE_USER <- Sys.getenv("R_PROFILE_USER", NA)
            if (is.na(R_PROFILE_USER)) {
                on.exit(Sys.unsetenv("R_PROFILE_USER"), add = TRUE, after = FALSE)
            } else {
                on.exit(Sys.setenv(R_PROFILE_USER = R_PROFILE_USER), add = TRUE, after = FALSE)
            }
            Sys.setenv(R_PROFILE_USER = Rprofile)


            R_ENVIRON_USER <- Sys.getenv("R_ENVIRON_USER", NA)
            if (is.na(R_ENVIRON_USER)) {
                on.exit(Sys.unsetenv("R_ENVIRON_USER"), add = TRUE, after = FALSE)
            } else {
                on.exit(Sys.setenv(R_ENVIRON_USER = R_ENVIRON_USER), add = TRUE, after = FALSE)
            }
            Sys.setenv(R_ENVIRON_USER = Renviron)


            ## we want to provide --vanilla to enable factory-default settings
            ## for Rgui.exe
            ##
            ## --vanilla is a combination of --no-save, --no-restore,
            ##           --no-site-file, --no-init-file, --no-environ,
            ##           and --no-Rconsole
            ##
            ## however, we want to run the environ and init file, so instead of
            ## --vanilla, use the other arguments except --no-environ and
            ## --no-init-file
            options <- c("--no-save", "--no-restore", "--no-site-file", "--no-Rconsole")


            n <- 0L
            args <- c(rgui, options)
            command <- paste(shQuote(args), collapse = " ")
            for (language in rownames(.languages)) {
                make_Renviron(language, ucrt)
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
                stopifnot(length(.read_C_strings(FILE.dat)) == n)
            }


            txt <- .read_C_strings_with_encoding(FILE.dat, divisor = 2L)


            r_editor <- txt[c(TRUE, FALSE)]
            untitled <- txt[c(FALSE, TRUE)]


            dir <- "./inst/extdata"
            valid_dir <- endsWith(
                tryCatch(normalizePath(dir, "/", TRUE), error = function(e) ""),
                "/this.path/inst/extdata"
            )
            r_editor_paths <- sprintf("%s/r-editor_%s.dat", dir, suffixes)
            untitled_paths <- sprintf("%s/untitled_%s.dat", dir, suffixes)


            if (testing) {


                first <- TRUE
                print_R_version <- function() {
                    if (first) {
                        first <<- FALSE
                        cat("\nin ", rgui, "\n\n", sep = "")
                    }
                }


                if (!valid_dir || is.na(r_editor_path <- r_editor_paths[match(TRUE, file.exists(r_editor_paths))])) {
                    print_R_version()
                    warning(
                        "\n no file to compare \" - R Editor\" strings against! Window titles:\n\n",
                        paste(unique(r_editor), collapse = "\n"),
                        call. = FALSE,
                        immediate. = TRUE,
                        domain = NA
                    )
                } else if (
                    any(invalid <- !(
                        vapply(
                            r_editor,
                            function(str) {
                                paste(charToRaw(str), collapse = "")
                            },
                            ""
                        ) %in% vapply(
                            .read_C_strings_with_encoding(r_editor_path),
                            function(str) {
                                paste(c(charToRaw(FILE.R), charToRaw(str)), collapse = "")
                            },
                            ""
                        )
                    ))
                ) {
                    print_R_version()
                    warning(
                        ngettext(
                            sum(invalid),
                            "invalid \" - R Editor\" string:\n",
                            "invalid \" - R Editor\" strings:\n"
                        ),
                        paste(utils::capture.output(r_editor[invalid]), collapse = "\n"),
                        call. = FALSE,
                        immediate. = TRUE
                    )
                } else if (!quiet_on_success) {
                    print_R_version()
                    cat("\nAll \" - R Editor\" strings are valid!\n", r_editor, sep = "\n")
                }


                if (!valid_dir || is.na(untitled_path <- untitled_paths[match(TRUE, file.exists(untitled_paths))])) {
                    print_R_version()
                    warning(
                        "\n no file to compare \"Untitled - R Editor\" strings against! Window titles:\n\n",
                        paste(unique(untitled), collapse = "\n"),
                        call. = FALSE,
                        immediate. = TRUE,
                        domain = NA
                    )
                } else if (
                    any(invalid <- !(
                        vapply(
                            untitled,
                            function(str) {
                                paste(charToRaw(str), collapse = "")
                            },
                            ""
                        ) %in% vapply(
                            .read_C_strings_with_encoding(untitled_path),
                            function(str) {
                                paste(charToRaw(str), collapse = "")
                            },
                            ""
                        )
                    ))
                ) {
                    print_R_version()
                    warning(
                        ngettext(
                            sum(invalid),
                            "invalid \"Untitled - R Editor\" string:\n",
                            "invalid \"Untitled - R Editor\" strings:\n"
                        ),
                        paste(utils::capture.output(untitled[invalid]), collapse = "\n"),
                        call. = FALSE,
                        immediate. = TRUE
                    )
                } else if (!quiet_on_success) {
                    print_R_version()
                    cat("\nAll \"Untitled - R Editor\" strings are valid!\n", untitled, sep = "\n")
                }


            } else {


                ## string comparisons often involve translating between encodings
                ## so we will do raw comparisons instead to avoid translations
                r_editor <- vapply(r_editor, function(str) {
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
                }, "", USE.NAMES = FALSE)


                ## don't use unique() since that converts strings to same encoding,
                ## instead, convert to raw then remove duplicates
                r_editor <- r_editor[!duplicated(lapply(r_editor, charToRaw))]
                untitled <- untitled[!duplicated(lapply(untitled, charToRaw))]


                if (valid_dir) {


                    .write_C_strings <- function(x, path) {
                        conn <- file(path, "wb")
                        on.exit(close(conn), add = TRUE, after = FALSE)
                        lapply(
                            rbind(x, Encoding(x)),
                            function(y) writeBin(c(charToRaw(y), as.raw(0L)), conn)
                        )
                        invisible()
                    }


                    r_editor_path <- r_editor_paths[[1L]]
                    untitled_path <- untitled_paths[[1L]]


                    .write_C_strings(r_editor, r_editor_path)
                    .write_C_strings(untitled, untitled_path)
                    ## if the files are redundant, remove them
                    for (previous in r_editor_paths[-1L]) {
                        if (file.exists(previous)) {
                            if (identical(
                                readBin(r_editor_path, "raw", file.size(r_editor_path)),
                                readBin(previous, "raw", file.size(previous))
                            )) {
                                unlink(r_editor_path)
                            }
                            break
                        }
                    }
                    for (previous in untitled_paths[-1L]) {
                        if (file.exists(previous)) {
                            if (identical(
                                readBin(untitled_path, "raw", file.size(untitled_path)),
                                readBin(previous, "raw", file.size(previous))
                            )) {
                                unlink(untitled_path)
                            }
                            break
                        }
                    }


                } else {


                    warning(
                        "\n no directory in which to write \" - R Editor\" strings:\n\n",
                        paste(r_editor, collapse = "\n"),
                        call. = FALSE,
                        immediate. = TRUE,
                        domain = NA
                    )


                    warning(
                        "\n no directory in which to write \"Untitled - R Editor\" strings:\n\n",
                        paste(untitled, collapse = "\n"),
                        call. = FALSE,
                        immediate. = TRUE,
                        domain = NA
                    )


                }


            }


            invisible(list(r_editor = r_editor, untitled = untitled))
        }


        invisible(.mapply(
            write_r_editor,
            x[c("rgui", "ucrt", "suffixes")],
            NULL
        ))
    }


    main()
}
