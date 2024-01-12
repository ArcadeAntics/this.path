local({
    if (!file.exists(this.path::here("tools", "maintainers-copy")))
        stop("unable to '.check_this()', not the maintainer's copy")
    essentials:::.update_DESCRIPTION_Date()
    essentials:::.check_this(  ## this.path
        special = TRUE,

        # INSTALL = FALSE, # html = TRUE, latex = TRUE,
        # with.keep.source = TRUE,

        check = FALSE, no.stop.on.test.error = TRUE,
        as.cran = TRUE, `_R_CHECK_CRAN_INCOMING_` = TRUE,

        chdir = TRUE
    )
})


local({  ## for submitting to R Mac Builder https://mac.r-project.org/macbuilder/submit.html
    FILE <- "./tools/for-r-mac-builder"
            if (!file.create(FILE)) stop(sprintf("unable to create file '%s'", FILE))
    on.exit(if (!file.remove(FILE)) stop(sprintf("unable to remove file '%s'", FILE)))
    essentials:::.check_this(INSTALL = FALSE, check = FALSE, chdir = TRUE)
})


local({  ## for submitting to CRAN https://cran.r-project.org/submit.html
    upcoming.CRAN.version <- "2.4.0"
    if (!file.exists(this.path::here("tools", "maintainers-copy")))
        stop("unable to '.check_this()', not the maintainer's copy")





    DESCRIPTION.dcf <- this.path::here("DESCRIPTION")
    DESCRIPTION <- read.dcf(DESCRIPTION.dcf)
    if (nrow(DESCRIPTION) != 1L)
        stop("contains a blank line", call. = FALSE)
    DESCRIPTION <- DESCRIPTION[1L, ]


    ## re-read the file, providing 'keep.white' this time
    DESCRIPTION <- read.dcf(DESCRIPTION.dcf, keep.white = names(DESCRIPTION))
    if (nrow(DESCRIPTION) != 1L)
        stop("contains a blank line", call. = FALSE)
    DESCRIPTION <- DESCRIPTION[1L, ]


    DESCRIPTION[["Version"]] <- upcoming.CRAN.version
    temp.DESCRIPTION.dcf <- tempfile(fileext = ".dcf")
    if (!file.copy(DESCRIPTION.dcf, temp.DESCRIPTION.dcf, overwrite = TRUE, copy.date = TRUE))
        stop(sprintf("unable to copy file '%s' to '%s'", DESCRIPTION.dcf, temp.DESCRIPTION.dcf))
    on.exit({
        if (!file.copy(temp.DESCRIPTION.dcf, DESCRIPTION.dcf, overwrite = TRUE, copy.date = TRUE))
            stop(sprintf("unable to copy file '%s' to '%s'", temp.DESCRIPTION.dcf, DESCRIPTION.dcf))
    }, add = TRUE, after = FALSE)
    write.dcf(t(DESCRIPTION), DESCRIPTION.dcf, useBytes = !l10n_info()[["UTF-8"]],
        keep.white = names(DESCRIPTION))





    info.dcf <- this.path::here("tools", "info.dcf")
    info <- read.dcf(info.dcf)
    if (nrow(info) != 1L)
        stop("contains a blank line", call. = FALSE)
    info <- info[1L, ]


    ## re-read the file, providing 'keep.white' this time
    info <- read.dcf(info.dcf, keep.white = names(info))
    if (nrow(info) != 1L)
        stop("contains a blank line", call. = FALSE)
    info <- info[1L, ]


    info[["devel"]] <- "FALSE"
    temp.info.dcf <- tempfile(fileext = ".dcf")
    if (!file.copy(info.dcf, temp.info.dcf, overwrite = TRUE, copy.date = TRUE))
        stop(sprintf("unable to copy file '%s' to '%s'", info.dcf, temp.info.dcf))
    on.exit({
        if (!file.copy(temp.info.dcf, info.dcf, overwrite = TRUE, copy.date = TRUE))
            stop(sprintf("unable to copy file '%s' to '%s'", temp.info.dcf, info.dcf))
    }, add = TRUE, after = FALSE)
    write.dcf(t(info), info.dcf, useBytes = !l10n_info()[["UTF-8"]],
        keep.white = names(info))





    essentials:::.update.DESCRIPTION.Date()
    essentials:::.check_this(INSTALL = FALSE, check = FALSE, chdir = TRUE)
})


local({
    x <- this.path:::.readFiles(list.files("./src", full.names = TRUE))
    pattern <- "\n    do_start(?:|_no_call|_no_op|_no_rho|_no_call_op|_no_call_rho|_no_op_rho|_no_call_op_rho)\\(\"([^\n\"]+)\" *, *(-?[[:digit:]]+)\\);\n"
    m <- gregexec(pattern, x)
    keep <- which(lengths(m) > 1L)
    x <- x[keep]
    m <- m[keep]
    y <- regmatches(x, m)
    y <- lapply(y, `[`, c(FALSE, TRUE, TRUE))
    filenames <- names(y)
    times <- as.integer(lengths(y)/2)


    x <- readLines("./src/init.c")
    pattern <- "^\\{\"([^\"]+)\" *, \\(DL_FUNC\\) &do_.* *, *(-?[[:digit:]]+)\\},$"
    m <- regexec(pattern, x)
    keep <- which(lengths(m) > 1L)
    x <- x[keep]
    m <- m[keep]
    z <- regmatches(x, m)
    z <- lapply(z, `[`, c(FALSE, TRUE, TRUE))


    y <- matrix(unlist(y), nrow = 2L)
    z <- matrix(unlist(z), nrow = 2L)
    if (any(x <- !(y[1L, ] %in% z[1L, ]))) {
        warning("names found in files but not in init.c:\n",
            paste(utils::capture.output(split(y[1L, x], rep(filenames, times)[x])), collapse = "\n"))
        y <- y[, !x, drop = FALSE]
    }
    if (length(x <- setdiff(z[1L, ], y[1L, ]))) {
        warning("names found in init.c but not in files:\n",
            paste(utils::capture.output(x), collapse = "\n"))
        z <- z[, !(z[1L, ] %in% x), drop = FALSE]
    }
    x <- cbind(y, z)
    x <- lapply(split(seq_len(ncol(x)), x[1L, ]), function(i) as.integer(x[2L, i]))
    x <- x[lengths(lapply(x, unique)) != 1L]
    if (length(x))
        warning("found differing number of expected arguments:\n",
            paste(utils::capture.output(x), collapse = "\n"))
})


local({  ## testing this.path() with source(gzcon())
    FILE <- tempfile(fileext = ".R")
    on.exit(unlink(FILE), add = TRUE, after = FALSE)
    writeLines(c(
        "sys.frame(this.path:::.getframenumber())$ofile",
        "this.path::this.path(original = TRUE)",
        "this.path::this.path()"
    ), FILE)
    conn1 <- file(this.path::relpath(FILE))
    on.exit(close(conn1), add = TRUE, after = FALSE)
    source(conn1, echo = TRUE)
    conn2 <- gzcon(file(this.path::relpath(FILE), "rb"))
    on.exit(close(conn2), add = TRUE, after = FALSE)
    source(conn2, echo = TRUE)
})


local({
    files <- list.files(all.files = TRUE, full.names = TRUE, recursive = TRUE, include.dirs = FALSE, no.. = TRUE)
    files <- files[!startsWith(files, "./.git/")]
    files <- files[!startsWith(files, "./.Rproj.user/")]
    files <- files[!startsWith(files, "./this.path.Rcheck/")]
    files <- grep("^\\./this\\.path_([[:digit:]]+[.-]){1,}[[:digit:]]+(\\.tar\\.gz|\\.zip|\\.tgz)",
        files, value = TRUE, invert = TRUE)
    Rfiles <- files[grepl("(?i)\\.R$", basename(files))]
    Rdfiles <- files[grepl("(?i)\\.Rd$", basename(files))]
    files
    Rfiles
    Rdfiles


    x <- this.path:::.readFiles(files)
    x <- grep("\\.startup_info", x, value = TRUE)
    x <- x |> names() |> print(quote = FALSE, width = 10)
    x |> file.edit()


    ## ^.{0,62}\\$|^.{63,65535} +\\$


    x <- this.path:::.readFiles(Rfiles)
    x <- grep("utils::", x, value = TRUE)
    x <- x |> names() |> print(quote = FALSE, width = 10)
    x |> file.edit()


    x <- this.path:::.readFiles(Rdfiles)
    x <- grep(r"(\\Emacs|\\Jupyter|\\Python|\\radian|\\RStudio|\\VSCode)", x, value = TRUE)
    x <- x |> names() |> print(quote = FALSE, width = 10)
    x |> file.edit()
})


# .regexps <- list()
# .regexps$hexadecimal <- paste0(
#     "([-+])?",
#     "0[xX]",
#     "(",
#             "[[:xdigit:]]+(\\.[[:xdigit:]]*)?",
#         "|",
#             "\\.[[:xdigit:]]+",
#     ")",
#     "([Pp]([-+]?[[:digit:]]+))?"
# )
# .regexps$decimal <- paste0(
#     "(",
#             "[[:digit:]]+(\\.[[:digit:]]*)?",
#         "|",
#             "\\.[[:digit:]]+",
#     ")",
#     "([Ee]([-+]?[[:digit:]]+))?"
# )
# .regexps$numeric <- paste0(
#     "(",
#             .regexps$hexadecimal,
#         "|",
#             .regexps$decimal,
#     ")"
# )
#
#
# num.choices <- list(
#     sign  = c("", "-", "+"),
#     start = c("0x", "0X"),
#     num   = c("9AB", "9AB.", "9.AB", ".9AB")
# )
# exp.choices <- list(
#     start = c("P", "p"),
#     sign  = c("", "-", "+"),
#     num   = c("123")
# )
# combinations <- function (x, lex.order = FALSE)
# {
#     lens <- lengths(x)
#     length.out <- prod(lens)
#     if (length.out <= 0L)
#         return(list())
#     each <- if (lex.order)
#         rev(cumprod(c(1L, rev(lens)[-length(lens)])))
#     else    cumprod(c(1L,      lens[-length(lens)]))
#     essentials::plapply(list(
#         x = x,
#         each = each
#     ), base::rep, length.out = length.out)
# }
# x <- combinations(num.choices)
# y <- combinations(num.choices, lex.order = TRUE)
# essentials::psapply(x, paste0, USE.NAMES = FALSE)
# essentials::psapply(y, paste0, USE.NAMES = FALSE)
#
#
# num.choices <- essentials::pvapply(combinations(num.choices), paste0, FUN.VALUE = "", USE.NAMES = FALSE)
# exp.choices <- essentials::pvapply(combinations(exp.choices), paste0, FUN.VALUE = "", USE.NAMES = FALSE)
#
#
# choices <- list(
#     num.choices = num.choices,
#     exp.choices = c("", exp.choices)  # the exponent is optional
# )
# choices <- essentials::pvapply(combinations(choices), paste0, FUN.VALUE = "", USE.NAMES = FALSE)
#
#
# all(grepl(paste0("^(", .regexps$hexadecimal, ")$"), choices))
