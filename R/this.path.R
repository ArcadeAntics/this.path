unix.command.line.argument.file.containing.space.fix <- function (path)
{
    # on a Unix-alike OS, you will experience the following bug when running R
    # scripts from the terminal:
    # all " " in argument 'FILE' will be replaced with "~+~".
    # To elaborate, the 'R' executable does open the correct file:
    #
    # R -f     '/tmp/RtmpqapV6Q/temp R script 15f42409a2.R'
    # R --file='/tmp/RtmpqapV6Q/temp R script 15f42409a2.R'
    #
    # will, in fact, open the file "/tmp/RtmpqapV6Q/temp R script 15f42409a2.R"
    # and start parsing and evaluating the code within that file as it should,
    # but the argument 'FILE' will be recorded incorrectly. If, in the script
    # "/tmp/RtmpqapV6Q/temp R script 15f42409a2.R", you had a piece of code that
    # looked like:
    #
    # print(commandArgs())
    #
    # you would see something like:
    #
    # [1] "R"
    # [2] "-f"
    # [3] "/tmp/RtmpqapV6Q/temp~+~R~+~script~+~15f42409a2.R"
    #
    # or
    #
    # [1] "R"
    # [2] "--file=/tmp/RtmpqapV6Q/temp~+~R~+~script~+~15f42409a2.R"
    #
    # as you can see, the filename is recorded incorrectly. this function should
    # be able to solve this issue


    # if the file exists with replacing all instances of "~+~" with " ", it is
    # assumed to be correct, and will be returned
    x <- gsub("~+~", " ", path, fixed = TRUE)
    if (file.exists(x))
        return(normalizePath(x, mustWork = TRUE))


    # if the file exists without replacing any instances of "~+~" with " ", it
    # is assumed to be correct, and will be returned
    if (file.exists(path) || !grepl("~+~", path, fixed = TRUE))
        return(normalizePath(path, mustWork = TRUE))


    # if we passed to condition above, it means
    # * original filename 'path' does not exist
    # * 'path' with all "~+~" replaced with " " does not exist
    # this must mean that some combination of "~+~" need to be replaced, but not
    # all and not none.
    #
    # record the original 'path' argument so
    # that it can be used in error messages
    opath <- path


    # find every instance of "~+~" in 'path' including nested cases ("~+~+~+~")
    first <- seq.int(1, nchar(path) - 2)
    m <- which(substring(path, first, first + 2L) == "~+~")
    attr(m, "match.length") <- rep(3L, length(m))
    attr(m, "index.type") <- "chars"
    attr(m, "useBytes") <- TRUE


    # the issue is that I don't know which instances of "~+~" need to be
    # replaced with " " and which do not need to be replaced
    #
    # the idea is that we attempt all possible combinations of replacing
    # "~+~" with " " and (hopefully) end up with one existing file
    #
    # Example: we have five instances of "~+~", then we have several
    # combinations to try:
    #
    # 0 replacements,  1 combination
    # 1 replacement ,  5 combinations
    # 2 replacements, 10 combinations
    # 3 replacements, 10 combinations
    # 4 replacements,  5 combinations
    # 5 replacements,  1 combination
    #
    # some of these combinations may be invalid
    # due to overlap, those will be handled later


    tmp <- c(TRUE, FALSE)
    tmp <- .mapply(base::rep, list(each = length(tmp)^(seq_along(m) - 1)), list(tmp))
    tmp <- asplit(do.call("cbind", tmp), 1)


    # each element of 'tmp' should be a different
    # combination of replacing "~+~" with " "
    #
    # each element of 'tmp' should be equal in length to the length of 'm'
    # (number of "~+~" found)
    #
    # there should be '2^length(m)' elements in 'tmp'
    #
    # there should be NO DUPLICATE ELEMENTS !!


    tmp <- lapply(tmp, function(i) {
        if (sum(i) == 0L) {  # if we are replacing NONE of "~+~"
            value <- -1L
            attr(value, "match.length") <- -1L
        }
        else {
            value <- m[i]
            attr(value, "match.length") <- attr(m, "match.length")[i]
        }


        # if 'value' contains no overlap
        if (all((value + attr(value, "match.length"))[-length(value)] <=
            value[-1L])) {
            attr(value, "index.type") <- "chars"
            attr(value, "useBytes") <- TRUE
            value
        }
    })


    # remove NULL values
    tmp <- tmp[!vapply(tmp, "is.null", NA)]


    # replace instances of "~+~" with " "
    path <- rep(path, length(tmp))
    regmatches(path, tmp) <- " "


    path <- path[file.exists(path)]  # select the filenames that exist
    if (length(path) == 0) {
        stop("unable to resolve Unix terminal argument 'FILE' conflict for file\n",
            "  ", encodeString(opath, quote = "\""), "\n",
            "* when running an R script that contains \" \" from the Unix terminal,\n",
            "  the R script's path is recorded incorrectly\n",
            "* each \" \" in argument 'FILE' is replaced by \"~+~\"\n",
            "* all possible combinations of replacing instances of \"~+~\" with \" \"\n",
            "  were attempted, but no file was found.")
    }
    else if (length(path) > 1) {
        stop("unable to resolve Unix terminal argument 'FILE' conflict for file\n",
            "  ", encodeString(opath, quote = "\""), "\n",
            "* when running an R script that contains \" \" from the Unix terminal,\n",
            "  the R script's path is recorded incorrectly\n",
            "* each \" \" in argument 'FILE' is replaced by \"~+~\"\n",
            "* all possible combinations of replacing instances of \"~+~\" with \" \"\n",
            "  were attempted, but multiple files were found.")
    }
    else normalizePath(path, mustWork = TRUE)
}


initialize.__file__ <- function ()
{
    # running from Windows command-line
    if (.Platform$OS.type == "windows" && .Platform$GUI == "RTerm") {


        # when running R from the Windows command-line, there are a few
        # things to keep in mind when trying to select the correct 'FILE' to
        # return. First, there are a few command-line arguments where the
        # name and value are separate. This means that the name of the
        # argument is the n-th argument and the value of the argument is the
        # n+1-th argument. Second, the --args command-line flag means that
        # all arguments after are for the R script to use while the
        # arguments before are for the 'R' executable. Finally, to take
        # input from a file, the two accepted methods are -f FILE and
        # --file=FILE where the input is taken from 'FILE'. If multiple of
        # these arguments are supplied, (it seems as though) 'R' takes input
        # from the last 'FILE' argument.


        ca <- commandArgs()


        # select the command-line arguments intended for the R executable.
        # Also remove the first argument, this is the name of the executable
        # by which this R process was invoked (not useful here)
        ca <- ca[seq_len(length(ca) - length(commandArgs(trailingOnly = TRUE)))]
        ca <- ca[-1L]
        if (!length(ca))
            return(invisible())


        # remove the --encoding enc command-line flags. this flag has the
        # highest priority, it is always handled first, regardless of the
        # preceding argument.
        #
        # Example:
        #
        # R -f --encoding UTF-8 FILE
        #
        # you might think the value for -f would be --encoding but it's
        # actually FILE because --encoding enc is processed first
        enc <- ca == "--encoding"
        if (any(enc)) {
            for (n in seq_len(length(enc) - 1L)) {
                if (enc[[n]])
                    enc[[n + 1L]] <- FALSE
            }
            which.enc <- which(enc)
            ca <- ca[-c(which.enc, which.enc + 1L)]
            if (!length(ca))
                return(invisible())
        }


        # there are 17 more arguments that are evaluated before -f is
        # grouped with its corresponding value. Some of them are static (the
        # first fifteen) while the others are variable (same beginning,
        # different values after)
        #
        # Example:
        #
        # R -f --silent --max-ppsize=100 FILE
        #
        # you might think the value of -f would be --silent but it's
        # actually FILE because --silent and --max-ppsize=N are processed
        # before -f is processed
        pre <- ca %in% c("--save", "--no-save", "--no-environ",
            "--no-site-file", "--no-init-file", "--restore",
            "--no-restore-data", "--no-restore-history", "--no-restore",
            "--vanilla", "-q", "--quiet", "--silent", "--no-echo",
            "--verbose") | grepl("^--(encoding|max-ppsize)=", ca)
        if (any(pre)) {
            ca <- ca[-which(pre)]
            if (!length(ca))
                return(invisible())
        }


        # next, we group the command-line arguments where the name and value
        # are separate. there are two left being -f FILE and -e EXPR.
        # find the locations of these special arguments
        special <- ca %in% c("-f", "-e")


        # next, we figure out which of these special arguments are ACTUALLY
        # special.
        #
        # Example:
        #
        # R -f -f -f -e
        #
        # here, the first and third -f are special
        # while the second -f and first -e are not
        if (any(special)) {
            for (n in seq_len(length(special) - 1L)) {
                if (special[[n]])
                    special[[n + 1L]] <- FALSE
            }
        }
        which.special <- which(special)


        # with the locations of the special arguments,
        #     figure out which of those are -f FILE arguments
        which.f <- which.special[ca[which.special] == "-f"]


        # use the locations of the special arguments to
        #     find the non-special argument locations
        which.non.special <- setdiff(seq_along(ca), c(which.special,
            which.special + 1L))


        # in these non-special command-line arguments,
        #     figure out which arguments start with --file=
        which.file <- which.non.special[grep("^--file=", ca[which.non.special])]


        # given the number of -f FILE and --file=FILE arguments,
        #     signal a possible error or warning
        n.file <- length(which.f) + length(which.file)
        if (!n.file)
            return(invisible())
        else if (n.file > 1L)
            stop("R is being run from the command-line and formal argument 'FILE' matched by multiple actual arguments")


        # since 'R' uses the last -f FILE or --file=FILE argument,
        #     use max to find the index of the last one of these arguments
        n <- max(which.f, which.file)
        if (n %in% which.file)
            path <- sub("^--file=", "", ca[[n]])
        else path <- ca[[n + 1L]]
        path <- normalizePath(path, winslash = "/", mustWork = TRUE)
        attr(path, "this.path.from.command-line") <- TRUE
        .__file__ <<- path
    }


    # running R from the Unix terminal with the default GUI
    else if (.Platform$OS.type == "unix" && .Platform$GUI == "X11") {


        # when running R from the Unix terminal, the command-line arguments
        # are parsed in a different manner (of course they are). It is far
        # less confusing to grab argument 'FILE' than above


        ca <- commandArgs()
        ca <- ca[seq_len(length(ca) - length(commandArgs(trailingOnly = TRUE)))]
        ca <- ca[-1L]
        if (!length(ca))
            return(invisible())


        # remove the --encoding enc command-line flags. this flag has the
        # highest priority, it is always handled first, regardless of the
        # preceding argument.
        #
        # Example:
        #
        # R --encoding -f FILE
        #
        # R --encoding --file=FILE
        #
        # you might think that -f FILE and --file=FILE would get processed,
        # but instead the encoding is taken as -f or --file=FILE
        enc <- ca == "--encoding"
        if (any(enc)) {
            for (n in seq_len(length(enc) - 1L)) {
                if (enc[[n]])
                    enc[[n + 1L]] <- FALSE
            }
            which.enc <- which(enc)
            ca <- ca[-c(which.enc, which.enc + 1L)]
            if (!length(ca))
                return(invisible())
        }


        which.f <- which(ca == "-f")
        which.file <- grep("^--file=", ca)


        n.file <- length(which.f) + length(which.file)
        if (!n.file)
            return(invisible())
        else if (n.file > 1L)
            stop("R is being run from the command-line and formal argument 'FILE' matched by multiple actual arguments")


        n <- max(which.f, which.file)
        if (n %in% which.file)
            path <- sub("^--file=", "", ca[[n]])
        else path <- ca[[n + 1L]]
        path <- unix.command.line.argument.file.containing.space.fix(path)
        attr(path, "this.path.from.command-line") <- TRUE
        .__file__ <<- path
    }
    invisible()
}
environment(initialize.__file__) <- new.env()
environment(initialize.__file__)$.__file__ <- NULL


tools.rstudio <- function (name)
{
    name <- substitute(name)
    name <- if (typeof(name) == "symbol")
        as.character(name)
    else if (typeof(name) == "character" && length(name) >= 1)
        .subset2(name, 1L)
    else stop("bad variable name")
    get(name, "tools:rstudio", inherits = FALSE)
}





.this.path_regexps <- list()
.this.path_regexps$windows.basename          <- local({


    # a regular expression for one character, determines if character is good in
    # windows BASENAMES. there are places where some of these characters are
    # allowed, for example, drives have a colon as the second character, and
    # folder paths have backslash or slash


    # abandoned because it is too many bytes long
    windows.basename.good.char <- function(excluding = "") {
        paste0("[^\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037\"*/:<>?\\\\|",
            excluding, "]")
    }
    windows.basename.good.char <- function(excluding = "") {
        paste0("(\177|[^[:cntrl:]\"*/:<>?\\\\|", excluding, "])")
    }


    # a windows basename is a series of one or more good characters
    #
    #
    # * does not start with a space
    # * does not end with a space or full stop
    #
    # or
    #
    # * is exactly "."
    # * is exactly ".."
    paste0(
        "(",
                "(",
                    windows.basename.good.char(excluding = " "),
                    windows.basename.good.char(), "*",
                ")?",
                windows.basename.good.char(excluding = " ."),
            "|",
                "\\.{1,2}",
        ")"
    )
})
.this.path_regexps$windows.relative.path     <- local({


    # a windows relative path is at least one basename,
    # separated from each other by backslash or slash
    paste0("(", .this.path_regexps$windows.basename, "[/\\\\])*", .this.path_regexps$windows.basename)
})
.this.path_regexps$windows.drive             <- local({


    # a windows drive is any upper case ASCII letter,
    # a colon, and a backslash or slash
    "[ABCDEFGHIJKLMNOPQRSTUVWXYZ]:[/\\\\]"
})
.this.path_regexps$windows.UNC.drive         <- local({


    paste0("[/\\\\]{2}", .this.path_regexps$windows.basename, "[/\\\\]", .this.path_regexps$windows.basename)
})
.this.path_regexps$windows.absolute.path     <- local({


    paste0(.this.path_regexps$windows.drive, "(", .this.path_regexps$windows.relative.path, ")?")
})
.this.path_regexps$windows.UNC.absolute.path <- local({


    paste0("[/\\\\]{2}", "(", .this.path_regexps$windows.basename, "[/\\\\])+", .this.path_regexps$windows.basename)
})
.this.path_regexps$Rgui.REditor              <- local({


    # as of this.path_0.5.0, we look for a different regular expression,
    # one that handles non-english languages. look for
    #
    # document-name (at least one character) followed by
    # " - R word1 word2 ..." or " - word1 word2 ... R"
    #
    # the number of words is at least one, translating to "R Editor". we allow
    # for multiple words for languages like "it" which ends with
    # " - Editor di R". words (including "R") may be separated by space or
    # hyphen. this means " - " will not count as a word, so the regular
    # expression won't be too greedy.


    # note that the definition of word here is different than the
    # usual word regular expression "\w+" meaning "[[:alnum:]_]+"
    #
    # here, a word is a collection of at least one character, none of which
    # are hyphen, space, full stop, slash, or backslash. we exclude
    #
    #     hyphen and space
    #         the word separators
    #
    #     full stop
    #         a path with a  file extension won't be flagged
    #         "file0123456789 - R Script.R"
    #
    #     slash and blackslash
    #         a path won't be flagged by accident
    #         "project0123456789 - R Project/file0123456789"


    word <- "[^- ./\\\\]+"
    sep <- "[- ]"
    words <- paste0("(", word, sep, ")*", word)
    end <- paste0(" - ", "(R", sep, words, "|", words, sep, "R)")
    paste0("(.+)", end)
})


.this.path_regexps$windows.absolute.path2     <- paste0("^", .this.path_regexps$windows.absolute.path    , "$")
.this.path_regexps$windows.UNC.absolute.path2 <- paste0("^", .this.path_regexps$windows.UNC.absolute.path, "$")
.this.path_regexps$Rgui.REditor2              <- paste0("^", .this.path_regexps$Rgui.REditor             , "$")





if (!all(nchar(.this.path_regexps, type = "bytes") < 256L))
    stop(gettext("each regular expression in '.this.path_regexps' must be less than 256 bytes"))





this.path_not_exists_error <- function (message, ..., class = "this.path_this.path_not_exists_error",
    call = NULL)
errorCondition(message = message, ..., class = class, call = call)


this.path_unimplemented_error <- function (message, ..., class = "this.path_this.path_unimplemented_error",
    call = NULL)
errorCondition(message = message, ..., class = class, call = call)


this.path <- function (verbose = getOption("verbose"))
{
    # function to print the method in which the
    # path of the executing script was determined
    where <- function(x) {
        if (verbose)
            cat("Source: ", x, "\n", sep = "")
    }


    # functions to get or check for an object in the n-th frame
    existsn <- function(x) exists(x, envir = sys.frame(n), inherits = FALSE)
    getn <- function(x) get(x, envir = sys.frame(n), inherits = FALSE)


    # function to save a path in the n-th frame
    assign.__file__ <- function(value = `attr<-`(normalizePath(path,
        winslash = "/", mustWork = TRUE), "this.path.n", n)) {
        assign(".__file__", value, envir = sys.frame(n), inherits = FALSE)
    }


    # loop through functions that lead here from most recent to earliest looking
    # for an appropriate source call (a call to function base::source,
    # base::sys.source, debugSource, or testthat::source_file)
    #
    # An appropriate source call is a source call in which argument 'file'
    # ('fileName' in the case of debugSource, 'path' in the case of
    # testthat::source_file) has been evaluated (forced). For example, the
    # following is an inappropriate source call:
    #
    # source(this.path())
    #
    # The argument 'file' is stored as a promise containing the expression
    # this.path(). When the value of 'file' is requested, it assigns the value
    # returned by evaluating this.path() to variable 'file'
    #
    # There are two functions on the calling stack at this point being source
    # and this.path. Clearly, you don't want to request the 'file' argument from
    # that source call because the value of 'file' is under evaluation right
    # now! The trick is to ask if a variable exists in that function's
    # evaluation environment that is only created AFTER 'file' has been forced.
    # For base::source, we ask if variable 'ofile' exists. For base::sys.source
    # and testthat::source_file, we ask if variable 'exprs' exists. For
    # debugSource, we cannot use this trick. Refer to the documentation below.
    #
    # If that variable exists, then argument 'file' has been forced and the
    # source call is deemed appropriate. If that variable does not exist, then
    # argument 'file' has not been forced and the source call is deemed
    # inappropriate. The loop continues to the next iteration (if available)


    # as of this.path_0.2.0, compatibility with 'debugSource' from the
    # 'RStudio' environment was added. 'debugSource' presents challenges that
    # other source functions do not. For example, it is impossible to test if
    # argument 'fileName' has been forced since all of the work is done
    # internally in C. This is why we use a 'tryCatch' statement instead of an
    # 'existsn'
    dbgS <- if (.Platform$GUI == "RStudio")
        tools.rstudio("debugSource")


    # as of this.path_0.4.0, compatibility with 'source_file' from package
    # 'testthat' was added. 'testthat::source_file' is almost identical to
    # 'base::sys.source'
    srcf <- if (isNamespaceLoaded("testthat"))
        getExportedValue("testthat", "source_file")


    for (n in seq.int(sys.nframe(), 1L)[-1L]) {
        if (identical(sys.function(n), base::source)) {


            # if the argument 'file' to 'base::source' has not been forced,
            # continue to the next iteration
            if (!existsn("ofile"))
                next


            # if the path has yet to be saved
            if (!existsn(".__file__")) {


                # retrieve the unmodified 'file' argument
                path <- getn("ofile")


                # there are two options for 'file'
                # * connection
                # * character string
                # start with character string
                if (is.character(path)) {


                    # use of "" refers to the R-level 'standard input' stdin.
                    # this means 'base::source' did not open a file, so we
                    # assign .__file__ the value of NULL and continue to the
                    # next iteration. We use .__file__ as NULL to skip this
                    # source call the next time this.path leads here
                    if (path == "") {
                        assign.__file__(NULL)
                        next
                    }


                    # use of "clipboard", "clipboard-128", and "stdin" refer to
                    # the clipboard or to the C-level 'standard input' of the
                    # process. this means 'base::source' did not open a file, so
                    # we assign .__file__ the value of NULL and continue to the
                    # next iteration. We use .__file__ as NULL to skip this
                    # source call the next time this.path leads here
                    else if (path %in% c("clipboard", "clipboard-128", "stdin")) {
                        assign.__file__(NULL)
                        next
                    }


                    # as of this.path_0.5.0, throw an error that 'file' cannot
                    # be a URL when mixed with 'this.path'
                    #
                    # previously, the error would've occured at 'normalizePath',
                    # so it makes more sense to have this error here
                    else if (grepl("^(ftp|ftps|http|https)://", path))
                        stop("'this.path' makes no sense for a URL")


                    # as of this.path_0.4.3, the path is determined slightly
                    # differently. this change is to account for two possible
                    # scenarios
                    # * source("file://absolute or relative path")
                    # * source("file:///absolute path")
                    # the description of this connection should remove the
                    # leading characters
                    else if (grepl("^file://", path)) {
                        con <- file(path, "r")
                        on.exit(close(con))
                        path <- summary.connection(con)$description
                        on.exit()
                        close(con)
                    }


                    # as of this.path_0.3.0, compatibility was added when
                    # using 'base::source' with 'chdir = TRUE'. this changes
                    # the working directory to the directory of the 'file'
                    # argument. since the 'file' argument was relative to
                    # the working directory prior to being changed, we need
                    # to change it to the previous working directory before
                    # we can normalize the path
                    else if (existsn("owd")) {
                        cwd <- getwd()
                        on.exit(setwd(cwd))
                        setwd(getn("owd"))
                    }
                }
                else {
                    # local({
                    #     on.exit(closeAllConnections())
                    #     cbind(
                    #         summary(a <- stdin()),
                    #         summary(c <- file("clipboard")),
                    #         summary(d <- file("clipboard-128")),
                    #         summary(e <- file("stdin")),
                    #         summary(f <- file("file://clipboard")),
                    #         summary(g <- file("file://clipboard-128")),
                    #         summary(h <- file("file://stdin"))
                    #     )
                    # })


                    path <- summary.connection(path)
                    if (path$class %in% c("terminal", "clipboard")) {
                        assign.__file__(NULL)
                        next
                    }
                    path <- path$description
                }


                # assign .__file__ as the absolute path
                assign.__file__()
            }
            else if (is.null(getn(".__file__")))
                next
            where("call to function source")
            return(getn(".__file__"))
        }
        else if (identical(sys.function(n), base::sys.source)) {


            # as with 'base::source', we check that argument 'file' has been
            # forced, and continue to the next iteration if not
            if (!existsn("exprs"))
                next


            # much the same as 'base::source' except simpler, we don't have to
            # account for argument 'file' being a connection or ""
            if (!existsn(".__file__")) {


                path <- getn("file")


                # unlike 'base::source', 'base::sys.source' is intended to
                # source a file (not a connection), so we have to throw an
                # error if the user attempts to source a file named
                # "clipboard", "clipboard-128", or "stdin" since none of these
                # refer to files
                if (path %in% c("clipboard", "clipboard-128", "stdin"))
                    stop(errorCondition("invalid 'file', must not be \"clipboard\", \"clipboard-128\", nor \"stdin\"",
                        call = sys.call(n)))


                else if (existsn("owd")) {
                    cwd <- getwd()
                    on.exit(setwd(cwd))
                    setwd(getn("owd"))
                }


                assign.__file__()
            }
            where("call to function sys.source")
            return(getn(".__file__"))
        }
        else if (!is.null(dbgS) && identical(sys.function(n), dbgS)) {


            # unlike 'base::source' and 'base::sys.source', there is no way to
            # check that argument 'fileName' has been forced, since all of the
            # work is done internally in C. Instead, we have to use a
            # 'tryCatch' statement. If argument 'fileName' has been forced, the
            # statement will proceed without an issue. If it has not, it is
            # because argument 'fileName' depends on itself recursively with
            # message "promise already under evaluation: recursive default
            # argument reference or earlier problems?". If you'd like to see,
            # try this:
            #
            # test <- function() get("fileName", sys.frame(1), inherits = FALSE)
            # debugSource(test())
            #
            # and you should see the error in which I'm referring. So the trick
            # is to use the 'tryCatch' statement to request argument
            # 'fileName', return TRUE if the statement proceeded without error
            # and FALSE if the statement produced an error.
            cond <- tryCatch({
                path <- getn("fileName")
                TRUE
            }, error = function(c) FALSE)
            if (!cond)
                next


            if (!existsn(".__file__")) {


                # we have to use 'enc2utf8' because 'debugSource' does as well
                path <- enc2utf8(path)


                if (path == "") {
                    assign.__file__(NULL)
                    next
                }
                else if (path %in% c("clipboard", "clipboard-128", "stdin")) {
                    assign.__file__(NULL)
                    next
                }
                else if (grepl("^(ftp|http|https)://", path))
                    stop("'this.path' makes no sense for a URL")
                else if (grepl("^file://", path)) {
                    con <- file(path, "r")
                    on.exit(close(con))
                    path <- summary.connection(con)$description
                    on.exit()
                    close(con)
                }
                assign.__file__()
            }
            else if (is.null(getn(".__file__")))
                next
            where("call to function debugSource in RStudio")
            return(getn(".__file__"))
        }
        else if (!is.null(srcf) && identical(sys.function(n), srcf)) {


            # as with 'base::source' and 'base::sys.source', we check that
            # argument 'path' has been forced, and continue to the next
            # iteration if not
            if (!existsn("exprs"))
                next


            if (!existsn(".__file__")) {


                path <- getn("path")


                # like 'base::sys.source', 'testthat::source_file' is intended
                # to source a file (not a connection), so we have to throw an
                # error if the user attempts to source a file named
                # "clipboard", "clipboard-128", or "stdin" since none of these
                # refer to files
                if (path %in% c("clipboard", "clipboard-128", "stdin"))
                    stop(errorCondition("invalid 'path' argument, must not be \"clipboard\", \"clipboard-128\", nor \"stdin\"",
                        call = sys.call(n)))


                else if (existsn("old_dir")) {
                    cwd <- getwd()
                    on.exit(setwd(cwd))
                    setwd(getn("old_dir"))
                }


                assign.__file__()
            }
            where("call to function source_file in package testthat")
            return(getn(".__file__"))
        }
    }


    # if the for loop is passed, no appropriate
    # source call was found up the calling stack
    #
    # next, check how R is being run
    #
    # * from Windows command-line or Unix terminal
    # * from Unix terminal with GUI 'Tk' (was treated as an unrecognized manner
    #       until this.path_0.5.0)
    # * from 'RStudio'
    # * from 'Rgui' on Windows (added in this.path_0.2.0)
    # * from 'Rgui' on macOS (also called 'AQUA'), signal an error
    # * unrecognized manner, signal an error


    if (.Platform$OS.type == "windows" && .Platform$GUI == "RTerm" ||  # running from Windows command-line
        .Platform$OS.type == "unix"    && .Platform$GUI == "X11") {    # running from Unix terminal with default GUI


        if (is.null(.__file__))
            stop(this.path_not_exists_error(paste0("'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* R is being run from the command-line and argument 'FILE' is missing"),
                call = sys.call(sys.nframe())))
        where("command-line argument 'FILE'")
        return(.__file__)
    }


    # running from Unix terminal with GUI 'Tk'
    else if (.Platform$OS.type == "unix" && .Platform$GUI == "Tk") {


        stop(this.path_not_exists_error(paste0(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from Tk which requires a source call on the calling stack"),
            call = sys.call(sys.nframe())))
    }


    # running from 'RStudio'
    else if (.Platform$GUI == "RStudio") {


        # function ".rs.api.getActiveDocumentContext" from the environment
        # "tools:rstudio" returns a list of information about the document where
        # your cursor is located
        #
        # function ".rs.api.getSourceEditorContext" from the environment
        # "tools:rstudio" returns a list of information about the document open
        # in the current tab
        #
        # element 'id' is a character string, an identification for the document
        # element 'path' is a character string, the path of the document


        context <- tools.rstudio(".rs.api.getActiveDocumentContext")()
        active <- context$id != "#console"
        if (!active) {
            context <- tools.rstudio(".rs.api.getSourceEditorContext")()
            if (is.null(context))
                stop(this.path_not_exists_error(paste0(
                    "'this.path' used in an inappropriate fashion\n",
                    "* no appropriate source call was found up the calling stack\n",
                    "* R is being run from RStudio with no documents open\n",
                    "  (or source document has no path)"),
                    call = sys.call(sys.nframe())))
        }
        path <- context$path
        if (nzchar(path)) {
            where(if (active)
                "active document in RStudio"
            else "source document in RStudio")
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }
        else stop("'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            if (active)
                "* active document in RStudio does not exist"
            else "* source document in RStudio does not exist")
    }


    # running from 'Rgui' on Windows
    else if (.Platform$OS.type == "windows" && .Platform$GUI == "Rgui") {


        # function "getWindowsHandles" from package "utils" (Windows exclusive)
        # returns a list of external pointers containing the windows handles.
        # the thing of interest are the names of this list, these should be the
        # names of the windows belonging to the current R process. Since 'Rgui'
        # can have files besides R scripts open (such as images), a regular
        # expression is used to subset only windows handles with names that
        # exactly match the string "R Console" or end with
        # " - R Editor". I highly suggest that you NEVER end a document's
        # filename with " - R Editor". From there, similar checks are done as in
        # the above section for 'RStudio'
        #
        #
        # as of this.path_0.5.0, we look for a different regular expression, one
        # that handles non-english languages. look for
        #
        # "R Console" (same as before)
        #
        # or
        #
        # "document-name" followed by
        # " - R word1 word2 ..." or " - word1 word2 ... R"
        #
        # the number of words is at least one, translating to "R Editor". we
        # allow for multiple words for languages like "it" which ends with
        # "Editor di R". the words (including "R") may be separated by a single
        # space or hyphen for languages like "nn" which ends with "R-redigering"
        #
        #
        # now, we do something even more different. retrieve all the script's
        # name


        # the previous regular expression exceeded 256 bytes, more than the
        # POSIX standard. now, each part of the regular expression is its own
        # regular expression of less than 256 bytes
        nm <- names(utils::getWindowsHandles())
        nm <- nm[nm == "R Console" |
            grepl(.this.path_regexps$Rgui.REditor2, nm) |
            grepl(.this.path_regexps$windows.absolute.path2, nm, ignore.case = TRUE) |
            grepl(.this.path_regexps$windows.UNC.absolute.path2, nm)]
        if (!length(nm))
            stop("no windows in Rgui; should never happen, please report!")
        if (active <- nm[[1L]] != "R Console") nm <- nm[[1L]]
        else if (length(nm) >= 2L) nm <- nm[[2L]]
        else stop(this.path_not_exists_error(paste0(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from Rgui with no documents open"),
            call = sys.call(sys.nframe())))
        path <- sub(.this.path_regexps$Rgui.REditor2, "\\1", nm)
        active <- active && nm != path
        if (grepl(.this.path_regexps$windows.absolute.path2, path, ignore.case = TRUE) ||
            grepl(.this.path_regexps$windows.UNC.absolute.path2, path)) {
            where(if (active)
                "active document in Rgui"
            else "source document in Rgui")
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }
        else stop("'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            if (active)
                "* active document in Rgui does not exist"
            else "* source document in Rgui does not exist")
    }


    # running from 'Rgui' on macOS
    else if (.Platform$OS.type == "unix" && .Platform$GUI == "AQUA") {


        stop(this.path_unimplemented_error(paste0(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from AQUA which requires a source call on the calling stack"),
            call = sys.call(sys.nframe())))
    }


    # running R in another manner
    else stop(this.path_unimplemented_error(paste0(
        "'this.path' used in an inappropriate fashion\n",
        "* no appropriate source call was found up the calling stack\n",
        "* R is being run in an unrecognized manner"),
        call = sys.call(sys.nframe())))
}
environment(this.path) <- environment(initialize.__file__)


this.dir <- function (...)
dirname(this.path(...))


`R CMD INSTALL-ing` <- function (pkgname)
{
    if (missing(pkgname))
        !is.na(Sys.getenv("R_INSTALL_PKG", NA))
    else Sys.getenv("R_INSTALL_PKG", NA) %in% pkgname
}


# i wanted to use 'this.path' to install this package, but that's a cyclic
# dependency. so i wrote this that will handle it correctly. we unload
# "this.path", use 'this.dir' from above to figure out where we are, and then
# 'INSTALL' the package accordingly
if (!`R CMD INSTALL-ing`("this.path")) {
    unloadNamespace("essentials")
    unloadNamespace("this.path")
    initialize.__file__()
    utils::install.packages(
        pkgs  = dirname(this.dir(verbose = FALSE)),
        repos = NULL,
        type  = "source",
        INSTALL_opts = "--with-keep.source"
    )
}
