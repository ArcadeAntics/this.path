.Defunct2 <- function (new, old = as.character(sys.call(sys.parent()))[1L])
{
    .Defunct(msg = c(
        gettextf("'%s' is defunct.\n", old, domain = "R-base"),
        gettextf("Use '%s' instead.\n", new, domain = "R-base"),
        gettextf("See help(\"Defunct\")", domain = "R-base")
    ))
}





.shFILE <- function (original = TRUE)
{
    # the details between how the shell arguments are handled is
    # different on Windows and under Unix-alikes, see the comments below.


    # when running R from a shell, there are a few things to keep in mind when
    # trying to select the correct 'FILE' to return. First, there are a few
    # shell arguments where the name and value are separate. This means that
    # the name of the argument is the n-th argument and the value of the
    # argument is the (n+1)-th argument. Second, the --args shell argument
    # means that all arguments after are for the R script to use while the
    # arguments before are for the 'R' executable. Finally, to take input from
    # a file, the two accepted methods are -f FILE and --file=FILE where the
    # input is taken from 'FILE'. If multiple of these arguments are supplied,
    # (it seems as though) 'R' takes input from the last 'FILE' argument.


    # return the already saved result, if available
    if (is.null(.__ofile__)) {


        # running R from a shell on Windows
        if (os.windows.in.shell) {


            ca <- commandArgs()


            # select the shell arguments intended for the R executable. also remove
            # the first argument, this is the name of the executable by which this
            # R process was invoked (not useful here)
            ca <- ca[seq_len(length(ca) - length(commandArgs(trailingOnly = TRUE)))]
            ca <- ca[-1L]
            if (length(ca) <= 0L) {
                .__ofile__ <<- NA_character_
                return(fun(original))
            }


            # remove the --encoding enc shell arguments. these arguments have the
            # highest priority (ALWAYS handled first), regardless of the preceding
            # argument.
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
                enc <- which(enc)
                ca <- ca[-c(enc, enc + 1L)]
                if (length(ca) <= 0L) {
                    .__ofile__ <<- NA_character_
                    return(fun(original))
                }
            }


            # there are 17 more arguments that are evaluated before -f is
            # grouped with its corresponding value. Some of them are constant (the
            # first fifteen) while the others are variable (same beginning,
            # different values after)
            #
            # Example:
            #
            # R -f --silent --max-ppsize=10000 FILE
            #
            # you might think the value of -f would be --silent but it's
            # actually FILE because --silent and --max-ppsize=N are processed
            # before -f is processed
            pre <- ca %in% c(
                "--save"              , "--no-save"           ,
                "--no-environ"        , "--no-site-file"      , "--no-init-file"      ,
                "--restore"           , "--no-restore-data"   ,
                "--no-restore-history", "--no-restore"        ,
                "--vanilla"           ,
                "-q"                  , "--quiet"             , "--silent"            ,
                "--no-echo"           , "--verbose"
            ) | grepl("^--(encoding|max-ppsize)=", ca)
            if (any(pre)) {
                ca <- ca[!pre]
                if (length(ca) <= 0L) {
                    .__ofile__ <<- NA_character_
                    return(fun(original))
                }
            }


            # next, we group the shell arguments where the name and value are
            # separate. there are two left being -f FILE and -e EXPR. find the
            # locations of these special arguments
            special <- ca %in% c("-f", "-e")


            # next, we figure out which of these special arguments are ACTUALLY
            # special.
            #
            # Example:
            #
            # R -f -f -f -e
            # R -e -e -e -f
            #
            # the first and third arguments are special
            # while the second and fourth are not
            if (any(special)) {
                for (n in seq_len(length(special) - 1L)) {
                    if (special[[n]])
                        special[[n + 1L]] <- FALSE
                }
            }
            special <- which(special)


            # with the locations of the special arguments,
            #     figure out which of those are -f FILE arguments
            f <- special[ca[special] == "-f"]


            # use the locations of the special arguments to
            #     find the non-special argument locations
            not.special <- setdiff(seq_along(ca), c(special, special + 1L))


            # in these non-special shell arguments,
            #     figure out which arguments start with --file=
            file <- not.special[grep("^--file=", ca[not.special])]


            # given the number of -f FILE and --file=FILE arguments,
            #     signal a possible error or warning
            n <- length(f) + length(file)
            if (n <= 0L) {
                .__ofile__ <<- NA_character_
                return(fun(original))
            }
            else if (n > 1L)
                stop("R is being run from a shell and formal argument 'FILE' matched by multiple actual arguments")


            # since 'R' uses the last -f FILE or --file=FILE argument,
            #     use max to find the index of the last one of these arguments
            n <- max(f, file)
            .__ofile__ <<- {
                if (n %in% file)
                    sub("^--file=", "", ca[[n]])
                else ca[[n + 1L]]
            }
        }


        # running R from a shell under Unix-alikes with the default GUI (or similar)
        else if (os.unix.in.shell) {


            # when running R from a shell under Unix-alikes, the shell arguments
            # are parsed in a different manner (of course they are, smh). luckily,
            # it is far less confusing to grab argument 'FILE' than above


            ca <- commandArgs()


            ca <- ca[seq_len(length(ca) - length(commandArgs(trailingOnly = TRUE)))]
            ca <- ca[-1L]
            if (length(ca) <= 0L) {
                .__ofile__ <<- NA_character_
                return(fun(original))
            }


            enc <- ca == "--encoding"
            if (any(enc)) {
                for (n in seq_len(length(enc) - 1L)) {
                    if (enc[[n]])
                        enc[[n + 1L]] <- FALSE
                }
                enc <- which(enc)
                ca <- ca[-c(enc, enc + 1L)]
                if (length(ca) <= 0L) {
                    .__ofile__ <<- NA_character_
                    return(fun(original))
                }
            }


            f <- which(ca == "-f")
            file <- grep("^--file=", ca)


            n <- length(f) + length(file)
            if (n <= 0L) {
                .__ofile__ <<- NA_character_
                return(fun(original))
            }
            else if (n > 1L)
                stop("R is being run from a shell and formal argument 'FILE' matched by multiple actual arguments")


            n <- max(f, file)
            .__ofile__ <<- gsub("~+~", " ", {
                if (n %in% file)
                    sub("^--file=", "", ca[[n]])
                else ca[[n + 1L]]
            }, fixed = TRUE)
        }


        else .__ofile__ <<- NA_character_
    }


    fun(original)
}
tmp <- new.env()
environment(.shFILE) <- tmp
evalq(envir = tmp, {
    .__ofile__ <- NULL
    delayedAssign(".__file__", normalizePath(.__ofile__, winslash = "/", mustWork = TRUE))
    fun <- function(original) {
        if (original) .__ofile__ else .External2(C_getpromisewithoutwarning, ".__file__")
    }
})
lockEnvironment(tmp)
lockBinding(".__file__", tmp)
lockBinding("fun", tmp)
rm(tmp)


shFILE <- function (original = FALSE, default, else.)
{
    if (missing(default))
        if (missing(else.))
            .shFILE(original)
        else stop("'shFILE' with 'else.' but not 'default' makes no sense")
    else {
        if (missing(else.)) {
            if (is.na(.shFILE()))
                default
            else .shFILE(original)
        }
        else {
            if (is.na(.shFILE()))
                default
            else {
                else. <- match.fun(else.)
                value <- .shFILE(original)
                else.(value)
            }
        }
    }
}


delayedAssign("has.shFILE", in.shell && !is.na(.shFILE()))


normalized.shFILE <- function (...)
.Defunct2("shFILE", old = "normalized.shFILE")





.this.path_regexps <- list()
.this.path_regexps$windows.fsep                <- "[/\\\\]"
.this.path_regexps$windows.basename            <- local({


    # a regular expression for one character, determines if character is good in
    # windows BASENAMES. there are places where some of these characters are
    # allowed, for example, drives have a colon as the second character, and
    # folder paths have backslash or slash


    # # abandoned because it is too many bytes long
    # windows.basename.good.char <- function(excluding = "") {
    #     paste0("[^\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037\"*/:<>?\\\\|",
    #         excluding, "]")
    # }
    #
    #
    # # abandoned, not because it is too long, but because the new is shorter
    # windows.basename.good.char <- function(excluding = "") {
    #     paste0("(\177|[^[:cntrl:]\"*/:<>?\\\\|", excluding, "])")
    # }


    # the current method, stopped using [[:cntrl:]] and now uses a \001-\037
    windows.basename.good.char <- function(excluding = "") {
        paste0("[^\001-\037\"*/:<>?\\\\|", excluding, "]")
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
.this.path_regexps$windows.relative.path       <- local({


    # a windows relative path is at least one basename,
    # separated from each other by a file separator
    paste0(
        "(",
            .this.path_regexps$windows.basename,
            .this.path_regexps$windows.fsep    ,
        ")*",
        .this.path_regexps$windows.basename
    )
})
.this.path_regexps$windows.local.drive.no.sep  <- local({

    # a windows drive is any ASCII letter and a colon
    "([a-zA-Z]:)?"
})
.this.path_regexps$windows.local.drive         <- local({


    # a windows drive is any ASCII letter, a colon,
    # and a backslash or slash. the letter and colon may be omitted
    paste0(
        .this.path_regexps$windows.local.drive.no.sep,
        .this.path_regexps$windows.fsep
    )
})
.this.path_regexps$windows.UNC.drive           <- local({


    paste0(
        .this.path_regexps$windows.fsep,
        "(",
            .this.path_regexps$windows.fsep    ,
            .this.path_regexps$windows.basename,
        "){2}"
    )
})
.this.path_regexps$windows.local.absolute.path <- local({


    # paste0(.this.path_regexps$windows.local.drive, "(", .this.path_regexps$windows.relative.path, ")?")
    paste0(
        "(",
                .this.path_regexps$windows.local.drive,
            "|",
                .this.path_regexps$windows.local.drive.no.sep,
                "(",
                    .this.path_regexps$windows.fsep    ,
                    .this.path_regexps$windows.basename,
                ")+",
        ")"
    )
})
.this.path_regexps$windows.UNC.absolute.path   <- local({


    # paste0("[/\\\\]{2}", "(", .this.path_regexps$windows.basename, "[/\\\\])+", .this.path_regexps$windows.basename)
    paste0(
        .this.path_regexps$windows.fsep,
        "(",
            .this.path_regexps$windows.fsep    ,
            .this.path_regexps$windows.basename,
        "){2,}"
    )
})
.this.path_regexps$windows.absolute.path       <- local({


    paste0(
        "(",
                .this.path_regexps$windows.local.absolute.path,
            "|",
                .this.path_regexps$windows.UNC.absolute.path,
        ")"
    )
})
.this.path_regexps$R.Editor.not_ucrt            <- readLines("inst/extdata/r_editor_regexp_not_ucrt.txt", encoding = "UTF-8")
.this.path_regexps$R.Editor.ucrt                <- readLines("inst/extdata/r_editor_regexp_ucrt.txt", encoding = "UTF-8")


.this.path_regexps$windows.local.absolute.path.anchored <- paste0("^", .this.path_regexps$windows.local.absolute.path, "$")
.this.path_regexps$windows.UNC.absolute.path.anchored   <- paste0("^", .this.path_regexps$windows.UNC.absolute.path  , "$")
.this.path_regexps$windows.absolute.path.anchored       <- paste0("^", .this.path_regexps$windows.absolute.path      , "$")
.this.path_regexps$R.Editor.not_ucrt.anchored           <- paste0("^", .this.path_regexps$R.Editor.not_ucrt          , "$")
.this.path_regexps$R.Editor.ucrt.anchored               <- paste0("^", .this.path_regexps$R.Editor.ucrt              , "$")





if (!all(nchar(.this.path_regexps, type = "bytes") < 256L))
    stop(gettext("each regular expression in '.this.path_regexps' must be less than 256 bytes"))





untitled.not_ucrt <- readLines("inst/extdata/untitled_not_ucrt.txt", encoding = "UTF-8")
untitled.ucrt     <- readLines("inst/extdata/untitled_ucrt.txt", encoding = "UTF-8")





delayedAssign("R.Editor.regexp", {
    if (gui.rgui) {
        if (ucrt)
            .this.path_regexps$R.Editor.ucrt.anchored
        else .this.path_regexps$R.Editor.not_ucrt.anchored
    }
})
delayedAssign("untitled", {
    if (gui.rgui) {
        if (ucrt)
            untitled.ucrt
        else untitled.not_ucrt
    }
})
windows.abs.path <- .this.path_regexps$windows.absolute.path.anchored





Error <- function (..., call. = TRUE, domain = NULL, class = NULL,
    call = if ((n <- sys.parents()[[sys.nframe()]] - 3L) > 0L) sys.call(n))
errorCondition(message = .makeMessage(..., domain = domain),
    class = class, call = if (call.) call)


tmpfun <- function (x)
{
    e <- substitute(x)
    classes <- x
    if (!is.call(e) || e[[1L]] != "<-") stop("invalid argument, must be a call to <-")
    e <- e[[2L]]
    if (!is.name(e)) stop("invalid argument, second element must be a symbol")
    e <- as.character(e)
    if (!endsWith(e, "_class")) stop("invalid argument, second element must end with '_class_'")
    x <- sub("_class$", "", e)
    value <- function(...) NULL
    body(value) <- bquote(Error(..., class = .(str2lang(deparse1(classes)))))
    environment(value) <- parent.frame()
    assign(x, value, parent.frame())
}


tmpfun(thisPathNotExistsError_class                   <- c("this.path::thisPathNotExistsError"             , "this.path::thisPathNotExistError"       , "this.path_this.path_not_exists_error"))
tmpfun(thisPathNotImplementedError_class              <- c("this.path::thisPathNotImplementedError"        , "this.path_this.path_unimplemented_error", "notImplementedError"                 , "NotImplementedError"))


tmpfun(thisPathInZipFileError_class                   <- c("this.path::thisPathInZipFileError"             ))
tmpfun(thisPathUnrecognizedConnectionClassError_class <- c("this.path::thisPathUnrecognizedConnectionClassError"))
tmpfun(thisPathInAQUAError_class                      <- c("this.path::thisPathInAQUAError"                , thisPathNotImplementedError_class        ))
tmpfun(thisPathUnrecognizedMannerError_class          <- c("this.path::thisPathUnrecognizedMannerError"    ))


rm(tmpfun)


# helper functions for .this.path()   ----


is.clipboard.or.stdin <- function (file)
{
    # if we were to open a connection to this file (e.g. file(file, "r"))
    # would it connect to a clipboard or to the C-level standard input?
    #
    # this is useful because we'd like to know if 'file' actually connected
    # to a file
    if (os.windows)
        file %in% c("clipboard", "stdin") || startsWith(file, "clipboard-")
    else file %in% c("clipboard", "stdin", "X11_primary", "X11_secondary", "X11_clipboard")
}


assign.NULL <- function (envir)
{
    assign(".__ofile__", NULL, envir = envir)
    delayedAssign(".__file__", NULL, assign.env = envir)


    # force .__file__ so that garbage collection can collect this environment
    getInFrame(".__file__", envir)
}


assign.URL <- function (path, envir)
{
    assign(".__ofile__", path, envir = envir)
    delayedAssign(".__file__", normalizeURL(path), assign.env = envir)


    # force .__file__ so that garbage collection can collect this environment
    getInFrame(".__file__", envir)
}


assign.chdir <- function (path, envir, owd)
{
    assign(".__ofile__", path, envir = envir)
    owd  # force 'owd'
    fun <- function() {
        cwd <- getwd()
        on.exit(setwd(cwd))
        setwd(owd)
        normalizePath(path, winslash = "/", mustWork = TRUE)
    }
    delayedAssign(".__file__", fun(), assign.env = envir)
}


assign.default <- function (path, envir)
{
    assign(".__ofile__", path, envir = envir)
    delayedAssign(".__file__", normalizePath(path, winslash = "/", mustWork = TRUE), assign.env = envir)
}


getInFrame <- function (x, frame)
get(x, envir = frame, inherits = FALSE)


existsInFrame <- function (x, frame)
exists(x, envir = frame, inherits = FALSE)


# this.path(), this.dir(), and here() ----


.this.path <- function (verbose = FALSE, original = FALSE, for.msg = FALSE,
    N = sys.nframe() - 1L, get.frame.number = FALSE)
{
    # function to print the method in which the
    # path of the executing script was determined
    where <- function(x) {
        if (verbose)
            cat("Source: ", x, "\n", sep = "")
    }


    # loop through functions that lead here from most recent to earliest
    # looking for an appropriate source call (a call to function source,
    # sys.source, debugSource in RStudio, testthat::source_file, or
    # knitr::knit)
    #
    # an appropriate source call is a source call in which argument 'file'
    # ('fileName' in the case of debugSource, 'path' in the case of
    # testthat::source_file) has been evaluated (forced). for example, the
    # following is an inappropriate source call:
    #
    # source(this.path())
    #
    # the argument 'file' is stored as an unevaluated promise (see ?promise) to
    # evaluate 'this.path()' if/when 'file'  is requested.
    #
    # there are two functions on the calling stack at this point being 'source'
    # and 'this.path'. We'd like to do is something like
    # 'get("file", envir = , inherits = FALSE)', but you don't want to request
    # the 'file' argument from that source call because the value of 'file' is
    # under evaluation right now!
    #
    # instead, we use a C function to get variable 'file', check whether it is
    # an unevaluated promise or something else, in which case we can safely
    # retrieve it.
    #
    # however, this only makes sense for sys.source, debugSource, and
    # testthat::source_file. see source section for an alternate workaround)
    #
    # knitr::knit is similar to source in that we don't look for an unevaluated
    # promise, but look for a variable to exist or not


    # in 0.2.0, compatibility with 'debugSource' in 'RStudio' was added
    debugSource <- if (gui.rstudio)
        get("debugSource", "tools:rstudio", inherits = FALSE)


    # in 0.4.0, compatibility with 'testthat::source_file' was added. it is
    # almost identical to 'sys.source'
    source_file <- if (isNamespaceLoaded("testthat"))
        testthat::source_file
        # getExportedValue("testthat", "source_file")


    # in 1.1.0, compatibility with 'knitr::knit' was added
    knit <- if (isNamespaceLoaded("knitr"))
        knitr::knit


    for (n in seq.int(to = 1L, by = -1L, length.out = N)) {


        if (identical(sys.function(n), source)) {


            # if the path has yet to be saved
            if (!existsInFrame(".__ofile__", frame <- sys.frame(n))) {


                # 'ofile' is a copy of the original 'file' argument
                # if it does not exist, we have not entered a file yet,
                # so go to the next iteration
                if (!existsInFrame("ofile", frame))
                    next


                path <- getInFrame("ofile", frame)


                # there are two options for 'file'
                # * connection
                # * character string
                # start with character string
                if (is.character(path)) {


                    # use of "" refers to the R-level 'standard input' stdin.
                    # this means 'source' did not open a file, so we assign
                    # .__file__ the value of NULL and continue to the next
                    # iteration. We use .__file__ as NULL to skip this source
                    # call the next time this.path leads here
                    if (path == "") {
                        assign.NULL(frame)
                        next
                    }


                    # use of "clipboard" and "stdin" refer to the clipboard or
                    # to the C-level 'standard input' of the process.
                    # this means 'source' did not open a file, so we
                    # assign .__file__ the value of NULL and continue to the
                    # next iteration. We use .__file__ as NULL to skip this
                    # source call the next time this.path leads here
                    else if (is.clipboard.or.stdin(path)) {
                        assign.NULL(frame)
                        next
                    }


                    # as of this.path_0.5.0, throw an error that 'file' cannot
                    # be a URL when mixed with 'this.path'
                    #
                    # previously, the error would've occured at 'normalizePath',
                    # so it makes more sense to have this error here
                    else if (grepl("^(ftp|ftps|http|https)://", path)) {
                        assign.URL(path, frame)
                    }


                    # as of this.path_0.4.3, the path is determined slightly
                    # differently. this change is to account for two possible
                    # scenarios
                    # * source("file://absolute or relative path")
                    # * source("file:///absolute path")
                    # the description of this connection should remove the
                    # leading characters
                    else if (grepl("^file://", path)) {
                        assign.default(file.URL.path.1(path), frame)
                    }


                    # in 0.3.0, compatibility was added when using
                    # 'source(chdir = TRUE)'. this changes the working
                    # directory to the directory of the 'file' argument. since
                    # the 'file' argument was relative to the working directory
                    # prior to being changed, we need to change it to the
                    # previous working directory before we can normalize the
                    # path
                    else if (existsInFrame("owd", frame)) {
                        assign.chdir(path, frame, getInFrame("owd", frame))
                    }


                    else {
                        assign.default(path, frame)
                    }
                }


                # 'file' is not a character string, but a connection
                else {


                    # # an example demonstrating the different 'description' and
                    # # 'class' of a connection based on the argument provided
                    # local({
                    #     on.exit(closeAllConnections())
                    #     cbind(
                    #         summary(a <- stdin (                      )),
                    #         summary(b <- file  ("clipboard"           )),
                    #         summary(c <- file  ("stdin"               )),
                    #         summary(d <- file  ("file://clipboard"    )),
                    #         summary(e <- file  ("file://stdin"        )),
                    #         summary(f <- url   ("ftp://clipboard"     )),
                    #         summary(g <- url   ("ftps://clipboard"    )),
                    #         summary(h <- url   ("http://clipboard"    )),
                    #         summary(i <- url   ("https://clipboard"   )),
                    #         summary(j <- gzfile("clipboard"           )),
                    #         summary(k <- bzfile("clipboard"           )),
                    #         summary(l <- xzfile("clipboard"           )),
                    #         summary(m <- unz   ("clipboard", "stdin"  )),
                    #         summary(n <- pipe  ("clipboard"           )),
                    #         summary(o <- fifo  ("clipboard"           ))
                    #     )
                    # })


                    path <- summary.connection(path)
                    switch(path[["class"]],


                    file   = ,
                    gzfile = ,
                    bzfile = ,
                    xzfile = ,
                    fifo   = {


                        # the file:// in a file URL will already be removed
                        assign.default(path[["description"]], frame)
                    },


                    `url-libcurl` = ,
                    `url-wininet` = {
                        assign.URL(path[["description"]], frame)
                    },


                    terminal  = ,
                    clipboard = ,
                    pipe      = {
                        assign.NULL(frame)
                        next
                    },


                    unz = {
                        if (for.msg)
                            return(path[["description"]])
                        else stop(thisPathInZipFileError("'this.path' cannot be used within a zip file"))
                    },


                    {
                        if (for.msg)
                            return(NA_character_)
                        else {
                            cond <- thisPathUnrecognizedConnectionClassError(
                                "'this.path' not implemented when source-ing a connection of class ",
                                sQuote(path[["class"]]))
                            attr(cond, "summary.connection") <- path
                            stop(cond)
                        }
                    })
                }
            }


            path <- getInFrame(".__ofile__", frame)
            if (is.null(path))
                next
            if (get.frame.number)
                return(n)
            else if (for.msg) {
                return({
                    if (original)
                        path
                    else tryCatch({
                        .External2(C_getpromisewithoutwarning, ".__file__", frame)
                    }, error = function(cond) path)
                })
            }
            else if (original) {}
            else path <- .External2(C_getpromisewithoutwarning, ".__file__", frame)


            where("call to function source")
            return(path)
        }


        else if (identical(sys.function(n), sys.source)) {


            # much the same as 'source' except simpler, we don't have to
            # account for argument 'file' being a connection or ""
            if (!existsInFrame(".__ofile__", frame <- sys.frame(n))) {


                # as with 'source', we check that argument 'file' has been
                # forced, and continue to the next iteration if not
                #
                # in source, we could do exists("ofile"), but we can't do the
                # same here, so call this C function to test if file is an
                # unevaluated promise
                if (.External2(C_isunevaluatedpromise, "file", frame))
                    next


                path <- getInFrame("file", frame)


                # unlike 'source', 'sys.source' is intended to
                # source a file (not a connection), so we have to throw an
                # error if the user attempts to source a file named
                # "clipboard" or "stdin" since none of these refer to files
                if (is.clipboard.or.stdin(path))
                    stop(simpleError("invalid 'file', must not be \"clipboard\" nor \"stdin\"",
                        call = sys.call(n)))


                else if (existsInFrame("owd", frame)) {
                    assign.chdir(path, frame, getInFrame("owd", frame))
                }


                else {
                    assign.default(path, frame)
                }
            }


            path <- getInFrame(".__ofile__", frame)
            if (is.null(path))
                next
            if (get.frame.number)
                return(n)
            else if (for.msg) {
                return({
                    if (original)
                        path
                    else tryCatch({
                        .External2(C_getpromisewithoutwarning, ".__file__", frame)
                    }, error = function(cond) path)
                })
            }
            else if (original) {}
            else path <- .External2(C_getpromisewithoutwarning, ".__file__", frame)


            where("call to function sys.source")
            return(path)
        }


        else if (!is.null(debugSource) && identical(sys.function(n), debugSource)) {


            if (!existsInFrame(".__ofile__", frame <- sys.frame(n))) {


                if (.External2(C_isunevaluatedpromise, "fileName", frame))
                    next


                path <- getInFrame("fileName", frame)


                # we have to use 'enc2utf8' because 'debugSource' does as well
                path <- enc2utf8(path)


                if (path == "") {
                    assign.NULL(frame)
                    next
                }
                else if (is.clipboard.or.stdin(path)) {
                    assign.NULL(frame)
                    next
                }
                else if (grepl("^(ftp|ftps|http|https)://", path)) {
                    assign.URL(path, frame)
                }
                else if (grepl("^file://", path)) {
                    assign.default(file.URL.path.1(path), frame)
                }
                else {
                    assign.default(path, frame)
                }
            }


            path <- getInFrame(".__ofile__", frame)
            if (is.null(path))
                next
            if (get.frame.number)
                return(n)
            else if (for.msg) {
                return({
                    if (original)
                        path
                    else tryCatch({
                        .External2(C_getpromisewithoutwarning, ".__file__", frame)
                    }, error = function(cond) path)
                })
            }
            else if (original) {}
            else path <- .External2(C_getpromisewithoutwarning, ".__file__", frame)


            where("call to function debugSource in RStudio")
            return(path)
        }


        else if (!is.null(source_file) && identical(sys.function(n), source_file)) {


            if (!existsInFrame(".__ofile__", frame <- sys.frame(n))) {


                # as with 'source' and 'sys.source', we check that
                # argument 'path' has been forced, and continue to the next
                # iteration if not
                if (.External2(C_isunevaluatedpromise, "path", frame))
                    next


                path <- getInFrame("path", frame)


                # like 'sys.source', 'testthat::source_file' is intended
                # to source a file (not a connection), so we have to throw an
                # error if the user attempts to source a file named
                # "clipboard" or "stdin" since none of these refer to files
                if (is.clipboard.or.stdin(path))
                    stop(simpleError("invalid 'path' argument, must not be \"clipboard\" nor \"stdin\"",
                        call = sys.call(n)))


                else if (existsInFrame("old_dir", frame)) {
                    assign.chdir(path, frame, getInFrame("old_dir", frame))
                }


                else {
                    assign.default(path, frame)
                }
            }


            path <- getInFrame(".__ofile__", frame)
            if (is.null(path))
                next
            if (get.frame.number)
                return(n)
            else if (for.msg) {
                return({
                    if (original)
                        path
                    else tryCatch({
                        .External2(C_getpromisewithoutwarning, ".__file__", frame)
                    }, error = function(cond) path)
                })
            }
            else if (original) {}
            else path <- .External2(C_getpromisewithoutwarning, ".__file__", frame)


            where("call to function source_file in package testthat")
            return(path)
        }


        else if (!is.null(knit) && identical(sys.function(n), knit)) {


            # if the path has yet to be saved
            if (!existsInFrame(".__ofile__", frame <- sys.frame(n))) {


                if (!existsInFrame("in.file", frame))
                    next


                # if we are not in a file, go to next iteration
                #
                # also, assign NULL so we can skip this next time
                if (!getInFrame("in.file", frame)) {
                    assign.NULL()
                    next
                }


                # we care about 'oenvir' because 'input' may be edited in lines
                # 10 and 11 of knitr::knit, but after 'oenvir' is created on
                # line 36, 'input' is never edited again
                if (!existsInFrame("oenvir", frame))
                    next


                path <- getInFrame("input", frame)


                if (path == "")
                    stop(simpleError("invalid 'input' argument, must not be \"\"",
                        call = sys.call(n)))


                else if (is.clipboard.or.stdin(path))
                    stop(simpleError("invalid 'input' argument, must not be \"clipboard\" nor \"stdin\"",
                        call = sys.call(n)))


                else if (grepl("^(ftp|ftps|http|https)://", path))
                    stop(simpleError("invalid 'input' argument, cannot be a URL",
                        call = sys.call(n)))


                else if (grepl("^file://", path))
                    stop(simpleError("invalid 'input' argument, cannot be a file URL",
                        call = sys.call(n)))


                owd <- knitr::opts_knit$get("output.dir")
                if (!is.null(owd)) {
                    assign.chdir(path, frame, owd)
                }
                else {
                    assign.default(path, frame)
                }
            }


            path <- getInFrame(".__ofile__", frame)
            if (is.null(path))
                next
            if (get.frame.number)
                return(n)
            else if (for.msg) {
                return({
                    if (original)
                        path
                    else tryCatch({
                        .External2(C_getpromisewithoutwarning, ".__file__", frame)
                    }, error = function(cond) path)
                })
            }
            else if (original) {}
            else path <- .External2(C_getpromisewithoutwarning, ".__file__", frame)


            where("call to function knit in package knitr")
            return(path)
        }
    }


    # if you're only interested in the frame number, return 0,
    # meaning that a frame with an appropriate source call was not found
    if (get.frame.number) return(0L)


    # if the for loop is passed, no appropriate
    # source call was found up the calling stack
    #
    # next, check how R is being run
    #
    # * from 'VSCode'
    # * from a shell
    # * from a shell under Unix-alikes with GUI 'Tk' (was treated as an
    #       unrecognized manner until this.path_0.5.0)
    # * from 'RStudio'
    # * from 'Rgui' on Windows (added in this.path_0.2.0)
    # * from 'Rgui' on macOS (also called 'AQUA'), signal an error
    # * unrecognized manner, signal an error


    if (in.shell) {


        if (for.msg)
            return({
                if (original)
                    .shFILE()
                else tryCatch({
                    .shFILE(FALSE)
                }, error = function(cond) .shFILE())
            })


        value <- shFILE(original, default = {
            stop(thisPathNotExistsError(
                "'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* R is being run from a shell and argument 'FILE' is missing"))
        })
        where("shell argument 'FILE'")
        return(value)
    }


    # running from 'RStudio'
    else if (gui.rstudio) {


        # ".rs.api.getActiveDocumentContext" from "tools:rstudio" returns a
        # list of information about the document where your cursor is located
        #
        # ".rs.api.getSourceEditorContext" from "tools:rstudio" returns a list
        # of information about the document open in the current tab
        #
        # element 'id' is a character string, an identification for the document
        # element 'path' is a character string, the path of the document


        context <- get(".rs.api.getActiveDocumentContext", "tools:rstudio", inherits = FALSE)()
        active <- context[["id"]] != "#console"
        if (!active) {
            context <- get(".rs.api.getSourceEditorContext", "tools:rstudio", inherits = FALSE)()
            if (is.null(context)) {
                if (for.msg)
                    return(NA_character_)
                else stop(thisPathNotExistsError(
                    "'this.path' used in an inappropriate fashion\n",
                    "* no appropriate source call was found up the calling stack\n",
                    "* R is being run from RStudio with no documents open\n",
                    "  (or source document has no path)"))
            }
        }
        path <- context[["path"]]


        # the encoding is not explicitly set (at least on Windows),
        # so we have to do that ourselves
        Encoding(path) <- "UTF-8"


        if (nzchar(path)) {
            where(if (active)
                "active document in RStudio"
            else "source document in RStudio")
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }
        else if (for.msg)
            return(NA_character_)
        else stop(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            if (active)
                "* active document in RStudio does not exist"
            else "* source document in RStudio does not exist")
    }


    else if (in.vscode) {


        context <- rstudioapi::getSourceEditorContext()
        if (is.null(context)) {
            if (for.msg)
                return(NA_character_)
            else stop(thisPathNotExistsError(
                "'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* R is being run from VSCode with no documents open\n",
                "  (or document has no path)"
            ))
        }


        if (startsWith(context[["id"]], "untitled:")) {
            if (for.msg)
                return(NA_character_)
            else stop(
                "'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* document in VSCode does not exist")
        }


        path <- context[["path"]]
        Encoding(path) <- "UTF-8"


        if (nzchar(path)) {
            where("document in VSCode")
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }
        else if (for.msg)
            return(NA_character_)
        else stop(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* document in VSCode does not exist")
    }


    # running from 'Rgui' on Windows
    else if (gui.rgui) {


        # "getWindowsHandles" from "utils" (Windows exclusive) returns a list
        # of external pointers containing the windows handles. the thing of
        # interest are the names of this list, these should be the names of the
        # windows belonging to the current R process.
        #
        # we are only interested in window handles that:
        # * starts with "R Console" (for example "R Console", "R Console (64-bit)", etc.)
        # * look like an open R script; ending with " - R Editor" or any valid
        #   translation, see
        #
        #       this.path:::R.Editor.regexp
        #
        #   and
        #
        #       essentials::file.open(system.file(package = "this.path", "extdata", "write_r_editor_regexp.R"))
        #
        # * looks like a windows path
        # * matches one of the untitled document patterns, see
        #
        #       print(this.path:::untitled, width = 10)
        #
        # we keep track of "R Console" because we want to know if the R script
        # is the active document or the source document. looking for the above
        # patterns will remove unwanted handles like images
        #
        # from there, similar checks are done
        # as in the above section for 'RStudio'


        # the previous regular expression exceeded 256 bytes, more than the
        # POSIX standard. now, each part of the regular expression is its own
        # regular expression of less than 256 bytes
        nm <- names(utils::getWindowsHandles(minimized = TRUE))
        nm <- nm[
            startsWith(nm, "R Console") |
            grepl(R.Editor.regexp , nm) |
            grepl(windows.abs.path, nm) |
            nm %in% untitled
        ]
        if (!length(nm))
            stop("no windows in Rgui; should never happen, please report!")
        else if (active <- !startsWith(nm[[1L]], "R Console"))
            nm <- nm[[1L]]
        else if (length(nm) >= 2L)
            nm <- nm[[2L]]
        else if (for.msg)
            return(NA_character_)
        else stop(thisPathNotExistsError(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from Rgui with no documents open"))
        if (nm %in% untitled) {
            if (for.msg)
                return(NA_character_)
            else stop(
                "'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                if (active)
                    "* active document in Rgui does not exist"
                else "* source document in Rgui does not exist")
        }
        path <- sub(R.Editor.regexp, "\\1", nm)
        active <- active && nm != path
        if (grepl(windows.abs.path, path)) {
            where(if (active)
                "active document in Rgui"
            else "source document in Rgui")
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }
        else stop("invalid windows handles; should not happen, please report!")
    }


    else if (gui.aqua) {


        if (for.msg)
            NA_character_
        else stop(thisPathInAQUAError(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from AQUA which is currently unimplemented\n",
            "  consider using RStudio / / VSCode until such a time when this is implemented"))
    }


    # running from a shell under Unix-alikes with GUI 'Tk'
    else if (gui.tk) {


        if (for.msg)
            NA_character_
        else stop(thisPathNotExistsError(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from Tk which does not make use of its -f FILE, --file=FILE arguments"))
    }


    # running R in another manner
    else {


        if (for.msg)
            NA_character_
        else stop(thisPathNotImplementedError(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run in an unrecognized manner"))
    }
}


.this.dir <- function (verbose = FALSE)
{
    path <- .this.path(verbose = FALSE)
    if (grepl("^(ftp|ftps|http|https)://", path)) {
        # path <- normalizeURL.1("https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R")


        y <- strsplit(sub(URL.pattern, "\\2", path), "/+")[[1L]]
        paste(c(sub(URL.pattern, "\\1", path), y[-length(y)]), collapse = "/")
    }
    else dirname2(path)
}





this.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE, default, else.)
tryCatch({
    .this.path(verbose, original, for.msg)
}, function(c) default)


this.dir <- function (verbose = getOption("verbose"), default, else.)
tryCatch({
    .this.dir(verbose)
}, function(c) default)


thisPathNotExistsError_class[[1L]] ->
    names(body(this.path))[3L] ->
    names(body(this.dir ))[3L]


tmp <- body(this.path)
tmp[[1L]] <- as.name("tryCatch2")
tmp[[2L]] <- call("<-", as.name("value"), tmp[[2L]])
body(this.path) <- bquote({
    if (missing(default))
        if (missing(else.))
            .(body(this.path)[[2L]])
        else stop("'this.path' with 'else.' but not 'default' makes no sense")
    else {
        if (missing(else.))
            .(body(this.path))
        else {
            .(as.call(append(as.list(tmp), list(else. = quote(match.fun(else.)(value))))))
        }
    }
})
rm(tmp)


tmp <- body(this.dir)
tmp[[1L]] <- as.name("tryCatch2")
tmp[[2L]] <- call("<-", as.name("value"), tmp[[2L]])
body(this.dir) <- bquote({
    if (missing(default))
        if (missing(else.))
            .(body(this.dir)[[2L]])
        else stop("'this.dir' with 'else.' but not 'default' makes no sense")
    else {
        if (missing(else.))
            .(body(this.dir))
        else {
            .(as.call(append(as.list(tmp), list(else. = quote(match.fun(else.)(value))))))
        }
    }
})
rm(tmp)





this.path2 <- function (...)
.Defunct2("this.path(..., default = NULL)", old = "this.path2(...)")


this.dir2 <- function (...)
.Defunct2("this.dir(..., default = NULL)", old = "this.dir2(...)")


this.dir3 <- function (...)
.Defunct2("this.dir(..., default = getwd())", old = "this.dir3(...)")





here <- ici <- function (..., .. = 0L)
{
    base <- .this.path()
    if (grepl("^(ftp|ftps|http|https)://", base)) {
        # base <- normalizeURL.1("https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R")
        # .. <- "2"


        y <- strsplit(sub(URL.pattern, "\\2", base), "/+")[[1L]]
        len <- length(y) - length(seq_len(..)) - 1L
        base <- if (len <= 0L)
            sub(URL.pattern, "\\1", base)
        else paste(c(sub(URL.pattern, "\\1", base), y[seq_len(len)]), collapse = "/")
    }


    # base <- "//host-name/share-name/path/to/file"
    # base <- "C:/Users/andre/Documents/this.path/man/this.path.Rd"
    # .. <- "10"
    else base <- .External2(C_dirname2, base, ..)
    path.join(base, ...)
}





Sys.path <- function ()
.this.path()


Sys.dir <- function ()
.this.dir()
