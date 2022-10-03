.shFILE <- evalq(envir = new.env(), function ()
{
    # shFILE {this.path}                                         R Documentation
    #
    # Get Argument 'FILE' Provided to R by a Shell
    #
    #
    #
    # Description:
    #
    # Look through the shell arguments, extracting argument 'FILE' from the
    # following:
    # --file=FILE
    # -f FILE
    #
    #
    #
    # Usage:
    #
    # shFILE()
    # normalized.shFILE()
    #
    #
    #
    # Details:
    #
    # Both functions will save their return values; this makes them faster
    # when called subsequent times.
    #
    # For 'shFILE', if there are no such arguments, 'NA' is returned.
    # For 'normalized.shFILE', if there are no such arguments, an error is
    # raised.
    #
    # For both functions, if there are multiple such arguments, an error is
    # raised.
    #
    #
    #
    # Value:
    #
    # A string. For 'normalized.shFILE', an absolute path.
    #
    #
    #
    # Note:
    #
    # For 'normalized.shFILE', the path on Windows will use / as the file
    # separator.


    # The details between how the shell arguments are handled is
    # different on Windows and under Unix-alikes, see the comments below.


    # return the already saved result, if available
    if (!is.null(.__file__))
        return(.__file__)


    .__file__ <<- NA_character_


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


    # running R from a shell on Windows
    if (os.windows.in.shell) {


        ca <- commandArgs()


        # select the shell arguments intended for the R executable. also remove
        # the first argument, this is the name of the executable by which this
        # R process was invoked (not useful here)
        ca <- ca[seq_len(length(ca) - length(commandArgs(trailingOnly = TRUE)))]
        ca <- ca[-1L]
        if (length(ca) <= 0L)
            return(.__file__)


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
            if (length(ca) <= 0L)
                return(.__file__)
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
            if (length(ca) <= 0L)
                return(.__file__)
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
        if (n <= 0L)
            return(.__file__)
        else if (n > 1L)
            stop("R is being run from a shell and formal argument 'FILE' matched by multiple actual arguments")


        # since 'R' uses the last -f FILE or --file=FILE argument,
        #     use max to find the index of the last one of these arguments
        n <- max(f, file)
        .__file__ <<- {
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
        if (length(ca) <= 0L)
            return(.__file__)


        enc <- ca == "--encoding"
        if (any(enc)) {
            for (n in seq_len(length(enc) - 1L)) {
                if (enc[[n]])
                    enc[[n + 1L]] <- FALSE
            }
            enc <- which(enc)
            ca <- ca[-c(enc, enc + 1L)]
            if (length(ca) <= 0L)
                return(.__file__)
        }


        f <- which(ca == "-f")
        file <- grep("^--file=", ca)


        n <- length(f) + length(file)
        if (n <= 0L)
            return(.__file__)
        else if (n > 1L)
            stop("R is being run from a shell and formal argument 'FILE' matched by multiple actual arguments")


        n <- max(f, file)
        .__file__ <<- gsub("~+~", " ", {
            if (n %in% file)
                sub("^--file=", "", ca[[n]])
            else ca[[n + 1L]]
        }, fixed = TRUE)
    }
    return(.__file__)
})
evalq(envir = environment(.shFILE), {
    .__file__ <- NULL
})
lockEnvironment(environment(.shFILE))


shFILE <- function (default, else.)
{
    if (missing(default))
        if (missing(else.))
            .shFILE()
        else stop("'shFILE' with 'else.' but not 'default' makes no sense")
    else {
        if (missing(else.)) {
            ans <- .shFILE()
            if (is.na(ans))
                default
            else ans
        }
        else {
            ans <- .shFILE()
            if (is.na(ans))
                default
            else else.(ans)
        }
    }
}


.normalized.shFILE <- evalq(envir = new.env(), function ()
{
    # return the already saved result, if available
    if (!is.null(.__file__))
        return(.__file__)


    file <- .shFILE()
    file <- normalizePath(file, winslash = "/", mustWork = TRUE)
    .__file__ <<- file
    return(.__file__)
})
evalq(envir = environment(.normalized.shFILE), {
    .__file__ <- NULL
})
lockEnvironment(environment(.normalized.shFILE))


normalized.shFILE <- function (default, else.)
{
    if (missing(default))
        if (missing(else.))
            .normalized.shFILE()
        else stop("'normalized.shFILE' with 'else.' but not 'default' makes no sense")
    else {
        if (missing(else.)) {
            if (is.na(.shFILE()))
                default
            else .normalized.shFILE()
        }
        else {
            if (is.na(.shFILE()))
                default
            else else.(.normalized.shFILE())
        }
    }
}





.this.path_regexps <- list()
.this.path_regexps$windows.file.sep            <- "[/\\\\]"
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
            .this.path_regexps$windows.file.sep,
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
        .this.path_regexps$windows.file.sep
    )
})
.this.path_regexps$windows.UNC.drive           <- local({


    paste0(
        .this.path_regexps$windows.file.sep,
        "(",
            .this.path_regexps$windows.file.sep,
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
                    .this.path_regexps$windows.file.sep,
                    .this.path_regexps$windows.basename,
                ")+",
        ")"
    )
})
.this.path_regexps$windows.UNC.absolute.path   <- local({


    # paste0("[/\\\\]{2}", "(", .this.path_regexps$windows.basename, "[/\\\\])+", .this.path_regexps$windows.basename)
    paste0(
        .this.path_regexps$windows.file.sep,
        "(",
            .this.path_regexps$windows.file.sep,
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





thisPathNotExistsError_class <- c("this.path::thisPathNotExistsError", "this.path::thisPathNotExistError", "this.path_this.path_not_exists_error")
thisPathNotImplementedError_class <- c("this.path::thisPathNotImplementedError", "this.path_this.path_unimplemented_error")
AQUAError_class <- c("this.path::AQUAError", thisPathNotImplementedError_class)
thisPathUnrecognizedMannerError_class <- c("this.path::thisPathUnrecognizedMannerError", thisPathNotImplementedError_class)


Error <- function (..., call. = TRUE, domain = NULL, class = NULL,
    call = if ((n <- sys.parents()[[sys.nframe()]] - 3L) > 0L) sys.call(n))
errorCondition(message = .makeMessage(..., domain = domain),
    class = class, call = if (call.) call)


thisPathNotExistsError <- function(...) NULL
body(thisPathNotExistsError) <- bquote(Error(..., class = .(thisPathNotExistsError_class)))


thisPathNotImplementedError <- function(...) NULL
body(thisPathNotImplementedError) <- bquote(Error(..., class = .(thisPathNotImplementedError_class)))


AQUAError <- function(...) NULL
body(AQUAError) <- bquote(Error(..., class = .(AQUAError_class)))


thisPathUnrecognizedMannerError <- function(...) NULL
body(thisPathUnrecognizedMannerError) <- bquote(Error(..., class = .(thisPathUnrecognizedMannerError_class)))


is.clipboard.or.stdin <- function (file)
{
    if (os.windows)
        file %in% c("clipboard", "stdin") || startsWith(file, "clipboard-")
    else file %in% c("clipboard", "X11_primary", "X11_secondary", "X11_clipboard", "stdin")
}


.this.path <- function (verbose = getOption("verbose"))
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
    assign.__file__ <- function(

        value = `attr<-`(full.path, "this.path::n", n),

        full.path = if (URL)
            normalizeURL(opath)
        else normalizePath(opath, winslash = "/", mustWork = TRUE),

        opath = path,

        URL = FALSE)
    {
        assign(".__file__", value, envir = sys.frame(n), inherits = FALSE)
        value
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


    for (n in seq.int(to = 1L, by = -1L, length.out = sys.nframe() - 1L)) {


        if (identical(sys.function(n), source)) {


            # if the path has yet to be saved
            if (!existsn(".__file__")) {


                # 'ofile' is a copy of the original 'file' argument
                # if it does not exist, we have not entered a file yet,
                # then continue to the next iteration
                if (!existsn("ofile"))
                    next


                path <- getn("ofile")


                URL <- FALSE


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
                        assign.__file__(NULL)
                        next
                    }


                    # use of "clipboard" and "stdin" refer to the clipboard or
                    # to the C-level 'standard input' of the process.
                    # this means 'source' did not open a file, so we
                    # assign .__file__ the value of NULL and continue to the
                    # next iteration. We use .__file__ as NULL to skip this
                    # source call the next time this.path leads here
                    else if (is.clipboard.or.stdin(path)) {
                        assign.__file__(NULL)
                        next
                    }


                    # as of this.path_0.5.0, throw an error that 'file' cannot
                    # be a URL when mixed with 'this.path'
                    #
                    # previously, the error would've occured at 'normalizePath',
                    # so it makes more sense to have this error here
                    else if (grepl("^(ftp|ftps|http|https)://", path)) {
                        # stop("'this.path' makes no sense for a URL")
                        URL <- TRUE
                    }


                    # as of this.path_0.4.3, the path is determined slightly
                    # differently. this change is to account for two possible
                    # scenarios
                    # * source("file://absolute or relative path")
                    # * source("file:///absolute path")
                    # the description of this connection should remove the
                    # leading characters
                    else if (grepl("^file://", path)) {
                        path <- file.URL.path.1(path)
                    }


                    # in 0.3.0, compatibility was added when using
                    # 'source(chdir = TRUE)'. this changes the working
                    # directory to the directory of the 'file' argument. since
                    # the 'file' argument was relative to the working directory
                    # prior to being changed, we need to change it to the
                    # previous working directory before we can normalize the
                    # path
                    else if (existsn("owd")) {
                        cwd <- getwd()
                        on.exit(setwd(cwd))
                        setwd(getn("owd"))
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
                    switch(path$class, file = , gzfile = , bzfile = ,
                        xzfile = , fifo = {


                        path <- path$description


                    }, `url-libcurl` = , `url-wininet` = {


                        # stop("'this.path' makes no sense for a URL")
                        URL <- TRUE
                        path <- path$description


                    }, terminal = , clipboard = , pipe = {


                        assign.__file__(NULL)
                        next


                    }, unz = {


                        stop("'this.path' cannot be used within a zip file")


                    }, stop(thisPathNotImplementedError(
                        "'this.path' unimplemented when source-ing a connection of class ",
                        sQuote(path$class))))
                }


                assign.__file__(URL = URL)
            }
            else if (is.null(getn(".__file__")))
                next
            where("call to function source")
            return(getn(".__file__"))
        }


        else if (identical(sys.function(n), sys.source)) {


            # much the same as 'source' except simpler, we don't have to
            # account for argument 'file' being a connection or ""
            if (!existsn(".__file__")) {


                # if file is an unevaluated promise


                # as with 'source', we check that argument 'file' has been
                # forced, and continue to the next iteration if not
                if (.External2(C_isunevaluatedpromise, "file", sys.frame(n)))
                    next


                path <- getn("file")


                # unlike 'source', 'sys.source' is intended to
                # source a file (not a connection), so we have to throw an
                # error if the user attempts to source a file named
                # "clipboard" or "stdin" since none of these refer to files
                if (is.clipboard.or.stdin(path))
                    stop(Error("invalid 'file', must not be \"clipboard\" nor \"stdin\"",
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


        else if (!is.null(debugSource) && identical(sys.function(n), debugSource)) {


            if (!existsn(".__file__")) {


                # unlike 'source' and 'sys.source', there is no way to check
                # that argument 'fileName' has been forced (since all of the
                # work is done internally in C).
                #
                # instead, we have to use a 'tryCatch' statement. If argument
                # 'fileName' has been forced, the statement will proceed
                # without an issue. If it has not, it is
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


                if (.External2(C_isunevaluatedpromise, "fileName", sys.frame(n)))
                    next


                path <- getn("fileName")


                URL <- FALSE


                # we have to use 'enc2utf8' because 'debugSource' does as well
                path <- enc2utf8(path)


                if (path == "") {
                    assign.__file__(NULL)
                    next
                }
                else if (is.clipboard.or.stdin(path)) {
                    assign.__file__(NULL)
                    next
                }
                else if (grepl("^(ftp|ftps|http|https)://", path)) {
                    URL <- TRUE
                }
                else if (grepl("^file://", path)) {
                    path <- file.URL.path.1(path)
                }


                assign.__file__(URL = URL)
            }
            else if (is.null(getn(".__file__")))
                next
            where("call to function debugSource in RStudio")
            return(getn(".__file__"))
        }


        else if (!is.null(source_file) && identical(sys.function(n), source_file)) {


            if (!existsn(".__file__")) {


                # as with 'source' and 'sys.source', we check that
                # argument 'path' has been forced, and continue to the next
                # iteration if not
                if (.External2(C_isunevaluatedpromise, "path", sys.frame(n)))
                    next


                path <- getn("path")


                # like 'sys.source', 'testthat::source_file' is intended
                # to source a file (not a connection), so we have to throw an
                # error if the user attempts to source a file named
                # "clipboard" or "stdin" since none of these refer to files
                if (is.clipboard.or.stdin(path))
                    stop(Error("invalid 'path' argument, must not be \"clipboard\" nor \"stdin\"",
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


        else if (!is.null(knit) && identical(sys.function(n), knit)) {


            # if the path has yet to be saved
            if (!existsn(".__file__")) {


                if (!existsn("in.file"))
                    next


                # if we are not in a file, go to next iteration
                #
                # also, assign NULL so we can skip this next time
                if (!getn("in.file")) {
                    assign.__file__(NULL)
                    next
                }


                # we care about 'oenvir' because 'input' may be edited in lines
                # 10 and 11 of knitr::knit, but after 'oenvir' is created on
                # line 36, 'input' is never edited again
                if (!existsn("oenvir"))
                    next


                path <- getn("input")


                URL <- FALSE


                if (path == "")
                    stop(Error("invalid 'input' argument, must not be \"\"",
                        call = sys.call(n)))


                else if (is.clipboard.or.stdin(path))
                    stop(Error("invalid 'input' argument, must not be \"clipboard\" nor \"stdin\"",
                        call = sys.call(n)))


                else if (grepl("^(ftp|ftps|http|https)://", path))
                    stop(Error("invalid 'input' argument, cannot be a URL"))


                else if (grepl("^file://", path))
                    stop(Error("invalid 'input' argument, cannot be a file URL"))


                outwd <- knitr::opts_knit$get("output.dir")
                if (!is.null(outwd))
                    path <- path.join(outwd, path)


                assign.__file__(URL = URL)
            }
            else if (is.null(getn(".__file__")))
                next
            where("call to function knit in package knitr")
            return(getn(".__file__"))
        }
    }


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


        value <- normalized.shFILE(default = {
            stop(thisPathNotExistsError(
                "'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* R is being run from a shell and argument 'FILE' is missing"))
        })
        attr(value, "this.path::from.shell") <- TRUE
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
            if (is.null(context))
                stop(thisPathNotExistsError(
                    "'this.path' used in an inappropriate fashion\n",
                    "* no appropriate source call was found up the calling stack\n",
                    "* R is being run from RStudio with no documents open\n",
                    "  (or source document has no path)"))
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
        else stop(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            if (active)
                "* active document in RStudio does not exist"
            else "* source document in RStudio does not exist")
    }


    else if (in.vscode) {


        context <- rstudioapi::getSourceEditorContext()
        if (is.null(context))
            stop(thisPathNotExistsError(
                "'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* R is being run from VSCode with no documents open\n",
                "  (or document has no path)"
            ))


        if (startsWith(context[["id"]], "untitled:"))
            stop(
                "'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* document in VSCode does not exist")


        path <- context[["path"]]
        Encoding(path) <- "UTF-8"


        if (nzchar(path)) {
            where("document in VSCode")
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }
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
        else stop(thisPathNotExistsError(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from Rgui with no documents open"))
        if (nm %in% untitled)
            stop(
                "'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                if (active)
                    "* active document in Rgui does not exist"
                else "* source document in Rgui does not exist")
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


        stop(AQUAError(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from AQUA which is currently unimplemented\n",
            "  consider using RStudio / / VSCode until such a time when this is implemented"))
    }


    # running from a shell under Unix-alikes with GUI 'Tk'
    else if (gui.tk) {


        stop(thisPathNotExistsError(
            "'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from Tk which does not make use of its -f FILE, --file=FILE arguments"))
    }


    # running R in another manner
    else stop(thisPathNotImplementedError(
        "'this.path' used in an inappropriate fashion\n",
        "* no appropriate source call was found up the calling stack\n",
        "* R is being run in an unrecognized manner"))
}


.this.dir <- function (verbose = getOption("verbose"))
{
    path <- .this.path(verbose)
    if (grepl("^(ftp|ftps|http|https)://", path)) {
        # path <- normalizeURL.1("https://raw.githubusercontent.com////////////ArcadeAntics///testing/.././this.path/./main/tests/this.path_w_URLs.R")


        y <- strsplit(sub(URL.pattern, "\\2", path), "/+")[[1L]]
        paste(c(sub(URL.pattern, "\\1", path), y[-length(y)]), collapse = "/")
    }
    else dirname2(path)
}





this.path <- function (verbose = getOption("verbose"), default, else.)
tryCatch({
    .this.path(verbose)
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
            .this.path(verbose)
        else stop("'this.path' with 'else.' but not 'default' makes no sense")
    else {
        if (missing(else.))
            .(body(this.path))
        else {
            .(as.call(append(as.list(tmp), list(else. = quote(else.(value))))))
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
            .this.dir(verbose)
        else stop("'this.dir' with 'else.' but not 'default' makes no sense")
    else {
        if (missing(else.))
            .(body(this.dir))
        else {
            .(as.call(append(as.list(tmp), list(else. = quote(else.(value))))))
        }
    }
})
rm(tmp)





this.path2 <- function (...)
.Defunct(msg = c(
    gettextf("'%s' is defunct.\n", "this.path2(...)", domain = "R-base"),
    gettextf("Use '%s' instead.\n", "this.path(..., default = NULL)", domain = "R-base"),
    gettextf("See help(\"Defunct\")", domain = "R-base")
))


this.dir2 <- function (...)
.Defunct(msg = c(
    gettextf("'%s' is defunct.\n", "this.dir2(...)", domain = "R-base"),
    gettextf("Use '%s' instead.\n", "this.dir(..., default = NULL)", domain = "R-base"),
    gettextf("See help(\"Defunct\")", domain = "R-base")
))


this.dir3 <- function (...)
.Defunct(msg = c(
    gettextf("'%s' is defunct.\n", "this.dir3(...)", domain = "R-base"),
    gettextf("Use '%s' instead.\n", "this.dir(..., default = getwd())", domain = "R-base"),
    gettextf("See help(\"Defunct\")", domain = "R-base")
))





here <- ici <- function (..., .. = 0L)
{
    base <- .this.path(verbose = FALSE)
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
