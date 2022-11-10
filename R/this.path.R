# a character vector of symbols to remove (including itself)
rm.list <- "rm.list"





tmp <- readLines("src/hooks-for-namespace-events.c", warn = FALSE)
tmp <- list(
    thispathofile       = str2lang(tmp[[grep("#define thispathofileChar"      , tmp) + 1L]]),
    thispathfile        = str2lang(tmp[[grep("#define thispathfileChar"       , tmp) + 1L]]),
    thispathformsg      = str2lang(tmp[[grep("#define thispathformsgChar"     , tmp) + 1L]]),
    thispatherror       = str2lang(tmp[[grep("#define thispatherrorChar"      , tmp) + 1L]]),
    thispathassocwfile  = str2lang(tmp[[grep("#define thispathassocwfileChar" , tmp) + 1L]]),
    thispathdone        = str2lang(tmp[[grep("#define thispathdoneChar"       , tmp) + 1L]]),
    insidesourcewashere = str2lang(tmp[[grep("#define insidesourcewashereChar", tmp) + 1L]])
)
if (!all(vapply(tmp, function(x) is.character(x) && length(x) == 1 && !is.na(x), NA)))
    stop("could not determine variable names")
for (i in seq_along(tmp)) assign(names(tmp)[[i]], tmp[[i]])
rm.list <- c(rm.list, names(tmp))
thispathChars <- tmp
rm.list <- c(rm.list, "thispathChars")
rm(i, tmp)





.Defunct2 <- function (new, old = as.character(sys.call(sys.parent()))[1L])
{
    .Defunct(msg = c(
        gettextf("'%s' is defunct.\n", old, domain = "R-base"),
        gettextf("Use '%s' instead.\n", new, domain = "R-base"),
        gettextf("See help(\"Defunct\")", domain = "R-base")
    ))
}





GETSHFILE <- function ()
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


    # running R from a shell on Windows
    if (os.windows.in.shell) {


        ca <- commandArgs()


        # select the shell arguments intended for the R executable. also remove
        # the first argument, this is the name of the executable by which this
        # R process was invoked (not useful here)
        ca <- ca[seq_len(length(ca) - length(commandArgs(trailingOnly = TRUE)))]
        ca <- ca[-1L]
        if (length(ca) <= 0L)
            return(NA_character_)


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
                return(NA_character_)
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
        ) | grepl("^--(encoding|max-ppsize)=", ca, useBytes = TRUE)
        if (any(pre)) {
            ca <- ca[!pre]
            if (length(ca) <= 0L)
                return(NA_character_)
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
        file <- not.special[grep("^--file=", ca[not.special], useBytes = TRUE)]


        # given the number of -f FILE and --file=FILE arguments,
        #     signal a possible error or warning
        n <- length(f) + length(file)
        if (n < 1L)
            return(NA_character_)
        else if (n > 1L)
            stop("R is being run from a shell and formal argument 'FILE' matched by multiple actual arguments")


        # since 'R' uses the last -f FILE or --file=FILE argument,
        #     use max to find the index of the last one of these arguments
        n <- max(f, file)
        if (n %in% file)
            sub("^--file=", "", ca[[n]], useBytes = TRUE)
        else ca[[n + 1L]]
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
            return(NA_character_)


        enc <- ca == "--encoding"
        if (any(enc)) {
            for (n in seq_len(length(enc) - 1L)) {
                if (enc[[n]])
                    enc[[n + 1L]] <- FALSE
            }
            enc <- which(enc)
            ca <- ca[-c(enc, enc + 1L)]
            if (length(ca) <= 0L)
                return(NA_character_)
        }


        f <- which(ca == "-f")
        file <- grep("^--file=", ca, useBytes = TRUE)


        n <- length(f) + length(file)
        if (n < 1L)
            return(NA_character_)
        else if (n > 1L)
            stop("R is being run from a shell and formal argument 'FILE' matched by multiple actual arguments")


        n <- max(f, file)
        gsub("~+~", " ", {
            if (n %in% file)
                sub("^--file=", "", ca[[n]], useBytes = TRUE)
            else ca[[n + 1L]]
        }, fixed = TRUE, useBytes = TRUE)
    }


    else NA_character_
}




.shFILE <- function (original = TRUE, for.msg = FALSE)
.External2(C_shfile, original, for.msg)


tmp <- new.env()
environment(.shFILE) <- tmp
evalq(envir = tmp, {
    GETSHFILE <- GETSHFILE
    delayedAssign(thispathofile, GETSHFILE())
    eval(call("delayedAssign", thispathfile, call(".normalizePath", as.symbol(thispathofile))))
})
rm(GETSHFILE)
lockEnvironment(tmp, bindings = TRUE)
rm(tmp)


shFILE <- function (original = FALSE, for.msg = FALSE, default, else.)
{
    if (missing(default))
        if (missing(else.))
            .shFILE(original, for.msg)
        else stop("'shFILE' with 'else.' but not 'default' makes no sense")
    else {
        if (missing(else.)) {
            if (is.na(.shFILE()))
                default
            else .shFILE(original, for.msg)
        }
        else {
            if (is.na(.shFILE()))
                default
            else {
                value <- .shFILE(original, for.msg)
                (else.)(value)
            }
        }
    }
}


delayedAssign("has.shFILE", in.shell && !is.na(.shFILE()))


normalized.shFILE <- function (...)
.Defunct2("shFILE", old = "normalized.shFILE")





is.abs.path <- function (path)
.External2(C_isabspath, path)





r.editor_not.ucrt <- readLines("inst/extdata/r-editor_not-ucrt.txt", encoding = "UTF-8")
r.editor_ucrt     <- readLines("inst/extdata/r-editor_ucrt.txt"    , encoding = "UTF-8")


untitled_not.ucrt <- readLines("inst/extdata/untitled_not-ucrt.txt", encoding = "UTF-8")
untitled_ucrt     <- readLines("inst/extdata/untitled_ucrt.txt"    , encoding = "UTF-8")





delayedAssign("r.editor", {
    if (gui.rgui) {
        if (ucrt)
            r.editor_ucrt
        else r.editor_not.ucrt
    }
})
delayedAssign("untitled", {
    if (gui.rgui) {
        if (ucrt)
            untitled_ucrt
        else untitled_not.ucrt
    }
})
delayedAssign("nchar_r.editor", {
    if (gui.rgui) {
        nchar(r.editor)
    }
})
is.r.editor <- function (x)
vapply(x, function(xx) any(endsWith(xx, r.editor)), NA)





getCurrentCall <- function (n = 3L)
{
    # find the call that stop() would have also found
    #
    # look down the calling stack, picking the
    # most recent closure besides `stop`
    #
    # this is intended to be used as such:
    # stop(errorMakingFunction())
    #
    # where errorMakingFunction calls getCurrentCall()


    n <- sys.nframe() - n
    if (n <= 0L)
        return(NULL)
    n <- sys.parents()[[n]]
    if (n <= 0L)
        return(NULL)
    skip.stop <- TRUE
    for (n in seq.int(to = 1L, by = -1L, length.out = n)) {
        if (typeof(fun <- sys.function(n)) == "closure") {
            if (skip.stop && identical2(fun, stop)) {
                skip.stop <- FALSE
                next
            }
            return(sys.call(n))
        }
    }
    NULL
}


thisPathUnrecognizedConnectionClassError <- function (con, call = getCurrentCall(), call. = TRUE)
.External2(C_thispathunrecognizedconnectionclasserror, if (call.) call, con)


thisPathUnrecognizedMannerError <- function (call = getCurrentCall(), call. = TRUE)
.External2(C_thispathunrecognizedmannererror, if (call.) call)


thisPathNotImplementedError <- function (..., call. = TRUE, domain = NULL, call = getCurrentCall())
.External2(C_thispathnotimplementederror, .makeMessage(..., domain = domain), call = if (call.) call)


thisPathNotExistsError <- function (..., call. = TRUE, domain = NULL, call = getCurrentCall())
.External2(C_thispathnotexistserror, .makeMessage(..., domain = domain), call = if (call.) call)


thisPathInZipFileError <- function (description, call = getCurrentCall(), call. = TRUE)
.External2(C_thispathinzipfileerror, if (call.) call, description)


thisPathInAQUAError <- function (call = getCurrentCall(), call. = TRUE)
.External2(C_thispathinaquaerror, if (call.) call)


# helper functions for .this.path()   ----


is.unevaluated.promise <- function (sym, env)
.External2(C_isunevaluatedpromise, sym, env)


this_path_used_in_an_inappropriate_fashion <- local({
    tmp <- readLines("src/thispathdefn.h", warn = FALSE)
    tmp <- tmp[[grep("#define this_path_used_in_an_inappropriate_fashion", tmp, fixed = TRUE) + 1L]]
    tmp <- str2lang(tmp)
    if (!is.character(tmp) || length(tmp) != 1L ||
        is.na(tmp))
    {
        stop("could not determine error message")
    }
    tmp
})


is.clipboard <- function (file)
.External2(C_isclipboard, file)


thispathnamespace <- getNamespace("this.path")


assign.NULL <- function(frame) NULL
body(assign.NULL) <- bquote({
    assign(.(thispathofile), NULL, frame)
    lockBinding(.(thispathofile), frame)
    delayedAssign(.(thispathfile), NULL, emptyenv(), frame)
    lockBinding(.(thispathfile), frame)
    getInFrame(.(thispathfile), frame)
})


assign.URL <- function(path, frame) NULL
body(assign.URL) <- bquote({
    assign(.(thispathofile), path, frame)
    lockBinding(.(thispathofile), frame)
    eval(call("delayedAssign", .(thispathfile), call("normalizeURL.1", path), thispathnamespace, frame))
    lockBinding(.(thispathfile), frame)
    getInFrame(.(thispathfile), frame)
})


assign.fileURL <- function(path, frame) NULL
body(assign.fileURL) <- bquote({
    assign(.(thispathofile), path, frame)
    lockBinding(.(thispathofile), frame)
    eval(call("delayedAssign", .(thispathfile), call(".normalizePath", file.URL.path.1(path)), thispathnamespace, frame))
    lockBinding(.(thispathfile), frame)
})


assign.fileURL2 <- function(path, frame) NULL
body(assign.fileURL2) <- bquote({
    assign(.(thispathofile), paste0("file://", path), frame)
    lockBinding(.(thispathofile), frame)
    eval(call("delayedAssign", .(thispathfile), call(".normalizePath", path), thispathnamespace, frame))
    lockBinding(.(thispathfile), frame)
})


assign.chdir <- function(path, frame, owd) NULL
body(assign.chdir) <- bquote({
    assign(.(thispathofile), path, frame)
    lockBinding(.(thispathofile), frame)
    owd  # force 'owd'
    eval(call("delayedAssign", .(thispathfile), call(".normalizeAgainst", path, owd), thispathnamespace, frame))
    lockBinding(.(thispathfile), frame)
})


assign.default <- function(path, frame) NULL
body(assign.default) <- bquote({
    assign(.(thispathofile), path, frame)
    lockBinding(.(thispathofile), frame)
    eval(call("delayedAssign", .(thispathfile), call(".normalizePath", path), thispathnamespace, frame))
    lockBinding(.(thispathfile), frame)
})


assign.done <- function(frame) NULL
body(assign.done) <- bquote({
    assign(.(thispathdone), NULL, frame)
    lockBinding(.(thispathdone), frame)
})


getInFrame <- function (x, frame)
get(x, envir = frame, inherits = FALSE)


existsInFrame <- function (x, frame)
exists(x, envir = frame, inherits = FALSE)


rm.list <- c(rm.list, "checkfile")
checkfile <- function(name, call = quote(sys.call(n)), character.only = FALSE,
    file.only = FALSE, exists.owd = FALSE, get.owd = NULL, do.enc2utf8 = FALSE,
    normalize = FALSE) NULL
body(checkfile) <- bquote({
    simplify(substitute({
        file <- getInFrame(name, frame)
        if (is.character(file)) {


            if (length(file) != 1L)
                stop(simpleError(
                    sprintf("invalid '%s', must be a character string", EncodeChar(name)),
                    call
                ))


            else if (is.na(file))
                stop(simpleError(
                    sprintf("invalid '%s', must not be NA", EncodeChar(name)),
                    call
                ))


            else {


                if (do.enc2utf8)
                    file <- enc2utf8(file)


                if (normalize) {
                    if (exists.owd) {
                        if (!is.null(owd <- get.owd))
                            assign.chdir(file, frame, owd)
                        else assign.default(file, frame)
                    }
                    else assign.default(file, frame)
                }


                # use of "" refers to the R-level 'standard input' stdin.
                # this means 'source' did not open a file, so we assign
                # thispathfile the value of NULL and continue to the next
                # iteration. We use thispathfile as NULL to skip this source
                # call the next time this.path leads here
                else if (file == "") {
                    if (file.only)
                        stop(simpleError(
                            sprintf("invalid '%s', must not be \"\"", EncodeChar(name)),
                            call
                        ))
                    else assign.NULL(frame)
                }


                # use of "clipboard" and "stdin" refer to the clipboard or
                # to the C-level 'standard input' of the process.
                # this means 'source' did not open a file, so we
                # assign thispathfile the value of NULL and continue to the
                # next iteration. We use thispathfile as NULL to skip this
                # source call the next time this.path leads here
                else if (is.clipboard(file) || file == "stdin") {
                    if (file.only)
                        stop(simpleError(
                            sprintf("invalid '%s', must not be \"clipboard\" nor \"stdin\"", EncodeChar(name)),
                            call
                        ))
                    else assign.NULL(frame)
                }


                # as of this.path_0.5.0, throw an error that 'file' cannot
                # be a URL when mixed with 'this.path'
                #
                # previously, the error would've occured at 'normalizePath',
                # so it makes more sense to have this error here
                else if (grepl("^(ftp|ftps|http|https)://", file)) {
                    if (file.only)
                        stop(simpleError(
                            sprintf("invalid '%s', cannot be a URL", EncodeChar(name)),
                            call
                        ))
                    else assign.URL(file, frame)
                }


                # as of this.path_0.4.3, the path is determined slightly
                # differently. this change is to account for two possible
                # scenarios
                # * source("file://absolute or relative path")
                # * source("file:///absolute path")
                # the description of this connection should remove the
                # leading characters
                else if (grepl("^file://", file)) {
                    if (file.only)
                        stop(simpleError(
                            sprintf("invalid '%s', cannot be a file URL", EncodeChar(name)),
                            call
                        ))
                    else assign.fileURL(file, frame)
                }


                # in 0.3.0, compatibility was added when using
                # 'source(chdir = TRUE)'. this changes the working
                # directory to the directory of the 'file' argument. since
                # the 'file' argument was relative to the working directory
                # prior to being changed, we need to change it to the
                # previous working directory before we can normalize the
                # path
                else if (exists.owd) {
                    if (!is.null(owd <- get.owd))
                        assign.chdir(file, frame, owd)
                    else assign.default(file, frame)
                }
                else assign.default(file, frame)
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


            if (character.only)
                stop(simpleError(
                    sprintf("invalid '%s', must be a character string", EncodeChar(name)),
                    call
                ))
            else if (!inherits(file, "connection"))
                stop(simpleError(
                    sprintf("invalid '%s', must be a string or connection", EncodeChar(name)),
                    call
                ))
            else {
                summary <- summary.connection(file)
                description <- summary[["description"]]
                klass <- summary[["class"]]
                if (klass %in% c("file", "gzfile", "bzfile", "xzfile", "fifo")) {
                    # the file:// in a file URL will already be removed
                    assign.fileURL2(description, frame)
                }
                else if (klass %in% c("url-libcurl", "url-wininet")) {
                    if (file.only)
                        stop(simpleError(
                            sprintf("invalid '%s', cannot be a URL connection", EncodeChar(name)),
                            call
                        ))
                    else assign.URL(description, frame)
                }
                else if (is.clipboard(klass) || klass %in% c("pipe", "terminal")) {
                    if (file.only)
                        stop(simpleError(
                            sprintf("invalid '%s', cannot be a clipboard / / pipe / / terminal connection", EncodeChar(name)),
                            call
                        ))
                    else assign.NULL(frame)
                }
                else if (klass == "unz") {
                    assign(.(thispatherror), thisPathInZipFileError(description), frame)
                    lockBinding(.(thispatherror), frame)
                    assign(.(thispathformsg), description, frame)
                    lockBinding(.(thispathformsg), frame)
                    assign(.(thispathassocwfile), NULL, frame)
                    lockBinding(.(thispathassocwfile), frame)
                }
                else if (klass %in% c("textConnection", "rawConnection", "sockconn", "servsockconn")) {
                    if (file.only)
                        stop(simpleError(
                            sprintf("invalid '%s', cannot be a textConnection / / rawConnection / / sockconn / / servsockconn", EncodeChar(name)),
                            call
                        ))
                    else assign.NULL(frame)
                }
                else {
                    if (file.only)
                        stop(simpleError(
                            sprintf(
                                sprintf("invalid '%s' (a connection of class '%%s'), expected a file connection", sprintfEncode(EncodeChar(name))),
                                EncodeChar(klass)
                            ),
                            call
                        ))
                    else {
                        assign(.(thispatherror), thisPathUnrecognizedConnectionClassError(file), frame)
                        lockBinding(.(thispatherror), frame)
                        assign(.(thispathformsg), description, frame)
                        lockBinding(.(thispathformsg), frame)
                    }
                }
            }
        }


        assign.done(frame)
    }, list(
        name = name, call = call,
        character.only = character.only, file.only = file.only,
        exists.owd = exists.owd, get.owd = get.owd,
        do.enc2utf8 = do.enc2utf8,
        normalize = normalize
    )))
})


# simplify ----


sprintfEncode <- function (x)
gsub("%", "%%", x, fixed = TRUE, useBytes = TRUE)


EncodeChar <- function (x)
encodeString(x, na.encode = FALSE)


.is.simple <- function (x)
!is.object(x) && !(typeof(x) %in% c("symbol", "language"))


.simplify <- function (expr, condition = FALSE)
{
    if (identical(environment()[["expr"]], quote(expr = )))
        return(quote(expr = ))
    fun <- function(e) {
        if (is.pairlist(e))
            as.pairlist(lapply(e, .simplify))
        else if (is.call(e)) {
            if (identical(e[[1L]], quote(`(`))) {
                if (length(e) != 2L)
                    stop("incorrect number of arguments to '('")
                fun(e[[2L]])
            }
            else if (identical(e[[1L]], quote(`{`))) {
                x <- list()
                for (i in seq_len(length(e) - 1L)) {
                    if (is.null(e[[i]])) {}
                    else if (is.call(e[[i]])) {
                        if (identical(e[[i]][[1L]], quote(`{`))) {
                            x <- c(x, lapply(e[[i]][-1], .simplify))
                        }
                        else x <- c(x, list(.simplify(e[[i]])))
                    }
                    else x <- c(x, list(.simplify(e[[i]])))
                }
                # remove any NULL of "semi-NULL" elements
                x <- x[!vapply(x, function(xx) {
                    is.null(xx) || identical(xx, quote(if (FALSE) NULL))
                }, NA)]
                x <- c(x, list(fun(e[[length(e)]])))
                if (length(x) == 1L)
                    NULL
                else if (length(x) == 2L)
                    x[[2L]]
                else as.call(x)
            }
            else if (identical(e[[1L]], quote(`!`))) {
                if (length(e) != 2L)
                    stop("incorrect number of arguments to '!'")
                e[2L] <- list(fun(e[[2L]]))
                if (.is.simple(e[[2L]])) {
                    val <- !e[[2L]]
                    if (condition) {
                        if (val)
                            TRUE
                        else FALSE
                    }
                    else val
                }
                else e
            }
            else if (identical(e[[1L]], quote(`&&`))) {
                if (length(e) != 3L)
                    stop("incorrect number of argument to '&&'")
                e[2L] <- list(fun(e[[2L]]))
                e[3L] <- list(fun(e[[3L]]))
                if (.is.simple(e[[2L]]) &&
                    .is.simple(e[[3L]]))
                {
                    val <- e[[2L]] && e[[3L]]
                    if (condition) {
                        if (val)
                            TRUE
                        else FALSE
                    }
                    else val
                }
                else e
            }
            else if (identical(e[[1L]], quote(`||`))) {
                if (length(e) != 3L)
                    stop("incorrect number of argument to '||'")
                e[2L] <- list(fun(e[[2L]]))
                e[3L] <- list(fun(e[[3L]]))
                if (.is.simple(e[[2L]]) &&
                    .is.simple(e[[3L]]))
                {
                    val <- e[[2L]] || e[[3L]]
                    if (condition) {
                        if (val)
                            TRUE
                        else FALSE
                    }
                    else val
                }
                else e
            }
            else if (identical(e[[1L]], quote(xor))) {
                if (length(e) != 3L)
                    stop("incorrect number of argument to 'xor'")
                e[2L] <- list(fun(e[[2L]]))
                e[3L] <- list(fun(e[[3L]]))
                if (.is.simple(e[[2L]]) &&
                    .is.simple(e[[3L]]))
                {
                    val <- xor(e[[2L]], e[[3L]])
                    if (condition) {
                        if (val)
                            TRUE
                        else FALSE
                    }
                    else val
                }
                else e
            }
            else if (identical(e[[1L]], quote(isTRUE))) {
                if (length(e) != 2L)
                    stop("incorrect number of argument to 'isTRUE'")
                e[2L] <- list(.simplify(e[[2L]]))
                if (.is.simple(e[[2L]]))
                    isTRUE(e[[2L]])
                else e
            }
            else if (identical(e[[1L]], quote(isFALSE))) {
                if (length(e) != 2L)
                    stop("incorrect number of argument to 'isFALSE'")
                e[2L] <- list(.simplify(e[[2L]]))
                if (.is.simple(e[[2L]]))
                    isFALSE(e[[2L]])
                else e
            }
            else if (identical(e[[1L]], quote(is.null))) {
                if (length(e) != 2L)
                    stop("incorrect number of argument to 'is.null'")
                e[2L] <- list(.simplify(e[[2L]]))
                if (.is.simple(e[[2L]]))
                    is.null(e[[2L]])
                else e
            }
            else if (identical(e[[1L]], quote(`if`))) {
                if (length(e) == 3L) {
                    e[2L] <- list(.simplify(e[[2L]], condition = TRUE))
                    if (.is.simple(e[[2L]])) {
                        if (e[[2L]])
                            .simplify(e[[3L]])
                        else quote(if (FALSE) NULL)
                    }
                    else {
                        e[3L] <- list(.simplify(e[[3L]]))
                        e
                    }
                }
                else if (length(e) == 4L) {
                    e[2L] <- list(.simplify(e[[2L]], condition = TRUE))
                    if (.is.simple(e[[2L]])) {
                        if (e[[2L]])
                            .simplify(e[[3L]])
                        else .simplify(e[[4L]])
                    }
                    else {
                        e[3L] <- list(.simplify(e[[3L]]))
                        e[4L] <- list(.simplify(e[[4L]]))
                        e
                    }
                }
                else stop("incorrect number of argument to 'if'")
            }
            else if (identical(e[[1L]], quote(`while`))) {
                if (length(e) != 3L)
                    stop("incorrect number of argument to 'while'")
                e[2L] <- list(.simplify(e[[2L]], condition = TRUE))
                if (.is.simple(e[[2L]])) {
                    if (e[[2L]])
                        call("repeat", .simplify(e[[3L]]))
                    else quote(if (FALSE) NULL)
                }
                else {
                    e[3L] <- list(.simplify(e[[3L]]))
                    e
                }
            }
            else if (identical(e[[1L]], quote(sprintf))) {
                e[-1L] <- lapply(e[-1L], .simplify)
                if (all(vapply(e[-1L], .is.simple, NA)))
                    eval(e, baseenv())
                else e
            }
            else if (identical(e[[1L]], quote(sprintfEncode))) {
                if (length(e) != 2L)
                    stop("incorrect number of argument to 'sprintfEncode'")
                e[2L] <- list(.simplify(e[[2L]]))
                if (.is.simple(e[[2L]]))
                    sprintfEncode(e[[2L]])
                else e
            }
            else if (identical(e[[1L]], quote(EncodeChar))) {
                if (length(e) != 2L)
                    stop("incorrect number of argument to 'EncodeChar'")
                e[2L] <- list(.simplify(e[[2L]]))
                if (.is.simple(e[[2L]]))
                    EncodeChar(e[[2L]])
                else e
            }
            else if (identical(e[[1L]], quote(`+`))) {
                if (length(e) == 2L) {
                    e[2L] <- list(.simplify(e[[2L]]))
                    if (.is.simple(e[[2L]]))
                        +e[[2L]]
                    else e
                }
                else if (length(e) == 3L) {
                    e[2L] <- list(.simplify(e[[2L]]))
                    e[3L] <- list(.simplify(e[[3L]]))
                    if (.is.simple(e[[2L]]) &&
                        .is.simple(e[[3L]]))
                    {
                        e[[2L]] + e[[3L]]
                    }
                    else e
                }
                else stop("incorrect number of argument to '+'")
            }
            else if (identical(e[[1L]], quote(`-`))) {
                if (length(e) == 2L) {
                    e[2L] <- list(.simplify(e[[2L]]))
                    if (.is.simple(e[[2L]]))
                        -e[[2L]]
                    else e
                }
                else if (length(e) == 3L) {
                    e[2L] <- list(.simplify(e[[2L]]))
                    e[3L] <- list(.simplify(e[[3L]]))
                    if (.is.simple(e[[2L]]) &&
                        .is.simple(e[[3L]]))
                    {
                        e[[2L]] - e[[3L]]
                    }
                    else e
                }
                else stop("incorrect number of argument to '-'")
            }
            else as.call(lapply(e, .simplify))
        }
        else e
    }
    fun(expr)
}


simplify <- function (expr)
.simplify(expr)


# this.path(), this.dir(), and here() ----


.this.path.toplevel <- function (verbose = FALSE, original = FALSE, for.msg = FALSE)
{
    if (in.shell) {


        value <- shFILE(original, for.msg, default = {
            stop(thisPathNotExistsError(
                this_path_used_in_an_inappropriate_fashion,
                "* R is being run from a shell and argument 'FILE' is missing",
                call = sys.call(sys.nframe())))
        })
        if (verbose) cat("Source: shell argument 'FILE'\n")
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


        # this might look stupid af, but we need to do this so the byte
        # compiler doesn't try to evaluate these promises early
        context <- (.rs.api.getActiveDocumentContext)()
        active <- context[["id"]] != "#console"
        if (!active) {
            context <- (.rs.api.getSourceEditorContext)()
            if (is.null(context)) {
                if (for.msg)
                    return(NA_character_)
                else stop(thisPathNotExistsError(
                    this_path_used_in_an_inappropriate_fashion,
                    "* R is being run from RStudio with no documents open\n",
                    "  (or source document has no path)"))
            }
        }
        path <- context[["path"]]


        # the encoding is not explicitly set (at least on Windows),
        # so we have to do that ourselves
        Encoding(path) <- "UTF-8"


        if (nzchar(path)) {
            if (verbose)
                cat(
                    if (active)
                        "Source: active document in RStudio\n"
                    else
                        "Source: source document in RStudio\n"
                )
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }
        else if (for.msg)
            return(NA_character_)
        else stop(
            this_path_used_in_an_inappropriate_fashion,
            if (active)
                "* active document in RStudio does not exist"
            else "* source document in RStudio does not exist")
    }


    else if (gui.vscode) {


        context <- rstudioapi::getSourceEditorContext()
        if (is.null(context)) {
            if (for.msg)
                return(NA_character_)
            else stop(thisPathNotExistsError(
                this_path_used_in_an_inappropriate_fashion,
                "* R is being run from VSCode with no documents open\n",
                "  (or document has no path)"
            ))
        }


        if (startsWith(context[["id"]], "untitled:")) {
            if (for.msg)
                return(NA_character_)
            else stop(
                this_path_used_in_an_inappropriate_fashion,
                "* document in VSCode does not exist")
        }


        path <- context[["path"]]
        Encoding(path) <- "UTF-8"


        if (nzchar(path)) {
            if (verbose) cat("Source: document in VSCode\n")
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }
        else if (for.msg)
            return(NA_character_)
        else stop(
            this_path_used_in_an_inappropriate_fashion,
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
        x <- names(utils::getWindowsHandles(minimized = TRUE))
        x <- x[
            startsWith(x, "R Console") |
            is.r.editor(x)             |
            is.abs.path(x)             |
            x %in% untitled
        ]
        if (!length(x))
            stop("no windows in Rgui; should never happen, please report!")
        else if (active <- !startsWith(x[[1L]], "R Console"))
            x <- x[[1L]]
        else if (length(x) >= 2L)
            x <- x[[2L]]
        else if (for.msg)
            return(NA_character_)
        else stop(thisPathNotExistsError(
            this_path_used_in_an_inappropriate_fashion,
            "* R is being run from Rgui with no documents open"))
        if (x %in% untitled) {
            if (for.msg)
                return(NA_character_)
            else stop(
                this_path_used_in_an_inappropriate_fashion,
                if (active)
                    "* active document in Rgui does not exist"
                else "* source document in Rgui does not exist")
        }
        if (any(i <- endsWith(x, r.editor)))
            x <- substr(x, 1L, nchar(x) - nchar_r.editor[[which(i)]])
        else active <- FALSE
        if (is.abs.path(x)) {
            if (verbose)
                cat(
                    if (active)
                        "Source: active document in Rgui\n"
                    else
                        "Source: source document in Rgui\n"
                )
            return(normalizePath(x, winslash = "/", mustWork = TRUE))
        }
        else stop("invalid windows handles, path preceding \" - R Editor\" must be absolute")
    }


    else if (gui.aqua) {


        if (for.msg)
            NA_character_
        else stop(thisPathInAQUAError())
    }


    # running from a shell under Unix-alikes with GUI 'Tk'
    else if (gui.tk) {


        if (for.msg)
            NA_character_
        else stop(thisPathNotExistsError(
            this_path_used_in_an_inappropriate_fashion,
            "* R is being run from Tk which does not make use of its -f FILE, --file=FILE arguments"))
    }


    # running R in another manner
    else {


        if (for.msg)
            NA_character_
        else stop(thisPathUnrecognizedMannerError())
    }
}


delayedAssign("identical2", {
    if (getRversion() >= "4.2.0") {
function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE,
    ignore.bytecode = FALSE, ignore.environment = FALSE, ignore.srcref = FALSE,
    extptr.as.ref = TRUE)
    }
    else {
function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE,
    ignore.bytecode = FALSE, ignore.environment = FALSE, ignore.srcref = FALSE)
    }
})


rm.list <- c(rm.list, "returnfile")
returnfile <- function(fun.name, character.only = FALSE, file.only = FALSE, maybe.decrement = FALSE) NULL
body(returnfile) <- bquote({
    simplify(substitute({
        path <- getInFrame(.(thispathofile), frame)
        if (!file.only) {
            if (is.null(path))
                next
        }
        if (!character.only) {
            if (existsInFrame(.(thispatherror), frame)) {
                if (for.msg)
                    return(getInFrame(.(thispathformsg), frame))
                else if (get.frame.number) {
                    if (!existsInFrame(.(thispathassocwfile), frame))
                        return(NA_integer_)
                    if (maybe.decrement) {
                        if (n >= 2L) {
                            return(n - 1L)
                        }
                    }
                    return(n)
                }
                else {
                    error <- getInFrame(.(thispatherror), frame)
                    error$call <- sys.call(sys.nframe())
                    stop(error)
                }
            }
        }
        if (get.frame.number) {
            if (maybe.decrement) {
                if (n >= 2L) {
                    return(n - 1L)
                }
            }
            return(n)
        }
        if (for.msg) {
            if (isTRUE(original))
                return(path)
            if (is.unevaluated.promise(.(thispathfile), frame))
                return(path)
            else return(getInFrame(.(thispathfile), frame))
        }
        if (isTRUE(original)) {}
        else {
            if (is.unevaluated.promise(.(thispathfile), frame)) {
                if (isFALSE(original))
                    path <- getInFrame(.(thispathfile), frame)
            }
            else path <- getInFrame(.(thispathfile), frame)
        }
        if (verbose) cat(sprintf("Source: call to function %s\n", fun.name))
        return(path)
    }, list(
        character.only = character.only,
        file.only = file.only,
        maybe.decrement = maybe.decrement,
        fun.name = fun.name
    )))
})


.this.path.old <- function (verbose = FALSE, original = FALSE, for.msg = FALSE,
    N = sys.nframe() - 1L, get.frame.number = FALSE) NULL
body(.this.path.old) <- bquote({
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


    verbose <- if (verbose) TRUE else FALSE
    original
    original <- tryCatch(if (original) TRUE else FALSE, error = function(e) NA)
    for.msg <- if (for.msg) TRUE else FALSE
    N <- as.integer(N)[[1L]]
    if (is.na(N) || N < 0L)
        stop(gettextf("invalid '%s' argument", "N", domain = "R"))
    get.frame.number <- if (get.frame.number) TRUE else FALSE


    if (get.frame.number && (!isFALSE(original) || for.msg))
        stop(sprintf("'%s' and '%s' must be FALSE when 'get.frame.number' is TRUE",
            "original", "for.msg", "get.frame.number"))


    if (N <= 0L) {
        if (get.frame.number)
            return(0L)
        return(.this.path.toplevel(verbose, original, for.msg))
    }


    # in 0.4.0, compatibility with 'testthat::source_file' was added. it is
    # almost identical to 'sys.source'
    source_file <- if (testthat_loaded <- isNamespaceLoaded("testthat"))
        testthat::source_file
        # getExportedValue("testthat", "source_file")


    # in 1.1.0, compatibility with 'knitr::knit' was added
    knit <- if (knitr_loaded <- isNamespaceLoaded("knitr"))
        knitr::knit


    for (n in seq.int(to = 1L, by = -1L, length.out = N)) {


        frame <- sys.frame(n)
        fun <- sys.function(n)


        if (identical2(fun, source)) {


            # if the path has yet to be saved
            if (!existsInFrame(.(thispathdone), frame)) {


                # 'ofile' is a copy of the original 'file' argument
                # if it does not exist, we have not entered a file yet,
                # so go to the next iteration
                if (!existsInFrame("ofile", frame))
                    next


                .(checkfile(
                    name       = "ofile",
                    exists.owd = quote(existsInFrame("owd", frame)),
                    get.owd    = quote(getInFrame("owd", frame)),
                ))
            }
            .(returnfile("source"))
        }


        else if (identical2(fun, sys.source)) {


            # much the same as 'source' except simpler, we don't have to
            # account for argument 'file' being a connection or ""
            if (!existsInFrame(.(thispathdone), frame)) {


                # as with 'source', we check that argument 'file' has been
                # forced, and continue to the next iteration if not
                #
                # in source, we could do exists("ofile"), but we can't do the
                # same here, so call this C function to test if file is an
                # unevaluated promise
                if (is.unevaluated.promise("file", frame))
                    next


                .(checkfile(
                    name           = "file",
                    character.only = TRUE,
                    file.only      = TRUE,
                    exists.owd     = quote(existsInFrame("owd", frame)),
                    get.owd        = quote(getInFrame("owd", frame))
                ))
            }
            .(returnfile("sys.source", character.only = TRUE, file.only = TRUE))
        }


        # in 0.2.0, compatibility with 'debugSource' in 'RStudio' was added
        else if (gui.rstudio && identical2(fun, debugSource)) {


            if (!existsInFrame(.(thispathdone), frame)) {


                if (is.unevaluated.promise("fileName", frame))
                    next


                .(checkfile(
                    name           = "fileName",
                    character.only = TRUE,
                    do.enc2utf8    = TRUE
                ))
            }
            .(returnfile("debugSource in RStudio", character.only = TRUE))
        }


        else if (testthat_loaded && identical2(fun, source_file)) {


            if (!existsInFrame(.(thispathdone), frame)) {


                # as with 'source' and 'sys.source', we check that
                # argument 'path' has been forced, and continue to the next
                # iteration if not
                if (is.unevaluated.promise("path", frame))
                    next


                .(checkfile(
                    name           = "path",
                    character.only = TRUE,
                    file.only      = TRUE,
                    exists.owd     = quote(existsInFrame("old_dir", frame)),
                    get.owd        = quote(getInFrame("old_dir", frame)),
                    normalize      = quote(testthat.uses.brio())
                ))
            }
            .(returnfile("source_file in package testthat", character.only = TRUE, file.only = TRUE))
        }


        else if (knitr_loaded && identical2(fun, knit)) {


            # if the path has yet to be saved
            if (!existsInFrame(.(thispathdone), frame)) {


                # we care about 'oenvir' because 'input' may be edited in lines
                # 10 and 11 of knitr::knit, but after 'oenvir' is created on
                # line 36, 'input' is never edited again
                if (!existsInFrame("oenvir", frame))
                    next


                if (eval(call("missing", as.symbol("input")), frame)) {
                    assign.NULL(frame)
                    assign.done(frame)
                    next
                }


                .(checkfile(
                    name           = "input",
                    exists.owd     = TRUE,
                    get.owd        = quote(knitr.output.dir())
                ))
            }
            .(returnfile("knit in package knitr"))
        }


        else if (identical2(fun, wrap.source)) {
            if (!existsInFrame(.(thispathdone), frame))
                next
            .(returnfile("wrap.source in package this.path", maybe.decrement = TRUE))
        }


        else if (existsInFrame(.(insidesourcewashere), frame)) {
            .(returnfile("inside.source in package this.path", maybe.decrement = TRUE))
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


    .this.path.toplevel(verbose, original, for.msg)
})





.normalizePath <- function (path)
normalizePath(path, winslash = "/", mustWork = TRUE)


.normalizeAgainst <- function (ofile, owd)
{
    cwd <- getwd()
    if (is.null(cwd))
        stop("cannot '.normalizeAgainst' as current directory is unknown")
    on.exit(setwd(cwd))
    setwd(owd)
    .normalizePath(ofile)
}


testthat.uses.brio <- function ()
as.numeric_version(getNamespaceVersion("testthat")) >= "3.1.2"


knitr.output.dir <- function ()
knitr::opts_knit$get("output.dir")


.this.path <- function(verbose = FALSE, original = FALSE, for.msg = FALSE,
    N = sys.nframe() - 1L, get.frame.number = FALSE)
.External2(C_thispath, verbose, original, for.msg, N, get.frame.number)


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





faster.subsequent.times.test <- function ()
{
    first.time <- microbenchmark::microbenchmark(
        `first time` = .this.path(),
        times = 1
    )
    subsequent <- microbenchmark::microbenchmark(
        subsequent = .this.path(),
        times = 100
    )
    rbind(first.time, subsequent)
}





this.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE, default, else.)
tryCatch({
    .this.path(verbose, original, for.msg)
}, function(c) default)


this.dir <- function (verbose = getOption("verbose"), default, else.)
tryCatch({
    .this.dir(verbose)
}, function(c) default)


local({
    tmp <- readLines("src/thispathdefn.h", warn = FALSE)
    tmp <- tmp[[grep("#define thisPathNotExistsErrorCls", tmp, fixed = TRUE) + 1L]]
    tmp <- str2lang(tmp)
    if (!is.character(tmp) || length(tmp) != 1L ||
        is.na(tmp))
    {
        stop("could not determine class name")
    }
    tmp
}) ->
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
            .(as.call(append(as.list(tmp), list(else. = quote((else.)(value))))))
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
            .(as.call(append(as.list(tmp), list(else. = quote((else.)(value))))))
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





rm(list = rm.list)
