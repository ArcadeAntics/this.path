# a character vector of symbols to remove (including itself)
rm.list <- "rm.list"
rm.list.append <- function (...)
{
    rm.list <<- c(rm.list, ...)
}
rm.list.append("rm.list.append")





tmp <- readLines("./src/symbols.h")
tmp <- list(
    thispathofile        = str2lang(tmp[[grep(paste0("^[ \t]*", "thispathofileSymbol"       , "[ \t]+INI_as[ \t]*\\([ \t]*install[ \t]*\\([ \t]*$"), tmp) + 1L]]),
    thispathfile         = str2lang(tmp[[grep(paste0("^[ \t]*", "thispathfileSymbol"        , "[ \t]+INI_as[ \t]*\\([ \t]*install[ \t]*\\([ \t]*$"), tmp) + 1L]]),
    thispathofilejupyter = str2lang(tmp[[grep(paste0("^[ \t]*", "thispathofilejupyterSymbol", "[ \t]+INI_as[ \t]*\\([ \t]*install[ \t]*\\([ \t]*$"), tmp) + 1L]]),
    thispathfilejupyter  = str2lang(tmp[[grep(paste0("^[ \t]*", "thispathfilejupyterSymbol" , "[ \t]+INI_as[ \t]*\\([ \t]*install[ \t]*\\([ \t]*$"), tmp) + 1L]])
)
if (!all(vapply(tmp, function(x) is.character(x) && length(x) == 1 && !is.na(x), NA)))
    stop("could not determine variable names")
for (i in seq_along(tmp)) assign(names(tmp)[[i]], tmp[[i]])
rm.list.append(names(tmp))
rm(i, tmp)





defunctError <- function (new, package = NULL, msg, old = as.character(sys.call(sys.parent()))[1L])
{
    msg <- if (missing(msg)) {
        msg <- gettextf("'%s' is defunct.\n", old, domain = "R-base")
        if (!missing(new))
            msg <- c(msg, gettextf("Use '%s' instead.\n", new, domain = "R-base"))
        c(msg, if (!is.null(package))
            gettextf("See help(\"Defunct\") and help(\"%s-defunct\").", package, domain = "R-base")
        else gettext("See help(\"Defunct\")", domain = "R-base"))
    }
    else as.character(msg)
    msg <- paste(msg, collapse = "")
    if (missing(new))
        new <- NULL
    errorCondition(msg, old = old, new = new, package = package,
        class = "defunctError")
}





.shFILE <- function (original = TRUE, for.msg = FALSE)
.External2(C_shfile, original, for.msg)
evalq(envir = environment(.shFILE) <- new.env(), {
    delayedAssign(thispathofile, if (in.shell) shINFO[["FILE"]] else NA_character_)
    eval(call("delayedAssign", thispathfile, call(".normalizePath", as.symbol(thispathofile))))
})


delayedAssign("has.shFILE", !is.na(.shFILE()))


shFILE <- function (original = FALSE, for.msg = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.))
            .shFILE(original, for.msg)
        else stop("'shFILE' with 'else.' but not 'default' makes no sense")
    }
    else {
        if (missing(else.)) {
            if (has.shFILE)
                .shFILE(original, for.msg)
            else if (for.msg)
                NA_character_
            else default
        }
        else {
            if (has.shFILE) {
                value <- .shFILE(original, for.msg)
                (else.)(value)
            }
            else if (for.msg) {
                value <- NA_character_
                (else.)(value)
            }
            else default
        }
    }
}


normalized.shFILE <- function (...)
stop(defunctError("shFILE", pkgname, old = "normalized.shFILE"))





is.abs.path <- function (path)
.External2(C_isabspath, path)





readLines2 <- function(path, default = character(0)) {
    conn <- tryCatch({
        file(path, "rb", encoding = "")
    }, error = function(e) NULL)
    if (is.null(conn))
        return(default)
    on.exit(close(conn))
    txt <- readLines(conn)
    encoding <- txt[c(FALSE, TRUE)]
    txt      <- txt[c(TRUE, FALSE)]
    Encoding(txt) <- encoding
    txt
}
r.editor_msvcrt <- readLines2("./inst/extdata/r-editor_msvcrt.txt", " - R Editor")
r.editor_ucrt   <- readLines2("./inst/extdata/r-editor_ucrt.txt"  , " - R Editor")


untitled_msvcrt <- readLines2("./inst/extdata/untitled_msvcrt.txt", "Untitled - R Editor")
untitled_ucrt   <- readLines2("./inst/extdata/untitled_ucrt.txt"  , "Untitled - R Editor")


rm(readLines2)





delayedAssign("r.editor", {
    if (gui.rgui) {
        if (ucrt)
            r.editor_ucrt
        else r.editor_msvcrt
    }
})
delayedAssign("untitled", {
    if (gui.rgui) {
        if (ucrt)
            untitled_ucrt
        else untitled_msvcrt
    }
})





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
            if (skip.stop && (identical2)(fun, stop)) {
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


promise.is.unevaluated <- function (sym, env)
.External2(C_promiseisunevaluated, sym, env)


is.clipboard <- function (file)
.External2(C_isclipboard, file)


rm.list.append("this_path_used_in_an_inappropriate_fashion")
this_path_used_in_an_inappropriate_fashion <- local({
    tmp <- readLines("./src/thispathdefn.h")
    tmp <- tmp[[grep("^[ \t]*#[ \t]*define[ \t]+this_path_used_in_an_inappropriate_fashion[ \t]*\\\\[ \t]*$", tmp) + 1L]]
    tmp <- str2lang(tmp)
    if (!is.character(tmp) || length(tmp) != 1L ||
        is.na(tmp))
    {
        stop("could not determine error message")
    }
    strsplit(tmp, "(?<=\\n)", perl = TRUE)[[1L]]
})


getContents <- function (file, encoding = getOption("encoding"))
{
    if (identical(encoding, "unknown")) {
        enc <- utils::localeToCharset()
        encoding <- enc[length(enc)]
    }
    else enc <- encoding
    if (length(enc) > 1L) {
        encoding <- NA
        owarn <- options(warn = 2)
        for (e in enc) {
            if (is.na(e))
                next
            zz <- file(file, encoding = e)
            res <- tryCatch(readLines(zz, warn = FALSE),
                error = identity)
            close(zz)
            if (!inherits(res, "error")) {
                encoding <- e
                break
            }
        }
        options(owarn)
    }
    if (is.na(encoding))
        stop("unable to find a plausible encoding")
    filename <- file
    file <- file(filename, "r", encoding = encoding)
    on.exit(close(file))
    readLines(file, warn = FALSE)
}


getIPythonNotebookContents <- function (..., do.unlist = TRUE, give.f = TRUE)
{
    lines <- getContents(...)
    source <- jsonlite::fromJSON(lines)[[c("cells", "source")]]
    if (do.unlist) {
        value <- unlist(source)
        if (give.f)
            attr(value, "f") <- as.factor(rep(seq_along(source), lengths(source)))
        value
    } else source
}


removeSource <- function (fn)
{
    recurse <- function(part) {
        if (is.name(part))
            return(part)
        attr(part, "srcref") <- NULL
        attr(part, "wholeSrcref") <- NULL
        attr(part, "srcfile") <- NULL
        if (is.language(part) && is.recursive(part)) {
            for (i in seq_along(part)) part[i] <- list(recurse(part[[i]]))
        }
        part
    }
    if (is.function(fn)) {
        if (!is.primitive(fn)) {
            attr(fn, "srcref") <- NULL
            at <- attributes(fn)
            attr(body(fn), "wholeSrcref") <- NULL
            attr(body(fn), "srcfile") <- NULL
            body(fn) <- recurse(body(fn))
            if (!is.null(at))
                attributes(fn) <- at
        }
        fn
    }
    else if (is.language(fn)) {
        recurse(fn)
    }
    else stop("argument is not a function or language object:",
        typeof(fn))
}


getNamedElement <- function (x, names)
{
    for (name in names) {
        if (i <- match(name, names(x), 0L, c("", NA_character_)))
            x <- x[[i]]
        else return()
    }
    x
}


# this.path(), this.dir(), and here() ----


isJupyterLoaded <- function ()
gui.jupyter && isNamespaceLoaded("IRkernel") && (identical2)(sys.function(1L), IRkernel::main)


# `utils::getWindowsHandles` (Windows exclusive) returns a list of
# external pointers containing the windows handles. the thing of
# interest are the names of this list, these should be the names of the
# windows belonging to the current R process.
.this.path.rgui <- function (verbose = FALSE, original = FALSE, for.msg = FALSE)
.External2(C_thispathrgui, names(utils::getWindowsHandles(minimized = TRUE)),
    untitled, r.editor, verbose, original, for.msg)


.this.path.toplevel <- function (verbose = FALSE, original = FALSE, for.msg = FALSE) NULL
body(.this.path.toplevel) <- bquote({
    if (in.shell) {


        value <- shFILE(original, for.msg, default = {
            stop(thisPathNotExistsError(
                ..(this_path_used_in_an_inappropriate_fashion),
                "* R is being run from a shell and argument 'FILE' is missing",
                call = sys.call()))
        })
        if (verbose) cat("Source: shell argument 'FILE'\n")
        return(value)
    }


    # running from 'RStudio'
    else if (gui.rstudio) {


        if (!`init.tools:rstudio`()) {
            if (for.msg) {
                return(NA_character_)
            }
        }


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
        context <- .rs.api.getActiveDocumentContext()
        active <- context[["id"]] != "#console"
        if (!active) {
            context <- .rs.api.getSourceEditorContext()
            if (is.null(context)) {
                if (for.msg)
                    return(NA_character_)
                else stop(thisPathNotExistsError(
                    ..(this_path_used_in_an_inappropriate_fashion),
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
            if (original)
                path
            else .normalizePath(path)
        }
        else if (for.msg)
            gettext("Untitled", domain = "RGui", trim = FALSE)
        else stop(
            ..(this_path_used_in_an_inappropriate_fashion),
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
                ..(this_path_used_in_an_inappropriate_fashion),
                "* R is being run from VSCode with no documents open\n",
                "  (or document has no path)"
            ))
        }


        if (startsWith(context[["id"]], "untitled:")) {
            if (for.msg)
                return(gettext("Untitled", domain = "RGui", trim = FALSE))
            else stop(
                ..(this_path_used_in_an_inappropriate_fashion),
                "* document in VSCode does not exist")
        }


        path <- context[["path"]]
        Encoding(path) <- "UTF-8"


        if (nzchar(path)) {
            if (verbose) cat("Source: document in VSCode\n")
            if (original)
                path
            else .normalizePath(path)
        }
        else if (for.msg)
            gettext("Untitled", domain = "RGui", trim = FALSE)
        else stop(
            ..(this_path_used_in_an_inappropriate_fashion),
            "* document in VSCode does not exist")
    }


    # running from 'jupyter'
    else if (gui.jupyter) {


        if (!is.na(.(as.symbol(thispathofilejupyter)))) {
            if (verbose) cat("Source: document in Jupyter\n")
            if (original)
                return(.(as.symbol(thispathofilejupyter)))
            else return(.External2(C_getpromisewithoutwarning, .(thispathfilejupyter)))
        }


        if (is.null(initwd)) {
            if (for.msg)
                return(NA_character_)
            else stop(thisPathNotExistsError(
                ..(this_path_used_in_an_inappropriate_fashion),
                "* R is being run from Jupyter but the initial working directory is unknown"))
        }


        if (!isJupyterLoaded()) {
            if (for.msg)
                return(NA_character_)
            else stop("Jupyter has not finished loading")
        }


        n <- sys.frame(1L)[["kernel"]][["executor"]][["nframe"]] + 2L
        ocall <- sys.call(n)
        call <- removeSource(ocall)


        files <- list.files(initwd, all.files = TRUE, full.names = TRUE,
            ..(
                if (getRversion() < "3.0.0")
                    expression()
                else expression(no.. = TRUE)
            )
        )
        files <- files[!dir.exists(files)]
        i <- grepl("\\.ipynb$", files, useBytes = TRUE)
        ipynb <- files[i]
        files <- files[!i]
        i <- grepl("\\.ipynb$", files, ignore.case = TRUE, useBytes = TRUE)
        IPYNB <- files[i]
        files <- files[!i]


        for (file in c(ipynb, IPYNB, files)) {
            contents <- tryCatch(getContents(file), error = identity)
            if (!inherits(contents, "error")) {
                contents <- tryCatch(jsonlite::parse_json(contents, simplifyVector = TRUE),
                    error = identity)
                if (!inherits(contents, "error")) {


                    language <- getNamedElement(contents, c("metadata", "kernelspec", "language"))
                    name <- getNamedElement(contents, c("metadata", "language_info", "name"))
                    version <- getNamedElement(contents, c("metadata", "language_info", "version"))
                    source <- getNamedElement(contents, c("cells", "source"))


                    # withAutoprint( { language; name; version; source } , spaced = TRUE, verbose = FALSE, width.cutoff = 60L); cat("\n\n\n\n\n")


                    if (is.character(language) && length(language) == 1L && !is.na(language) && language == "R" &&
                        is.character(name)     && length(name)     == 1L && !is.na(name)     && name     == "R" &&
                        is.character(version)  && length(version)  == 1L && !is.na(version)  && version  == as.character(getRversion()) &&
                        is.list(source)        && length(source)         && all(vapply(source, is.character, NA, USE.NAMES = FALSE)))
                    {
                        for (source0 in source) {
                            exprs <- tryCatch(parse(text = source0, n = -1, keep.source = FALSE, srcfile = NULL),
                                error = identity)
                            if (!inherits(exprs, "error")) {
                                for (expr in exprs) {
                                    if (identical(expr, call)) {
                                        .External2(C_setthispathjupyter, file)
                                        if (verbose) cat("Source: document in Jupyter\n")
                                        if (original)
                                            return(.(as.symbol(thispathofilejupyter)))
                                        else return(.External2(C_getpromisewithoutwarning, .(thispathfilejupyter)))
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }


        if (for.msg)
            NA_character_
        else stop(thisPathNotExistsError(
            ..(this_path_used_in_an_inappropriate_fashion),
            sprintf("* R is being run from Jupyter with initial working directory '%s'\n but could not find a file with contents matching:\n", encodeString(initwd)),
            {
                t <- attr(ocall, "srcref", exact = TRUE)
                paste(if (is.integer(t)) as.character(t) else deparse(ocall), collapse = "\n")
            }))
    }


    # running from 'Rgui' on Windows
    else if (gui.rgui) {


        .this.path.rgui(verbose, original, for.msg)
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
            ..(this_path_used_in_an_inappropriate_fashion),
            "* R is being run from Tk which does not make use of its -f FILE, --file=FILE arguments"))
    }


    # running R in another manner
    else {


        if (for.msg)
            NA_character_
        else stop(thisPathUnrecognizedMannerError())
    }
}, splice = TRUE)
evalq(envir = environment(.this.path.toplevel) <- new.env(), {
    assign(thispathofilejupyter, NA_character_)
    eval(call("delayedAssign", thispathfilejupyter, call(".normalizePath", as.symbol(thispathofilejupyter))))
})


set.this.path.jupyter <- function (...)
{
    if (!gui.jupyter)
        stop(gettextf("'%s' can only be called in Jupyter",
            "set.this.path.jupyter"))
    if (!isJupyterLoaded())
        stop(gettextf("'%s' can only be called after Jupyter has finished loading",
            "set.this.path.jupyter"))
    n <- sys.frame(1L)[["kernel"]][["executor"]][["nframe"]] + 2L
    if (sys.nframe() != n)
        stop(gettextf("'%s' can only be called from a top-level context",
            "set.this.path.jupyter"))
    path <- if (missing(...) || ...length() == 1L && (is.null(..1) || is.atomic(..1) && length(..1) == 1L && is.na(..1)))
        NA_character_
    else if (is.null(initwd))
        path.join(...)
    else path.join(initwd, ...)
    .External2(C_setthispathjupyter, path)
}


delayedAssign("identical2", {
    if (getRversion() >= "4.2.0") {


function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE,
    ignore.bytecode = FALSE, ignore.environment = FALSE, ignore.srcref = FALSE,
    extptr.as.ref = TRUE)


    } else if (getRversion() >= "3.4.0") {


function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE,
    ignore.bytecode = FALSE, ignore.environment = FALSE, ignore.srcref = FALSE)


    } else if (getRversion() >= "3.0.0") {


function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE,
    ignore.bytecode = FALSE, ignore.environment = FALSE)


    } else if (getRversion() >= "2.14.0") {


function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE,
    ignore.bytecode = FALSE)


    } else if (getRversion() >= "2.10.0") {


function (x, y)
identical(x, y, num.eq = FALSE, single.NA = FALSE, attrib.as.set = FALSE)


    } else {


function (x, y)
identical(x, y)


    }
})





.normalizePath <- function (path, winslash = "/", mustWork = TRUE)
normalizePath(path, winslash, mustWork)


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


.this.path <- function (verbose = FALSE, original = FALSE, for.msg = FALSE,
    N = sys.nframe() - 1L, get.frame.number = FALSE)
.External2(C_thispath, verbose, original, for.msg, N, get.frame.number)


get.frame.number <- function (N = sys.nframe() - 1L)
.External2(C_thispath, FALSE, FALSE, FALSE, N, TRUE)
# environment(get.frame.number) <- getNamespace("this.path")
# get.frame.number <- compiler::cmpfun(get.frame.number)


.this.dir <- function (verbose = FALSE)
{
    path <- .this.path(verbose)
    if (grepl("^(https|http|ftp|ftps)://", path)) {
        # path <- "https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R"
        p <- path.split.1(path)
        path.unsplit(if (length(p) >= 2L) p[-length(p)] else p)
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
    tmp <- readLines("./src/thispathdefn.h")
    tmp <- tmp[[grep("^[ \t]*#[ \t]*define[ \t]+thisPathNotExistsErrorCls[ \t]*\\\\[ \t]*$", tmp) + 1L]]
    tmp <- str2lang(tmp)
    if (!is.character(tmp) || length(tmp) != 1L ||
        is.na(tmp))
    {
        stop("could not determine class name")
    }
    tmp
})                             ->
    names(body(this.path))[3L] ->
    names(body(this.dir ))[3L]


tmp <- body(this.path)
tmp[[1L]] <- as.name("tryCatch2")
tmp[[2L]] <- call("<-", as.name("value"), tmp[[2L]])
body(this.path) <- bquote({
    if (missing(default)) {
        if (missing(else.))
            .(body(this.path)[[c(2L, 2L)]])
        else stop("'this.path' with 'else.' but not 'default' makes no sense")
    }
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
    if (missing(default)) {
        if (missing(else.))
            .(body(this.dir)[[c(2L, 2L)]])
        else stop("'this.dir' with 'else.' but not 'default' makes no sense")
    }
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
stop(defunctError("this.path(..., default = NULL)", pkgname, old = "this.path2(...)"))


this.dir2 <- function (...)
stop(defunctError("this.dir(..., default = NULL)", pkgname, old = "this.dir2(...)"))


this.dir3 <- function (...)
stop(defunctError("this.dir(..., default = getwd())", pkgname, old = "this.dir3(...)"))





here <- ici <- function (..., .. = 0L)
{
    base <- .this.path()
    base <- if (grepl("^(https|http|ftp|ftps)://", base)) {
        # base <- "https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R"
        # .. <- "2"


        p <- path.split.1(base)
        n <- length(p) - length(seq_len(..)) - 1L
        path.unsplit(if (n < 1L) p[1L] else p[seq_len(n)])
    }


    # base <- "//host/share/path/to/file"
    # base <- "C:/Users/iris/Documents/this.path/man/this.path.Rd"
    # .. <- "10"
    else .External2(C_dirname2, base, ..)
    path.join(base, ...)
}





Sys.path <- function ()
.this.path()


Sys.dir <- function ()
.this.dir()





try.shFILE <- function ()
tryCatch(.shFILE(FALSE), error = function(e) .shFILE())


try.this.path <- function ()
tryCatch(.this.path(), error = function(e) .this.path(for.msg = TRUE))





rm(list = rm.list)
