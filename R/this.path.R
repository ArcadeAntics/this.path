# a character vector of symbols to remove (including itself)
rm.list <- "rm.list"
rm.list.append <- function (...)
{
    rm.list <<- c(rm.list, ...)
}
rm.list.append("rm.list.append")


# str2lang() was added in R 3.6.0
if (getRversion() < "3.6.0") {
    rm.list.append("str2lang")
    str2lang <- function(s) {
        if (typeof(s) != "character")
            stop("argument must be character", domain = "R")
        if (length(s) != 1L)
            stop("argument must be a character string", domain = "R")
        ans <- parse(text = s, n = -1, keep.source = FALSE)
        if (length(ans) != 1L)
            stop(gettextf("parsing result not of length one, but %d", length(ans), domain = "R"), domain = NA)
        ans[[1L]]
    }
}


# bquote(splice = TRUE) was added in R 4.1.0
if (getRversion() < "4.1.0") {
    rm.list.append("bquote")
    bquote <- function(expr, where = parent.frame(), splice = FALSE) {
        if (!is.environment(where))
            where <- as.environment(where)
        unquote <- function(e) {
            if (is.pairlist(e))
                as.pairlist(lapply(e, unquote))
            else if (is.call(e)) {
                if (typeof(e[[1L]]) == "symbol" && e[[1L]] == ".")
                    eval(e[[2L]], where)
                else if (splice) {
                    if (typeof(e[[1L]]) == "symbol" && e[[1L]] == "..")
                        stop("can only splice inside a call", call. = FALSE)
                    else as.call(unquote.list(e))
                }
                else as.call(lapply(e, unquote))
            }
            else e
        }
        is.splice.macro <- function(e) is.call(e) && typeof(e[[1L]]) == "symbol" && e[[1L]] == ".."
        unquote.list <- function(e) {
            p <- Position(is.splice.macro, e, nomatch = NULL)
            if (is.null(p))
                lapply(e, unquote)
            else {
                n <- length(e)
                head <- if (p == 1)
                    NULL
                else e[1:(p - 1)]
                tail <- if (p == n)
                    NULL
                else e[(p + 1):n]
                macro <- e[[p]]
                mexp <- eval(macro[[2L]], where)
                if (!is.vector(mexp) && !is.expression(mexp))
                    stop("can only splice vectors")
                c(lapply(head, unquote), mexp, as.list(unquote.list(tail)))
            }
        }
        unquote(substitute(expr))
    }
}





tmp <- readLines("./src/symbols.h")
tmp <- list(
    thispathofile = str2lang(tmp[[grep("^[ \t]*thispathofileSymbol[ \t]+INI_as[ \t]*\\([ \t]*install[ \t]*\\([ \t]*$", tmp) + 1L]]),
    thispathfile  = str2lang(tmp[[grep("^[ \t]*thispathfileSymbol[ \t]+INI_as[ \t]*\\([ \t]*install[ \t]*\\([ \t]*$", tmp) + 1L]])
)
if (!all(vapply(tmp, function(x) is.character(x) && length(x) == 1 && !is.na(x), NA)))
    stop("could not determine variable names")
for (i in seq_along(tmp)) assign(names(tmp)[[i]], tmp[[i]])
rm.list.append(names(tmp))
rm(i, tmp)





.Defunct2 <- function (new, old = as.character(sys.call(sys.parent()))[1L])
{
    .Defunct(msg = c(
        gettextf("'%s' is defunct.\n", old, domain = "R-base"),
        gettextf("Use '%s' instead.\n", new, domain = "R-base"),
        gettextf("See help(\"Defunct\")", domain = "R-base")
    ))
}





.shFILE <- function (original = TRUE, for.msg = FALSE)
.External2(C_shfile, original, for.msg)


evalq(envir = environment(.shFILE) <- new.env(), {
    delayedAssign(thispathofile, shINFO[["FILE"]])
    eval(call("delayedAssign", thispathfile, call(".normalizePath", as.symbol(thispathofile))))
})


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


# this.path(), this.dir(), and here() ----


# `utils::getWindowsHandles` (Windows exclusive) returns a list of
# external pointers containing the windows handles. the thing of
# interest are the names of this list, these should be the names of the
# windows belonging to the current R process.
.this.path.rgui <- function (verbose = FALSE, for.msg = FALSE)
.External2(C_thispathrgui, names(utils::getWindowsHandles(minimized = TRUE)),
    untitled, r.editor, verbose, for.msg)


.this.path.toplevel <- function (verbose = FALSE, original = FALSE, for.msg = FALSE) NULL
body(.this.path.toplevel) <- bquote({
    if (in.shell) {


        value <- shFILE(original, for.msg, default = {
            stop(thisPathNotExistsError(
                ..(this_path_used_in_an_inappropriate_fashion),
                "* R is being run from a shell and argument 'FILE' is missing",
                call = sys.call(sys.nframe())))
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
            .normalizePath(path)
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
            .normalizePath(path)
        }
        else if (for.msg)
            gettext("Untitled", domain = "RGui", trim = FALSE)
        else stop(
            ..(this_path_used_in_an_inappropriate_fashion),
            "* document in VSCode does not exist")
    }


    # running from 'Rgui' on Windows
    else if (gui.rgui) {


        .this.path.rgui(verbose, for.msg)
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





rm(list = rm.list)
