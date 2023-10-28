.defunctError <- function (new, package = NULL, msg, old = as.character(sys.call(sys.parent()))[1L])
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





.shFILE <- evalq(envir = new.env(), {
    delayedAssign("ofile", if (.in.shell) .shINFO[["FILE"]] else NA_character_)
    delayedAssign("file", .normalizePath(ofile))
function (original = TRUE, for.msg = FALSE)
.External2(.C_shFILE, original, for.msg)
})


delayedAssign(".has.shFILE", !is.na(.shFILE()))


shFILE <- function (original = FALSE, for.msg = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.))
            .shFILE(original, for.msg)
        else stop("'shFILE' with 'else.' but not 'default' makes no sense")
    }
    else {
        if (missing(else.)) {
            if (.has.shFILE)
                .shFILE(original, for.msg)
            else if (for.msg)
                NA_character_
            else default
        }
        else {
            if (.has.shFILE) {
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


normalized.shFILE <- eval(call("function", as.pairlist(alist(... = )), bquote(
stop(.defunctError("shFILE", .(.pkgname), old = "normalized.shFILE"))
)))





.is.abs.path <- function (path)
.External2(.C_is.abs.path, path)





readLines2 <- function (path, default = character(0))
{
    conn <- tryCatch({
        file(path, "rb", encoding = "")
    }, error = function(e) NULL)
    if (is.null(conn))
        ## we do not throw a warning here because one should
        ## have already been thrown, something like:
        ##
        ## Warning message:
        ## In file(path, "rb", encoding = "") :
        ## cannot open file '%s': No such file or directory
        return(default)
    on.exit(close(conn))
    x <- readLines(conn)
    if (!length(x) || length(x) %% 2L)
        stop(sprintf("invalid '%s' contents", path))
    encoding <- x[c(FALSE, TRUE)]
    x        <- x[c(TRUE, FALSE)]
    Encoding(x) <- encoding
    x
}


.r.editor_msvcrt <- readLines2("./inst/extdata/r-editor_msvcrt.txt", " - R Editor")
.r.editor_ucrt   <- readLines2("./inst/extdata/r-editor_ucrt.txt"  , " - R Editor")


.untitled_msvcrt <- readLines2("./inst/extdata/untitled_msvcrt.txt", "Untitled - R Editor")
.untitled_ucrt   <- readLines2("./inst/extdata/untitled_ucrt.txt"  , "Untitled - R Editor")


rm(readLines2)





delayedAssign(".r.editor", {
    if (.gui.rgui) {
        if (.ucrt)
            .r.editor_ucrt
        else .r.editor_msvcrt
    }
})
delayedAssign(".untitled", {
    if (.gui.rgui) {
        if (.ucrt)
            .untitled_ucrt
        else .untitled_msvcrt
    }
})





.getCurrentCall <- function (n = 3L)
{
    ## find the call that stop() would have also found
    ##
    ## look down the calling stack, picking the
    ## most recent closure besides `stop`
    ##
    ## this is intended to be used as such:
    ## stop(errorMakingFunction())
    ##
    ## where errorMakingFunction calls .getCurrentCall()


    n <- sys.nframe() - n
    if (n <= 0L)
        return(NULL)
    n <- sys.parents()[[n]]
    if (n <= 0L)
        return(NULL)
    skip.stop <- TRUE
    for (n in seq.int(to = 1L, by = -1L, length.out = n)) {
        if (typeof(fun <- sys.function(n)) == "closure") {
            if (skip.stop && (.identical)(fun, stop)) {
                skip.stop <- FALSE
                next
            }
            return(sys.call(n))
        }
    }
    NULL
}


.thisPathUnrecognizedConnectionClassError <- function (con, call = .getCurrentCall(), call. = TRUE)
.External2(.C_thisPathUnrecognizedConnectionClassError, if (call.) call, con)


.thisPathUnrecognizedMannerError <- function (call = .getCurrentCall(), call. = TRUE)
.External2(.C_thisPathUnrecognizedMannerError, if (call.) call)


.thisPathNotImplementedError <- function (..., call. = TRUE, domain = NULL, call = .getCurrentCall())
.External2(.C_thisPathNotImplementedError, .makeMessage(..., domain = domain), call = if (call.) call)


.thisPathNotExistsError <- function (..., call. = TRUE, domain = NULL, call = .getCurrentCall())
.External2(.C_thisPathNotExistsError, .makeMessage(..., domain = domain), call = if (call.) call)


delayedAssign("thisPathNotExistsError", .thisPathNotExistsError)


.thisPathInZipFileError <- function (description, call = .getCurrentCall(), call. = TRUE)
.External2(.C_thisPathInZipFileError, if (call.) call, description)


.thisPathInAQUAError <- function (call = .getCurrentCall(), call. = TRUE)
.External2(.C_thisPathInAQUAError, if (call.) call)


## helper functions for sys.path()     ----


.is.clipboard <- function (file)
.External2(.C_is.clipboard, file)


.getContents <- function (file, encoding = getOption("encoding"))
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


.getJupyterNotebookContents <- function (..., do.unlist = FALSE, give.f = TRUE)
{
    lines <- .getContents(...)
    source <- jsonlite::parse_json(lines, simplifyVector = TRUE)
    source <- .getNamedElement(source, c("cells", "source"))
    if (do.unlist) {
        value <- unlist(source)
        if (give.f)
            attr(value, "f") <- as.factor(rep(seq_along(source), lengths(source)))
        value
    }
    else source
}


.removeSource <- function (fn)
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


.getNamedElement <- function (x, names)
{
    for (name in names) {
        if (i <- match(name, names(x), 0L, c("", NA_character_)))
            x <- x[[i]]
        else return()
    }
    x
}


## this.path(), this.dir(), and here() ----


.fixNewlines <- function (srcfile)
{
    srcfile$lines <- .External2(.C_fixNewlines, srcfile$lines)
    srcfile$fixedNewlines <- TRUE
    srcfile$lines
}


.isJupyterLoaded <- function ()
.gui.jupyter && isNamespaceLoaded("IRkernel") && (.identical)(sys.function(1L), IRkernel::main)


.validJupyterRNotebook <- function (path)
{
    contents <- tryCatch(.getContents(path), error = identity)
    if (!inherits(contents, "error")) {
        contents <- tryCatch(jsonlite::parse_json(contents, simplifyVector = TRUE),
            error = identity)
        if (!inherits(contents, "error")) {
            language <- .getNamedElement(contents, c("metadata", "kernelspec", "language"))
            name <- .getNamedElement(contents, c("metadata", "language_info", "name"))
            version <- .getNamedElement(contents, c("metadata", "language_info", "version"))
            source <- .getNamedElement(contents, c("cells", "source"))
            if (.IS_SCALAR_STR(language) && !is.na(language) && language == "R" &&
                .IS_SCALAR_STR(name)     && !is.na(name)     && name     == "R" &&
                .IS_SCALAR_STR(version)  && !is.na(version)  && version  == as.character(getRversion()) &&
                is.list(source) && length(source) && all(vapply(source, is.character, NA, USE.NAMES = FALSE)))
            {
                return(TRUE)
            }
        }
    }
    FALSE
}


.jupyter.path <- evalq(envir = new.env(), {
    delayedAssign("ofile", NA_character_)
    ofile
    delayedAssign("file", .normalizePath(ofile))
eval(call("function", as.pairlist(alist(verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)), bquote(
{
    if (!is.na(ofile))
        return(.External2(.C_jupyter.path, verbose, original, for.msg, contents))


    if (is.null(initwd)) {
        if (for.msg)
            return(NA_character_)
        else stop(.thisPathNotExistsError(
            "R is running from Jupyter but the initial working directory is unknown"))
    }


    if (!.isJupyterLoaded()) {
        if (for.msg)
            return(NA_character_)
        else stop("Jupyter has not finished loading")
    }


    n <- sys.frame(1L)[["kernel"]][["executor"]][["nframe"]] + 2L
    ocall <- sys.call(n)
    call <- .removeSource(ocall)


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
        CONTENTS <- tryCatch(.getContents(file), error = identity)
        if (!inherits(CONTENTS, "error")) {
            CONTENTS <- tryCatch(jsonlite::parse_json(CONTENTS, simplifyVector = TRUE),
                error = identity)
            if (!inherits(CONTENTS, "error")) {


                language <- .getNamedElement(CONTENTS, c("metadata", "kernelspec", "language"))
                name     <- .getNamedElement(CONTENTS, c("metadata", "language_info", "name"))
                version  <- .getNamedElement(CONTENTS, c("metadata", "language_info", "version"))
                source   <- .getNamedElement(CONTENTS, c("cells", "source"))


                # withAutoprint( { language; name; version; source } , spaced = TRUE, verbose = FALSE, width.cutoff = 60L); cat("\n\n\n\n\n")


                if (.IS_SCALAR_STR(language) && !is.na(language) && language == "R" &&
                    .IS_SCALAR_STR(name)     && !is.na(name)     && name     == "R" &&
                    .IS_SCALAR_STR(version)  && !is.na(version)  && version  == as.character(getRversion()) &&
                    is.list(source) && length(source) && all(vapply(source, is.character, NA, USE.NAMES = FALSE)))
                {
                    for (source0 in source) {
                        exprs <- tryCatch(parse(text = source0, n = -1, keep.source = FALSE, srcfile = NULL),
                            error = identity)
                        if (!inherits(exprs, "error")) {
                            for (expr in exprs) {
                                if (identical(expr, call)) {
                                    .External2(.C_set.jupyter.path, file, skipCheck = TRUE)
                                    return(.External2(.C_jupyter.path, verbose, original, for.msg, contents))
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
    else stop(.thisPathNotExistsError(
        sprintf("R is running from Jupyter with initial working directory '%s'\n", encodeString(initwd)),
        " but could not find a file with contents matching:\n",
        {
            t <- attr(ocall, "srcref", exact = TRUE)
            paste(if (is.integer(t)) as.character(t) else deparse(ocall), collapse = "\n")
        }))
}
, splice = TRUE)))
})


.rgui.path <- function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
.External2(.C_rgui.path, verbose, original, for.msg, contents, .untitled, .r.editor)


.gui.path <- function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
{
    if (.in.shell) {
        if (contents && .has.shFILE)
            for.msg <- FALSE
        value <- shFILE(original, for.msg, default = {
            stop(.thisPathNotExistsError(
                "R is running from a shell and argument 'FILE' is missing",
                call = sys.call()))
        })
        if (verbose) cat("Source: shell argument 'FILE'\n")
        value
    }


    ## running from 'RStudio'
    else if (.gui.rstudio) {


        if (!`.init.tools:rstudio`()) {
            if (for.msg) {
                return(NA_character_)
            }
        }


        ## ".rs.api.getActiveDocumentContext" from "tools:rstudio" returns a
        ## list of information about the document where your cursor is located
        ##
        ## ".rs.api.getSourceEditorContext" from "tools:rstudio" returns a list
        ## of information about the document open in the current tab
        ##
        ## element 'id' is a character string, an identification for the document
        ## element 'path' is a character string, the path of the document


        if (verbose) {
            context <- .rs.api.getActiveDocumentContext()
            active <- context[["id"]] != "#console"
            if (!active)
                context <- .rs.api.getSourceEditorContext()
        } else context <- .rs.api.getSourceEditorContext()


        if (is.null(context)) {
            if (for.msg)
                NA_character_
            else stop(.thisPathNotExistsError(
                "R is running from RStudio with no documents open\n",
                " (or source document has no path)"))
        }
        else if (contents) {
            if (verbose)
                cat(
                    if (active)
                        "Source: active document in RStudio\n"
                    else
                        "Source: source document in RStudio\n"
                )
            list(.External2(.C_remove_trailing_blank_string, context[["contents"]]))
        }
        else if (nzchar(path <- context[["path"]])) {
            ## the encoding is not explicitly set (at least on Windows),
            ## so we have to do that ourselves
            Encoding(path) <- "UTF-8"
            if (verbose)
                cat(
                    if (active)
                        "Source: active document in RStudio\n"
                    else
                        "Source: source document in RStudio\n"
                )
            if (.isfalse(original))
                .normalizePath(path)
            else path
        }
        else if (for.msg)
            gettext("Untitled", domain = "RGui", trim = FALSE)
        else stop(
            if (verbose) {
                if (active)
                    "active document in RStudio does not exist"
                else "source document in RStudio does not exist"
            } else "document in RStudio does not exist")
    }


    else if (.gui.vscode) {


        ## evaluate this BEFORE the tryCatch in case the package
        ## is not installed or the object is not exported
        rstudioapi::getSourceEditorContext
        ## now do the tryCatch
        tryCatch3({
            context <- rstudioapi::getSourceEditorContext()
        }, error = {
            stop(simpleError("package:rstudioapi is not set up to work with VSCode; try adding:\n\n```R\noptions(vsc.rstudioapi = TRUE)\n```\n\n  to the site-wide startup profile file or your user profile (see ?Startup),\n  then restart the R session and try again", sys.call()))
        })


        if (is.null(context)) {
            if (for.msg)
                NA_character_
            else stop(.thisPathNotExistsError(
                "R is running from VSCode with no documents open\n",
                " (or document has no path)"
            ))
        }
        else if (contents) {
            if (verbose) cat("Source: document in VSCode\n")
            list(.External2(.C_remove_trailing_blank_string, context[["contents"]]))
        }
        else if (startsWith(context[["id"]], "untitled:")) {
            if (for.msg)
                context[["path"]]
            else stop("document in VSCode does not exist")
        }
        else if (nzchar(path <- context[["path"]])) {
            Encoding(path) <- "UTF-8"
            if (verbose) cat("Source: document in VSCode\n")
            if (.isfalse(original))
                .normalizePath(path)
            else path
        }
        else if (for.msg)
            gettext("Untitled", domain = "RGui", trim = FALSE)
        else stop("document in VSCode does not exist")
    }


    ## running from 'jupyter'
    else if (.gui.jupyter) {
        .jupyter.path(verbose, original, for.msg, contents)
    }


    ## running from 'Rgui' on Windows
    else if (.gui.rgui) {


        .External2(.C_rgui.path, verbose, original, for.msg, contents, .untitled, .r.editor)
    }


    else if (.gui.aqua) {


        if (for.msg)
            NA_character_
        else stop(.thisPathInAQUAError())
    }


    ## running from a shell under Unix-alikes with GUI 'Tk'
    else if (.gui.tk) {


        if (for.msg)
            NA_character_
        else stop(.thisPathNotExistsError(
            "R is running from Tk which does not make use of its -f FILE, --file=FILE arguments"))
    }


    ## running R in another manner
    else {


        if (for.msg)
            NA_character_
        else stop(.thisPathUnrecognizedMannerError())
    }
}


set.jupyter.path <- function (...)
{
    if (!.gui.jupyter)
        stop(gettextf("'%s' can only be called in Jupyter",
            "set.jupyter.path"))
    if (!.isJupyterLoaded())
        stop(gettextf("'%s' can only be called after Jupyter has finished loading",
            "set.jupyter.path"))
    n <- sys.frame(1L)[["kernel"]][["executor"]][["nframe"]] + 2L
    if (sys.nframe() != n)
        stop(gettextf("'%s' can only be called from a top-level context",
            "set.jupyter.path"))
    path <- if (missing(...) || ...length() == 1L && (is.null(..1) || is.atomic(..1) && length(..1) == 1L && is.na(..1)))
        NA_character_
    else if (is.null(initwd))
        path.join(...)
    else path.join(initwd, ...)
    .External2(.C_set.jupyter.path, path)
}


delayedAssign("set.sys.path.jupyter", set.jupyter.path)


set.this.path.jupyter <- eval(call("function", as.pairlist(alist(... = )), bquote(
stop(.defunctError("set.jupyter.path", .(.pkgname), old = "set.this.path.jupyter"))
)))


set.gui.path <- function (...)
.External2(.C_set.gui.path)


delayedAssign(".identical", {
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


.normalizeNotDirectory <- function (path, winslash = "/", mustWork = TRUE)
{
    x <- normalizePath(path, winslash, mustWork)
    if (.isfalse(.isdir(x)))
        x
    else stop(sprintf("'%s' is not a regular file", path), domain = NA)
}


.normalizeFixDirectory <- function (path, winslash = "/", mustWork = TRUE)
{
    x <- normalizePath(path, winslash, mustWork)
    if (.istrue(.isdir(x)))
        path.join(x, ".")
    else x
}


.normalizeAgainst <- function (wd, path)
{
    owd <- getwd()
    if (is.null(owd))
        stop("cannot '.normalizeAgainst' as current directory is unknown")
    on.exit(setwd(owd))
    setwd(wd)
    .normalizePath(path)
}


.dir <- function (path)
{
    if (grepl("^(https|http|ftp|ftps)://", path)) {
        # path <- "https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R"
        p <- path.split.1(path)
        path.unsplit(if (length(p) >= 2L) p[-length(p)] else p)
    }
    else dirname2(path)
}





.faster.subsequent.times.test <- function ()
{
    first.time <- microbenchmark::microbenchmark(
        `first time` = .External2(.C_sys.path),
        times = 1
    )
    subsequent <- microbenchmark::microbenchmark(
        subsequent = .External2(.C_sys.path),
        times = 100
    )
    rbind(first.time, subsequent)
}





sys.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE,
    contents = FALSE, local = FALSE, default, else.)
.External2(.C_sys.path, verbose, original, for.msg, contents, local)


sys.dir <- function (verbose = getOption("verbose"), local = FALSE, default, else.)
{
    value <- .External2(.C_sys.path, verbose, local)
    .dir(value)
}


env.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE,
    contents = FALSE, n = 0L, envir = parent.frame(n + 1L), matchThisEnv = getOption("topLevelEnvironment"),
    default, else.)
.External2(.C_env.path, verbose, original, for.msg, contents, envir, matchThisEnv)


env.dir <- function (verbose = getOption("verbose"), n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"), default, else.)
{
    value <- .External2(.C_env.path, verbose, envir, matchThisEnv)
    .dir(value)
}


src.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE,
    contents = FALSE, n = 0L, srcfile = if (n) sys.parent(n) else 0L, default, else.)
.External2(.C_src.path, verbose, original, for.msg, contents, srcfile)


src.dir <- function (verbose = getOption("verbose"), n = 0L, srcfile = if (n) sys.parent(n) else 0L,
    default, else.)
{
    value <- .External2(.C_src.path, verbose, srcfile)
    .dir(value)
}


this.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE,
    contents = FALSE, local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"), srcfile = if (n) sys.parent(n) else 0L,
    default, else.)
.External2(.C_this.path, verbose, original, for.msg, contents, local, envir, matchThisEnv, srcfile)


this.dir <- function (verbose = getOption("verbose"), local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"), srcfile = if (n) sys.parent(n) else 0L,
    default, else.)
{
    value <- .External2(.C_this.path, verbose, local, envir, matchThisEnv, srcfile)
    .dir(value)
}


tmp <- readLines("./src/thispathdefn.h")
tmp <- tmp[[grep("^[ \t]*#[ \t]*define[ \t]+thisPathNotExistsErrorCls[ \t]*\\\\[ \t]*$", tmp) + 1L]]
tmp <- str2lang(tmp)
if (!is.character(tmp) || length(tmp) != 1L || is.na(tmp))
    stop("could not determine class name")
tmpfun <- function (fun)
{
    R_BraceSymbol <- as.symbol("{")
    e <- body(fun)
    e2 <- e
    if (e2[[1L]] != R_BraceSymbol)
        e2 <- call("{", e2)
    e2 <- bquote(tryCatch(.(e2), function(e) default))
    names(e2)[3L] <- tmp
    e3 <- e
    if (e3[[1L]] != R_BraceSymbol)
        e3 <- call("{", e3)
    e3 <- bquote(tryCatch2(value <- .(e3), function(e) default, else. = (else.)(value)))
    names(e3)[3L] <- tmp
    body(fun) <- bquote({
        ..(if ("n" %in% names(formals(fun))) expression(n <- .External2(.C_asIntegerGE0, n)) else list())
        if (missing(default)) {
            if (missing(else.))
                .(e)
            else stop(.(sprintf("'%s' with 'else.' but not 'default' makes no sense", as.character(substitute(fun)))))
        }
        else {
            ## force the promises
            ..(lapply(setdiff(names(formals(fun)), c("n", "default", "else.")), as.symbol))
            if (missing(else.))
                .(e2)
            else {
                .(e3)
            }
        }
    }, splice = TRUE)
    fun
}
sys.path  <- tmpfun(sys.path )
sys.dir   <- tmpfun(sys.dir  )
env.path  <- tmpfun(env.path )
env.dir   <- tmpfun(env.dir  )
src.path  <- tmpfun(src.path )
src.dir   <- tmpfun(src.dir  )
this.path <- tmpfun(this.path)
this.dir  <- tmpfun(this.dir )
rm(tmpfun, tmp)


sys.srcref <- function (n = 1L, which = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    .External2(.C_sys.srcref, which)
}





this.path2 <- eval(call("function", as.pairlist(alist(... = )), bquote(
stop(.defunctError("sys.path(..., default = NULL)", .(.pkgname), old = "this.path2(...)"))
)))


this.dir2 <- eval(call("function", as.pairlist(alist(... = )), bquote(
stop(.defunctError("sys.dir(..., default = NULL)", .(.pkgname), old = "this.dir2(...)"))
)))



this.dir3 <- eval(call("function", as.pairlist(alist(... = )), bquote(
stop(.defunctError("sys.dir(..., default = getwd())", .(.pkgname), old = "this.dir3(...)"))
)))





.here <- function (path, .. = 0L)
{
    if (grepl("^(https|http|ftp|ftps)://", path)) {
        # path <- "https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R"
        # .. <- "2"


        p <- path.split.1(path)
        n <- length(p) - length(seq_len(..)) - 1L
        path.unsplit(if (n < 1L) p[1L] else p[seq_len(n)])
    }


    # path <- "//host/share/path/to/file"
    # path <- "C:/Users/iris/Documents/this.path/man/this.path.Rd"
    # .. <- "10"
    else .External2(.C_dirname2, path, ..)
}


sys.here <- function (..., .. = 0L, local = FALSE)
{
    base <- .External2(.C_sys.path, local)
    base <- .here(base, ..)
    path.join(base, ...)
}


env.here <- function (..., .. = 0L, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"))
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_env.path, envir, matchThisEnv)
    base <- .here(base, ..)
    path.join(base, ...)
}


src.here <- function (..., .. = 0L, n = 0L, srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_src.path, srcfile)
    base <- .here(base, ..)
    path.join(base, ...)
}


here <- function (..., .. = 0L, local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_this.path, local, envir, matchThisEnv, srcfile)
    base <- .here(base, ..)
    path.join(base, ...)
}
delayedAssign("ici", here)





Sys.path <- eval(call("function", NULL, bquote(
stop(.defunctError("sys.path(verbose = FALSE)", .(.pkgname), old = "Sys.path()"))
)))


Sys.dir <- eval(call("function", NULL, bquote(
stop(.defunctError("sys.dir(verbose = FALSE)", .(.pkgname), old = "Sys.dir()"))
)))





try.shFILE <- function ()
tryCatch(.shFILE(FALSE), error = function(e) .shFILE())


try.sys.path <- function (contents = FALSE, local = FALSE)
{
    ## force the promises before proceeding
    contents
    local
    success <- tryCatch({
        value <- .External2(.C_sys.path, FALSE, FALSE, FALSE, contents, local)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        value
    else {
        .External2(.C_sys.path, FALSE, FALSE, TRUE, contents, local)
    }
}


try.env.path <- function (contents = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"))
{
    n <- .External2(.C_asIntegerGE0, n)
    contents
    envir
    matchThisEnv
    success <- tryCatch({
        value <- .External2(.C_env.path, FALSE, FALSE, FALSE, contents, envir, matchThisEnv)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        value
    else {
        .External2(.C_env.path, FALSE, FALSE, TRUE, contents, envir, matchThisEnv)
    }
}


try.src.path <- function (contents = FALSE, n = 0L, srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    contents
    srcfile
    success <- tryCatch({
        value <- .External2(.C_src.path, FALSE, FALSE, FALSE, contents, srcfile)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        value
    else {
        .External2(.C_src.path, FALSE, FALSE, TRUE, contents, srcfile)
    }
}


try.this.path <- function (contents = FALSE, local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    contents
    local
    envir
    matchThisEnv
    srcfile
    success <- tryCatch({
        value <- .External2(.C_this.path, FALSE, FALSE, FALSE,
            contents, local, envir, matchThisEnv, srcfile)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        value
    else {
        .External2(.C_this.path, FALSE, FALSE, TRUE, contents,
            local, envir, matchThisEnv, srcfile)
    }
}


FILE <- local({
    tmp <- try.this.path
    f <- formals(tmp)
    formals(tmp) <- NULL
    body(tmp) <- as.call(c(as.list(quote({
        contents <- FALSE
        local <- FALSE
        envir <- parent.frame()
        matchThisEnv <- getOption("topLevelEnvironment")
        srcfile <- 0L
    })), as.list(body(tmp)[-seq_len(length(f) + 1L)])))
    tmp
})





local.path <- eval(call("function", as.pairlist(alist(verbose = getOption("verbose"), original = FALSE, for.msg = FALSE, contents = FALSE, default = , else. = )), bquote(
stop(.defunctError("sys.path(..., local = TRUE)", .(.pkgname), old = "local.path(...)"))
)))
