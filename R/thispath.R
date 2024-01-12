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
    delayedAssign("ofile", { if (.in_shell) .shINFO[["FILE"]] else NA_character_ })
    delayedAssign("file" , { .normalizePath(ofile) })
           function (original = TRUE, for.msg = FALSE)
.External2(.C_shFILE, original, for.msg)
})


delayedAssign(".has_shFILE", { !is.na(.shFILE()) })


shFILE <- function (original = FALSE, for.msg = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.))
            .shFILE(original, for.msg)
        else stop("'shFILE' with 'else.' but not 'default' makes no sense")
    }
    else {
        if (missing(else.)) {
            if (.has_shFILE)
                .shFILE(original, for.msg)
            else if (for.msg)
                NA_character_
            else default
        }
        else {
            if (.has_shFILE) {
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





.is_abs_path <- function (path)
.External2(.C_is_abs_path, path)





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


.r_editor_msvcrt <- readLines2("./inst/extdata/r-editor_msvcrt.txt", " - R Editor")
.r_editor_ucrt   <- readLines2("./inst/extdata/r-editor_ucrt.txt"  , " - R Editor")


.untitled_msvcrt <- readLines2("./inst/extdata/untitled_msvcrt.txt", "Untitled - R Editor")
.untitled_ucrt   <- readLines2("./inst/extdata/untitled_ucrt.txt"  , "Untitled - R Editor")


rm(readLines2)





delayedAssign(".r_editor", {
    if (.GUI_Rgui) {
        if (.ucrt)
            .r_editor_ucrt
        else .r_editor_msvcrt
    }
})
delayedAssign(".untitled", {
    if (.GUI_Rgui) {
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
            if (skip.stop && .identical(fun, stop)) {
                skip.stop <- FALSE
                next
            }
            return(sys.call(n))
        }
    }
    NULL
}


.ThisPathInAQUAError <- function (call = .getCurrentCall(), call. = TRUE)
.External2(.C_ThisPathInAQUAError, if (call.) call)


.ThisPathInZipFileError <- function (description, call = .getCurrentCall(), call. = TRUE)
.External2(.C_ThisPathInZipFileError, if (call.) call, description)


.ThisPathNotExistsError <- function (..., call. = TRUE, domain = NULL, call = .getCurrentCall())
.External2(.C_ThisPathNotExistsError, .makeMessage(..., domain = domain), call = if (call.) call)


delayedAssign("thisPathNotExistsError", { .ThisPathNotExistsError })


.ThisPathNotFoundError <- function (..., call. = TRUE, domain = NULL, call = .getCurrentCall())
.External2(.C_ThisPathNotFoundError, .makeMessage(..., domain = domain), call = if (call.) call)


delayedAssign("thisPathNotFoundError", { .ThisPathNotFoundError })


.ThisPathNotImplementedError <- function (..., call. = TRUE, domain = NULL, call = .getCurrentCall())
.External2(.C_ThisPathNotImplementedError, .makeMessage(..., domain = domain), call = if (call.) call)


.ThisPathUnrecognizedConnectionClassError <- function (con, call = .getCurrentCall(), call. = TRUE)
.External2(.C_ThisPathUnrecognizedConnectionClassError, if (call.) call, con)


.ThisPathUnrecognizedMannerError <- function (call = .getCurrentCall(), call. = TRUE)
.External2(.C_ThisPathUnrecognizedMannerError, if (call.) call)


## helper functions for sys.path()     ----


.is_clipboard <- function (file)
.External2(.C_is_clipboard, file)


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
.GUI_jupyter && isNamespaceLoaded("IRkernel") && .identical(sys.function(1L), IRkernel::main)


.getJupyterRNotebookContents <- function (path)
{
    ## similar to .getJupyterNotebookContents(), but does some error handling
    ## and checks that the metadata is valid for a Jupyter R Notebook
    path
    contents <- tryCatch(.getContents(path), error = identity)
    if (!inherits(contents, "error")) {
        contents <- tryCatch(jsonlite::parse_json(contents, simplifyVector = TRUE),
            error = identity)
        if (!inherits(contents, "error")) {
            language <- .getNamedElement(contents, c("metadata", "kernelspec", "language"))
            name     <- .getNamedElement(contents, c("metadata", "language_info", "name"))
            version  <- .getNamedElement(contents, c("metadata", "language_info", "version"))
            source   <- .getNamedElement(contents, c("cells", "source"))
            # withAutoprint( { language; name; version; source } , spaced = TRUE, verbose = FALSE, width.cutoff = 60L); cat("\n\n\n\n\n")
            if (.scalar_streql(language, "R") &&
                .scalar_streql(name    , "R") &&
                .scalar_streql(version , as.character(getRversion())) &&
                is.list(source) && length(source) && all(vapply(source, is.character, NA, USE.NAMES = FALSE)))
            {
                return(source)
            }
        }
    }
    NULL
}


.jupyter_path <- evalq(envir = new.env(), {
    delayedAssign("ofile", { NA_character_ })
    ofile
    delayedAssign("file" , { .normalizePath(ofile) })
                 function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
{
    if (!is.na(ofile))
        return(.External2(.C_jupyter_path, verbose, original, for.msg, contents))


    if (is.null(initwd)) {
        if (for.msg)
            return(NA_character_)
        else stop(.ThisPathNotFoundError(
            "R is running from Jupyter but the initial working directory is unknown"))
    }


    if (!.isJupyterLoaded()) {
        if (for.msg)
            return(NA_character_)
        else stop(.ThisPathNotExistsError("Jupyter has not finished loading"))
    }


    n <- sys.frame(1L)[["kernel"]][["executor"]][["nframe"]] + 2L
    ocall <- sys.call(n)
    call <- .removeSource(ocall)


    files <- list.files(initwd, all.files = TRUE, full.names = TRUE, no.. = TRUE)
    files <- files[!dir.exists(files)]
    i <- grepl("\\.ipynb$", files, useBytes = TRUE)
    ipynb <- files[i]
    files <- files[!i]
    i <- grepl("\\.ipynb$", files, ignore.case = TRUE, useBytes = TRUE)
    IPYNB <- files[i]
    files <- files[!i]


    for (file in c(ipynb, IPYNB, files)) {
        for (lines in .getJupyterRNotebookContents(file)) {
            exprs <- tryCatch(parse(text = lines, srcfile = NULL, keep.source = FALSE),
                error = identity)
            if (!inherits(exprs, "error")) {
                for (expr in exprs) {
                    if (.identical(expr, call)) {
                        .External2(.C_set_jupyter_path, file, skipCheck = TRUE)
                        return(.External2(.C_jupyter_path, verbose, original, for.msg, contents))
                    }
                }
            }
        }
    }


    if (for.msg)
        NA_character_
    else stop(.ThisPathNotFoundError(
        sprintf("R is running from Jupyter with initial working directory '%s'\n", encodeString(initwd)),
        " but could not find a file with contents matching:\n",
        {
            t <- attr(ocall, "srcref", exact = TRUE)
            paste(if (is.integer(t)) as.character(t) else deparse(ocall), collapse = "\n")
        }))
}
})


.emacs_path <- evalq(envir = new.env(), {
    delayedAssign("emacsclient", {
        local({
            ## https://www.gnu.org/software/emacs/manual/html_node/emacs/Misc-Variables.html#index-emacs_005fdir
            if (.OS_windows) {
                x <- Sys.getenv("emacs_dir")
                if (!nzchar(x)) stop("environment variable 'emacs_dir' is unset; are you actually in Emacs?")
                if (requireNamespace("utils", quietly = TRUE))
                    x <- utils::shortPathName(x)
                paste0(x, "\\bin\\emacsclient.exe")
            } else {
                x <- Sys.which("emacsclient")
                if (!nzchar(x)) stop("command 'emacsclient' not found; add to PATH and retry")
                x
            }
        })
    })
    ## https://www.gnu.org/software/emacs/manual/html_node/elisp/Finding-All-Frames.html
    ## https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-File-Name.html
    expr <- "
        (let ((R-pid    %d)
              (filename %s)
              (contents %s)
              (frames (frame-list-z-order))
              ess-r-mode-buffer
              found-matching-pid-anywhere
              active)
          ;; for testing purposes
          (setq found-matching-pid-anywhere (= R-pid 0))
          (while frames
            (setq ess-r-mode-buffer nil)
            (setq active t)
            (let ((buffers (frame-parameter (car frames) 'buffer-list))
                  (found-matching-pid (= R-pid 0)))
              (while buffers
                (with-current-buffer (car buffers)
                  (if buffer-file-name
                      (if (and (not ess-r-mode-buffer) (or (string= major-mode \"ess-r-mode\") (string= major-mode \"ess-mode\"))) (progn
                          (setq ess-r-mode-buffer (current-buffer))
                          (if found-matching-pid (setq active nil))
                      ))
                  ;; else
                  (if (and (not found-matching-pid) (or (string= major-mode \"inferior-ess-r-mode\") (string= major-mode \"inferior-ess-mode\")))
                      (let ((process (get-buffer-process (current-buffer))))
                        (if (and process (= (process-id process) R-pid)) (progn
                            (setq found-matching-pid t)
                            (setq found-matching-pid-anywhere t)
                        ))
                      )
                  ))
                )
                (if (and ess-r-mode-buffer found-matching-pid) (progn
                    (setq buffers nil)
                    (setq frames nil)
                ))
                (setq buffers (cdr buffers))
              )
            )
            (setq frames (cdr frames))
          )
          (if found-matching-pid-anywhere
              (if ess-r-mode-buffer
                  (with-current-buffer ess-r-mode-buffer
                    (if contents (progn
                        (write-region nil nil filename nil 0)
                        (if active 'success-active 'success-source)
                    ) ;; else
                    (if buffer-file-number (progn
                        (write-region buffer-file-name nil filename nil 0)
                        (if active 'success-active 'success-source)
                    ) ;; else
                        (if active 'untitled-active 'untitled-source)
                    ))
                  )
              )
          ;; else
              'no-matching-pid
          )
        )
    "
    expr <- gsub("^[ \t\n]+|[ \t\n]+$", "", expr)
    expr <- gsub(";.*?\n", "\n", expr)
    expr <- gsub("[ \t\n]+", " ", expr)
    expr <- gsub("( ", "(", expr, fixed = TRUE)
    expr <- gsub(" )", ")", expr, fixed = TRUE)
               function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
{
    # exe <- Sys.which("emacsclient"); contents <- TRUE; Sys.getpid <- function() 0L; stop("comment this out later")


    file <- tempfile(fileext = ".txt")
    on.exit(unlink(file))
    ## create right now so no other program
    ## may claim this filename in the meantime
    file.create(file)
    exe <- .External2(.C_forcePromise_no_warn, "emacsclient")
    EXPR <- sprintf(expr,
        Sys.getpid(),
        encodeString(file, quote = "\""),
        if (contents) "t" else "nil"
    )
    args <- c(exe, "-e", EXPR)
    command <- paste(shQuote(args), collapse = " ")
    # cat(command, sep = "\n")
    rval <- suppressWarnings(system(command, intern = TRUE))
    if (!is.null(status <- attr(rval, "status")) && status) {
        if (status == -1L)
            stop(gettextf("'%s' could not be run", "emacsclient", domain = "R-base"), domain = NA)
        else stop(sprintf("'%s' execution failed with error code %d and message:\n\n", "emacsclient", status),
            paste(rval, collapse = "\n"),
            "\n\n perhaps add (server-start) to your ~/.emacs file and restart the session\n",
            " or type M-x server-start in your current session?", domain = NA)
    }


    if (!.IS_SCALAR_STR(rval))
        stop("command 'emacsclient' failed to return a character string")


    if (.scalar_streql(rval, "success-active")) {
        x <- readChar(file, nchars = file.size(file), useBytes = TRUE)
        if (verbose) cat("Source: active document in Emacs\n")
        if (contents)
            # list(.External2(.C_splitlines, x))
            strsplit(x, "\r\n|[\r\n]", useBytes = TRUE)
        else if (.isfalse(original))
            .normalizePath(x)
        else x
    }
    else if (.scalar_streql(rval, "success-source")) {
        x <- readChar(file, nchars = file.size(file), useBytes = TRUE)
        if (verbose) cat("Source: source document in Emacs\n")
        if (contents)
            # list(.External2(.C_splitlines, x))
            strsplit(x, "\r\n|[\r\n]", useBytes = TRUE)
        else if (.isfalse(original))
            .normalizePath(x)
        else x
    }
    else if (.scalar_streql(rval, "untitled-active")) {
        if (for.msg)
            gettext("Untitled", domain = "RGui", trim = FALSE)
        else stop(.ThisPathNotFoundError("active document in Emacs does not exist"))
    }
    else if (.scalar_streql(rval, "untitled-source")) {
        if (for.msg)
            gettext("Untitled", domain = "RGui", trim = FALSE)
        else stop(.ThisPathNotFoundError("source document in Emacs does not exist"))
    }
    else if (.scalar_streql(rval, "nil")) {
        if (for.msg)
            NA_character_
        else stop(.ThisPathNotFoundError("R is running from Emacs with no documents open"))
    }
    else if (.scalar_streql(rval, "no-matching-pid")) {
        if (for.msg)
            NA_character_
        else stop(.ThisPathNotFoundError(sprintf(
            "R process %d not found in primary Emacs sesion\n this.path() only works in primary Emacs session\n\n if you want to run multiple R sessions in Emacs, do not run multiple\n Emacs sessions, one R session each. instead run one Emacs session with\n multiple frames, one R session each. this gives the same appearance\n while allowing this.path() to work across all R sessions.",
            Sys.getpid()), domain = NA))
    }
    else stop("invalid 'emacsclient' return value")
}
})


.Rgui_path <- function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
.External2(.C_Rgui_path, verbose, original, for.msg, contents, .untitled, .r_editor)


.site_path <- function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
{
    if (contents && .startup_info[["has_site_file"]])
        for.msg <- FALSE
    value <- site.file(original, for.msg, default = {
        stop(.ThisPathNotExistsError(
            "site-wide startup profile file was not found",
            call = sys.call()))
    })
    if (verbose) cat("Source: site-wide startup profile file\n")
    value
}


.gui.path <- function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
{
    if (.in_shell) {
        if (contents && .has_shFILE)
            for.msg <- FALSE
        value <- shFILE(original, for.msg, default = {
            stop(.ThisPathNotExistsError(
                "R is running from a shell and argument 'FILE' is missing",
                call = sys.call()))
        })
        if (verbose) cat("Source: shell argument 'FILE'\n")
        value
    }
    else if (.GUI_RStudio) {


        indx <- match("tools:rstudio", search(), 0L)
        if (!indx) {
            if (for.msg)
                return(NA_character_)
            else stop(.ThisPathNotExistsError("RStudio has not finished loading"))
        }
        tools_rstudio <- as.environment(indx)


        ## ".rs.api.getActiveDocumentContext" from "tools:rstudio" returns a
        ## list of information about the document where your cursor is located
        ##
        ## ".rs.api.getSourceEditorContext" from "tools:rstudio" returns a list
        ## of information about the document open in the current tab
        ##
        ## element 'id' is a character string, an identification for the document
        ## element 'path' is a character string, the path of the document


        if (verbose) {
            context <- get(".rs.api.getActiveDocumentContext", envir = tools_rstudio, inherits = FALSE)()
            active <- context[["id"]] != "#console"
            if (!active)
                context <- get(".rs.api.getSourceEditorContext", envir = tools_rstudio, inherits = FALSE)()
        } else context <- get(".rs.api.getSourceEditorContext", envir = tools_rstudio, inherits = FALSE)()


        if (is.null(context)) {
            if (for.msg)
                NA_character_
            else stop(.ThisPathNotExistsError(
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
            x <- context[["contents"]]
            x <- .External2(.C_remove_trailing_blank_string, x)
            list(x)
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
        else stop(.ThisPathNotFoundError(
            if (verbose) {
                if (active)
                    "active document in RStudio does not exist"
                else "source document in RStudio does not exist"
            } else "document in RStudio does not exist"
        ))
    }
    else if (.GUI_vscode) {


        ## evaluate this BEFORE the tryCatch in case the package
        ## is not installed or the object is not exported
        rstudioapi::getSourceEditorContext
        ## now do the tryCatch
        tryCatch3({
            context <- rstudioapi::getSourceEditorContext()
        }, error = {
            stop(.ThisPathNotFoundError(
                "'package:rstudioapi' has not been set up to work with VSCode; try adding:\n\n",
                "```R\noptions(vsc.rstudioapi = TRUE)\n```\n\n",
                " to the site-wide startup profile file or the user profile (see ?Startup),\n",
                " then restart the R session and try again",
                call = sys.call()))
        })


        if (is.null(context)) {
            if (for.msg)
                NA_character_
            else stop(.ThisPathNotExistsError(
                "R is running from VSCode with no documents open\n",
                " (or document has no path)"
            ))
        }
        else if (contents) {
            if (verbose) cat("Source: document in VSCode\n")
            x <- context[["contents"]]
            x <- .External2(.C_remove_trailing_blank_string, x)
            list(x)
        }
        else if (startsWith(context[["id"]], "untitled:")) {
            if (for.msg)
                context[["path"]]
            else stop(.ThisPathNotFoundError("document in VSCode does not exist"))
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
        else stop(.ThisPathNotFoundError("document in VSCode does not exist"))
    }
    else if (.GUI_jupyter) {
        .jupyter_path(verbose, original, for.msg, contents)
    }
    else if (.GUI_emacs) {
        .emacs_path(verbose, original, for.msg, contents)
    }
    else if (.GUI_powerbi) {
        if (for.msg)
            NA_character_
        else stop(.ThisPathNotFoundError(
            "R is running from Power BI for which 'this.path' is unimplemented\n",
            " you should not need to use this.path() in Power BI since you can import data directly"))
    }
    else if (.in_callr) {
        if (for.msg)
            NA_character_
        else stop(.ThisPathNotFoundError(
            "R is running from 'package:callr' for which 'this.path' is unimplemented\n",
            " 'package:callr' calls a function in a separate session,\n",
            " you should not need the path of the script where it was written"))
    }


    ## running from 'Rgui' on Windows
    else if (.GUI_Rgui) {
        .External2(.C_Rgui_path, verbose, original, for.msg, contents, .untitled, .r_editor)
    }
    else if (.GUI_AQUA) {


        if (for.msg)
            NA_character_
        else stop(.ThisPathInAQUAError())
    }


    ## running from a shell under Unix-alikes with GUI 'Tk'
    else if (.GUI_Tk) {


        if (for.msg)
            NA_character_
        else stop(.ThisPathNotExistsError(
            "R is running from Tk which does not make use of its -f FILE, --file=FILE arguments"))
    }


    ## running R in another manner
    else {


        if (for.msg)
            NA_character_
        else stop(.ThisPathUnrecognizedMannerError())
    }
}


set.jupyter.path <- function (...)
{
    if (!.GUI_jupyter)
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
    .External2(.C_set_jupyter_path, path)
}


set.sys.path.jupyter <- function (...)
stop(.defunctError("set.jupyter.path", "this.path", old = "set.sys.path.jupyter"))


set.this.path.jupyter <- function (...)
stop(.defunctError("set.jupyter.path", "this.path", old = "set.this.path.jupyter"))


set.gui.path <- function (...)
.External2(.C_set_gui_path)


.identical <- if (getRversion() >= "4.2.0") {


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


.normalizeAgainst <- function (wd, path, winslash = "/", mustWork = TRUE)
{
    owd <- getwd()
    if (is.null(owd))
        stop("cannot '.normalizeAgainst' as current directory is unknown")
    on.exit(setwd(owd))
    setwd(wd)
    normalizePath(path, winslash, mustWork)
}


.normalizeNotDirectoryAgainst <- function (wd, path, winslash = "/", mustWork = TRUE)
{
    owd <- getwd()
    if (is.null(owd))
        stop("cannot '.normalizeAgainst' as current directory is unknown")
    on.exit(setwd(owd))
    setwd(wd)
    x <- normalizePath(path, winslash, mustWork)
    if (.isfalse(.isdir(x)))
        x
    else stop(sprintf("'%s' is not a regular file", path), domain = NA)
}


.normalizeFixDirectoryAgainst <- function (wd, path, winslash = "/", mustWork = TRUE)
{
    owd <- getwd()
    if (is.null(owd))
        stop("cannot '.normalizeAgainst' as current directory is unknown")
    on.exit(setwd(owd))
    setwd(wd)
    x <- normalizePath(path, winslash, mustWork)
    if (.istrue(.isdir(x)))
        path.join(x, ".")
    else x
}


.normalize_srcfilealias <- function (original, path, winslash = "/", mustWork = TRUE)
{
    value <- .External2(.C_src_path, original)
    value <- .dir(value)
    if (grepl("^(https|http|ftp|ftps)://", value)) {
        ## do not use file.path(), will convert text to native encoding
        .normalizeurl(paste(value, path, sep = "/"))
    }
    else {
        owd <- getwd()
        if (is.null(owd))
            stop("cannot '.normalize_srcfilealias' as current directory is unknown")
        on.exit(setwd(owd))
        setwd(value)
        normalizePath(path, winslash, mustWork)
    }
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





.faster_subsequent_times_test <- function ()
{
    first_time <- microbenchmark::microbenchmark(
        `first time` = .External2(.C_sys_path),
        times = 1
    )
    subsequent <- microbenchmark::microbenchmark(
        subsequent = .External2(.C_sys_path),
        times = 100
    )
    rbind(first_time, subsequent)
}





local({
    tmp <- readLines("./src/ns-hooks.c")
    tmp <- tmp[[grep("^[ \t]*#[ \t]*define[ \t]+ThisPathNotExistsErrorClass_string[ \t]*\\\\[ \t]*$", tmp) + 1L]]
    tmp <- str2lang(tmp)
    if (!is.character(tmp) || length(tmp) != 1L || is.na(tmp))
        stop("could not determine class name")
    expected <- "ThisPathNotExistsError"
    if (tmp != expected)
        stop(sprintf("ThisPathNotExistsErrorClass found in ./src/ns-hooks.c (%s)\n does not match expected value in ./R/thispath.R (%s)",
            tmp, expected))
})


sys.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE,
    contents = FALSE, local = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.))
            .External2(.C_sys_path, verbose, original, for.msg, contents, local)
        else stop("'sys.path' with 'else.' but not 'default' makes no sense")
    }
    else {
        ## force the promises
        verbose
        original
        for.msg
        contents
        local
        if (missing(else.))
            tryCatch({
                .External2(.C_sys_path, verbose, original, for.msg, contents, local)
            }, ThisPathNotExistsError = function(e) default)
        else {
            tryCatch2({
                value <- .External2(.C_sys_path, verbose, original, for.msg, contents, local)
            }, ThisPathNotExistsError = function(e) default,
                else. = (else.)(value))
        }
    }
}


sys.dir <- function (verbose = getOption("verbose"), local = FALSE, default, else.)
{
    if (missing(default)) {
        if (missing(else.)) {
            value <- .External2(.C_sys_path, verbose, local)
            .dir(value)
        }
        else stop("'sys.dir' with 'else.' but not 'default' makes no sense")
    }
    else {
        ## force the promises
        verbose
        local
        if (missing(else.))
            tryCatch({
                value <- .External2(.C_sys_path, verbose, local)
                .dir(value)
            }, ThisPathNotExistsError = function(e) default)
        else {
            tryCatch2({
                value <- .External2(.C_sys_path, verbose, local)
                value <- .dir(value)
            }, ThisPathNotExistsError = function(e) default,
                else. = (else.)(value))
        }
    }
}


env.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE,
    contents = FALSE, n = 0L, envir = parent.frame(n + 1L), matchThisEnv = getOption("topLevelEnvironment"),
    default, else.)
{
    n <- .External2(.C_asIntegerGE0, n)
    if (missing(default)) {
        if (missing(else.))
            .External2(.C_env_path, verbose, original, for.msg,contents, envir, matchThisEnv)
        else stop("'env.path' with 'else.' but not 'default' makes no sense")
    }
    else {
        ## force the promises
        verbose
        original
        for.msg
        contents
        envir
        matchThisEnv
        if (missing(else.))
            tryCatch({
                .External2(.C_env_path, verbose, original, for.msg, contents, envir, matchThisEnv)
            }, ThisPathNotExistsError = function(e) default)
        else {
            tryCatch2({
                value <- .External2(.C_env_path, verbose, original, for.msg, contents, envir, matchThisEnv)
            }, ThisPathNotExistsError = function(e) default,
                else. = (else.)(value))
        }
    }
}


env.dir <- function (verbose = getOption("verbose"), n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"), default, else.)
{
    n <- .External2(.C_asIntegerGE0, n)
    if (missing(default)) {
        if (missing(else.)) {
            value <- .External2(.C_env_path, verbose, envir, matchThisEnv)
            .dir(value)
        }
        else stop("'env.dir' with 'else.' but not 'default' makes no sense")
    }
    else {
        ## force the promises
        verbose
        envir
        matchThisEnv
        if (missing(else.))
            tryCatch({
                value <- .External2(.C_env_path, verbose, envir, matchThisEnv)
                .dir(value)
            }, ThisPathNotExistsError = function(e) default)
        else {
            tryCatch2({
                value <- .External2(.C_env_path, verbose, envir, matchThisEnv)
                value <- .dir(value)
            }, ThisPathNotExistsError = function(e) default,
                else. = (else.)(value))
        }
    }
}


src.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE,
    contents = FALSE, n = 0L, srcfile = if (n) sys.parent(n) else 0L, default, else.)
{
    n <- .External2(.C_asIntegerGE0, n)
    if (missing(default)) {
        if (missing(else.))
            .External2(.C_src_path, verbose, original, for.msg, contents, srcfile)
        else stop("'src.path' with 'else.' but not 'default' makes no sense")
    }
    else {
        ## force the promises
        verbose
        original
        for.msg
        contents
        srcfile
        if (missing(else.))
            tryCatch({
                .External2(.C_src_path, verbose, original, for.msg, contents, srcfile)
            }, ThisPathNotExistsError = function(e) default)
        else {
            tryCatch2({
                value <- .External2(.C_src_path, verbose, original, for.msg, contents, srcfile)
            }, ThisPathNotExistsError = function(e) default,
                else. = (else.)(value))
        }
    }
}


src.dir <- function (verbose = getOption("verbose"), n = 0L, srcfile = if (n) sys.parent(n) else 0L,
    default, else.)
{
    n <- .External2(.C_asIntegerGE0, n)
    if (missing(default)) {
        if (missing(else.)) {
            value <- .External2(.C_src_path, verbose, srcfile)
            .dir(value)
        }
        else stop("'src.dir' with 'else.' but not 'default' makes no sense")
    }
    else {
        ## force the promises
        verbose
        srcfile
        if (missing(else.))
            tryCatch({
                value <- .External2(.C_src_path, verbose, srcfile)
                .dir(value)
            }, ThisPathNotExistsError = function(e) default)
        else {
            tryCatch2({
                value <- .External2(.C_src_path, verbose, srcfile)
                value <- .dir(value)
            }, ThisPathNotExistsError = function(e) default,
                else. = (else.)(value))
        }
    }
}


this.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE,
    contents = FALSE, local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"), srcfile = if (n) sys.parent(n) else 0L,
    default, else.)
{
    n <- .External2(.C_asIntegerGE0, n)
    if (missing(default)) {
        if (missing(else.))
            .External2(.C_this_path, verbose, original, for.msg, contents, local, envir, matchThisEnv, srcfile)
        else stop("'this.path' with 'else.' but not 'default' makes no sense")
    }
    else {
        ## force the promises
        verbose
        original
        for.msg
        contents
        local
        envir
        matchThisEnv
        srcfile
        if (missing(else.))
            tryCatch({
                .External2(.C_this_path, verbose, original, for.msg, contents, local, envir, matchThisEnv, srcfile)
            }, ThisPathNotExistsError = function(e) default)
        else {
            tryCatch2({
                value <- .External2(.C_this_path, verbose, original, for.msg, contents, local, envir, matchThisEnv, srcfile)
            }, ThisPathNotExistsError = function(e) default,
                else. = (else.)(value))
        }
    }
}


this.dir <- function (verbose = getOption("verbose"), local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"), srcfile = if (n) sys.parent(n) else 0L,
    default, else.)
{
    n <- .External2(.C_asIntegerGE0, n)
    if (missing(default)) {
        if (missing(else.)) {
            value <- .External2(.C_this_path, verbose, local, envir, matchThisEnv, srcfile)
            .dir(value)
        }
        else stop("'this.dir' with 'else.' but not 'default' makes no sense")
    }
    else {
        ## force the promises
        verbose
        local
        envir
        matchThisEnv
        srcfile
        if (missing(else.))
            tryCatch({
                value <- .External2(.C_this_path, verbose, local, envir, matchThisEnv, srcfile)
                .dir(value)
            }, ThisPathNotExistsError = function(e) default)
        else {
            tryCatch2(value <- {
                value <- .External2(.C_this_path, verbose, local, envir, matchThisEnv, srcfile)
                .dir(value)
            }, ThisPathNotExistsError = function(e) default,
                else. = (else.)(value))
        }
    }
}


sys.srcref <- function (n = 1L, which = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    .External2(.C_sys.srcref, which)
}





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


sys.here <- function (..., local = FALSE, .. = 0L)
{
    base <- .External2(.C_sys_path, local)
    base <- .here(base, ..)
    path.join(base, ...)
}


env.here <- function (..., n = 0L, envir = parent.frame(n + 1L), matchThisEnv = getOption("topLevelEnvironment"),
    .. = 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_env_path, envir, matchThisEnv)
    base <- .here(base, ..)
    path.join(base, ...)
}


src.here <- function (..., n = 0L, srcfile = if (n) sys.parent(n) else 0L,
    .. = 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_src_path, srcfile)
    base <- .here(base, ..)
    path.join(base, ...)
}


here <- function (..., local = FALSE, n = 0L, envir = parent.frame(n + 1L),
    matchThisEnv = getOption("topLevelEnvironment"), srcfile = if (n) sys.parent(n) else 0L,
    .. = 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    base <- .External2(.C_this_path, local, envir, matchThisEnv, srcfile)
    base <- .here(base, ..)
    path.join(base, ...)
}
delayedAssign("ici", { here })





Sys.path <- function ()
stop(.defunctError("sys.path(verbose = FALSE)", "this.path", old = "Sys.path()"))


Sys.dir <- function ()
stop(.defunctError("sys.dir(verbose = FALSE)", "this.path", old = "Sys.dir()"))





try.shFILE <- function ()
tryCatch(.shFILE(FALSE), error = function(e) .shFILE())


try.sys.path <- function (contents = FALSE, local = FALSE)
{
    ## force the promises before proceeding
    contents
    local
    success <- tryCatch({
        value <- .External2(.C_sys_path, FALSE, FALSE, FALSE, contents, local)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        value
    else {
        .External2(.C_sys_path, FALSE, FALSE, TRUE, contents, local)
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
        value <- .External2(.C_env_path, FALSE, FALSE, FALSE, contents, envir, matchThisEnv)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        value
    else {
        .External2(.C_env_path, FALSE, FALSE, TRUE, contents, envir, matchThisEnv)
    }
}


try.src.path <- function (contents = FALSE, n = 0L, srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    contents
    srcfile
    success <- tryCatch({
        value <- .External2(.C_src_path, FALSE, FALSE, FALSE, contents, srcfile)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        value
    else {
        .External2(.C_src_path, FALSE, FALSE, TRUE, contents, srcfile)
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
        value <- .External2(.C_this_path, FALSE, FALSE, FALSE,
            contents, local, envir, matchThisEnv, srcfile)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        value
    else {
        .External2(.C_this_path, FALSE, FALSE, TRUE, contents,
            local, envir, matchThisEnv, srcfile)
    }
}


FILE <- function ()
{
    contents <- FALSE
    local <- FALSE
    envir <- parent.frame()
    matchThisEnv <- getOption("topLevelEnvironment")
    srcfile <- 0L
    success <- tryCatch({
        value <- .External2(.C_this_path, FALSE, FALSE, FALSE, contents, local, envir, matchThisEnv, srcfile)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        value
    else {
        .External2(.C_this_path, FALSE, FALSE, TRUE, contents, local, envir, matchThisEnv, srcfile)
    }
}





local.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE,
    contents = FALSE, default, else.)
stop(.defunctError("sys.path(..., local = TRUE)", "this.path", old = "local.path(...)"))
