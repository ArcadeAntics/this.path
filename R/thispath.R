## helper functions for this.path()    ----


.get_named_element <- function (x, names)
{
    for (name in names) {
        if (i <- match(name, names(x), 0L, c("", NA_character_)))
            x <- x[[i]]
        else return()
    }
    x
}


.get_contents <- function (file, encoding = getOption("encoding"))
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


.get_jupyter_notebook_contents <- function (..., do.unlist = FALSE, give.f = TRUE)
{
    lines <- .get_contents(...)
    source <- jsonlite::parse_json(lines, simplifyVector = TRUE)
    source <- .get_named_element(source, c("cells", "source"))
    if (do.unlist) {
        value <- unlist(source)
        if (give.f)
            attr(value, "f") <- as.factor(rep(seq_along(source), lengths(source)))
        value
    }
    else source
}


.get_jupyter_R_notebook_contents <- function (path)
{
    ## similar to .get_jupyter_notebook_contents(), but does some error
    ## handling and checks that the metadata is valid for a Jupyter R Notebook
    path
    contents <- tryCatch(.get_contents(path), error = identity)
    if (!inherits(contents, "error")) {
        contents <- tryCatch(jsonlite::parse_json(contents, simplifyVector = TRUE),
            error = identity)
        if (!inherits(contents, "error")) {
            language <- .get_named_element(contents, c("metadata", "kernelspec", "language"))
            name     <- .get_named_element(contents, c("metadata", "language_info", "name"))
            version  <- .get_named_element(contents, c("metadata", "language_info", "version"))
            source   <- .get_named_element(contents, c("cells", "source"))
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


.fixNewlines <- function (srcfile)
{
    srcfile$lines <- .External2(.C_fixNewlines, srcfile$lines)
    srcfile$fixedNewlines <- TRUE
    srcfile$lines
}


## path of active file in GUI          ----


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


.Rgui_path <- function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
.External2(.C_Rgui_path, verbose, original, for.msg, contents, .untitled, .r_editor)


.vscode_path <- evalq(envir = new.env(), {
    ## https://github.com/REditorSupport/vscode-R/blob/master/R/session/vsc.R#L1
    delayedAssign("pid", { Sys.getpid() })
    delayedAssign("wd", { getwd() })
    delayedAssign("homedir", { Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME") })
    delayedAssign("dir_watcher", { Sys.getenv("VSCODE_WATCHER_DIR", file.path(homedir, ".vscode-R")) })
    delayedAssign("request_file", { file.path(dir_watcher, "request.log") })
    delayedAssign("request_lock_file", { file.path(dir_watcher, "request.lock") })
    ## https://github.com/REditorSupport/vscode-R/blob/master/R/session/vsc.R#L357
    delayedAssign("dir_session", {
        local({
            dir_session <- tempfile("vscode-R")
            dir.create(dir_session, showWarnings = FALSE, recursive = TRUE)
            dir_session
        })
    })
    ## https://github.com/REditorSupport/vscode-R/blob/master/R/session/vsc.R#L812
    response_timeout <- 5
    delayedAssign("response_lock_file", {
        local({
            ## force the other promise first
            response_file
            response_lock_file <- file.path(dir_session, "response.lock")
            writeLines("", response_lock_file)
            response_lock_file
        })
    })
    delayedAssign("response_file", {
        local({
            response_file <- file.path(dir_session, "response.log")
            file.create(response_file, showWarnings = FALSE)
            response_file
        })
    })
                function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
{
    ## read the previous request time stamp before making a new request
    response_time_stamp <- readLines(response_lock_file)
    ## https://github.com/REditorSupport/vscode-R/blob/master/R/session/vsc.R#L204
    obj <- list(time = Sys.time(), pid = pid, wd = wd, command = "rstudioapi",
        action = "active_editor_context", args = list(), sd = dir_session)
    jsonlite::write_json(obj, request_file,
        auto_unbox = TRUE, null = "null", force = TRUE)
    cat(sprintf("%.6f", Sys.time()), file = request_lock_file)
    ## https://github.com/REditorSupport/vscode-R/blob/master/R/session/vsc.R#L838
    wait_start <- Sys.time()
    while (isTRUE(response_time_stamp == readLines(response_lock_file))) {
        if ((Sys.time() - wait_start) > response_timeout) {
            stop(.ThisPathNotFoundError(
                "Did not receive a response from VSCode-R API within ",
                response_timeout, " seconds."
            ))
        }
        Sys.sleep(0.1)
    }
    context <- jsonlite::read_json(response_file)
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
        list(.External2(.C_splitlines, context[["contents"]]))
    }
    else if (context[["id"]][["scheme"]] == "untitled") {
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
})


.is_jupyter_loaded <- function ()
# .GUI_jupyter && isNamespaceLoaded("IRkernel") && .identical(sys.function(1L), IRkernel::main)
.GUI_jupyter && !is.null(ns <- .getNamespace("IRkernel")) && .identical(sys.function(1L), getExportedValue(ns, "main"))


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


    if (!.is_jupyter_loaded()) {
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
        for (lines in .get_jupyter_R_notebook_contents(file)) {
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


set.jupyter.path <- function (...)
{
    if (!.GUI_jupyter)
        stop(gettextf("'%s' can only be called in Jupyter",
            "set.jupyter.path"))
    if (!.is_jupyter_loaded())
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
            list(.External2(.C_splitlines, x))
        else if (.isfalse(original))
            .normalizePath(x)
        else x
    }
    else if (.scalar_streql(rval, "success-source")) {
        x <- readChar(file, nchars = file.size(file), useBytes = TRUE)
        if (verbose) cat("Source: source document in Emacs\n")
        if (contents)
            list(.External2(.C_splitlines, x))
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


.site_file_path <- function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
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


.init_file_path <- function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
{
    if (contents && .startup_info[["has_init_file"]])
        for.msg <- FALSE
    value <- init.file(original, for.msg, default = {
        stop(.ThisPathNotExistsError(
            "user profile was not found",
            call = sys.call()))
    })
    if (verbose) cat("Source: user profile\n")
    value
}


.gui_path <- function (verbose = FALSE, original = FALSE, for.msg = FALSE, contents = FALSE)
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
        .vscode_path(verbose, original, for.msg, contents)
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


set.gui.path <- function (...)
.External2(.C_set_gui_path)


## this.path(), this.dir(), and here() ----


## check that class name is C files matches class name in R files
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


## part of _sys_path in ./src/thispath.c
.getframenumber <- function ()
.External2(.C_getframenumber)


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
