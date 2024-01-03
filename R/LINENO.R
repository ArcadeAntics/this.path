.fixslash <- function (s)
{
    if (.OS_windows) {
        path <- chartr("\\", "/", path)
        if (startsWith(path, "//"))
            substr(path, 1L, 2L) <- "\\\\"
    }
    path
}


.fixbackslash <- function (s)
{
    if (.OS_windows)
        path <- chartr("/", "\\", path)
    path
}


.LINENO <- function (path, to = 1L)
{
    for (which in seq.int(to = to, by = -1L, length.out = sys.nframe() - to)) {
        call <- sys.call(which)
        srcref <- attr(call, "srcref", exact = TRUE)
        if (!is.null(srcref)) {
            ## try to get the normalized filename
            success <- tryCatch({
                value <- .External2(.C_src_path, srcref)
                TRUE
            }, error = function(e) FALSE)
            if (success) {
                ## compare the filenames
                if (path == value) {
                    # return(srcref[7L])

                    ## srcref[1L] is better, it respects #line directives
                    return(srcref[1L])
                }
            }
            else {
                ## try to get the original filename
                success <- tryCatch({
                    value <- .External2(.C_src_path, FALSE, TRUE, FALSE, FALSE, srcref)
                    TRUE
                }, error = function(e) FALSE)
                if (success) {
                    if (grepl("^file://", value))
                        value <- .fixslash(.file_uri_path(value))
                    else if (grepl("^(https|http|ftp|ftps)://", value))
                        value <- .normalizeurl(value)
                    else value <- .fixslash(value)
                    if (path == value)
                        return(srcref[1L])
                }
            }
        }
    }
    NA_integer_
}


sys.LINENO <- function ()
{
    success <- tryCatch({
        context.number <- .External2(.C_getframenumber)
        if (is.na(context.number) || context.number == 0L)
            return(NA_integer_)
        path <- .External2(.C_sys_path)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        .LINENO(path, context.number + 1L)
    else NA_integer_
}


env.LINENO <- function (n = 0L, envir = parent.frame(n + 1L), matchThisEnv = getOption("topLevelEnvironment"))
{
    n <- .External2(.C_asIntegerGE0, n)
    envir
    matchThisEnv ## this is unused
    value <- NA_integer_
    if (typeof(envir) == "environment") {
        parents <- sys.parents()
        for (i in seq.int(to = 1L, by = -1L, along.with = parents)) {
            ## if the parent frame matches envir,
            ## look for a srcref on the corresponding call
            if (identical(envir, sys.frame(parents[[i]]))) {
                call <- sys.call(i)
                srcref <- attr(call, "srcref", exact = TRUE)
                if (!is.null(srcref)) {
                    value <- srcref[1L]
                    break
                }
            }
        }
    }
    value
}


src.LINENO <- function (n = 0L, srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    srcfile
    tryCatch({
        .External2(.C_src_LINENO, srcfile)
    }, error = function(e) NA_integer_)
}


LINENO <- function (n = 0L, envir = parent.frame(n + 1L), matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    srcfile
    value <- tryCatch({
        .External2(.C_src_LINENO, srcfile)
    }, error = function(e) NA_integer_)
    if (is.na(value)) {
        envir
        matchThisEnv
        value <- NA_integer_
        if (typeof(envir) == "environment") {
            parents <- sys.parents()
            for (i in seq.int(to = 1L, by = -1L, along.with = parents)) {
                if (identical(envir, sys.frame(parents[[i]]))) {
                    call <- sys.call(i)
                    srcref <- attr(call, "srcref", exact = TRUE)
                    if (!is.null(srcref)) {
                        value <- srcref[1L]
                        break
                    }
                }
            }
        }
        if (is.na(value)) {
            success <- tryCatch({
                context.number <- .External2(.C_getframenumber)
                if (is.na(context.number) || context.number == 0L)
                    return(NA_integer_)
                path <- .External2(.C_sys_path)
                TRUE
            }, error = function(e) FALSE)
            if (success)
                .LINENO(path, context.number + 1L)
            else NA_integer_
        }
        else value
    }
    else value
}


LINE <- function ()
{
    srcfile <- 0L
    value <- tryCatch({
        .External2(.C_src_LINENO, srcfile)
    }, error = function(e) NA_integer_)
    if (is.na(value)) {
        envir <- parent.frame()
        value <- NA_integer_
        parents <- sys.parents()
        for (i in seq.int(to = 1L, by = -1L, along.with = parents)) {
            if (identical(envir, sys.frame(parents[[i]]))) {
                call <- sys.call(i)
                srcref <- attr(call, "srcref", exact = TRUE)
                if (!is.null(srcref)) {
                    value <- srcref[1L]
                    break
                }
            }
        }
        if (is.na(value)) {
            success <- tryCatch({
                context.number <- .External2(.C_getframenumber)
                if (is.na(context.number) || context.number == 0L)
                    return(NA_integer_)
                path <- .External2(.C_sys_path)
                TRUE
            }, error = function(e) FALSE)
            if (success)
                .LINENO(path, context.number + 1L)
            else NA_integer_
        }
        else value
    }
    else value
}


.source <- function (file, local = FALSE, echo = verbose, print.eval = echo,
    exprs, spaced = use_file, verbose = getOption("verbose"),
    prompt.echo = getOption("prompt"), max.deparse.length = 150,
    width.cutoff = 60L, deparseCtrl = "showAttributes", chdir = FALSE,
    encoding = getOption("encoding"), continue.echo = getOption("continue"),
    skip.echo = 0, keep.source = getOption("keep.source"))
{
    file <- set.sys.path(file)
    envir <- if (isTRUE(local))
        parent.frame()
    else if (isFALSE(local))
        .GlobalEnv
    else if (is.environment(local))
        local
    else stop("'local' must be TRUE, FALSE or an environment")
    if (!missing(echo)) {
        if (!is.logical(echo))
            stop("'echo' must be logical")
        if (!echo && verbose) {
            warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'")
            echo <- TRUE
        }
    }
    if (verbose) {
        cat("'envir' chosen:")
        print(envir)
    }
    if (use_file <- missing(exprs)) {
        ofile <- file
        from_file <- FALSE
        srcfile <- NULL
        if (is.character(file)) {
            if (!length(file) || file == "")
                stop("empty file/url name")
            have_encoding <- !missing(encoding) && !identical(encoding, "unknown")
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
            if (verbose)
                cat(gettextf("encoding = \"%s\" chosen", encoding),
                  "\n", sep = "")
            {
                filename <- file
                file <- file(filename, "r", encoding = encoding)
                on.exit(close(file))
                if (isTRUE(keep.source)) {
                  lines <- readLines(file, warn = FALSE)
                  on.exit()
                  close(file)
                  srcfile <- srcfilecopy(filename, lines, file.mtime(filename)[1],
                    isFile = .is_abs_path(filename))
                }
                else {
                  from_file <- TRUE
                  srcfile <- filename
                }
                loc <- utils::localeToCharset()[1L]
                encoding <- if (have_encoding)
                  switch(loc, `UTF-8` = "UTF-8", `ISO8859-1` = "latin1",
                    "unknown")
                else "unknown"
            }
        }
        else {
            lines <- readLines(file, warn = FALSE)
            srcfile <- if (isTRUE(keep.source))
                srcfilecopy(deparse1(substitute(file), ""), lines)
            else deparse1(substitute(file), "")
        }
        if (verbose) {
            cat(sprintf(" --> from_file='%s'\n lines:", from_file))
            utils::str(lines)
        }
        exprs <- if (!from_file) {
            if (length(lines) && is.character(lines))
                parse(file = stdin(), n = -1, text = lines, keep.source = FALSE, srcfile = srcfile, encoding = encoding)
            else expression()
        }
        else parse(file = file, n = -1, keep.source = FALSE, srcfile = srcfile, encoding = encoding)
        on.exit()
        if (from_file)
            close(file)
        if (verbose)
            cat("--> parsed", length(exprs), "expressions; now eval(.)ing them:\n")
        if (chdir) {
            if (is.character(ofile)) {
                if (grepl("^(ftp|ftps|http|https|file)://", ofile))
                  warning("'chdir = TRUE' makes no sense for a URL")
                else if ((path <- dirname(ofile)) != ".") {
                  owd <- getwd()
                  if (is.null(owd))
                    stop("cannot 'chdir' as current directory is unknown")
                  on.exit(setwd(owd), add = TRUE)
                  setwd(path)
                }
            }
            else {
                warning("'chdir = TRUE' makes no sense for a connection")
            }
        }
    }
    else {
        if (!missing(file))
            stop("specify either 'file' or 'exprs' but not both")
        if (!is.expression(exprs))
            exprs <- as.expression(exprs)
    }
    Ne <- length(exprs)
    if (echo) {
        sd <- "\""
        nos <- "[^\"]*"
        oddsd <- paste0("^", nos, sd, "(", nos, sd, nos, sd,
            ")*", nos, "$")
        trySrcLines <- function(srcfile, showfrom, showto) {
            tryCatch(suppressWarnings(getSrcLines(srcfile, showfrom,
                showto)), error = function(e) character())
        }
    }
    yy <- NULL
    lastshown <- 0
    srcrefs <- attr(exprs, "srcref", exact = TRUE)
    if (verbose && !is.null(srcrefs)) {
        cat("has srcrefs:\n")
        utils::str(srcrefs)
    }
    for (i in seq_len(Ne + echo)) {
        tail <- i > Ne
        if (!tail) {
            if (verbose)
                cat("\n>>>> eval(expression_nr.", i, ")\n\t\t =================\n")
            ei <- exprs[i]
        }
        if (echo) {
            nd <- 0
            srcref <- if (tail)
                attr(exprs, "wholeSrcref", exact = TRUE)
            else if (i <= length(srcrefs))
                srcrefs[[i]]
            if (!is.null(srcref)) {
                if (i == 1)
                  lastshown <- min(skip.echo, srcref[8L] - 1)
                if (lastshown < srcref[8L]) {
                  srcfile <- attr(srcref, "srcfile", exact = TRUE)
                  dep <- trySrcLines(srcfile, lastshown + 1,
                    srcref[8L])
                  if (length(dep)) {
                    leading <- if (tail)
                      length(dep)
                    else srcref[7L] - lastshown
                    lastshown <- srcref[8L]
                    while (length(dep) && grepl("^[[:blank:]]*$",
                      dep[1L])) {
                      dep <- dep[-1L]
                      leading <- leading - 1L
                    }
                    dep <- paste0(rep.int(c(prompt.echo, continue.echo),
                      pmax(0L, c(leading, length(dep) - leading))),
                      dep, collapse = "\n")
                    nd <- nchar(dep, "c")
                  }
                  else srcref <- NULL
                }
            }
            if (is.null(srcref)) {
                if (!tail) {
                  dep <- substr(paste(deparse(ei, width.cutoff = width.cutoff,
                    control = deparseCtrl), collapse = "\n"),
                    12L, 1000000L)
                  dep <- paste0(prompt.echo, gsub("\n", paste0("\n",
                    continue.echo), dep, fixed = TRUE))
                  nd <- nchar(dep, "c") - 1L
                }
            }
            if (nd) {
                do.trunc <- nd > max.deparse.length
                dep <- substr(dep, 1L, if (do.trunc)
                  max.deparse.length
                else nd)
                cat(if (spaced)
                  "\n", dep, if (do.trunc)
                  paste(if (grepl(sd, dep) && grepl(oddsd, dep))
                    " ...\" ..."
                  else " ....", "[TRUNCATED] "), "\n", sep = "")
            }
        }
        if (!tail) {
            yy <- withVisible(eval(ei, envir))
            i.symbol <- mode(ei[[1L]]) == "name"
            if (!i.symbol) {
                curr.fun <- ei[[1L]][[1L]]
                if (verbose) {
                  cat("curr.fun:")
                  utils::str(curr.fun)
                }
            }
            if (verbose >= 2) {
                cat(".... mode(ei[[1L]])=", mode(ei[[1L]]), "; paste(curr.fun)=")
                utils::str(paste(curr.fun))
            }
            if (print.eval && yy$visible)
                .PrintValueEnv(yy$value, envir)
            if (verbose)
                cat(" .. after ", sQuote(deparse(ei, control = unique(c(deparseCtrl,
                  "useSource")))), "\n", sep = "")
        }
    }
    invisible(yy)
}


source.exprs <- function (exprs, evaluated = FALSE, envir = parent.frame(), echo = TRUE, print.eval = TRUE)
{
    if (!evaluated) {
        exprs <- substitute(exprs)
        if (is.call(exprs)) {
            if (identical(exprs[[1]], .R_BraceSymbol))
                exprs <- as.list(exprs)[-1]
        }
    }
    if (!is.expression(exprs))
        exprs <- as.expression(exprs)
    Ne <- length(exprs)
    if (echo)
        trySrcLines <- function(srcfile, from, to) {
            tryCatch(suppressWarnings(getSrcLines(srcfile, from, to)),
                error = function(e) character())
        }
    yy <- NULL
    lastshown <- 0L
    srcrefs <- attr(exprs, "srcref")
    for (i in seq_len(Ne + echo)) {
        tail <- i > Ne
        if (!tail)
            ei <- exprs[i]
        if (echo) {
            srcref <- if (tail)
                attr(exprs, "wholeSrcref")
            else if (i <= length(srcrefs))
                srcrefs[[i]]
            if (!is.null(srcref)) {
                firstl <- srcref[7L]
                lastl <- srcref[8L]
                if (i == 1L)
                    lastshown <- firstl - 1L
                if (lastshown < lastl) {
                    srcfile <- attr(srcref, "srcfile")
                    dep <- trySrcLines(srcfile, lastshown + 1L, lastl)
                    if (length(dep)) {
                        leading <- if (tail)
                            length(dep)
                        else firstl - lastshown
                        lastshown <- lastl
                        dep <- paste0(rep.int(c(getOption("prompt"), getOption("continue")),
                            pmax(0L, c(         leading            , length(dep) - leading))),
                            dep, collapse = "\n")
                        cat(dep, "\n", sep = "")
                    }
                }
            }
        }
        if (!tail) {
            yy <- withVisible(eval(ei, envir))
            if (print.eval && yy$visible)
                .PrintValueEnv(yy$value, envir)
        }
    }
    invisible(yy)
}
