.try.normalizePath <- function (path, wd)
{
    if (!missing(wd) && !is.null(wd)) {
        oldwd <- setwd(wd)
        on.exit(setwd(oldwd))
    }
    tryCatch({
        normalizePath(path, "/")
    }, warning = function(w) {
        if (.os.windows)
            chartr("\\", "/", path)
        else path
    })
}


.LINENO <- function (path, to = 1L)
{
    for (which in (sys.nframe() - 1L):to) {
        call <- sys.call(which)
        srcref <- attr(call, "srcref", exact = TRUE)
        if (!is.null(srcref)) {
            srcfile <- attr(srcref, "srcfile", exact = TRUE)
            filename <- .try.normalizePath(srcfile$filename, srcfile$wd)
            if (path == filename) {
                # return(srcref[7L])

                ## srcref[1L] is better, it respects #line directives
                return(srcref[1L])
            }
        }
    }
    NA_integer_
}


sys.LINENO <- function ()
{
    success <- tryCatch({
        context.number <- .External2(.C_getframenumber)
        if (context.number == 0L)
            return(NA_integer_)
        path <- .External2(.C_syspath)
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
    matchThisEnv
    success <- tryCatch({
        path <- .External2(.C_envpath, envir, matchThisEnv)
        TRUE
    }, error = function(e) FALSE)
    if (success)
        .LINENO(path)
    else NA_integer_
}


src.LINENO <- function (n = 0L, srcfile = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    srcfile
    tryCatch({
        .External2(.C_srclineno, srcfile)
    }, error = function(e) NA_integer_)
}


LINENO <- eval(call("function", as.pairlist(alist(n = 0L, envir = parent.frame(n + 1L), matchThisEnv = getOption("topLevelEnvironment"), srcfile = if (n) sys.parent(n) else 0L)), bquote({
    value <- .(body(src.LINENO))
    if (is.na(value)) {
        value <- .(body(env.LINENO)[-2L])
        if (is.na(value))
            .(body(sys.LINENO))
        else value
    }
    else value
})))


LINE <- eval(call("function", NULL, bquote({
    value <- .(
        local({
           tmp <- src.LINENO
           as.call(c(as.list(quote({
               srcfile <- sys.call()
           })), as.list(body(tmp)[-seq_len(length(formals(tmp)) + 1L)])))
        })
    )
    if (is.na(value)) {
        value <- .(
            local({
                tmp <- env.LINENO
                as.call(c(as.list(quote({
                    envir <- parent.frame()
                    matchThisEnv <- getOption("topLevelEnvironment")
                })), as.list(body(tmp)[-seq_len(length(formals(tmp)) + 1L)])))
            })
        )
        if (is.na(value))
            .(body(sys.LINENO))
        else value
    }
    else value
})))


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
            have_encoding <- !missing(encoding) && !identical(encoding,
                "unknown")
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
                    isFile = .is.abs.path(filename))
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
            if (print.eval && yy$visible) {
                if (isS4(yy$value))
                  # methods::show(yy$value)
                  get("show", envir = getNamespace("methods"), inherits = FALSE)(yy$value)
                else print(yy$value)
            }
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
            if (typeof(exprs[[1]]) == "symbol" && exprs[[1]] == "{")
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


.toplevel.source <- function (file)
{
    envir <- .GlobalEnv
    echo <- !isTRUE(.shINFO[["no.echo"]])
    file <- substitute(file)
    ofile <- file
    if (is.character(file)) {
    } else {
        parse_file <- function(file) {
            if (is.call(file)) {
                if (typeof(file[[1L]]) == "symbol" && file[[1L]] == "/") {
                    if (typeof(file[[3L]]) != "symbol")
                        stop("invalid '%s' argument", "file", domain = "R")
                    paste0(parse_file(file[[2L]]), "/", as.character(file[[3L]]))
                }
                else stop("invalid '%s' argument", "file", domain = "R")
            }
            else if (typeof(file) == "symbol")
                as.character(file)
            else stop("invalid '%s' argument", "file", domain = "R")
        }
        file <- if (is.call(file) && typeof(file[[1L]]) == "symbol" && file[[1L]] == "~")
            paste0("~", parse_file(file[[2L]]))
        else parse_file(file)
    }
    filename <- set.sys.path(file, path.only = TRUE, ignore.all = TRUE,
        Function = c("toplevel.source", "this.path"))
    file <- file(filename, "r", encoding = "")
    on.exit(close(file))
    lines <- readLines(file, warn = FALSE)
    on.exit()
    close(file)
    srcfile <- srcfilecopy(filename, lines, file.mtime(filename)[1], isFile = TRUE)
    exprs <- parse(n = -1, text = lines, keep.source = FALSE, srcfile = srcfile)
    Ne <- length(exprs)
    if (echo)
        trySrcLines <- function(from, to) {
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
                if (lastshown < lastl) {
                    dep <- trySrcLines(lastshown + 1L, lastl)
                    if (length(dep)) {
                        leading <- if (tail)
                            length(dep)
                        else firstl - lastshown
                        lastshown <- lastl
                        dep <- paste0(rep.int(c(getOption("prompt"), getOption("continue")),
                            pmax(0L, c(         leading,             length(dep) - leading))),
                            dep, collapse = "\n")
                        cat(dep, "\n", sep = "")
                    }
                }
            }
        }
        if (!tail) {
            yy <- withVisible(eval(ei, envir))
            if (yy$visible)
                .PrintValueEnv(yy$value, envir)
        }
    }
    invisible(yy)
}
