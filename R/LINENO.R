.normpath <- function (path, wd)
{
    if (!missing(wd) && !is.null(wd)) {
        oldwd <- setwd(wd)
        on.exit(setwd(oldwd))
    }
    tryCatch({
        normalizePath(path, "/")
    }, warning = function(w) {
        if (.Platform$OS.type == "windows")
            chartr("\\", "/", path)
        else path
    })
}


LINENO <- function ()
{
    failure <- TRUE
    tryCatch({
        cntxt.number <- .this.path(get.frame.number = TRUE)
        if (cntxt.number == 0L)
            return(NA_integer_)
        path <- .this.path()
        failure <- FALSE
    }, error = function(e) NULL)
    if (failure)
        return(NA_integer_)


    for (which in sys.nframe():(cntxt.number + 1L)) {
        call <- sys.call(which)
        srcref <- attr(call, "srcref")
        if (!is.null(srcref)) {
            srcfile <- attr(srcref, "srcfile")
            filename <- .normpath(srcfile$filename, srcfile$wd)
            if (path == filename) {
                # return(srcref[7L])

                # srcref[1L] is better, it respects #line directives
                return(srcref[1L])
            }
        }
    }


    return(NA_integer_)
}


source2 <- function (file, envir = parent.frame())
{
    file <- inside.source(file)
    if (!is.environment(envir))
        stop("'envir' must be an environment")
    envir <- as.environment(envir)
    if (is.character(file)) {
        con <- file(file, "r")
        on.exit(close(con))
        lines <- readLines(con, warn = FALSE)
        on.exit()
        close(con)
        srcfile <- srcfilecopy(file, lines, file.mtime(file)[1],
            isFile = is.abs.path(file))
    }
    else {
        lines <- readLines(file, warn = FALSE)
        summ <- summary.connection(file)
        srcfile <- srcfilecopy(
            sprintf("<'%s' connection: '%s'>", EncodeChar(summ[["class"]]), EncodeChar(summ[["description"]])),
            lines
        )
    }
    exprs <- parse(stdin(), n = -1, text = lines, prompt = "?", srcfile = srcfile)
    Ne <- length(exprs)
    trySrcLines <- function(srcfile, first, last) {
        tryCatch(suppressWarnings(getSrcLines(srcfile, first, last)),
            error = function(e) character())
    }
    yy <- NULL
    lastshown <- 0L
    srcrefs <- attr(exprs, "srcref")
    for (i in seq_len(Ne + 1L)) {
        tail <- i > Ne
        if (!tail) {
            ei <- exprs[i]
        }
        nd <- 0L
        srcref <- if (tail)
            attr(exprs, "wholeSrcref")
        else if (i <= length(srcrefs))
            srcrefs[[i]]
        if (!is.null(srcref)) {
            if (i == 1L)
                lastshown <- min(0L, srcref[8L] - 1L)
            if (lastshown < srcref[8L]) {
                srcfile <- attr(srcref, "srcfile")
                dep <- trySrcLines(srcfile, lastshown + 1L, srcref[8L])
                if (length(dep)) {
                    leading <- if (tail)
                        length(dep)
                    else srcref[7L] - lastshown
                    lastshown <- srcref[8L]
                    dep <- paste0(
                        rep.int(c("> ", "+ "), pmax(0L, c(leading, length(dep) - leading))),
                        dep, collapse = "\n"
                    )
                    nd <- nchar(dep)
                }
                else srcref <- NULL
            }
        }
        if (is.null(srcref)) {
            if (!tail) {
                dep <- substr(paste(deparse(ei), collapse = "\n"), 12L, 1000000L)
                dep <- paste0("> ", gsub("\n", "\n+ ", dep, fixed = TRUE))
                nd <- nchar(dep) - 1L
            }
        }
        if (nd) {
            dep <- substr(dep, 1L, nd)
            cat("\n", dep, "\n", sep = "")
        }
        if (!tail) {
            yy <- withVisible(eval(ei, envir))
            if (yy$visible) {
                print(yy$value)
            }
        }
    }
    invisible(yy)
}
environment(source2) <- getNamespace("this.path")
