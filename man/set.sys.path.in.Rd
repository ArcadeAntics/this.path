\name{set.sys.path}
\alias{set.sys.path}
\alias{set.env.path}
\alias{set.src.path}
\alias{unset.sys.path}
\alias{set.sys.path.function}
\alias{with_sys.path}
\alias{with_site.file}
\alias{with_init.file}
\alias{wrap.source}
\title{Implement 'this.path()' For Arbitrary 'source()'-Like Functions}
\description{
  \code{\link{sys.path}()} is implemented to work with \sourcesandlistlinks.

  \code{set.sys.path()} can be used to implement \code{sys.path()} for any
  other \sourcenolink-like functions.

  \code{set.env.path()} and \code{set.src.path()} can be used along side
  \code{set.sys.path()} to implement \code{\link{env.path}()} and
  \code{\link{src.path}()}, fully implementing \code{\link{this.path}()}. Note
  that \code{set.env.path()} only makes sense if the code is being modularized,
  see examples.

  \code{unset.sys.path()} will undo a call to \code{set.sys.path()}. You will
  need to use this if you wish to call \code{set.sys.path()} multiple times
  within a function.

  \code{set.sys.path.function()} is a special variant of \code{set.sys.path()}
  to be called within \code{callr::r()} on a function with an appropriate
  \code{\link[base:srcfile]{srcref}}.

  \code{with_sys.path()} is a convenient way to evaluate code within the
  context of a file. Whereas \code{set.sys.path()} can only be used within a
  function, \code{with_sys.path()} can only be used outside a function.
  \code{with_init.file()} provides a common use-case of \code{with_sys.path()};
  within a user profile. You could combine this with
  \code{\link{withAutoprint}()} to auto-print expressions as if in a toplevel
  context, for example: \preformatted{this.path::with_init.file(withAutoprint({
    this.path::this.path()
}, echo = FALSE))}

  See \code{?\link{sys.path}(local = TRUE)} which returns the path of the
  executing script, confining the search to the local environment in which
  \code{set.sys.path()} was called.

  \code{wrap.source()} should not be used, save for one specific use-case. See
  details.
}
\usage{
set.sys.path(file,
    path.only = FALSE,
    character.only = path.only,
    file.only = path.only,
    conv2utf8 = FALSE,
    allow.blank.string = FALSE,
    allow.clipboard = !file.only,
    allow.stdin = !file.only,
    allow.url = !file.only,
    allow.file.uri = !path.only,
    allow.unz = !path.only,
    allow.pipe = !file.only,
    allow.terminal = !file.only,
    allow.textConnection = !file.only,
    allow.rawConnection = !file.only,
    allow.sockconn = !file.only,
    allow.servsockconn = !file.only,
    allow.customConnection = !file.only,
    ignore.all = FALSE,
    ignore.blank.string = ignore.all,
    ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all,
    ignore.url = ignore.all,
    ignore.file.uri = ignore.all,
    Function = NULL, ofile)

set.env.path(envir, matchThisEnv = getOption("topLevelEnvironment"))

set.src.path(srcfile)

unset.sys.path()

set.sys.path.function(fun)

with_sys.path(file, expr, ...)
with_site.file(expr, n = 0)
with_init.file(expr, n = 0)

wrap.source(expr,
    path.only = FALSE,
    character.only = path.only,
    file.only = path.only,
    conv2utf8 = FALSE,
    allow.blank.string = FALSE,
    allow.clipboard = !file.only,
    allow.stdin = !file.only,
    allow.url = !file.only,
    allow.file.uri = !path.only,
    allow.unz = !path.only,
    allow.pipe = !file.only,
    allow.terminal = !file.only,
    allow.textConnection = !file.only,
    allow.rawConnection = !file.only,
    allow.sockconn = !file.only,
    allow.servsockconn = !file.only,
    allow.customConnection = !file.only,
    ignore.all = FALSE,
    ignore.blank.string = ignore.all,
    ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all,
    ignore.url = ignore.all,
    ignore.file.uri = ignore.all)
}
\arguments{
  \item{expr}{for \code{with_sys.path()} and \code{with_init.file()}, an
    expression to evaluate within the context of a file.

    for \code{wrap.source()}, an (unevaluated) call to a \sourcenolink-like
    function.}

  \item{file}{a
    \ifelse{html}{\codelink2{base:connections}{connection}}{connection} or a
    character string giving the pathname of the file or URL to read from.}

  \item{path.only}{must \code{file} be an existing path? This implies
    \code{character.only} and \code{file.only} are \code{TRUE} and implies
    \code{allow.file.uri} and \code{allow.unz} are \code{FALSE}, though these
    can be manually changed.}

  \item{character.only}{must \code{file} be a character string?}

  \item{file.only}{must \code{file} refer to an existing file?}

  \item{conv2utf8}{if \code{file} is a character string, should it be converted
    to UTF-8?}

  \item{allow.blank.string}{may \code{file} be a blank string, i.e.
    \blankstring?}

  \item{allow.clipboard}{may \code{file} be \string{clipboard} or a clipboard
    connection?}

  \item{allow.stdin}{may \code{file} be \string{stdin}? Note that
    \string{stdin} refers to the C-level \sQuote{standard input} of the
    process, differing from \codelink3{base:showConnections}{stdin}{()} which
    refers to the R-level \sQuote{standard input}.}

  \item{allow.url}{may \code{file} be a URL pathname or a connection of class
    \string{url-libcurl} / / \string{url-wininet}?}

  \item{allow.file.uri}{may \code{file} be a \samp{file://} URI?}

  \item{allow.unz, allow.pipe, allow.terminal, allow.textConnection,
        allow.rawConnection, allow.sockconn, allow.servsockconn}{may
    \code{file} be a connection of class
    \ifelse{html}{\code{"\link[base:connections]{unz}"}}{\string{unz}}
    / / \string{pipe} / / \string{terminal} / / \string{textConnection} / /
    \string{rawConnection} / / \string{sockconn} / / \string{servsockconn}?}

  \item{allow.customConnection}{may \code{file} be a custom connection?}

  \item{ignore.all, ignore.blank.string, ignore.clipboard, ignore.stdin,
        ignore.url, ignore.file.uri}{ignore the special meaning of these types
    of strings, treating it as a path instead?}

  \item{Function}{character vector of length \ifelse{text}{1}{\code{1}} or
    \ifelse{text}{2}{\code{2}}; the name of the function and package in which
    \code{set.sys.path()} is called.}

  \item{ofile}{a
    \ifelse{html}{\codelink2{base:connections}{connection}}{connection} or a
    character string specifying the original file argument. This overwrites the
    value returned by \code{\link{sys.path}(original = TRUE)}.}

  \item{envir, matchThisEnv}{arguments passed to
    \codelink3{base:ns-topenv}{topenv}{()} to determine the top level
    environment in which to assign an associated path.}

  \item{srcfile}{source file in which to assign a pathname.}

  \item{fun}{function with a \code{srcref}.}

  \item{\dots}{further arguments passed to \code{set.sys.path()}.}

  \item{n}{see \code{?\link{this.path}()}.}
}
\details{
  \code{set.sys.path()} should be added to the body of your \sourcenolink-like
  function before reading / / evaluating the expressions.

  \code{wrap.source()}, unlike \code{set.sys.path()}, does not accept an
  argument \code{file}. Instead, an attempt is made to extract the file from
  \code{expr}, after which \code{expr} is evaluated. It is assumed that the
  file is the first argument of the function, as is the case with
  \sourcesfirstargisfileandlistnolinks. The function of the call is evaluated,
  its \code{\link[base]{formals}()} are retrieved, and then the arguments of
  \code{expr} are searched for a name matching the name of the first formal
  argument. If a match cannot be found by name, the first unnamed argument is
  taken instead. If no such argument exists, the file is assumed missing.

  \code{wrap.source()} does non-standard evaluation and does some guess work to
  determine the file. As such, it is less desirable than \code{set.sys.path()}
  when the option is available. I can think of exactly one scenario in which
  \code{wrap.source()} might be preferable: suppose there is a
  \sourcenolink-like function \code{sourcelike()} in a foreign package (a
  package for which you do not have write permission). Suppose that you write
  your own function in which the formals are \code{(\dots)} to wrap
  \code{sourcelike()}: \preformatted{wrapper <- function (...)
{
    ## possibly more args to wrap.source()
    wrap.source(sourcelike(...))
}}

  This is the only scenario in which \code{wrap.source()} is preferable, since
  extracting the file from the \code{\dots} list would be a pain. Then again,
  you could simply change the formals of \code{wrapper()} from \code{(\dots)}
  to \code{(file, \dots)}. If this does not describe your exact scenario, use
  \code{set.sys.path()} instead.
}
\value{
  for \code{set.sys.path()}, if \code{file} is a path, then the normalized
  path with the same attributes, otherwise \code{file} itself. The return value
  of \code{set.sys.path()} should be assigned to a variable before use,
  something like: \preformatted{{
    file <- set.sys.path(file, ...)
    sourcelike(file)
}}

  for \code{wrap.source()}, the result of evaluating \code{expr}.
}
\section{Using 'ofile'}{
  \code{ofile} can be used when the \code{file} argument supplied to
  \code{set.sys.path()} is not the same as the \code{file} argument supplied to
  the \sourcenolink-like function:

\preformatted{sourcelike <- function (file)
{
    ofile <- file
    if (!is.character(ofile) || length(ofile) != 1)
        stop(gettextf("'\%s' must be a character string", "file"))
    ## if the file exists, do nothing
    if (file.exists(file)) {
    }
    ## look for the file in the home directory
    ## if it exists, do nothing
    else if (file.exists(file <- this.path::path.join("~", ofile))) {
    }
    ## you could add other directories to look in,
    ## but this is good enough for an example
    else stop(gettextf("'\%s' is not an existing file", ofile))
    file <- this.path::set.sys.path(file, ofile = ofile)
    exprs <- parse(n = -1, file = file)
    for (i in seq_along(exprs)) eval(exprs[i], envir)
    invisible()
}}
}
\examples{
FILE.R <- tempfile(fileext = ".R")
this.path:::.writeCode({
    this.path::sys.path(verbose = TRUE)
    try(this.path::env.path(verbose = TRUE))
    this.path::src.path(verbose = TRUE)
    this.path::this.path(verbose = TRUE)
}, FILE.R)


## here we have a source-like function, suppose this
## function is in a package for which you have write permission
sourcelike <- function (file, envir = parent.frame())
{
    ofile <- file
    file <- set.sys.path(file, Function = "sourcelike")
    lines <- readLines(file, warn = FALSE)
    filename <- sys.path(local = TRUE, for.msg = TRUE)
    isFile <- !is.na(filename)
    if (isFile) {
        timestamp <- file.mtime(filename)[1]
        ## in case 'ofile' is a URL pathname / / 'unz' connection
        if (is.na(timestamp))
            timestamp <- Sys.time()
    }
    else {
        filename <- if (is.character(ofile)) ofile else "<connection>"
        timestamp <- Sys.time()
    }
    srcfile <- srcfilecopy(filename, lines, timestamp, isFile)
    set.src.path(srcfile)
    exprs <- parse(text = lines, srcfile = srcfile, keep.source = FALSE)
    invisible(source.exprs(exprs, evaluated = TRUE, envir = envir))
}


sourcelike(FILE.R)
sourcelike(conn <- file(FILE.R)); close(conn)


## here we have another source-like function, suppose this function
## is in a foreign package for which you do not have write permission
sourcelike2 <- function (pathname, envir = globalenv())
{
    if (!(is.character(pathname) && file.exists(pathname)))
        stop(gettextf("'\%s' is not an existing file",
             pathname, domain = "R-base"))
    envir <- as.environment(envir)
    lines <- readLines(pathname, warn = FALSE)
    srcfile <- srcfilecopy(pathname, lines, isFile = TRUE)
    exprs <- parse(text = lines, srcfile = srcfile, keep.source = FALSE)
    invisible(source.exprs(exprs, evaluated = TRUE, envir = envir))
}


## the above function is similar to sys.source(), and it
## expects a character string referring to an existing file
##
## with the following, you should be able
## to use 'sys.path()' within 'FILE.R':
wrap.source(sourcelike2(FILE.R), path.only = TRUE)


# ## with R >= 4.1.0, use the forward pipe operator '|>' to
# ## make calls to 'wrap.source' more intuitive:
# sourcelike2(FILE.R) |> wrap.source(path.only = TRUE)


## 'wrap.source' can recognize arguments by name, so they
## do not need to appear in the same order as the formals
wrap.source(sourcelike2(envir = new.env(), pathname = FILE.R),
    path.only = TRUE)


## it it much easier to define a new function to do this
sourcelike3 <- function (...)
wrap.source(sourcelike2(...), path.only = TRUE)


## the same as before
sourcelike3(FILE.R)


## however, this is preferable:
sourcelike4 <- function (pathname, ...)
{
    ## pathname is now normalized
    pathname <- set.sys.path(pathname, path.only = TRUE)
    sourcelike2(pathname = pathname, ...)
}
sourcelike4(FILE.R)


## perhaps you wish to run several scripts in the same function
fun <- function (paths, ...)
{
    for (pathname in paths) {
        pathname <- set.sys.path(pathname, path.only = TRUE)
        sourcelike2(pathname = pathname, ...)
        unset.sys.path(pathname)
    }
}


## here we have a source-like function which modularizes its code
sourcelike5 <- function (file)
{
    ofile <- file
    file <- set.sys.path(file, Function = "sourcelike5")
    lines <- readLines(file, warn = FALSE)
    filename <- sys.path(local = TRUE, for.msg = TRUE)
    isFile <- !is.na(filename)
    if (isFile) {
        timestamp <- file.mtime(filename)[1]
        ## in case 'ofile' is a URL pathname / / 'unz' connection
        if (is.na(timestamp))
            timestamp <- Sys.time()
    }
    else {
        filename <- if (is.character(ofile)) ofile else "<connection>"
        timestamp <- Sys.time()
    }
    srcfile <- srcfilecopy(filename, lines, timestamp, isFile)
    set.src.path(srcfile)
    envir <- new.env(hash = TRUE, parent = .BaseNamespaceEnv)
    envir$.packageName <- filename
    oopt <- options(topLevelEnvironment = envir)
    on.exit(options(oopt))
    set.env.path(envir)
    exprs <- parse(text = lines, srcfile = srcfile, keep.source = FALSE)
    source.exprs(exprs, evaluated = TRUE, envir = envir)
    envir
}


sourcelike5(FILE.R)


## the code can be made much simpler in some cases
sourcelike6 <- function (file)
{
    ## we expect a character string refering to a file
    ofile <- file
    filename <- set.sys.path(file, path.only = TRUE, ignore.all = TRUE,
        Function = "sourcelike6")
    lines <- readLines(filename, warn = FALSE)
    timestamp <- file.mtime(filename)[1]
    srcfile <- srcfilecopy(filename, lines, timestamp, isFile = TRUE)
    set.src.path(srcfile)
    envir <- new.env(hash = TRUE, parent = .BaseNamespaceEnv)
    envir$.packageName <- filename
    oopt <- options(topLevelEnvironment = envir)
    on.exit(options(oopt))
    set.env.path(envir)
    exprs <- parse(text = lines, srcfile = srcfile, keep.source = FALSE)
    source.exprs(exprs, evaluated = TRUE, envir = envir)
    envir
}


sourcelike6(FILE.R)


unlink(FILE.R)
}
