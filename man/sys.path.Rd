\name{sys.path}
\alias{sys.path}
\alias{sys.dir}
\title{Determine Executing Script's Filename}
\description{
  \code{sys.path()} returns the
  \ifelse{html}{\codelink2{base:normalizePath}{normalized}}{normalized} path of
  the executing script.

  \code{sys.dir()} returns the directory in which the executing script is
  located.
}
\usage{
sys.path(verbose = getOption("verbose"), original = FALSE,
    for.msg = FALSE, local = FALSE, contents = FALSE,
    default, else.)
sys.dir(verbose = getOption("verbose"), default, else.)
}
\arguments{
  \item{verbose}{\code{TRUE} or \code{FALSE}; should the method in which the
    path was determined be printed?}

  \item{original}{\code{TRUE}, \code{FALSE}, or \code{NA}; should the original
    or the normalized path be returned? \code{NA} means the normalized path
    will be returned if it has already been forced, and the original path
    otherwise.}

  \item{for.msg}{\code{TRUE} or \code{FALSE}; do you want the path for the
    purpose of printing a diagnostic message / / warning / / error? This will
    return \codelink2{base:NA}{NA_character_} in most cases where an error
    would have been thrown.

    \code{for.msg = TRUE} will ignore \code{original = FALSE}, and will use
    \code{original = NA} instead.}

  \item{local}{\code{TRUE} or \code{FALSE}; should the search for the executing
    script be confined to the local environment in which
    \code{\link{set.sys.path}()} was called?}

  \item{contents}{\code{TRUE} or \code{FALSE}; should the contents of the
    executing script be returned instead? In \sQuote{Jupyter}, a list of
    character vectors will be returned, the contents separated into cells.
    Otherwise, if \code{for.msg} is \code{TRUE} and the executing script cannot
    be determined, \code{NULL} will be returned. Otherwise, a character vector
    will be returned. You could use
    \ifelse{html}{\code{\link[base:character]{as.character}(\link[base]{unlist}(sys.path(contents = TRUE)))}}{\code{as.character(unlist(sys.path(contents = TRUE)))}}
    if you require a character vector.

    This is intended for logging purposes. This is useful in \sQuote{RStudio}
    and \sQuote{VSCode} when the source document has contents but no path.}

  \item{default}{if there is no executing script, this value is returned.}

  \item{else.}{missing or a function to apply if there is an executing script.
    See \code{\link{tryCatch2}()} for inspiration.}
}
\details{
  There are three ways in which \R code is typically run:

  \enumerate{
    \item{in \sQuote{Rgui} / / \sQuote{\RStudio} / / \sQuote{\VSCode} / /
      \sQuote{\Jupyter} by running the current line / / selection with the
      \strong{Run} button / / appropriate keyboard shortcut}

    \item{through a source call: a call to function \sourcesorlistlinks}

    \item{from a shell, such as the Windows command-line / / Unix terminal}
  }

  To retrieve the executing script's filename, first an attempt is made to find
  a source call. The calls are searched in reverse order so as to grab the most
  recent source call in the case of nested source calls. If a source call was
  found, the file argument is returned from the function's evaluation
  environment. If you have your own \sourcenolink-like function that you would
  like to be recognized by \code{sys.path()}, please contact the package
  maintainer so that it can be implemented or use \code{\link{wrap.source}()} /
  / \code{\link{set.sys.path}()}.

  If no source call is found up the calling stack, then an attempt is made to
  figure out how \R is currently being used.

  If \R is being run from a shell, the shell arguments are searched for
  \option{-f} \option{FILE} or \option{--file=FILE} (the two methods of taking
  input from \option{FILE}). The last \option{FILE} is extracted and returned
  (ignoring \option{-f} \option{-} and \option{--file=-}). It is an error to
  use \code{sys.path()} if no arguments of either type are supplied.

  If \R is being run from a shell under Unix-alikes with \option{-g}
  \option{Tk} or \option{--gui=Tk}, \code{sys.path()} will throw an error.
  \sQuote{Tk} does not make use of its \option{-f} \option{FILE},
  \option{--file=FILE} arguments.

  If \R is being run from \sQuote{Rgui}, the source document's filename (the
  document most recently interacted with besides the \R Console) is returned
  (at the time of evaluation). Please note that minimized documents \emph{WILL}
  be included when looking for the most recently used document. It is important
  to not leave the current document (either by closing the document or
  interacting with another document) while any calls to \code{sys.path()} have
  yet to be evaluated in the run selection. It is an error for no documents to
  be open or for a document to not exist (not saved anywhere).

  If \R is being run from \sQuote{\RStudio}, the active document's filename
  (the document in which the cursor is active) is returned (at the time of
  evaluation). If the active document is the \R console, the source document's
  filename (the document open in the current tab) is returned (at the time of
  evaluation). Please note that the source document will \emph{NEVER} be a
  document open in another window (with the \strong{Show in new window}
  button). Please also note that an active document open in another window can
  sometimes lose focus and become inactive, thus returning the incorrect path.
  It is best \strong{NOT} to not run \R code from a document open in another
  window. It is important to not leave the current tab (either by closing or
  switching tabs) while any calls to \code{sys.path()} have yet to be
  evaluated in the run selection. It is an error for no documents to be open or
  for a document to not exist (not saved anywhere).

  If \R is being run from \sQuote{\VSCode}, the source document's filename is
  returned (at the time of evaluation). It is important to not leave the
  current tab (either by closing or switching tabs) while any calls to
  \code{sys.path()} have yet to be evaluated in the run selection. It is an
  error for a document to not exist (not saved anywhere).

  If \R is being run from \sQuote{\Jupyter}, the source document's filename is
  guessed by looking for \R notebooks in the initial working directory, then
  searching the contents of those files for an expression matching the
  top-level expression. Please be sure to save your notebook before using
  \code{sys.path()}, or explicitly use \code{\link{set.sys.path.jupyter}()}.

  If \R is being run from \sQuote{AQUA}, the executing script's path cannot be
  determined. Unlike \sQuote{Rgui}, \sQuote{RStudio}, and \sQuote{VSCode},
  there is currently no way to request the path of an open document. Until such
  a time that there is a method for requesting the path of an open document,
  consider using \sQuote{RStudio} or \sQuote{VSCode}.

  If \R is being run in another manner, it is an error to use
  \code{sys.path()}.

  If your GUI of choice is not implemented with \code{sys.path()}, please
  contact the package maintainer so that it can be implemented.
}
\value{
  character string; the executing script's filename.
}
\note{
  The first time \code{sys.path()} is called within a script, it will
  \ifelse{html}{\codelink2{base:normalizePath}{normalize}}{normalize} the
  script's path, checking that the script exists (throwing an error if it does
  not), and save it in the appropriate environment. When \code{sys.path()} is
  called subsequent times within the same script, it returns the saved path.
  This will be faster than the first time, will not check for file existence,
  and will be independent of the working directory.

  As a side effect, this means that a script can delete itself using
  \codelink3{base:files}{file.remove}{()} or \codelink3{base}{unlink}{()} but
  still know its own path for the remainder of the script.

  Within a script that contains calls to both \code{sys.path()} and
  \codelink3{base:getwd}{setwd}{()}, \code{sys.path()} \emph{MUST} be used
  \emph{AT LEAST} once before the first call to \code{setwd()}.

  Please \strong{DO NOT} use \code{sys.path()} inside the site-wide startup
  profile file, the user profile, nor the function \code{.First()} (see
  \ifelse{html}{\code{?\link[base]{Startup}}}{\code{?Startup}}). This has
  inconsistent results dependent on the GUI, and often incorrect. For example:

  \describe{
#ifdef unix
    \item{in \sQuote{X11}}{in all three cases, it returns \option{FILE} from
      the command line arguments: \preformatted{
> sys.path(original = TRUE)
Source: shell argument 'FILE'
[1] "./file569c63d647ba.R"

> sys.path()
[1] "/tmp/RtmpGMmR3A/file569c63d647ba.R"}}
#endif
#ifdef windows
    \item{in \sQuote{Rterm}}{in all three cases, it returns \option{FILE} from
      the command line arguments: \preformatted{> sys.path(original = TRUE)
Source: shell argument 'FILE'
[1] "./file569c63d647ba.R"

> sys.path()
[1] "C:/Users/iris/AppData/Local/Temp/RtmpGMmR3A/file569c63d647ba.R"}}

    \item{in \sQuote{Rgui}}{in all three cases, it throws an error: \preformatted{> sys.path(original = TRUE)
Error in .sys.path.toplevel(FALSE, TRUE) :
  R is being run from Rgui with no documents open}}
#endif

    \item{in \sQuote{RStudio}}{in all three cases, it throws an error: \preformatted{> sys.path(original = TRUE)
Error in .rs.api.getSourceEditorContext() :
  RStudio has not finished loading}}

    \item{in \sQuote{VSCode}}{in the site-wide startup profile file and the
      function \code{.First()}, it throws an error: \preformatted{> sys.path(original = TRUE)
Error : RStudio not running}

#ifdef unix
      but in the user profile, it returns: \preformatted{> sys.path(original = TRUE)
Source: call to function source
[1] "~/.Rprofile"

> sys.path()
[1] "/home/iris/.Rprofile"}
#endif
#ifdef windows
      but in the user profile, it returns: \preformatted{> sys.path(original = TRUE)
Source: call to function source
[1] "~/.Rprofile"

> sys.path()
[1] "C:/Users/iris/Documents/.Rprofile"}
#endif
    }

    \item{in \sQuote{Jupyter}}{in all three cases, it throws an error: \preformatted{> sys.path(original = TRUE)
Error in .sys.path.toplevel(FALSE, TRUE) :
  Jupyter has not finished loading}}}

  Sometimes it returns \option{FILE} from the command line arguments, sometimes
  it returns the path of the user profile, and other times it throws an error.
  Alternatively, you could use \code{\link{shFILE}()}, supplying a
  \code{default} argument when no \option{FILE} is specificed, and supplying an
  \code{else.} function for when one is specified.
}
\seealso{
  \code{\link{here}()}

  \code{\link{shFILE}()}

  \code{\link{wrap.source}()}, \code{\link{set.sys.path}()}

  \code{\link{this.path-package}}

  \sourceslistlinks

  \code{\link{R.from.shell}}
}
\examples{
FILE1.R <- tempfile(fileext = ".R")
this.path:::.write.code({
    this.path:::.withAutoprint({
        cat(sQuote(this.path::sys.path(verbose = TRUE, default = {
            stop("since the executing script's path will be found,\\n",
                " argument 'default' will not be evaluated, so this\\n",
                " error will not be thrown! you can use this to\\n",
                " your advantage in a similar manner, doing\\n",
                " arbitrary things only if the executing script\\n",
                " does not exist")
        })), "\\n\\n")
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 58L,
       prompt = Sys.getenv("R_PROMPT"), continue = Sys.getenv("R_CONTINUE"))
}, FILE1.R)


oenv <- this.path:::.envvars(R_PROMPT   = "FILE1.R> ",
                             R_CONTINUE = "FILE1.R+ ")


## 'sys.path()' works with 'source()'
source(FILE1.R, verbose = FALSE)


## 'sys.path()' works with 'sys.source()'
sys.source(FILE1.R, envir = environment())


## 'sys.path()' works with 'debugSource()' in 'RStudio'
if (.Platform$GUI == "RStudio")
    get("debugSource", "tools:rstudio", inherits = FALSE)(FILE1.R)


## 'sys.path()' works with 'testthat::source_file()'
if (requireNamespace("testthat"))
    testthat::source_file(FILE1.R, chdir = FALSE, wrap = FALSE)


## 'sys.path()' works with 'knitr::knit()'
if (requireNamespace("knitr")) {
    FILE2.Rmd <- tempfile(fileext = ".Rmd")
    FILE3.md <- tempfile(fileext = ".md")
    writeLines(c(
        "```{r}",
        ## same expression as above
        deparse(parse(FILE1.R)[[c(1L, 2L, 2L)]], width.cutoff = 55L),
        "```"
    ), FILE2.Rmd)


    # knitr::knit(FILE2.Rmd, output = FILE3.md, quiet = FALSE)
    ## the above does not work when using the 'Run examples' button in
    ## the HTML documentation. {knitr} cannot knit a document inside
    ## another document, pretty embarrassing oversight, so we have to
    ## launch a new R session and knit the document from there
    FILE4.R <- tempfile(fileext = ".R")
    this.path:::.write.code(bquote({
        knitr::knit(.(FILE2.Rmd), output = .(FILE3.md), quiet = TRUE)
    }), FILE4.R)
    this.path:::.Rscript(
        c("--default-packages=NULL", "--vanilla", FILE4.R),
        show.command = FALSE
    )
    unlink(FILE4.R)


    this.path:::.cat.file(FILE2.Rmd, number.nonblank = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE,
        show.command = TRUE)
    this.path:::.cat.file(FILE3.md, number.nonblank = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE,
        show.command = TRUE)
    unlink(c(FILE3.md, FILE2.Rmd))
}


## 'sys.path()' works with 'compiler::loadcmp()'
if (requireNamespace("compiler")) {
    FILE2.Rc <- tempfile(fileext = ".Rc")
    compiler::cmpfile(FILE1.R, FILE2.Rc)
    oenv2 <- this.path:::.envvars(R_PROMPT   = "FILE2.Rc> ",
                                  R_CONTINUE = "FILE2.Rc+ ")
    compiler::loadcmp(FILE2.Rc)
    this.path:::.envvars(oenv2)
    unlink(FILE2.Rc)
}


## 'sys.path()' works with 'box::use()'
if (requireNamespace("box")) {
    FILE2.R <- tempfile(fileext = ".R")
    this.path:::.write.code(bquote({
        ## we have to use box::set_script_path() because {box}
        ## does not allow us to import a module by its path
        script_path <- box::script_path()
        on.exit(box::set_script_path(script_path))
        box::set_script_path(.(normalizePath(FILE1.R, "/")))
        box::use(module = ./.(as.symbol(this.path::removeext(
            this.path::basename2(FILE1.R)
        ))))
        box::unload(module)
    }), FILE2.R)
    source(FILE2.R, echo = TRUE, spaced = FALSE, verbose = FALSE,
        prompt.echo = "FILE2.R> ", continue.echo = "FILE2.R+ ")
    unlink(FILE2.R)
}


## 'sys.path()' works with 'Rscript'
## it also works with other GUIs but that is
## not possible to show in a simple example
this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE1.R))
this.path:::.envvars(oenv)


## 'sys.path()' also works when 'source()'-ing a URL
## (included tryCatch in case an internet connection is not available)
tryCatch({
    source(paste0("https://raw.githubusercontent.com/ArcadeAntics/",
                  "this.path/main/tests/sys-path-with-urls.R"))
}, condition = this.path:::.cat.condition)


for (expr in c("sys.path()",
               "sys.path(default = NULL)",
               "sys.dir()",
               "sys.dir(default = NULL)",
               "sys.dir(default = getwd())"))
{
    cat("\\n\\n")
    this.path:::.Rscript(c("--default-packages=this.path",
                           "--vanilla", "-e", expr))
}


## an example from R package 'logr'
this.path::sys.path(verbose = FALSE, default = "script.log",
    else. = function(path) {
        ## replace extension (probably .R) with .log
        this.path::ext(path) <- ".log"
        path
        ## or you could use paste0(this.path::removeext(path), ".log")
    })


unlink(FILE1.R)
}
