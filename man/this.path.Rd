\name{this.path}
\alias{this.path}
\alias{this.dir}
\alias{Sys.path}
\alias{Sys.dir}
\title{
Determine Executing Script's Filename
}
\description{
\code{this.path()} returns the \code{\link[base:normalizePath]{normalized}}
path of the executing script.

\code{this.dir()} returns the \code{\link[base:normalizePath]{normalized}} path
of the directory in which the executing script is located.

\code{Sys.path()} and \code{Sys.dir()} are versions of \code{this.path()} and
\code{this.dir()} that takes no arguments.

See also \code{\link{here}()} and \code{\link{this.proj}()} for constructing
paths to files, starting with \code{this.dir()} or the project's directory.
}
\usage{
this.path(verbose = getOption("verbose"), original = FALSE,
    for.msg = FALSE, default, else.)
this.dir (verbose = getOption("verbose"), default, else.)

Sys.path()  # short for 'this.path(verbose = FALSE)'
Sys.dir ()  # short for 'this.dir (verbose = FALSE)'
}
\arguments{
  \item{verbose}{
\code{TRUE} or \code{FALSE}; should the method in which the path was determined
be printed?
  }
  \item{original}{
\code{TRUE}, \code{FALSE}, or \code{NA}; should the original or the normalized
path be returned? \code{NA} means the normalized path will be returned if it
has already been forced, and the original path otherwise.
  }
  \item{for.msg}{
\code{TRUE} or \code{FALSE}; do you want the path for the purpose of printing a
diagnostic message / / warning / / error? This will return
\code{\link[base:NA]{NA_character_}} in most cases where an error would have
been thrown.

\code{for.msg = TRUE} will ignore \code{original = FALSE}, and will
use \code{original = NA} instead.
  }
  \item{default}{
if there is no executing script, this value is returned.
  }
  \item{else.}{
missing or a function to apply if there is an executing script. See
\code{\link{tryCatch2}()} for inspiration.
  }
}
\details{
There are three ways in which \R code is typically run:
\enumerate{
  \item{in \sQuote{Rgui} / / \sQuote{\RStudio} / / \sQuote{\VSCode} / /
\sQuote{\Jupyter} by running the current line / / selection with the
\strong{Run} button / / appropriate keyboard shortcut
  }
  \item through a source call: a call to function \sourcesorlistlinks{}
  \item from a shell, such as the Windows command-line / / Unix terminal
}

To retrieve the executing script's filename, first an attempt is made to find a
source call. The calls are searched in reverse order so as to grab the most
recent source call in the case of nested source calls. If a source call was
found, the file argument is returned from the function's evaluation
environment. If you have your own \sourcenolink{}-like function that you would
like to be recognized by \code{this.path()}, please contact the package
maintainer so that it can be implemented or use \code{\link{wrap.source}()} / /
\code{\link{inside.source}()} / / \code{\link{set.this.path}()}.

If no source call is found up the calling stack, then an attempt is made to
figure out how \R is currently being used.

If \R is being run from a shell, the shell arguments are searched for
\option{-f} \option{FILE} or \option{--file=FILE} (the two methods of taking
input from \option{FILE}). The last \option{FILE} is extracted and returned
(ignoring \option{-f} \option{-} and \option{--file=-}). It is an error to use
\code{this.path()} if no arguments of either type are supplied.

If \R is being run from a shell under Unix-alikes with \option{-g} \option{Tk}
or \option{--gui=Tk}, \code{this.path()} will signal an error. \sQuote{Tk} does
not make use of its \option{-f} \option{FILE}, \option{--file=FILE} arguments.

If \R is being run from \sQuote{Rgui}, the source document's filename (the
document most recently interacted with besides the \R Console) is returned (at
the time of evaluation). Please note that minimized documents \emph{WILL} be
included when looking for the most recently used document. It is important to
not leave the current document (either by closing the document or interacting
with another document) while any calls to \code{this.path()} have yet to be
evaluated in the run selection. It is an error for no documents to be open or
for a document to not exist (not saved anywhere).

If \R is being run from \sQuote{\RStudio}, the active document's filename (the
document in which the cursor is active) is returned (at the time of
evaluation). If the active document is the \R console, the source document's
filename (the document open in the current tab) is returned (at the time of
evaluation). Please note that the source document will \emph{NEVER} be a
document open in another window (with the \strong{Show in new window} button).
It is important to not leave the current tab (either by closing or switching
tabs) while any calls to \code{this.path()} have yet to be evaluated in the run
selection. It is an error for no documents to be open or for a document to not
exist (not saved anywhere).

If \R is being run from \sQuote{\VSCode}, the source document's filename is
returned (at the time of evaluation). It is important to not leave the current
tab (either by closing or switching tabs) while any calls to \code{this.path()}
have yet to be evaluated in the run selection. It is an error for a document to
not exist (not saved anywhere).

If \R is being run from \sQuote{\Jupyter}, the source document's filename is
guessed by looking for \R notebooks in the initial working directory, then
searching the contents of those files for an expression matching the top-level
expression.

If \R is being run from \sQuote{AQUA}, the executing script's path cannot be
determined. Unlike \sQuote{Rgui}, \sQuote{RStudio}, and \sQuote{VSCode}, there
is currently no way to request the path of an open document. Until such a time
that there is a method for requesting the path of an open document, consider
using \sQuote{RStudio} or \sQuote{VSCode}.

If \R is being run in another manner, it is an error to use \code{this.path()}.

If your GUI of choice is not implemented with \code{this.path()}, please
contact the package maintainer so that it can be implemented.
}
\value{
character string; the executing script's filename.
}
\note{
The first time \code{this.path()} is called within a script, it will
\code{\link[base:normalizePath]{normalize}} the script's path, checking that
the script exists (throwing an error if it does not), and save it in the
appropriate environment. When \code{this.path()} is called subsequent times
within the same script, it returns the saved path. This will be faster than the
first time, will not check for file existence, and will be independent of the
working directory.

As a side effect, this means that a script can delete itself using
\code{\link[base:files]{file.remove}()} or \code{\link[base]{unlink}()} but
still know its own path for the remainder of the script.

Within a script that contains calls to both \code{this.path()} and
\code{\link[base:getwd]{setwd}()}, \code{this.path()} \emph{MUST} be used
\emph{AT LEAST} once before the first call to \code{setwd()}. This is not
always necessary; if you ran a script using its absolute path as opposed to its
relative path, changing the working directory has no effect. However, it is
still advised against.

The following is \emph{NOT} an example of bad practice:

\code{setwd(this.path::this.dir())}

\code{setwd()} is most certainly written before \code{this.path()}, but
\code{this.path()} will be evaluated first. It is not the written order that is
bad practice, but the order of evaluation. Do not change the working directory
before calling \code{this.path()} at least once.

Please \strong{DO NOT} use \code{this.path()} inside the site-wide startup
profile file, the user profile, nor the function \code{.First()} (see
\code{?\link[base]{Startup}}). This has inconsistent results dependent on the
GUI, and often incorrect. For example:

\describe{
#ifdef unix
  \item{in \sQuote{X11}}{
in all three cases, it returns the command line argument \option{FILE}:
\preformatted{
> this.path(original = TRUE)
Source: shell argument 'FILE'
[1] "./file569c63d647ba.R"

> this.path()
[1] "/tmp/RtmpGMmR3A/file569c63d647ba.R"
}
  }
#endif
#ifdef windows
  \item{in \sQuote{Rterm}}{
in all three cases, it returns the command line argument \option{FILE}:
\preformatted{
> this.path(original = TRUE)
Source: shell argument 'FILE'
[1] "./file569c63d647ba.R"

> this.path()
[1] "C:/Users/iris/AppData/Local/Temp/RtmpGMmR3A/file569c63d647ba.R"
}
  }
  \item{in \sQuote{Rgui}}{
in all three cases, it throws an error:
\preformatted{
> this.path(original = TRUE)
Error in .this.path(verbose, original, for.msg) :
  'this.path' used in an inappropriate fashion
* no appropriate source call was found up the calling stack
* R is being run from Rgui with no documents open
}
  }
#endif
  \item{in \sQuote{RStudio}}{
in all three cases, it throws an error:
\preformatted{
> this.path(original = TRUE)
Error in .rs.api.getActiveDocumentContext() :
  RStudio has not finished loading
}
  }
  \item{in \sQuote{VSCode}}{
in the site-wide startup profile file and the function \code{.First()}, it
throws an error:
\preformatted{
> this.path(original = TRUE)
Error : RStudio not running
}
but in the user profile, it returns:
#ifdef unix
\preformatted{
> this.path(original = TRUE)
Source: call to function source
[1] "~/.Rprofile"

> this.path()
[1] "/home/iris/.Rprofile"
}
#endif
#ifdef windows
\preformatted{
> this.path(original = TRUE)
Source: call to function source
[1] "~/.Rprofile"

> this.path()
[1] "C:/Users/iris/Documents/.Rprofile"
}
#endif
  }
  \item{in \sQuote{Jupyter}}{
in all three cases, it throws an error:
\preformatted{
> this.path(original = TRUE)
Error in .this.path.toplevel(FALSE, TRUE) :
  Jupyter has not finished loading
}
  }
}

Sometimes it returns the command line argument \option{FILE}, sometimes it
returns the path of the user profile, and other times it throws an error.
Alternatively, you could use \code{\link{shFILE}()}, supplying a \code{default}
argument when no \option{FILE} is specificed, and supplying an \code{else.}
function for when one is specified.
}
\seealso{
\code{\link{here}()}

\code{\link{shFILE}()}

\code{\link{wrap.source}()}, \code{\link{inside.source}()},
\code{\link{set.this.path}()}

\code{\link{this.path-package}}

\sourceslistlinks{}

\code{\link{R.from.shell}}
}
\examples{
FILE <- tempfile(fileext = ".R")
this.path:::write.code({
    this.path:::withAutoprint({
        cat(sQuote(this.path::this.path(verbose = TRUE, default = {
            stop("since the executing script's path will be found,\\n",
                " argument 'default' will not be evaluated, so this\\n",
                " error will not be thrown! you can use this to\\n",
                " your advantage in a similar manner, doing\\n",
                " arbitrary things only if the executing script\\n",
                " does not exist")
        })), "\\n\\n")
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L,
        prompt.echo = "FILE> ", continue.echo = "FILE+ ")
}, FILE)


source(FILE, verbose = FALSE)
sys.source(FILE, envir = environment())
if (.Platform$GUI == "RStudio")
    get("debugSource", "tools:rstudio", inherits = FALSE)(FILE)
if (requireNamespace("testthat"))
    testthat::source_file(FILE, chdir = FALSE, wrap = FALSE)
if (requireNamespace("knitr")) {
    FILE2 <- tempfile(fileext = ".Rmd")
    FILE3 <- tempfile(fileext = ".md")
    writeLines(c(
        "```{r}",
        # same expression as above
        deparse(parse(FILE)[[c(1L, 2L, 2L)]]),
        "```"
    ), FILE2)
    knitr::knit(input = FILE2, output = FILE3, quiet = TRUE)
    this.path:::cat.file(FILE2, number.nonblank = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE,
        print.command = TRUE)
    this.path:::cat.file(FILE3, number.nonblank = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE,
        print.command = TRUE)
    unlink(c(FILE3, FILE2))
}
if (requireNamespace("box")) {
    FILE4 <- tempfile(fileext = ".R")
    this.path:::write.code(bquote({
        this.path:::withAutoprint({
            # we have to use box::set_script_path() because {box}
            # does not allow us to import a module by its path
            script_path <- box::script_path()
            on.exit(box::set_script_path(script_path))
            box::set_script_path(.(normalizePath(FILE, "/")))
            box::use(./.(as.symbol(this.path::removeext(
                this.path::basename2(FILE)
            ))))
            box::unload(.(as.symbol(this.path::removeext(
                this.path::basename2(FILE)
            ))))
        }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L,
            prompt.echo = "FILE4> ", continue.echo = "FILE4+ ")
    }), FILE4)
    source(FILE4, verbose = FALSE)
    unlink(FILE4)
}


this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE))


# this.path also works when source-ing a URL
# (included tryCatch in case an internet connection is not available)
tryCatch({
    source(paste0("https://raw.githubusercontent.com/ArcadeAntics/",
                  "this.path/main/tests/this.path_w_URLs.R"))
}, condition = this.path:::cat.condition)


for (expr in c("this.path()",
               "this.path(default = NULL)",
               "this.dir()",
               "this.dir(default = NULL)",
               "this.dir(default = getwd())"))
{
    cat("\\n\\n")
    suppressWarnings(this.path:::.Rscript(
        c("--default-packages=this.path", "--vanilla", "-e", expr)
    ))
}


# an example from R package 'logr'
this.path::this.path(verbose = FALSE, default = "script.log",
    else. = function(path) {
        # replace extension (probably .R) with .log
        this.path::ext(path) <- ".log"
        path
        # or you could use paste0(this.path::removeext(path), ".log")
    })
\dontshow{unlink(FILE)}}
