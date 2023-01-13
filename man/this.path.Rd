\name{this.path}
\alias{this.path}
\alias{this.dir}
\alias{Sys.path}
\alias{Sys.dir}
\title{Determine Executing Script's Filename
}
\description{
\code{this.path()} returns the \code{\link[base:normalizePath]{normalized}}
path of the executing script.

\code{this.dir()} returns the \code{\link[base:normalizePath]{normalized}} path
of the directory in which the executing script is located.

See also \code{\link{here}()} for constructing paths to files, starting with
\code{this.dir()}.

\code{Sys.path()} and \code{Sys.dir()} are versions of \code{this.path()} and
\code{this.dir()} that takes no arguments.
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
\code{\link{tryCatch2}} for inspiration.
  }
}
\details{
There are three ways in which \R code is typically run:
\enumerate{
  \item
    in \sQuote{RStudio} / / \sQuote{Rgui} / / \sQuote{VSCode} by running the
    current line / / selection with the \strong{Run} button / / appropriate
    keyboard shortcut
  \item
    through a source call: a call to function \sourceorlist{}
  \item
    from a shell, such as the Windows command-line / / Unix terminal
}

To retrieve the executing script's filename, first an attempt is made to find a
source call. The calls are searched in reverse order so as to grab the most
recent source call in the case of nested source calls. If a source call was
found, the file argument is returned from the function's evaluation
environment. If you have your own source-like function that you would like to
be recognized by \code{this.path}, please contact the package maintainer so
that it can be implemented or use \code{\link{inside.source}()}.

If no source call is found up the calling stack, then an attempt is made to
figure out how \R is currently being used.

If \R is being run from a shell, the shell arguments are searched for
\option{-f} \option{FILE} or \option{--file=FILE} (the two methods of taking
input from \option{FILE}). \code{this.path()} used to insist that exactly one
of either type of argument must be supplied, but this has since been changed.
Now, the last \option{FILE} argument is extracted (ignoring \option{-f}
\option{-} and \option{--file=-}) and returned. It is an error to use
\code{this.path()} if no arguments of either type are supplied.

If \R is being run from a shell under Unix-alikes with \option{-g} \option{Tk}
or \option{--gui=Tk}, \code{this.path()} will signal an error. \sQuote{Tk} does
not make use of its \option{-f} \option{FILE}, \option{--file=FILE} arguments.

If \R is being run from \sQuote{RStudio}, the active document's filename (the
document in which the cursor is active) is returned (at the time of evaluation).
If the active document is the \R console, the source document's filename (the
document open in the current tab) is returned (at the time of evaluation).
Please note that the source document will \emph{NEVER} be a document open in
another window (with the \strong{Show in new window} button). It is important to
not leave the current tab (either by closing or switching tabs) while any calls
to \code{this.path()} have yet to be evaluated in the run selection. It is an
error for no documents to be open or for a document to not exist (not saved
anywhere).

If \R is being run from \sQuote{Rgui}, the source document's filename (the
document most recently interacted with besides the \R Console) is returned (at
the time of evaluation). Please note that minimized documents \emph{WILL} be
included when looking for the most recently used document. It is important to
not leave the current document (either by closing the document or
interacting with another document) while any calls to \code{this.path()} have
yet to be evaluated in the run selection. It is an error for no documents to be
open or for a document to not exist (not saved anywhere).

If \R is being run \sQuote{VSCode}, the source document's filename is returned
(at the time of evaluation). It is important to not leave the current tab
(either by closing or switching tabs) while any calls to \code{this.path()}
have yet to be evaluated in the run selection. It is an error for a document to
not exist (not saved anywhere).

If \R is being run from \sQuote{AQUA}, the executing script's path cannot be
determined. Unlike \sQuote{RStudio}, \sQuote{Rgui}, and \sQuote{VSCode}, there
is currently no way to request the path of an open document. Until such a time
that there is a method for requesting the path of an open document, consider
using \sQuote{RStudio} or \sQuote{VSCode}.

If \R is being run in another manner, it is an error to use \code{this.path}.

If your GUI of choice is not implemented with \code{this.path}, please contact
the package maintainer so that it can be implemented.
}
\value{
character string; the executing script's filename.
}
% There are a few reasons why there would be no executing script:
% \itemize{
%   \item{
% \R is being run from a shell without supplying argument \option{-f}
% \option{FILE} or \option{--file=FILE}
%   }
%   \item{
% \R is being run from a shell under Unix-alikes with option \option{-g}
% \option{Tk} or \option{--gui=Tk} (GUI \sQuote{Tk} does not make use of its
% \option{-f} \option{FILE} or \option{--file=FILE} arguments)
%   }
%   \item{
% \R is being run from \sQuote{RStudio} with no documents open
%   }
%   \item{
% \R is being run from \sQuote{Rgui} with no documents open
%   }
% }
% The following cases will still throw an error:
% \itemize{
%   \item{
% unable to \code{\link[base:normalizePath]{normalize}} the executing script's
% filename
%   }
%   \item{
% using \syssourcelink{} or \sourcefilelink{} on a file named
% \dQuote{clipboard} or \dQuote{stdin}
%   }
%   \item{
% active / / source document in \sQuote{RStudio} / / \sQuote{Rgui} does not exist
% (not saved anywhere)
%   }
%   \item{
% in \sQuote{AQUA}, running the current line or selection from an open \R script
% (currently unimplemented)
%   }
% }
\note{
The first time \code{this.path()} is called within a script, it will
\code{\link[base:normalizePath]{normalize}} the script's path, checking that
the script exists (throwing an error if it does not), and save it in the
appropriate environment. When \code{this.path()} is called subsequent times
within the same script, it returns the saved path. This will be faster than the
first time, will not check for file existence, and will be independent of the
working directory.

As a side effect, this means that a script can delete itself using
\code{\link[base:files]{file.remove}} or \code{\link[base]{unlink}} but still
know its own path for the remainder of the script.

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
profile file, the user profile, nor the function \code{.First} (see
\code{?\link[base]{Startup}}). This has inconsistent results dependent on the
GUI, and often incorrect. For example:

\describe{
  \item{in \sQuote{RStudio}}{
in all three cases, it throws an error:
\preformatted{
> this.path(original = TRUE)
Error in (.rs.api.getActiveDocumentContext)() :
  RStudio has not finished loading
}
  }
  \item{in \sQuote{VSCode}}{
in the site-wide startup profile file and the function \code{.First}, it throws
an error:
\preformatted{
> this.path(original = TRUE)
Error : RStudio not running
}
but in the user profile, it returns:
#ifdef windows
\preformatted{
> this.path(original = TRUE)
Source: call to function source
[1] "~/.Rprofile"

> this.path()
[1] "C:/Users/iris/Documents/.Rprofile"
}
#endif
#ifdef unix
\preformatted{
> this.path(original = TRUE)
Source: call to function source
[1] "~/.Rprofile"

> this.path()
[1] "/home/iris/.Rprofile"
}
#endif
  }
#ifdef windows
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
#endif
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
}

Sometimes it returns the command line argument \option{FILE}, sometimes it
returns the path of the user profile, and other times it throws an error.
Alternatively, you could use \code{\link{shFILE}()}, supplying a \code{default}
argument when no \option{FILE} is specificed, and supplying an \code{else.}
function for when one is specified.
}
\seealso{
\code{\link{here}}

\code{\link{shFILE}}

\code{\link{wrap.source}}, \code{\link{inside.source}}

\code{\link{this.path-package}}

\sourcelinks{}

\code{\link{R.from.shell}}
}
%\examples{
%\dontrun{
%The following will create a temporary R script containing
%calls to 'this.path'. You should see that 'this.path' works
%through a call to 'source', a call to 'sys.source', a call
%to 'debugSource' (if running from 'RStudio'), and when
%running R from a shell.
%
%Unfortunately, it is impossible to use 'example(this.path)'
%to demonstrate the functionality of 'this.path' in 'RStudio'
%and 'Rgui'. If you would like to see this functionality, you
%could try this:
%* make a new R script containing just this one command:
%    this.path::this.path(verbose = TRUE)
%* open this script in 'RStudio' or 'Rgui'
%* run that command directly from the script
%  (both should print "Source: active document ..." along
%      with the script's path)
%* copy and paste that command into the R Console and run
%  that command again
%  (both should print "Source: source document ..." along
%      with the script's path)
%* try closing all your documents and run that same command
%  in the R Console
%  (both should raise an error "R is being run ... with no
%      documents open")
%}
%
%tryCatch((function() {
%    .interactive <- interactive()
%    if (.interactive) {
%        cat("\\n")
%
%
%        ask <- function(question) {
%            prompt <- paste0(question, " (Yes/No/Cancel): ")
%            done <- FALSE
%            for (n in seq_len(100L)) {
%                response <- tolower(substr(readline(prompt), 1, 1))
%                if (response \%in\% c("y", "n", "c")) {
%                  done <- TRUE
%                  break
%                }
%            }
%            if (!done)
%                stop("unable to get response")
%            response
%        }
%
%
%        response <- ask("Would you like to run this example interactively?")
%        if (response == "c") {
%            cat("\\n")
%            return(invisible())
%        }
%        .interactive <- response == "y"
%    }
%    if (.interactive) {
%        pressEnter2Continue <- function(x = "\\n",
%            prompt = "Hit <Return> to continue: ") {
%            readline(prompt)
%            cat(x)
%        }
%    }
%    else pressEnter2Continue <- function(...) NULL
%
%
%    oopt <- options(useFancyQuotes = TRUE)
%    on.exit(options(oopt))
%
%
%    # minor change for macOS, normalize the 'tempdir' first
%    tmpdir <- normalizePath(tempdir(check = TRUE),
%        winslash = "/", mustWork = TRUE)
%
%
%    FILE <- tryCatch({
%        normalizePath(tempfile("scrp", tmpdir = tmpdir, fileext = ".R"),
%            winslash = "/", mustWork = FALSE)
%    }, condition = function(c) {
%        stop(errorCondition(paste0(conditionMessage(c),
%            "\\nunable to create temporary R script"),
%            call = conditionCall(c)))
%    })
%    on.exit(unlink(FILE), add = TRUE)
%
%
%    results.file <- tryCatch({
%        .Sys.time <- format(Sys.time(), "\%Y-\%m-\%d_\%H.\%M.\%OS_")
%        normalizePath(tempfile(paste0("rslt", .Sys.time),
%            tmpdir = tmpdir, fileext = ".txt"),
%            winslash = "/", mustWork = FALSE)
%    }, condition = base::as.null)
%    on.exit(unlink(results.file), add = TRUE)
%
%
%    write.results <- function(expr, split = FALSE) {
%        if (!is.null(results.file)) {
%            sink(file = results.file, append = TRUE, split = split)
%            on.exit(sink())
%        }
%        expr
%    }
%
%
%    cat2 <- function(msg, ..., appendLF = TRUE) {
%        cat(if (appendLF) "\\n", paste0(strwrap(msg, exdent = 2),
%            "\\n", collapse = ""), ..., sep = "")
%    }
%
%
%    FILEcode <- substitute({
%        options(useFancyQuotes = TRUE)
%        x <- `FILE sub`
%        results.file <- `results.file sub`
%        write.results <- `write.results sub`
%        cat2 <- `cat2 sub`
%        cat("\\n")
%        write.results({
%            y <- tryCatch({
%                this.path::this.path(verbose = TRUE)
%            }, condition = base::force)
%            cat("this.path status: ")
%        })
%        if (inherits(y, "condition")) {
%            msg <- conditionMessage(y)
%            call <- conditionCall(y)
%            write.results({
%                if (!is.null(call))
%                  cat("Error in ", deparse(call), " :\\n  ", msg, "\\n",
%                      sep = "")
%                else cat("Error: ", msg, "\\n", sep = "")
%            }, split = TRUE)
%            cat(sQuote("this.path"), " could not determine the executing ",
%                "script's filename\\n", sep = "")
%        }
%        else {
%            cat("Executing script's filename:\\n")
%            cat(sQuote(x), "\\n\\n", sep = "")
%            cat("Executing script's filename (as determined by ",
%                sQuote("this.path"), "):\\n", sep = "")
%            cat(sQuote(this.path::this.path(verbose = TRUE)), "\\n", sep = "")
%
%
%            if (x == y) {
%                write.results(cat("success\\n"))
%            }
%            else if (basename(dirname(x)) == basename(dirname(y)) &&
%                basename(x) == basename(y)) {
%                cat2(paste0(
%                    "Hmm ... looks like these paths are equivalent, it is ",
%                    "likely that they refer to the same file but have ",
%                    "different paths because of a hard link or multiple mounts",
%                    " (see ?normalizePath)"
%                ))
%                write.results(cat("success?\\n"))
%            }
%            else {
%                cat("\\n")
%                write.results({
%                    cat("Error: ", sQuote("this.path"), " could not correctly ",
%                        "determine the executing script's filename\\n", sep = "")
%                }, split = TRUE)
%            }
%        }
%    }, list(
%        `write.results sub` = write.results,
%        `FILE sub`          = FILE,
%        `results.file sub`  = results.file,
%        `cat2 sub`          = cat2
%    ))
%
%
%    tryCatch({
%        this.path:::write.code(FILEcode, FILE)
%    }, condition = function(c) {
%        stop(errorCondition(paste0(conditionMessage(c),
%            "\\nunable to write R code to file ", sQuote(FILE)),
%            call = conditionCall(c)))
%    })
%
%
%    cat2(paste0("Created an example R script. This script will be run in ",
%        "all possible ways that are compatible with ", sQuote("this.path"),
%        " that are currently available."))
%    if (.interactive) {
%        response <- ask("Would you like to see the script?")
%        if (response == "y") {
%            cat2(paste0("Attempting to open the example R script. If the ",
%                "script did not open automatically, the script's path is:"),
%                sQuote(normalizePath(FILE)), "\\n")
%            tryCatch({
%                this.path::file.open(FILE)
%            }, condition = base::invisible)
%            pressEnter2Continue("")
%        }
%    }
%
%
%    write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
%        " when using ", sQuote("source")), appendLF = FALSE))
%
%
%    tryCatch({
%        cat("\\n* first, using ", sQuote("source"), "\\n", sep = "")
%        source(FILE, local = TRUE)
%        pressEnter2Continue("")
%    }, condition = function(c) {
%        msg <- paste0(conditionMessage(c), "\\nunable to source file ",
%            sQuote(FILE))
%        call <- conditionCall(c)
%        if (!is.null(call))
%            cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
%        else cat("Error: ", msg, "\\n", sep = "")
%    })
%
%
%    write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
%        " when using ", sQuote("sys.source"))))
%
%
%    tryCatch({
%        cat("\\n* second, using ", sQuote("sys.source"), "\\n", sep = "")
%        sys.source(FILE, envir = environment())
%        pressEnter2Continue("")
%    }, condition = function(c) {
%        msg <- paste0(conditionMessage(c), "\\nunable to source file ",
%            sQuote(FILE))
%        call <- conditionCall(c)
%        if (!is.null(call))
%            cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
%        else cat("Error: ", msg, "\\n", sep = "")
%    })
%
%
%    if (.Platform$GUI == "RStudio") {
%        write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
%            " when using ", sQuote("debugSource"))))
%
%
%        tryCatch({
%            cat("\\n* third, using ", sQuote("debugSource"),
%                " from ", sQuote("RStudio"), "\\n", sep = "")
%            debug.source <- this.path:::tools.rstudio("debugSource")
%            debug.source(FILE, local = TRUE)
%            pressEnter2Continue("")
%        }, condition = function(c) {
%            msg <- paste0(conditionMessage(c), "\\nunable to source file ",
%                sQuote(FILE))
%            call <- conditionCall(c)
%            if (!is.null(call))
%                cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
%            else cat("Error: ", msg, "\\n", sep = "")
%        })
%    }
%    else write.results({
%        cat2(paste0("Unfortunately, it is impossible to demonstrate the ",
%            "functionality of ", sQuote("this.path"), " when using ",
%            sQuote("debugSource"), " because ", sQuote("RStudio"), " is not ",
%            "presently running."))
%    })
%
%
%    if (!isNamespaceLoaded("testthat")) {
%        if (.interactive) {
%            cat2(paste0(sQuote("this.path"), " also works with function ",
%                sQuote("source_file"), " from package ", sQuote("testthat"),
%                ", but this package is not presently loaded."))
%            response <- ask("Would you like to load this package?")
%        }
%        else response <- "y"
%        if (response == "y") {
%            if (requireNamespace("testthat", quietly = TRUE)) {
%                on.exit(unloadNamespace("testthat"), add = TRUE)
%                cat2(paste0("Package ", sQuote("testthat"), " will be ",
%                  "unloaded once the example concludes."))
%            }
%            else if (.interactive && !"testthat" \%in\%
%                rownames(utils::installed.packages())) {
%                cat2(paste0("Package ", sQuote("testthat"), " was not ",
%                  "successfully loaded, possibly because it is not installed."))
%                response <- ask("Would you like to install this package?")
%                if (response == "y") {
%                  cat("\\n")
%                  tryCatch({
%                    utils::install.packages("testthat")
%                  }, error = base::invisible)
%                  if (requireNamespace("testthat", quietly = TRUE)) {
%                    on.exit(unloadNamespace("testthat"), add = TRUE)
%                    cat2(paste0("Package ", sQuote("testthat"), " will be ",
%                      "unloaded once the example concludes."))
%                  }
%                  else cat2(paste0("Package ", sQuote("testthat"), " was not ",
%                    "successfully installed and loaded."))
%                }
%            }
%            else cat2(paste0("Package ", sQuote("testthat"), " was not ",
%                "successfully loaded."))
%        }
%    }
%    if (isNamespaceLoaded("testthat")) {
%        write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
%            " when using ", sQuote("testthat::source_file"))))
%
%
%        tryCatch({
%            cat("\\n* ", if (.Platform$GUI != "RStudio")
%                "third"
%            else "fourth", ", using ", sQuote("testthat::source_file"), "\\n",
%                sep = "")
%            testthat::source_file(FILE)
%            pressEnter2Continue("")
%        }, condition = function(c) {
%            msg <- paste0(conditionMessage(c), "\\nunable to source file ",
%                sQuote(FILE))
%            call <- conditionCall(c)
%            if (!is.null(call))
%                cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
%            else cat("Error: ", msg, "\\n", sep = "")
%        })
%    }
%    else write.results({
%        cat2(paste0("Unfortunately, it is impossible to demonstrate the ",
%            "functionality of ", sQuote("this.path"), " when using ",
%            sQuote("source_file"), " because package ", sQuote("testthat"),
%            " is not presently loaded."))
%    })
%
%
%    cmt <- if (.Platform$OS.type == "windows")
%        "Windows command-line"
%    else "Unix terminal"
%    write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
%        " when running from the ", cmt)))
%
%
%    tryCatch({
%        cat("\\n* last, running from the ", cmt, "\\n\\n", sep = "")
%        this.path::Rscript("--default-packages=NULL", FILE)
%        pressEnter2Continue()
%    }, condition = function(c) {
%        msg <- paste0(conditionMessage(c), "\\nunable to run file ",
%            sQuote(FILE), "\\n from the ", cmt)
%        call <- conditionCall(c)
%        if (!is.null(call))
%            cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
%        else cat("Error: ", msg, "\\n", sep = "")
%    })
%
%
%    write.results({
%        cat2(paste0("Unfortunately, it is impossible to use ",
%            sQuote("example(this.path)"), " to demonstrate the functionality ",
%            "of ", sQuote("this.path"), " in ", sQuote("RStudio"), " and ",
%            sQuote("Rgui"), ". If you would like to see this functionality, ",
%            "you could try this:"),
%            "* make a new R script containing just this one command:\\n",
%            "    this.path::this.path(verbose = TRUE)\\n",
%            "* open this script in ", sQuote("RStudio"), " or ", sQuote("Rgui"),
%                "\\n",
%            "* run that command directly from the script\\n",
%            "    (both should print \\"Source: active document ...\\" along ",
%                "with the script's path)\\n",
%            "* copy and paste that command into the R Console and run that ",
%                "command again\\n",
%            "    (both should print \\"Source: source document ...\\" along ",
%                "with the script's path)\\n",
%            "* try closing all your documents and run that same command in ",
%                "the R Console\\n",
%            "    (both should raise an error \\"R is being run ... with no ",
%                "documents open\\")\\n")
%    })
%
%
%    write.results({
%        cat2(paste0("If ", sQuote("this.path"), " did not correctly determine ",
%            "the executing script's filename, please send a bug report ",
%            "using:"), "\\nutils::bug.report(package = \\"this.path\\")\\n")
%    })
%
%
%    if (.interactive) {
%        this.path::file.open(results.file)
%        pressEnter2Continue(prompt = "Hit <Return> to delete tempfiles: ")
%    }
%    else if (!is.null(results.file)) {
%        cat("\\n* results\\n\\n")
%        writeLines(readLines(results.file))
%    }
%    invisible()
%})(), error = function(c) {
%    msg <- conditionMessage(c)
%    call <- conditionCall(c)
%    if (!is.null(call))
%        cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
%    else cat("Error: ", msg, "\\n", sep = "")
%})
%
%
%
%
%
\examples{\dontshow{.unload.testthat <- !isNamespaceLoaded("testthat") ; .unload.knitr <- !isNamespaceLoaded("knitr")}
FILE <- tempfile(fileext = ".R")
this.path:::write.code({
    withAutoprint({
        cat(sQuote(this.path::this.path(verbose = TRUE, default = {
            stop("since the executing script's path will be found, argument\\n",
                " 'default' will not be evaluated, so this error will not\\n",
                " be thrown! you can use this to your advantage in a\\n",
                " similar manner, doing arbitrary things only if the\\n",
                " executing script does not exist")
        })), "\\n\\n")
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L)
}, FILE)


source(FILE, verbose = FALSE)
sys.source(FILE, envir = environment())
if (.Platform$GUI == "RStudio")
    get("debugSource", "tools:rstudio", inherits = FALSE)(FILE)
if (requireNamespace("testthat"))
    testthat::source_file(FILE, chdir = FALSE, wrap = FALSE)
if (requireNamespace("knitr")) {
    writeLines(con = FILE2 <- tempfile(fileext = ".Rmd"), c(
        "```{r}",
        # same expression as above
        deparse(parse(FILE)[[c(1L, 2L, 2L)]]),
        "```"
    ))
    knitr::knit(input = FILE2, output = FILE3 <- tempfile(fileext = ".md"),
        quiet = TRUE)
    cat(sprintf("\\n$ cat -bsT \%s\\n", shQuote(FILE2)))
    this.path:::cat.file(FILE2, number.nonblank = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE)
    cat(sprintf("\\n$ cat -bsT \%s\\n", shQuote(FILE3)))
    this.path:::cat.file(FILE3, number.nonblank = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE)
    cat("\\n")
    unlink(c(FILE3, FILE2))
}


this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE))


# this.path also works when source-ing a URL
# (included tryCatch in case an internet connection is not available)
tryCatch({
    source("https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R")
}, condition = message)


for (expr in c("this.path()",
               "this.path(default = NULL)",
               "this.dir()",
               "this.dir(default = NULL)",
               "this.dir(default = getwd())"))
{
    cat("\\n\\n")
    this.path:::.Rscript(c("--default-packages=this.path", "--vanilla", "-e", expr))
}


# an example from R package 'logr'
this.path::this.path(verbose = FALSE, default = "script.log",
    else. = function(path) {
        # replace extension (probably .R) with .log
        this.path::ext(path) <- ".log"
        path
        # or you could use paste0(this.path::removeext(path), ".log")
    })
\dontshow{unlink(FILE); if (.unload.testthat) unloadNamespace("testthat"); if (.unload.knitr) unloadNamespace("knitr")}}
