\name{this.path}
\alias{this.path}
\alias{this.dir}
\title{Determine Executing Script's Filename
}
\description{
\code{this.path()} returns the \code{\link[base:normalizePath]{normalized}} path
of the executing script.

\code{this.dir()} is a shorter way of writing
\code{\link[base:basename]{dirname}(this.path())}, returning the
\code{\link[base:normalizePath]{normalized}} path of the directory in which the
executing script is located.
}
\usage{
this.path(verbose = getOption("verbose"))
this.dir(...)
}
\arguments{
  \item{verbose}{
\code{TRUE} or \code{FALSE}; should the method in which the path of the
executing script was determined be printed?
  }
  \item{\dots}{
arguments passed to \code{this.path}.
  }
}
\details{
There are three ways in which \R code is typically run; in \sQuote{RStudio} or
\sQuote{Rgui} by running the current line or selection with the \strong{Run}
button (or appropriate keyboard shortcut), through a source call (a call to
function \code{\link[base]{source}}, \code{\link[base]{sys.source}},
\href{https://support.rstudio.com/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE}{
\code{debugSource}} (\sQuote{RStudio} exclusive), or
\code{\link[testthat:source_file]{testthat::source_file}}), and from the Windows
command-line / / Unix terminal.

To retrieve the executing script's filename, first an attempt is made to find a
source call. The calls are searched in reverse order so as to grab the most
recent source call in the case of nested source calls. If a source call was
found, the argument \var{file} (\var{fileName} in the case of
\code{debugSource}, \var{path} in the case of \code{testthat::source_file}) is
returned from the function's evaluation environment.

If no source call is found up the calling stack, then an attempt is made to
figure out how \R is currently being used.

If \R is being run from the command-line / / terminal, the command-line
arguments are searched for \option{-f} \option{FILE} or \option{--file=FILE}
(the two methods of taking input from \option{FILE}). If exactly one of either
type of argument is supplied, the text \option{FILE} is returned. It is an error
to use \code{this.path} when none or multiple arguments of either type are
supplied.

If \R is being run from \sQuote{RStudio}, the active document's filename (the
document in which the cursor is active) is returned (at the time of evaluation).
If the active document is the \R console, the source document's filename (the
document open in the current tab) is returned (at the time of evaluation).
Please note that the source document will \emph{NEVER} be a document open in
another window (with the \strong{Show in new window} button). It is important to
not leave the current tab (either by closing or switching tabs) while any calls
to \code{this.path} have yet to be evaluated in the run selection. It is an
error for no documents to be open or for a document to not exist (not saved
anywhere).

If \R is being run from \sQuote{Rgui}, the source document's filename (the
document most recently interacted with besides the \R Console) is returned (at
the time of evaluation). Please note that minimized documents will be
\emph{IGNORED}. It is important to not leave the current document (either by
closing the document or interacting with another document) while any calls to
\code{this.path} have yet to be evaluated in the run selection. It is an error
for no documents to be open or for a document to not exist (not saved anywhere).

If \R is being run in another manner, it is an error to use \code{this.path}.
}
\value{
character string; the executing script's filename.
}
\note{
The first time \code{this.path} is called within a script, it will
\code{\link[base:normalizePath]{normalize}} the script's path, check that the
script exists (throwing an error if it does not), and save it in the appropriate
environment. When \code{this.path} is called subsequent times within the same
script, it returns the saved path. This will be faster than the first time, will
not check for file existence, and will not depend on the working directory. This
means that a script can delete itself using
\code{\link[base:files]{file.remove}} or \code{\link[base]{unlink}} but still
know its own path for the remainder of the script.

Within a script that contains calls to both \code{this.path} and
\code{\link[base:getwd]{setwd}}, \code{this.path} \emph{MUST} be used
\emph{AT LEAST} once before the first call to \code{setwd} since the script's
path is normalized against the working directory.
}
\seealso{
\code{\link{this.path-package}}

\code{\link[base]{source}}

\code{\link[base]{sys.source}}

\code{\link[testthat:source_file]{testthat::source_file}}

\code{\link{Running.R.from.command-line}}
}
\examples{
\dontrun{
The following will create a temporary R script containing
calls to 'this.path'. You should see that 'this.path' works
through a call to 'source', a call to 'sys.source', a call
to 'debugSource' (if running from 'RStudio'), and when
running R from the command-line / / terminal.

Unfortunately, it is impossible to use 'example(this.path)'
to demonstrate the functionality of 'this.path' in 'RStudio'
and 'Rgui'. If you would like to see this functionality, you
could try this:
* make a new R script containing just this one command:
    this.path::this.path(verbose = TRUE)
* open this script in 'RStudio' or 'Rgui'
* run that command directly from the script
  (both should print "Source: active document ..." along
      with the script's path)
* copy and paste that command into the R Console and run
  that command again
  (both should print "Source: source document ..." along
      with the script's path)
* try closing all your documents and run that same command
  in the R Console
  (both should raise an error "R is being run ... with no
      documents open")
}

tryCatch((function() {
    .interactive <- interactive()
    if (.interactive) {
        cat("\\n")


        ask <- function(question) {
            prompt <- paste0(question, " (Yes/No/Cancel): ")
            done <- FALSE
            for (n in seq_len(100L)) {
                response <- tolower(substr(readline(prompt), 1, 1))
                if (response \%in\% c("y", "n", "c")) {
                  done <- TRUE
                  break
                }
            }
            if (!done)
                stop("unable to get response")
            response
        }


        response <- ask("Would you like to run this example interactively?")
        if (response == "c") {
            cat("\\n")
            return(invisible())
        }
        .interactive <- response == "y"
    }
    if (.interactive) {
        pressEnter2Continue <- function(x = "\\n",
            prompt = "Hit <Return> to continue: ") {
            readline(prompt)
            cat(x)
        }
    }
    else pressEnter2Continue <- function(...) NULL


    oopt <- options(useFancyQuotes = TRUE)
    on.exit(options(oopt))


    FILE <- tryCatch({
        normalizePath(tempfile("scrp",
            tmpdir = tempdir(check = TRUE), fileext = ".R"),
            winslash = "/", mustWork = FALSE)
    }, condition = function(c) {
        stop(errorCondition(paste0(conditionMessage(c),
            "\\nunable to create temporary R script"),
            call = conditionCall(c)))
    })
    on.exit(unlink(FILE), add = TRUE)


    results.file <- tryCatch({
        .Sys.time <- format(Sys.time(), "\%Y-\%m-\%d_\%H.\%M.\%OS_")
        normalizePath(tempfile(paste0("rslt", .Sys.time),
            tmpdir = dirname(FILE), fileext = ".txt"),
            winslash = "/", mustWork = FALSE)
    }, condition = base::as.null)
    on.exit(unlink(results.file), add = TRUE)


    write.results <- function(expr, split = FALSE) {
        if (!is.null(results.file)) {
            sink(file = results.file, append = TRUE, split = split)
            on.exit(sink())
        }
        expr
    }


    cat2 <- function(msg, ..., appendLF = TRUE) {
        cat(if (appendLF) "\\n", paste0(strwrap(msg, exdent = 2),
            "\\n", collapse = ""), ..., sep = "")
    }


    FILEcode <- substitute({
        options(useFancyQuotes = TRUE)
        x <- `FILE sub`
        results.file <- `results.file sub`
        write.results <- `write.results sub`
        cat2 <- `cat2 sub`
        cat("\\n")
        write.results({
            y <- tryCatch({
                this.path::this.path(verbose = TRUE)
            }, condition = base::force)
            cat("this.path status: ")
        })
        if (inherits(y, "condition")) {
            msg <- conditionMessage(y)
            call <- conditionCall(y)
            write.results({
                if (!is.null(call))
                  cat("Error in ", deparse(call), " :\\n  ", msg, "\\n",
                      sep = "")
                else cat("Error: ", msg, "\\n", sep = "")
            }, split = TRUE)
            cat(sQuote("this.path"), " could not determine the executing ",
                "script's filename\\n", sep = "")
        }
        else {
            cat("Executing script's filename:\\n")
            cat(sQuote(x), "\\n\\n", sep = "")
            cat("Executing script's filename (as determined by ",
                sQuote("this.path"), "):\\n", sep = "")
            cat(sQuote(this.path::this.path(verbose = TRUE)), "\\n", sep = "")


            if (x == y) {
                write.results(cat("success\\n"))
            }
            else if (basename(dirname(x)) == basename(dirname(y)) &&
                basename(x) == basename(y)) {
                cat2(paste0(
                    "Hmm ... looks like these paths are equivalent, it's ",
                    "likely that they refer to the same file but have ",
                    "different paths because of a hard link or multiple mounts",
                    " (see ?normalizePath)"
                ))
                write.results(cat("success?\\n"))
            }
            else {
                cat("\\n")
                write.results({
                    cat("Error: ", sQuote("this.path"), " could not correctly ",
                        "determine the executing script's filename\\n", sep = "")
                }, split = TRUE)
            }
        }
    }, list(
        `write.results sub` = write.results,
        `FILE sub`          = FILE,
        `results.file sub`  = results.file,
        `cat2 sub`          = cat2
    ))


    writeRcode2file <- function(x, file) {
        tryCatch({
            x <- vapply(as.list(FILEcode[-1]), "deparse1",
                width.cutoff = 60L, collapse = "\\n", "")
            writeLines(x, file)
        }, condition = function(c) {
            stop(errorCondition(paste0(conditionMessage(c),
                "\\nunable to write R code to file ", sQuote(file)),
                call = conditionCall(c)))
        })
    }


    writeRcode2file(FILEcode, FILE)


    cat2(paste0("Created an example R script. This script will be run in ",
        "all possible ways that are compatible with ", sQuote("this.path"),
        " that are currently available."))
    if (.interactive) {
        response <- ask("Would you like to see the script?")
        if (response == "y") {
            cat2(paste0("Attempting to open the example R script. If the ",
                "script did not open automatically, the script's path is:"),
                sQuote(normalizePath(FILE)), "\\n")
            tryCatch({
                this.path::file.open(FILE)
            }, condition = base::invisible)
            pressEnter2Continue("")
        }
    }


    write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
        " when using ", sQuote("source")), appendLF = FALSE))


    tryCatch({
        cat("\\n* first, using ", sQuote("source"), "\\n", sep = "")
        source(FILE, local = TRUE)
        pressEnter2Continue("")
    }, condition = function(c) {
        msg <- paste0(conditionMessage(c), "\\nunable to source file ",
            sQuote(FILE))
        call <- conditionCall(c)
        if (!is.null(call))
            cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
        else cat("Error: ", msg, "\\n", sep = "")
    })


    write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
        " when using ", sQuote("sys.source"))))


    tryCatch({
        cat("\\n* second, using ", sQuote("sys.source"), "\\n", sep = "")
        sys.source(FILE, envir = environment())
        pressEnter2Continue("")
    }, condition = function(c) {
        msg <- paste0(conditionMessage(c), "\\nunable to source file ",
            sQuote(FILE))
        call <- conditionCall(c)
        if (!is.null(call))
            cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
        else cat("Error: ", msg, "\\n", sep = "")
    })


    if (.Platform$GUI == "RStudio") {
        write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
            " when using ", sQuote("debugSource"))))


        tryCatch({
            cat("\\n* third, using ", sQuote("debugSource"),
                " from ", sQuote("RStudio"), "\\n", sep = "")
            debug.source <- this.path:::tools.rstudio("debugSource")
            debug.source(FILE, local = TRUE)
            pressEnter2Continue("")
        }, condition = function(c) {
            msg <- paste0(conditionMessage(c), "\\nunable to source file ",
                sQuote(FILE))
            call <- conditionCall(c)
            if (!is.null(call))
                cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
            else cat("Error: ", msg, "\\n", sep = "")
        })
    }
    else write.results({
        cat2(paste0("Unfortunately, it is impossible to demonstrate the ",
            "functionality of ", sQuote("this.path"), " when using ",
            sQuote("debugSource"), " because ", sQuote("RStudio"), " is not ",
            "presently running."))
    })


    if (!isNamespaceLoaded("testthat")) {
        if (.interactive) {
            cat2(paste0(sQuote("this.path"), " also works with function ",
                sQuote("source_file"), " from package ", sQuote("testthat"),
                ", but this package is not presently loaded."))
            response <- ask("Would you like to load this package?")
        }
        else response <- "y"
        if (response == "y") {
            if (requireNamespace("testthat", quietly = TRUE)) {
                on.exit(unloadNamespace("testthat"), add = TRUE)
                cat2(paste0("Package ", sQuote("testthat"), " will be ",
                  "unloaded once the example concludes."))
            }
            else if (.interactive && !"testthat" \%in\%
                rownames(utils::installed.packages())) {
                cat2(paste0("Package ", sQuote("testthat"), " was not ",
                  "successfully loaded, possibly because it is not installed."))
                response <- ask("Would you like to install this package?")
                if (response == "y") {
                  cat("\\n")
                  tryCatch({
                    utils::install.packages("testthat")
                  }, error = base::invisible)
                  if (requireNamespace("testthat", quietly = TRUE)) {
                    on.exit(unloadNamespace("testthat"), add = TRUE)
                    cat2(paste0("Package ", sQuote("testthat"), " will be ",
                      "unloaded once the example concludes."))
                  }
                  else cat2(paste0("Package ", sQuote("testthat"), " was not ",
                    "successfully installed and loaded."))
                }
            }
            else cat2(paste0("Package ", sQuote("testthat"), " was not ",
                "successfully loaded."))
        }
    }
    if (isNamespaceLoaded("testthat")) {
        write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
            " when using ", sQuote("testthat::source_file"))))


        tryCatch({
            cat("\\n* ", if (.Platform$GUI != "RStudio")
                "third"
            else "fourth", ", using ", sQuote("testthat::source_file"), "\\n",
                sep = "")
            testthat::source_file(FILE)
            pressEnter2Continue("")
        }, condition = function(c) {
            msg <- paste0(conditionMessage(c), "\\nunable to source file ",
                sQuote(FILE))
            call <- conditionCall(c)
            if (!is.null(call))
                cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
            else cat("Error: ", msg, "\\n", sep = "")
        })
    }
    else write.results({
        cat2(paste0("Unfortunately, it is impossible to demonstrate the ",
            "functionality of ", sQuote("this.path"), " when using ",
            sQuote("source_file"), " because package ", sQuote("testthat"),
            " is not presently loaded."))
    })


    cmt <- if (.Platform$OS.type == "windows")
        "Windows command-line"
    else "Unix terminal"
    write.results(cat2(paste0("Attempting to use ", sQuote("this.path"),
        " when running from the ", cmt)))


    tryCatch({
        cat("\\n* last, running from the ", cmt, "\\n\\n", sep = "")
        this.path::Rscript("--default-packages=NULL", FILE)
        pressEnter2Continue()
    }, condition = function(c) {
        msg <- paste0(conditionMessage(c), "\\nunable to run file ",
            sQuote(FILE), "\\n from the ", cmt)
        call <- conditionCall(c)
        if (!is.null(call))
            cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
        else cat("Error: ", msg, "\\n", sep = "")
    })


    write.results({
        cat2(paste0("Unfortunately, it is impossible to use ",
            sQuote("example(this.path)"), " to demonstrate the functionality ",
            "of ", sQuote("this.path"), " in ", sQuote("RStudio"), " and ",
            sQuote("Rgui"), ". If you would like to see this functionality, ",
            "you could try this:"),
            "* make a new R script containing just this one command:\\n",
            "    this.path::this.path(verbose = TRUE)\\n",
            "* open this script in ", sQuote("RStudio"), " or ", sQuote("Rgui"),
                "\\n",
            "* run that command directly from the script\\n",
            "    (both should print \\"Source: active document ...\\" along ",
                "with the script's path)\\n",
            "* copy and paste that command into the R Console and run that ",
                "command again\\n",
            "    (both should print \\"Source: source document ...\\" along ",
                "with the script's path)\\n",
            "* try closing all your documents and run that same command in ",
                "the R Console\\n",
            "    (both should raise an error \\"R is being run ... with no ",
                "documents open\\")\\n")
    })


    write.results({
        cat2(paste0("If ", sQuote("this.path"), " did not correctly determine ",
            "the executing script's filename, please send a bug report to the ",
            "package maintainer, ",
            utils::packageDescription("this.path")$Maintainer, ". Please ",
            "include your session information in your bug report, which can ",
            "be found with the following command:"), "utils::sessionInfo()\\n")
    })


    if (.interactive) {
        this.path::file.open(results.file)
        pressEnter2Continue(prompt = "Hit <Return> to delete tempfiles: ")
    }
    else if (!is.null(results.file)) {
        cat("\\n* results\\n\\n")
        writeLines(readLines(results.file))
    }
    invisible()
})(), error = function(c) {
    msg <- conditionMessage(c)
    call <- conditionCall(c)
    if (!is.null(call))
        cat("Error in ", deparse(call), " :\\n  ", msg, "\\n", sep = "")
    else cat("Error: ", msg, "\\n", sep = "")
})
}
