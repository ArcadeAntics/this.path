\name{this.path}
\alias{this.path}
\alias{this.dir}
\title{Determine Executing Script's Filename
}
\description{
\code{this.path()} returns the full path of the executing script.
\code{this.dir()} is a shortcut for \code{dirname(this.path())}, returning the
full path of the directory where the executing script is located.
}
\usage{
this.path(verbose = getOption("verbose"))
this.dir(verbose = getOption("verbose"))
}
\arguments{
  \item{verbose}{
TRUE or FALSE; controls whether the method of determining the path is printed
}
}
\details{
There are three ways in which R code is typically run; in \sQuote{RStudio} or
\sQuote{RGui} by running the current line or selection with the \strong{Run}
button (or appropriate keyboard shortcut), through a source call (a call to
function \code{base::source} or \code{base::sys.source} or \code{debugSource}
(\sQuote{RStudio} exclusive)), and finally from the command-line / / terminal.

To retrieve the executing script's filename, first an attempt is made to find a
source call. The calls are searched in reverse order so as to grab the most
recent source call in the case of nested source calls. If a source call was
found, the argument \var{file} (\var{fileName} in the case of
\code{debugSource}) is returned from the function's evaluation environment (not
the function's environment).

If no source call is found up the calling stack, then an attempt is made to
figure out how R is currently being used.

If R is being run from the command-line / / terminal, the command-line arguments
are searched for \option{-f file} or \option{--file=file} (the two methods of
taking input from \sQuote{file}). If \option{-f file} is used, then
\sQuote{file} is returned. If \option{--file=file} is used, then the text
following \option{--file=} is returned. When multiple arguments of either type
are supplied, the last of these arguments is returned (with a warning). It is an
error to use \code{this.path} when no arguments of either type are supplied.

If R is being run from \sQuote{RStudio}, the source document's filename (the
document open in the current tab) is returned (at the time of evaluation). It is
important to not leave the current tab (either by closing or switching tabs)
while any calls to \code{this.path} have yet to be evaluated in the run
selection. It is an error for no documents to be open or for a document to not
exist (not saved anywhere).

If R is being run from \sQuote{RGui}, the source document's filename (the
document most recently interacted with besides the R Console) is returned (at
the time of evaluation). It is important to not leave the current document
(either by closing the document or interacting with another document) while any
calls to \code{this.path} have yet to be evaluated in the run selection. It is
an error for no documents to be open or for a document to not exist (not saved
anywhere).

If R is being run in another manner, it is an error to use \code{this.path}.
}
\value{
A character vector of length 1; the executing script's filename.
}
\seealso{
\code{\link[base]{source}}

\code{\link[base]{sys.source}}

\code{\link[utils]{Rscript}}
}
\examples{
\dontrun{
To run the following example, try running this command:
example(topic = "this.path", package = "this.path")

The following will create two temporary directories and three temporary R scripts,
each containing calls to 'this.path'. You should see that it works through a source
call, nested source calls, and when running R from the command-line.

Unfortunately, it is impossible to use 'example(topic = "this.path", package = "this.path")'
to demonstrate the functionality of 'this.path' in 'RStudio' and 'RGui'. If you would
like to see this functionality, you could try this:
* make a new R script containing just this one command:
    this.path::this.path(verbose = TRUE)
* open this script in 'RStudio' or 'RGui'
* run that command directly from the script
    (both should print "Source: active document ..." along with the script's path)
* copy and paste that command into the R Console and run that command again
    (both should print "Source: source document ..." along with the script's path)
* try closing all your documents and run that same command in the R Console
    (both should raise an error "R is being run ... with no documents open")
}

tryCatch((function() {
    .interactive <- interactive()
    if (.interactive) {
        cat("\n")
        prompt <- "Would you like to run this example interactively? (Yes/No/Cancel): "
        repeat {
            tmp <- tolower(substr(readline(prompt), 1L, 1L))
            if (tmp \%in\% c("y", "n", "c"))
                break
        }
        if (tmp == "c") {
            cat("\n")
            return(invisible())
        }
        .interactive <- tmp == "y"
    }
    if (.interactive) {
        pressEnter2Continue <- function(x = "\n") {
            readline("Hit <Return> to continue: ")
            cat(x)
        }
        cat2 <- function(..., sep = " ", timeout = 0.025) {
                x <- list(...)
            x <- vapply(X = x, FUN = "paste0", collapse = sep, FUN.VALUE = "")
            x <- paste0(x, collapse = sep)
            for (char in strsplit(x, split = NULL)[[1L]]) {
                Sys.sleep(timeout)
                base::cat(char)
            }
        }
    }
    else {
        pressEnter2Continue <- function(...) NULL
        cat2 <- function(...) NULL
    }


    tmpdir <- tryCatch({
        tempdir(check = TRUE)
    }, warning = function(c) {
        cat("\nunable to create temporary directory using 'tempdir'\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    }, error = function(c) {
        cat("\nunable to create temporary directory using 'tempdir'\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    })
    if (is.null(tmpdir))
        return(invisible())


    oopt <- options(useFancyQuotes = TRUE)
    on.exit(options(oopt), add = TRUE)


    tmp.main.dir <- tryCatch({
        tmp.main.dir <- tempfile(pattern = "project folder ", tmpdir = tmpdir)
        on.exit(unlink(tmp.main.dir, recursive = TRUE, force = TRUE), add = TRUE)
        dir.create(tmp.main.dir, recursive = TRUE)
        tmp.main.dir
    }, warning = function(c) {
        cat("\nunable to create temporary directory\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    }, error = function(c) {
        cat("\nunable to create temporary directory\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    })
    if (is.null(tmp.main.dir))
        return(invisible())


    tmp.dirs <- tryCatch({
        tmp.dirs <- normalizePath(file.path(tmp.main.dir,
            c("code 1", "code 2", "input", "output")), mustWork = FALSE)
        lapply(X = tmp.dirs, FUN = "dir.create", showWarnings = FALSE,
            recursive = TRUE)
        tmp.dirs
    }, warning = function(c) {
        cat("\nunable to create temporary folders\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    }, error = function(c) {
        cat("\nunable to create temporary folders\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    })
    if (is.null(tmp.dirs))
        return(invisible())


    tmp.R.scripts <- tryCatch({
        normalizePath(file.path(tmp.dirs[c(1, 1, 2)],
            sprintf("R script \%d.R", 1:3)), mustWork = FALSE)
    }, warning = function(c) {
        cat("\nunable to create temporary R scripts\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    }, error = function(c) {
        cat("\nunable to create temporary R scripts\n* ",
            conditionMessage(c), "\n", sep = "")
        NULL
    })
    if (is.null(tmp.R.scripts))
        return(invisible())


    results.file <- tryCatch({
        .Sys.time <- format(Sys.time(), format = "\%Y-\%m-\%d \%H-\%M-\%OS",
            usetz = TRUE)
        tempfile(
            pattern = paste0("this.path example results ", .Sys.time, " "),
            tmpdir = tmpdir, fileext = ".txt"
        )
    }, warning = function(c) {
        NULL
    }, error = function(c) {
        NULL
    })


    write.results <- function(expr) {
        if (!is.null(results.file)) {
            sink(file = results.file, append = TRUE)
            on.exit(sink())
        }
        expr
    }


    env <- list(
        `write.results sub` = write.results,
        `tmp.R.script.1 sub` = tmp.R.scripts[1],
        `tmp.R.script.2 sub` = tmp.R.scripts[2],
        `tmp.R.script.3 sub` = tmp.R.scripts[3],
        `results.file sub` = results.file
    )
    tmp.R.script.1.code <- substitute({
        results.file <- `results.file sub`
        write.results <- `write.results sub`


        write.results({
            cat("this.path status : ", x <- tryCatch({
                this.path::this.path(verbose = TRUE)
                "success"
            }, warning = function(c) {
                paste0("failure\n* ", conditionMessage(c))
            }, error = function(c) {
                paste0("failure\n* ", conditionMessage(c))
            }), "\n", sep = "")
        })


        if (x == "success") {
            cat("\nExecuting script's filename:\n")
            cat("  ", sQuote(`tmp.R.script.1 sub`), "\n", sep = "")
            cat("\nExecuting script's filename (as determined by 'this.path'):\n")
            cat("  ", sQuote(this.path::this.path(verbose = TRUE)), "\n", sep = "")
            source(file.path(this.path::this.dir(verbose = FALSE),
                "R script 2.R"))
        }
    }, env)


    tmp.R.script.2.code <- substitute({
        cat("\nExecuting script's filename:\n")
        cat("  ", sQuote(`tmp.R.script.2 sub`), "\n", sep = "")
        cat("\nExecuting script's filename (as determined by 'this.path'):\n")
        cat("  ", sQuote(this.path::this.path(verbose = TRUE)), "\n", sep = "")
        source(file.path(dirname(this.path::this.dir(verbose = FALSE)),
            "code 2", "R script 3.R"))
    }, env)


    tmp.R.script.3.code <- substitute({
        cat("\nExecuting script's filename:\n")
        cat("  ", sQuote(`tmp.R.script.3 sub`), "\n", sep = "")
        cat("\nExecuting script's filename (as determined by 'this.path'):\n")
        cat("  ", sQuote(this.path::this.path(verbose = TRUE)), "\n", sep = "")
    }, env)


    writeRcode2file <- function(x, file) {
        tryCatch({
            lines <- vapply(X = as.list(x[-1]), FUN = function(y) {
                paste0(deparse(y), collapse = "\n")
            }, FUN.VALUE = "")
            writeLines(lines, con = file)
            TRUE
        }, warning = function(c) {
            cat("\nunable to write R code to file: ",
                sQuote(file), "\n* ", conditionMessage(c), "\n",
                sep = "")
            FALSE
        }, error = function(c) {
            cat("\nunable to write R code to file: ",
                sQuote(file), "\n* ", conditionMessage(c), "\n",
                sep = "")
            FALSE
        })
    }


    if (!writeRcode2file(tmp.R.script.1.code, tmp.R.scripts[1]))
        return(invisible())
    if (!writeRcode2file(tmp.R.script.2.code, tmp.R.scripts[2]))
        return(invisible())
    if (!writeRcode2file(tmp.R.script.3.code, tmp.R.scripts[3]))
        return(invisible())


    msg <- sprintf(paste0("Created an example project folder. This is a ",
        "relatively simple project folder structure with only \%d sub-folders, ",
        "you would typically find a complex structure in actual projects. The ",
        "main R script will be run in all possible ways that are compatible ",
        "with ", sQuote("this.path"), " and are currently available."
    ), length(tmp.dirs))
    cat("\n", paste0(strwrap(msg, exdent = 2), "\n", collapse = ""), sep = "")
    if (.interactive) {
        cat("\nAttempting to open the example project folder.\n",
            "  If the folder did not open automatically, the folder's path is:\n",
            sep = "")
        cat("    ", sQuote(tmp.main.dir), "\n", sep = "")
        tryCatch({
            this.path:::file.open(tmp.main.dir)
        }, warning = function(c) {
        }, error = function(c) {
        })
        pressEnter2Continue("")
    }


    write.results({
        cat("** session info\n\n")
        print(utils::sessionInfo())
        cat("\n** the above is information the package maintainer will likely ",
            "need if you report a bug\n", sep = "")
    })


    write.results({
        cat("\nAttempting to use 'this.path' when using 'source'\n")
    })


    tryCatch({
        cat("\n* first, using ", sQuote("source"), "\n", sep = "")
        source(tmp.R.scripts[1], local = TRUE)
        cat2("\nYou should see that 'this.path' correctly determined each of t",
            "he executing script's\n  filenames when using 'source'. If you be",
            "lieve 'this.path' did not correctly determine\n  any of the execu",
            "ting script's filenames, please send a bug report to the package ",
            "maintainer.\n", sep = "", timeout = 0.015)
        pressEnter2Continue("")
    }, warning = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script.1, "\n* ", conditionMessage(c),
            "\n", sep = "")
    }, error = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script.1, "\n* ", conditionMessage(c),
            "\n", sep = "")
    })


    write.results({
        cat("\nAttempting to use 'this.path' when using 'sys.source'\n")
    })


    tryCatch({
        cat("\n* second, using ", sQuote("sys.source"), "\n", sep = "")
        sys.source(tmp.R.scripts[1], envir = environment())
        cat2("\nYou should see that 'this.path' correctly determined each of t",
            "he executing script's\n  filenames when using 'sys.source'. If yo",
            "u believe 'this.path' did not correctly determine\n  any of the e",
            "xecuting script's filenames, please send a bug report to the pack",
            "age maintainer.\n", sep = "", timeout = 0.015)
        pressEnter2Continue("")
    }, warning = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script.1, "\n* ", conditionMessage(c),
            "\n", sep = "")
    }, error = function(c) {
        cat("\nunexpected error when attempting to source file: ",
            tmp.R.script.1, "\n* ", conditionMessage(c),
            "\n", sep = "")
    })


    if (.Platform$GUI == "RStudio") {
        write.results({
            cat("\nAttempting to use 'this.path' when using 'debugSource'\n")
        })


        tryCatch({
            dbs <- get("debugSource", mode = "function", "tools:rstudio",
                inherits = FALSE)
            cat("\n* third, using ", sQuote("debugSource"),
                " from ", sQuote("RStudio"), "\n", sep = "")
            dbs(tmp.R.scripts[1], local = TRUE)
            cat2("\nYou should see that 'this.path' correctly determined each ",
                "of the executing script's\n  filenames when using 'debugSourc",
                "e'. If you believe 'this.path' did not correctly determine\n ",
                " any of the executing script's filenames, please send a bug r",
                "eport to the package maintainer.\n", sep = "", timeout = 0.015)
            pressEnter2Continue("")
        }, warning = function(c) {
            cat("\nunexpected error when attempting to source file: ",
                tmp.R.script.1, "\n* ", conditionMessage(c),
                "\n", sep = "")
        }, error = function(c) {
            cat("\nunexpected error when attempting to source file: ",
                tmp.R.script.1, "\n* ", conditionMessage(c),
                "\n", sep = "")
        })
    }
    else write.results({
        cat2("\nUnfortunately, it is impossible to demonstrate the functionalit",
            "y of 'this.path'\n  when using 'debugSource' because 'RStudio' is",
            " not presently running.\n", sep = "")
        pressEnter2Continue("")
    })

    write.results({
        cat("\nAttempting to use 'this.path' when running from command-line\n")
    })


    owd <- getwd()
    if (!is.null(owd))
        on.exit(setwd(owd), add = TRUE)
    setwd(dirname(tmp.R.scripts[1]))
    command <- sprintf("Rterm --no-echo --no-restore --file=\"\%s\"",
        basename(tmp.R.scripts[1]))
    tryCatch({
        cat("\n* last, running from the command-line / / terminal\n")
        cat("\nProcess finished with exit code ",
            system(command), "\n", sep = "")
        cat2("\nYou should see that 'this.path' correctly determined each of t",
            "he executing script's\n  filenames when running from the command-",
            "line / / terminal. If you believe 'this.path'\n  did not correctl",
            "y determine any of the executing script's filenames, please send",
            "\n  a bug report to the package maintainer.\n",
            sep = "", timeout = 0.015)
        pressEnter2Continue()
    }, warning = function(c) {
        cat("\nunexpected error when attempting to run file: ",
            tmp.R.script.1, "\nfrom the command-line\n* ",
            conditionMessage(c), "\n", sep = "")
    }, error = function(c) {
        cat("\nunexpected error when attempting to run file: ",
            tmp.R.script.1, "\nfrom the command-line\n* ",
            conditionMessage(c), "\n", sep = "")
    })
    setwd(owd)


    write.results({
        cat("\nUnfortunately, it is impossible to use 'example(topic = \"this.",
            "path\", package = \"this.path\")'\n  to demonstrate the functiona",
            "lity of 'this.path' in 'RStudio' and 'RGui'. If you\n  would like",
            " to see this functionality, you could try this:\n* make a new R s",
            "cript containing just this one command:\n    this.path::this.path",
            "(verbose = TRUE)\n* open this script in 'RStudio' or 'RGui'\n* ru",
            "n that command directly from the script\n    (both should print ",
            "\"Source: active document ...\" along with the script's path)\n* ",
            "copy and paste that command into the R Console and run that comma",
            "nd again\n    (both should print \"Source: source document ...\" ",
            "along with the script's path)\n* try closing all your documents a",
            "nd run that same command in the R Console\n    (both should raise",
            " an error \"R is being run ... with no documents open\")\n",
            sep = "")
    })


    if (.interactive) {
        tryCatch({
            this.path:::file.open(results.file)
        }, warning = function(c) {
            cat("\n")
            cat(readLines(results.file), sep = "\n")
        }, error = function(c) {
            cat("\n")
            cat(readLines(results.file), sep = "\n")
        })
    }
    else if (!is.null(results.file)) {
        cat("\n")
        cat(readLines(results.file), sep = "\n")
    }
    invisible()
})(), warning = conditionMessage, error = conditionMessage)
}
