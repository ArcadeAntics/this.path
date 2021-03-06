

file.open <- function (file)
{
    value <- file
    file <- as.character(file)[1L]
    if (is.na(file) || !nzchar(file))
        return(invisible(value))
    file <- normalizePath(file, mustWork = TRUE)
    if (.Platform$OS.type != "windows") {
        if (getRversion() >= "4.0.0")
            os <- utils::osVersion
        else os <- utils::sessionInfo()$running
        if (is.null(os))
            system(sprintf("xdg-open \"%s\"", file))
        else if (startsWith(os, "macOS "))
            system(sprintf("open \"%s\"", file))
        else system(sprintf("xdg-open \"%s\"", file))
    }
    else shell.exec(file)
    return(invisible(value))
}


unix.command.line.argument.file.containing.space.fix <- function (path)
{
    # on a computer running a Unix OS, you will experience the following bug when
    #     running R scripts from the command-line:
    # all " " in argument 'file' will be replaced with "~+~"
    # To elaborate, the Rscript executable does open the correct file, for instance:
    #
    # /path/to/Rscript "/tmp/RtmpqapV6Q/temp R script 15f42409a2.R"
    #
    # will, in fact, open the file "/tmp/RtmpqapV6Q/temp R script 15f42409a2.R"
    #     and start parsing and evaluating the code within that file as it should,
    #     but the argument 'file' will be recorded incorrectly. If, in the script
    #     "/tmp/RtmpqapV6Q/temp R script 15f42409a2.R", you had a piece of code
    #     that looked like:
    #
    # print(commandArgs())
    #
    # you would see something like:
    #
    # [1] "/path/to/Rscript"
    # [2] "--no-echo"
    # [3] "--no-restore"
    # [4] "--file=/tmp/RtmpqapV6Q/temp~+~R~+~script~+~15f42409a2.R"
    #
    # the first element is computer dependent, this may be different for you.
    # the thing to notice is that the command-line argument 'file' is incorrect,
    #     and that this function should correct this error


    opath <- path


    # since the bug only happens on Unix, do not fix if not on Unix
    if (.Platform$OS.type != "unix")
        return(normalizePath(path, mustWork = TRUE))


    # even if you're on Unix, if the file exists it is assumed to be correct
    if (file.exists(path))
        return(normalizePath(path, mustWork = TRUE))


    # if we passed the condition above
    # * we MUST be running on Unix
    # * the filename 'path' does NOT exist
    # one more simple case we can test is replacing all "~+~" with " "
    # if that exists, it is assumed to be correct
    x <- gsub("~+~", " ", path, fixed = TRUE)
    if (file.exists(x))
        return(normalizePath(x, mustWork = TRUE))


    # if we passed to condition above, it means
    # * we MUST be running on Unix
    # * original filename 'path' does not exist
    # * filename 'path' with all "~+~" replaced with " " does not exist
    # this must mean that some combination of "~+~" need to be replaced, but not all and not none
    # even still, we check that 'path' contains "~+~" regardless of the above conditions
    if (grepl("~+~", path, fixed = TRUE)) {


        # find every instance of "~+~" in 'path' including nested cases ("~+~+~+~")
        first <- seq.int(1, nchar(path) - 2)
        m <- which(substring(path, first, first + 2L) == "~+~")
        attr(m, "match.length") <- rep(3L, length(m))
        attr(m, "index.type") <- "chars"
        attr(m, "useBytes") <- TRUE


        # the issue is that I don't know which of the "~+~" should be replaced with " "
        #     and which should NOT be replaced
        # the idea is that we attempt all possible combinations of replacing "~+~" with " "
        # if we had, for example, five instances of "~+~", then we have several combinations to try:
        # 0 replacements, 1  combination
        # 1 replacement , 5  combinations
        # 2 replacements, 10 combinations
        # 3 replacements, 10 combinations
        # 4 replacements, 5  combinations
        # 5 replacements, 1  combination
        # some of these combitions may be invalid due to overlapping matches,
        #     but those will be handled later


        tmp <- c(TRUE, FALSE)
        tmp <- .mapply(rep, list(each = length(tmp)^(seq_along(m) - 1)), list(x = tmp))
        tmp <- do.call("cbind", tmp)
        tmp <- asplit(x = tmp, MARGIN = 1L)


        # each element of 'tmp' should be a different combination of replacing "~+~"
        #
        # each element of 'tmp' should be equal in length to the length of 'm' (number of "~+~" found)
        # there should be '2^length(m)' elements in 'tmp'
        # there should be NO DUPLICATE ELEMENTS !!


        tmp <- lapply(tmp, function(i) {
            if (sum(i) == 0L) {  # if we are replacing NONE of "~+~"
                value <- -1L
                attr(value, "match.length") <- -1L
            }
            else {
                value <- m[i]
                attr(value, "match.length") <- attr(m,
                    "match.length")[i]
            }


            # if value contains overlapping patterns, it is invalid and should be removed
            if (any((value + attr(value, "match.length"))[-length(value)] >
                value[-1L]))
                value <- NULL
            else {
                attr(value, "index.type") <- "chars"
                attr(value, "useBytes") <- TRUE
            }
            return(value)
        })


        # remove NULL values
        tmp <- tmp[!vapply(X = tmp, FUN = "is.null", FUN.VALUE = NA)]


        # replace instances of "~+~" with " "
        path <- rep(path, length(tmp))
        regmatches(path, tmp) <- " "


        path <- path[file.exists(path)]  # select the filenames that exist
        if (length(path) == 0L) {
            stop("unable to resolve Unix command-line argument 'file' conflict for file\n",
                "    ", deparse(opath), "\n",
                "* when running an R script that contains \" \" from the Unix command-line,\n",
                "    the R script is opened and starts executing its code (as it should),\n",
                "    but the R script's path is recorded incorrectly\n",
                "* each \" \" is replaced by \"~+~\", so when you ask for the command argument 'file',\n",
                "    it returns something undesirable.\n",
                "* all possible combinations of replacing instances of \"~+~\" with \" \"\n",
                "    were attempted, but no file was found.\n")
        }
        else if (length(path) > 1L) {
            stop("unable to resolve Unix command-line argument 'file' conflict for file\n",
                "    ", deparse(opath), "\n",
                "* when running an R script that contains \" \" from the Unix command-line,\n",
                "    the R script is opened and starts executing its code (as it should),\n",
                "    but the R script's path is recorded incorrectly\n",
                "* each \" \" is replaced by \"~+~\", so when you ask for the command argument 'file',\n",
                "    it returns something undesirable.\n",
                "* all possible combinations of replacing instances of \"~+~\" with \" \"\n",
                "    were attempted, but multiple files were found.\n")
        }
    }
    return(normalizePath(path, mustWork = TRUE))
}


this.path <- function (verbose = getOption("verbose"))
{
    where <- function(x) if (verbose)
        cat("Source: ", x, "\n", sep = "")


    # loop through functions that lead here from most recent to earliest looking
    #     for an appropriate source call (a call to function base::source or base::sys.source)
    # an appropriate source call is a source call in which
    #     argument 'file' has been evaluated (forced)
    # this means, for example, the following is an inappropriate source call:
    #     source(this.path())
    # the argument 'file' is stored as a promise
    #     containing the expression "this.path()"
    # when the value of 'file' is requested, it assigns the value
    #     returned by evaluating "this.path()" to variable 'file'
    # there are two functions on the calling stack at
    #     this point being 'source' and 'this.path'
    # clearly, you don't want to request the 'file' argument from that source
    #     call because the value of 'file' is under evaluation right now!
    # the trick is to ask if variable ('ofile' for base::source, 'exprs' for base::sys.source)
    #     exists in that function's evaluation environment. this is because that
    #     variable is created AFTER argument 'file' has been forced
    # if that variable does exist, then argument 'file' has been forced and the
    #     source call is deemed appropriate. For base::source, the filename we want
    #     is the variable 'ofile' from that function's evaluation environment. For
    #     base::sys.source, the filename we want is the variable 'file' from that
    #     function's evaluation environment.
    # if that variable does NOT exist, then argument 'file' hasn't been forced and
    #     the source call is deemed inappropriate. The 'for' loop moves to the next
    #     function up the calling stack (if available)
    #
    # unfortunately, there is no way to check the argument 'fileName' has been forced
    #     for 'debugSource' since all the work is done internally in C. Instead,
    #     we have to use a 'tryCatch' statement. When we ask for an object by name
    #     using 'get', R is capable of realizing if a variable is asking for its
    #     own definition (a recursive definition). The exact error is "promise already
    #     under evaluation" which indicates that the promise evaluation is requesting
    #     its own value. So we use the 'tryCatch' to get the argument 'fileName'
    #     from the evaluation environment of 'debugSource', and if it does not raise
    #     an error, then we are safe to return that value. If not, the condition
    #     returns false and the 'for' loop moves to the next function up the calling
    #     stack (if available)


    dbs <- if (.Platform$GUI == "RStudio")
        get("debugSource", mode = "function", "tools:rstudio",
            inherits = FALSE)
    for (n in seq.int(sys.nframe(), 1L)[-1L]) {
        if (identical(sys.function(n), base::source)) {
            if (!exists("ofile", envir = sys.frame(n), inherits = FALSE))
                next
            if (!exists("__file__", envir = sys.frame(n), inherits = FALSE)) {
                path <- get("ofile", envir = sys.frame(n), inherits = FALSE)
                if (!is.character(path))
                  path <- summary.connection(path)$description
                if (exists("owd", envir = sys.frame(n), inherits = FALSE)) {
                  owd <- getwd()
                  on.exit(setwd(owd))
                  setwd(get("owd", envir = sys.frame(n), inherits = FALSE))
                }
                assign("__file__", normalizePath(path, mustWork = TRUE),
                  envir = sys.frame(n))
            }
            where("call to function source")
            return(get("__file__", envir = sys.frame(n), inherits = FALSE))
        }
        else if (identical(sys.function(n), base::sys.source)) {
            if (!exists("exprs", envir = sys.frame(n), inherits = FALSE))
                next
            if (!exists("__file__", envir = sys.frame(n), inherits = FALSE)) {
                path <- get("file", envir = sys.frame(n), inherits = FALSE)
                if (exists("owd", envir = sys.frame(n), inherits = FALSE)) {
                  owd <- getwd()
                  on.exit(setwd(owd))
                  setwd(get("owd", envir = sys.frame(n), inherits = FALSE))
                }
                assign("__file__", normalizePath(path, mustWork = TRUE),
                  envir = sys.frame(n))
            }
            where("call to function sys.source")
            return(get("__file__", envir = sys.frame(n), inherits = FALSE))
        }
        else if (identical(sys.function(n), dbs)) {
            cond <- tryCatch({
                path <- get("fileName", envir = sys.frame(n),
                  inherits = FALSE)
                TRUE
            }, error = function(c) {
                FALSE
            })
            if (!cond)
                next
            if (!exists("__file__", envir = sys.frame(n), inherits = FALSE))
                assign("__file__", normalizePath(path, mustWork = TRUE),
                  envir = sys.frame(n))
            where("call to function debugSource in RStudio")
            return(get("__file__", envir = sys.frame(n), inherits = FALSE))
        }
    }


    # if the for loop is passed, no appropriate
    #     source call was found up the calling stack
    # next, check if the user is running R from the command-line
    #     on a Windows OS, the GUI is "RTerm"
    #     on a Unix    OS, the GUI is "X11"


    if (.Platform$OS.type == "windows" && .Platform$GUI == "RTerm" ||  # running from Windows command-line
        .Platform$OS.type == "unix" && .Platform$GUI == "X11") {       # running from Unix command-line


        if (is.null(`__file__`)) {


            # when running R from the command-line, there are a few things to keep in
            # mind when trying to select the correct 'file' to return. First, there
            # are a few command-line arguments where the name and value are separate.
            # This means that the name of the argument is the i-th argument and the
            # value of the argument is the i+1-th argument. Second, the --args command-line
            # flag to R means that all arguments after are for the R script to use,
            # while the arguments before are for the 'Rterm' executable. Finally, to
            # take input from a file, the two accepted methods are -f file and --file=file
            # where the input is taken from 'file'. If multiple of these arguments are
            # supplied, (it seems as though) 'Rterm' takes input from the last 'file' argument.


            ca <- commandArgs()


            # select the command-line arguments intended for the Rterm executable
            # also remove the first argument, this is the name of the executable by
            # which this R process was invoked (not useful here)
            ca <- ca[seq_len(length(ca) - length(commandArgs(trailingOnly = TRUE)))]
            ca <- ca[-1L]


            # remove the --encoding=enc and --encoding enc command-line flags to R
            # these flags have highest priority, they are always handled first, regardless
            # of the preceding argument.
            # Example:
            # /path/to/Rterm -f --encoding=UTF-8 file
            # you might think the value for -f would be --encoding=UTF-8 but it's actually
            # file because --encoding=enc and --encoding enc are processed first
            which.rm <- which(ca == "--encoding")
            which.rm <- c(which.rm, which.rm + 1L)
            ca <- ca[setdiff(seq_along(ca), which.rm)]


            # there are 17 more arguments that are evaluated before -f is grouped with
            # its corresponding value. Some of them are static (the first fifteen) while
            # the others are variable (same beginning, different values after)
            # Example:
            # /path/to/Rterm -f --silent --max-ppsize=100 file
            # you might think the value of -f would be --silent but it's actually file
            # because --silent and --max-ppsize=N are processed before
            which.rm <- which(ca %in% c("--save", "--no-save",
                "--no-environ", "--no-site-file", "--no-init-file",
                "--restore", "--no-restore-data", "--no-restore-history",
                "--no-restore", "--vanilla", "-q", "--quiet", "--silent",
                "--no-echo", "--verbose") | grepl("^--encoding=|^--max-ppsize=", ca))
            ca <- ca[setdiff(seq_along(ca), which.rm)]


            # next, we group the command-line arguments where the name and value are separate
            # there are two left being -f file and -e expression
            # find the locations of these special arguments
            special <- ca %in% c("-f", "-e")


            # next, we figure out which of these special arguments are ACTUALLY special
            # Example:
            # /path/to/Rterm -f -f -f -e
            # here, the first and third -f are special
            #     while the second -f and first -e are not
            for (n in seq_len(max(length(special) - 1L, 0L))) {
                if (special[n])
                  special[n + 1L] <- FALSE
            }
            which.special <- which(special)


            # with the locations of the special arguments,
            #     figure out which of those are -f file arguments
            which.f <- which.special[ca[which.special] == "-f"]


            # use the locations of the special arguments to
            #     find the non-special argument locations
            which.non.special <- setdiff(seq_along(ca), c(which.special,
                which.special + 1L))


            # in these non-special command-line arguments,
            #     figure out which arguments start with --file=
            which.file <- which.non.special[grep("^--file=",
                ca[which.non.special])]


            # given the number of -f file and --file=file arguments,
            #     signal a possible error or warning
            n.file <- length(which.f) + length(which.file)
            if (!n.file)
                stop("'this.path' used in an inappropriate fashion\n",
                  "* no appropriate source call was found up the calling stack\n",
                  "* R is being run from the command-line and argument 'file' is missing\n")
            if (n.file > 1L)
                warning("command-line formal argument 'file' matched by multiple actual arguments\n")


            # since 'Rterm' uses the last -f file or --file=file argument,
            #     use max to find the index of the last one of these arguments
            n <- max(which.f, which.file)
            if (n %in% which.file)
                path <- sub("^--file=", "", ca[n])
            else path <- ca[n + 1L]


            # R has a weird bug when running from the Unix command-line,
            #     all " " in argument 'file' are replaced with "~+~"
            # the following is a work around and may not work in all instances
            # if you'd like to take a look at the details, try:
            # this.path:::unix.command.line.argument.file.containing.space.fix


            utils::assignInMyNamespace("__file__", unix.command.line.argument.file.containing.space.fix(path))
        }
        where("Command-line argument 'file'")
        return(`__file__`)
    }
    else if (.Platform$GUI == "RStudio") {  # running R from 'RStudio'


        # function ".rs.api.getActiveDocumentContext" from the environment "tools:rstudio"
        #     returns a list of information about the document where your cursor is located
        #
        # function ".rs.api.getSourceEditorContext" from the environment "tools:rstudio"
        #     returns a list of information about the document open in the current tab
        #
        # element 'id' is a character string, an identification for the document
        # element 'path' is a character string, the path of the document


        adc <- get(".rs.api.getActiveDocumentContext",
            mode = "function", "tools:rstudio", inherits = FALSE)()
        if (adc$id != "#console") {
            path <- adc$path
            if (nzchar(path)) {
                where("active document in RStudio")
                return(normalizePath(path, mustWork = FALSE))
            }
            else stop("'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* active document in RStudio does not exist\n")
        }


        sec <- get(".rs.api.getSourceEditorContext", mode = "function",
            "tools:rstudio", inherits = FALSE)()
        if (!is.null(sec)) {
            path <- sec$path
            if (nzchar(path)) {
                where("source document in RStudio")
                return(normalizePath(path, mustWork = FALSE))
            }
            else stop("'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* source document in RStudio does not exist\n")
        }
        else stop("'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from RStudio with no documents open\n")
    }
    else if (.Platform$OS.type == "windows" && .Platform$GUI == "Rgui") {  # running R from 'RGui' on Windows


        # on a Windows OS only, the function "getWindowsHandles" from the base
        # package "utils" returns a list of external pointers containing the windows
        # handles. The thing of interest are the names of this list, these should
        # be the names of the windows belonging to the current R process. Since
        # RGui can have files besides R scripts open (such as images), a regular
        # expression is used to subset only windows handles with names that exactly
        # match the string "R Console" or end with " - R Editor". I highly suggest
        # that you NEVER end a document's filename with " - R Editor". From there,
        # similar checks are done as in the above section for 'RStudio'


        wh <- names(utils::getWindowsHandles(pattern = "^R Console$| - R Editor$",
            minimized = TRUE))


        if (!length(wh))
            stop("this error SHOULD be unreachable, as far as I know it's impossible to have an R\n",
                "  process running without the R Console open. If you reached this error, please\n",
                "  send a bug report to the package maintainer")


        path <- wh[1L]
        if (path != "R Console") {
            path <- sub(" - R Editor$", "", path)
            if (path != "Untitled") {
                where("active document in RGui")
                return(normalizePath(path, mustWork = FALSE))
            }
            else stop("'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* active document in RGui does not exist\n")
        }


        path <- wh[2L]
        if (!is.na(path)) {
            path <- sub(" - R Editor$", "", path)
            if (path != "Untitled") {
                where("source document in RGui")
                return(normalizePath(path, mustWork = FALSE))
            }
            else stop("'this.path' used in an inappropriate fashion\n",
                "* no appropriate source call was found up the calling stack\n",
                "* source document in RGui does not exist\n")
        }
        else stop("'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from RGui with no documents open\n")
    }
    else if (.Platform$OS.type == "unix" && .Platform$GUI == "AQUA") {  # running R from 'RGui' on Unix
        stop("'this.path' used in an inappropriate fashion\n",
            "* no appropriate source call was found up the calling stack\n",
            "* R is being run from AQUA which requires a source call on the calling stack\n")
    }
    else stop("'this.path' used in an inappropriate fashion\n",
        "* no appropriate source call was found up the calling stack\n",
        "* R is being run in an unrecognized manner\n")
}


this.dir <- function (verbose = getOption("verbose"))
dirname(this.path(verbose = verbose))


`__file__` <- NULL


if (FALSE) {
    (function() {
        odir <- getwd()
        on.exit(setwd(odir))
        setwd("~")
        pkg <- "this.path"
        system(sprintf("Rcmd build %s", pkg))
        cat("\n")
        system(sprintf("Rcmd check --as-cran %s_%s.tar.gz", pkg, utils::packageVersion(pkg)))
    })()
}
