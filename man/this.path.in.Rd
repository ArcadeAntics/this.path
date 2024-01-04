\name{this.path}
\alias{this.path}
\alias{this.dir}
\title{Determine Script's Filename}
\description{
  \code{this.path()} returns the
  \ifelse{html}{\codelink2{base:normalizePath}{normalized}}{normalized} path of
  the script in which it was written.

  \code{this.dir()} returns the directory of \code{this.path()}.
}
\usage{
this.path(verbose = getOption("verbose"), original = FALSE,
          for.msg = FALSE, contents = FALSE, local = FALSE,
          n = 0, envir = parent.frame(n + 1),
          matchThisEnv = getOption("topLevelEnvironment"),
          srcfile = if (n) sys.parent(n) else 0,
          default, else.)

this.dir(verbose = getOption("verbose"), local = FALSE,
         n = 0, envir = parent.frame(n + 1),
         matchThisEnv = getOption("topLevelEnvironment"),
         srcfile = if (n) sys.parent(n) else 0,
         default, else.)
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

  \item{contents}{\code{TRUE} or \code{FALSE}; should the contents of the
    script be returned instead? In \sQuote{Jupyter}, a list of character
    vectors will be returned, the contents separated into cells. Otherwise, if
    \code{for.msg} is \code{TRUE} and the executing script cannot be
    determined, \code{NULL} will be returned. Otherwise, a character vector
    will be returned. You could use
    \ifelse{html}{\code{\link[base:character]{as.character}(\link[base]{unlist}(this.path(contents = TRUE)))}}{\code{as.character(unlist(this.path(contents = TRUE)))}}
    if you require a character vector.

    This is intended for logging purposes. This is useful in \sQuote{Rgui},
    \sQuote{RStudio}, \sQuote{VSCode}, and \sQuote{Emacs} when the source
    document has contents but no path.}

  \item{local}{\code{TRUE} or \code{FALSE}; should the search for the executing
    script be confined to the local environment in which
    \code{\link{set.sys.path}()} was called?}

  \item{n}{the number of additional generations to go back. By default,
    \code{this.path()} will look for a path based on the
    \code{\link[base:srcfile]{srcref}} of the call to \code{this.path()} and
    the environment in which \code{this.path()} was called. This can be changed
    to be based on the \code{srcref} of the call and the calling environment
    \code{n} generations up the stack. See section \strong{Argument 'n'} for
    more details.}

  \item{envir, matchThisEnv}{arguments passed to
    \codelink3{base:ns-topenv}{topenv}{()} to determine the top level
    environment in which to search for an associated path.}

  \item{srcfile}{source file in which to search for a pathname, or an object
    containing a source file. This includes a source reference, a call, an
    expression object, or a closure.}

  \item{default}{this value is returned if there is no executing script.}

  \item{else.}{function to apply if there is an executing script. See
    \code{\link{tryCatch2}()} for inspiration.}
}
\details{
  \code{this.path()} starts by examining argument \code{srcfile}. It looks at
  the bindings \code{filename} and \code{wd} to determine the associated file
  path. Filenames such as \blankstring, \string{clipboard}, and \string{stdin}
  will be ignored since they do not refer to files. A source file of class
  \string{srcfilecopy} in which binding \code{isFile} is \code{FALSE} will also
  be ignored. A source file of class \string{srcfilealias} will use the aliased
  \code{filename} in determining the associated path.

  If it does not find a path associated with \code{srcfile}, it will next
  examine arguments \code{envir} and \code{matchThisEnv}. Specifically, it
  calculates \codelink3{base:ns-topenv}{topenv}{(envir, matchThisEnv)} then
  looks for an associated path. It will find a path associated with the top
  level environment in two ways:

  \enumerate{
    \item{from a \ifelse{text}{\sQuote{package:box}}{\CRANpkg{box}} module's
      namespace.}

    \item{from an attribute \string{path}.}
  }

  If it does not find an associated path with \code{envir} and
  \code{matchThisEnv}, it will next examine the call stack looking for a source
  call: a call to function \sourcesorlistlinks. If a source call is found, the
  file argument is returned from the function's evaluation environment. If you
  have your own \sourcenolink-like function that you would like to be
  recognized by \code{this.path()}, please use \code{\link{set.sys.path}()} or
  contact the package maintainer so that it can be implemented.

  If no source call is found up the calling stack, it will next examine the GUI
  in use. If \R is running from:

  \describe{
    \item{a shell, such as the Windows command-line / / Unix terminal}{then the
      shell arguments are searched for \option{-f} \option{FILE} or
      \option{--file=FILE} (the two methods of taking input from \option{FILE})
      (\option{-f} \option{-} and \option{--file=-} are ignored). The last
      \option{FILE} is extracted and returned. If no arguments of either type
      are supplied, an error is thrown.

      If \R is running from a shell under a Unix-alike with \option{-g}
      \option{Tk} or \option{--gui=Tk}, an error is thrown. \sQuote{Tk} does
      not make use of its \option{-f} \option{FILE}, \option{--file=FILE}
      arguments.}

    \item{\sQuote{Rgui}}{then the source document's filename (the document most
      recently interacted with) is returned (at the time of evaluation). Please
      note that minimized documents \emph{WILL} be included when looking for
      the most recently used document. It is important to not leave the current
      document (either by closing the document or interacting with another
      document) while any calls to \code{this.path()} have yet to be evaluated
      in the run selection. If no documents are open or the source document
      does not exist (not saved anywhere), an error is thrown.}

    \item{\sQuote{\RStudio}}{then the active document's filename (the document
      in which the cursor is active) is returned (at the time of evaluation).
      If the active document is the \R console, the source document's filename
      (the document open in the current tab) is returned (at the time of
      evaluation). Please note that the source document will \emph{NEVER} be a
      document open in another window (with the \strong{Show in new window}
      button). Please also note that an active document open in another window
      can sometimes lose focus and become inactive, thus returning the
      incorrect path. It is best \strong{NOT} to not run \R code from a
      document open in another window. It is important to not leave the current
      tab (either by closing or switching tabs) while any calls to
      \code{this.path()} have yet to be evaluated in the run selection. If no
      documents are open or the source document does not exist (not saved
      anywhere), an error is thrown.}

    \item{\sQuote{\VSCode}}{then the source document's filename is returned (at
      the time of evaluation). It is important to not leave the current tab
      (either by closing or switching tabs) while any calls to
      \code{this.path()} have yet to be evaluated in the run selection. If no
      documents are open or the source document does not exist (not saved
      anywhere), an error is thrown.}

    \item{\sQuote{\Jupyter}}{then the source document's filename is guessed by
      looking for \R notebooks in the initial working directory, then searching
      the contents of those files for an expression matching the top-level
      expression. Please be sure to save your notebook before using
      \code{this.path()}, or explicitly use \code{\link{set.jupyter.path}()}.}

    \item{\sQuote{\Emacs}}{then the source document's filename is returned (at
      the time of evaluation). \sQuote{Emacs} must be running as a server,
      either by running \code{(server-start)} (consider adding to your
      \file{~/.emacs} file) or typing \code{M-x server-start}. It is important
      to not leave the current window (either by closing or switching buffers)
      while any calls to \code{this.path()} have yet to be evaluated in the run
      selection. If multiple frames are active, \code{this.path()} will pick
      the first frame containing the corresponding \R session.

      If multiple \sQuote{Emacs} sessions are active, \code{this.path()} will
      only work in the primary session due to limitations in
#ifdef windows
      \sQuote{emacsclient.exe}.
#endif
#ifdef unix
      \sQuote{emacsclient}.
#endif
      If you want to run multiple \R sessions, it is better to run one
      \sQuote{Emacs} session with multiple frames, one \R session per frame.
      Use \code{M-x make-frame} to make a new frame, or \code{C-x 5 f} to visit
      a file in a new frame.

      Additionally, never use \code{C-c C-b} to send the current buffer to the
      \R process. This copies the buffer contents to a new file which is then
      \code{source()}-ed. The source references now point to the wrong file.
      Instead, use \code{C-x h} to select the entire buffer then \code{C-c C-r}
      to evaluate the selection.}

    \item{\sQuote{AQUA}}{then the executing script's path cannot be determined.
      Unlike \sQuote{Rgui}, \sQuote{RStudio}, \sQuote{VSCode},
      \sQuote{Jupyter}, and \sQuote{Emacs}, there is currently no way to
      request the path of an open document. Until such a time that there is a
      method for requesting the path of an open document, consider using
      another GUI.}
  }

  If \R is running in another manner, an error is thrown.

  If your GUI of choice is not implemented with \code{this.path()}, please
  contact the package maintainer so that it can be implemented.
}
\value{
  character string.
}
\section{Argument 'n'}{
  By default, \code{this.path()} will look for a path based on the
  \code{srcref} of the call to \code{this.path()} and the environment in which
  \code{this.path()} was called. For example: \preformatted{{
#line 1 "file1.R"
fun <- function() this.path::this.path(original = TRUE)
fun()
}


{
#line 1 "file2.R"
fun()
}}

  Both of these will return \string{file1.R} because that is where the call to
  \code{this.path()} is written.

  But suppose we do not care where \code{this.path()} is called, but instead
  want to know where \code{fun()} is called. Pass argument \code{n = 1} to do
  so; \code{this.path()} will inspect the call and the calling environment one
  generation up the stack: \preformatted{{
#line 1 "file1.R"
fun <- function() this.path::this.path(original = TRUE, n = 1)
fun()
}


{
#line 1 "file2.R"
fun()
}}

  These will return \string{file1.R} and \string{file2.R}, respectively,
  because those are where the calls to \code{fun()} are written.

  But now suppose we wish to make a second function that uses \code{fun()}. We
  do not care where \code{fun()} is called, but instead want to know where
  \code{fun2()} is called. Add a formal argument \code{n = 0} to each function
  and pass \code{n = n + 1} to each sub-function: \preformatted{{
#line 1 "file1.R"
fun <- function(n = 0) {
    this.path::this.path(original = TRUE, n = n + 1)
}
fun()
}


{
#line 1 "file2.R"
fun2 <- function(n = 0) fun(n = n + 1)
list(fun = fun(), fun2 = fun2())
}


{
#line 1 "file3.R"
fun3 <- function(n = 0) fun2(n = n + 1)
list(fun = fun(), fun2 = fun2(), fun3 = fun3())
}}

  Within each file, all these functions will return the path in which they are
  called, regardless of how deep \code{this.path()} is called.
}
\note{
  If you need to use \code{this.path()} inside a user profile, please use
  \code{\link{with_init.file}()}, something along the lines of:
  \preformatted{this.path::with_init.file({
    <expr 1>
    <expr 2>
    <...>
})}
}
\seealso{
  \code{\link{shFILE}()}

  \code{\link{set.sys.path}()}
}
\examples{
FILE1.R <- tempfile(fileext = ".R")
writeLines("writeLines(sQuote(this.path::this.path()))", FILE1.R)

## 'this.path()' works with 'source()'
source(FILE1.R)

## 'this.path()' works with 'sys.source()'
sys.source(FILE1.R, envir = environment())

## 'this.path()' works with 'debugSource()' in 'RStudio'
if (.Platform$GUI == "RStudio")
    get("debugSource", "tools:rstudio", inherits = FALSE)(FILE1.R)

## 'this.path()' works with 'testthat::source_file()'
if (requireNamespace("testthat"))
    testthat::source_file(FILE1.R, chdir = FALSE, wrap = FALSE)

## 'this.path()' works with 'compiler::loadcmp()'
if (requireNamespace("compiler")) {
    FILE2.Rc <- tempfile(fileext = ".Rc")
    compiler::cmpfile(FILE1.R, FILE2.Rc)
    compiler::loadcmp(FILE2.Rc)
    unlink(FILE2.Rc)
}

## 'this.path()' works with 'Rscript'
this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE1.R))

## 'this.path()' also works when 'source()'-ing a URL
## (included tryCatch in case an internet connection is not available)
tryCatch({
    source(paste0("https://raw.githubusercontent.com/ArcadeAntics/",
                  "this.path/main/tests/sys-path-with-urls.R"))
}, condition = this.path:::.cat_condition)

unlink(FILE1.R)
}
