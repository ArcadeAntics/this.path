\name{set.gui.path}
\alias{set.gui.path}
\alias{thisPathNotExistsError}
\alias{thisPathNotFoundError}
\title{Declare GUI's Active Document}
\description{
  \code{set.gui.path()} can be used to implement \code{\link{this.path}()} for
  arbitrary GUIs.
}
\usage{
set.gui.path(...)

thisPathNotExistsError(..., call. = TRUE, domain = NULL,
                       call = .getCurrentCall())

thisPathNotFoundError(..., call. = TRUE, domain = NULL,
                      call = .getCurrentCall())
}
\arguments{
  \item{\dots, call., domain, call}{See details.}
}
\details{
  \code{thisPathNotExistsError()} and \code{thisPathNotFoundError()} are
  provided for use inside \code{set.gui.path()}, and should not be used
  elsewhere.

  If no arguments are passed to \code{set.gui.path()}, the default behaviour
  will be restored.

  If one argument is passed to \code{set.gui.path()}, it must be a function
  that returns the path of the active document in your GUI. It must accept the
  following arguments: \code{(verbose, original, for.msg, contents)} (default
  values are unnecessary). This makes sense for a GUI which can edit and run \R
  code from several different documents such as RGui, \RStudio, \Positron,
  \VSCode + \REditorSupport, and \Emacs + \ESS.

  If two or three arguments are passed to \code{set.gui.path()}, they must be
  the name of the GUI, the path of the active document, and optionally a
  function to get the contents of the document. If provided, the function must
  accept at least one argument which will be the normalized path of the
  document. This makes sense for a GUI which can edit and run \R code from only
  one document such as \Jupyter and shell.

  It is best to call this function as a user hook.
  \preformatted{setHook(packageEvent("@R_PACKAGE_NAME@"),
function(pkgname, pkgpath)
{
    @R_PACKAGE_NAME@::set.gui.path(<...>)
}, action = "prepend")}

  An example for a GUI which can run code from multiple documents:
  \preformatted{evalq(envir = new.env(parent = .BaseNamespaceEnv), {
    .guiname <- "myGui"
    .custom_gui_path <- function(verbose, original, for.msg, contents) {
        if (verbose)
            cat("Source: document in", .guiname, "\\n")

        ## your GUI needs to know which document is active
        ## and some way to retrieve that document from R
        doc <- <.myGui_activeDocument()>

        ## if no documents are open, 'doc' should be NULL
        ## or some other object to represent no documents open
        if (is.null(doc)) {
            if (for.msg)
                NA_character_
            else stop(@R_PACKAGE_NAME@::thisPathNotExistsError(
                "R is running from ", .guiname, " with no documents open\\n",
                " (or document has no path)"))
        }
        else if (contents) {
            ## somehow, get and return the contents of the document
            <doc$contents>
        }
        else {
            ## somehow, get the path of the document
            path <- <doc$path>
            if (nzchar(path)) {
                ## if the path is not normalized, this will normalize it
                if (isFALSE(original))
                    normalizePath(path, "/", TRUE)
                else path
                # ## otherwise, you could just do:
                # path
            }
            else if (for.msg)
                ## return "Untitled" possibly translated
                gettext("Untitled", domain = "RGui", trim = FALSE)
            else
                stop(@R_PACKAGE_NAME@::thisPathNotFoundError(
                    "document in ", .guiname, " has no associated path (has yet to be saved)"))
        }
    }
    ## recommended to prevent tampering
    lockEnvironment(environment(), bindings = TRUE)
    setHook(packageEvent("@R_PACKAGE_NAME@"),
    function(pkgname, pkgpath) {
        @R_PACKAGE_NAME@::set.gui.path(.custom_gui_path)
    }, action = "prepend")
})}

  An example for a GUI which can run code from only one document:
  \preformatted{evalq(envir = new.env(parent = .BaseNamespaceEnv), {
    .guiname <- "myGui"
    .path <- "~/example.R"
    .custom_get_contents <- function(path) {
        ## get the contents of the document
        readLines(path, warn = FALSE)
    }
    ## recommended to prevent tampering
    lockEnvironment(environment(), bindings = TRUE)
    setHook(packageEvent("@R_PACKAGE_NAME@"), function(pkgname, pkgpath) {
        @R_PACKAGE_NAME@::set.gui.path(.guiname, .path, .custom_get_contents)
    }, action = "prepend")
    # ## if your GUI does not have/need a .custom_get_contents
    # ## function, then this works just as well:
    # setHook(packageEvent("@R_PACKAGE_NAME@"), function(pkgname, pkgpath) {
    #     @R_PACKAGE_NAME@::set.gui.path(.guiname, .path)
    # }, action = "prepend")
})}
}
\value{
  a list of the previous settings for \code{set.gui.path()}, similar to
  \codelink3{base:options}{options}{()}.
}
