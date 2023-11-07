\name{NEWS}
\title{\pkg{this.path} News}
\section{CHANGES IN VERSION devel}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{Previously defunct functions \code{this.path2()},
        \code{this.dir2()}, \code{this.dir3()}, and \code{normalized.shFILE()}
        were removed entirely.}
    }
  }
}
\section{CHANGES IN VERSION 2.2.0 (2023-10-29)}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{Active bindings \codeorsQuote{FILE} and \codeorsQuote{LINE} have
        been converted to regular bindings \code{FILE()} and \code{LINE()}
        after the unfortunate realization that active binding functions are
        always evaluated in the global environment.}
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item{\code{this.path()} works significantly better with source
        references.}

      \item{\code{this.path()} is now compatible with package
        \CRANpkg{targets}.}

      \item{Added \code{with_site.file()} and \code{with_init.file()} that
        allow \code{this.path()} to be used within the site-wide startup
        profile file or a user profile.}

      \item{\code{this.path(contents = TRUE)} in \sQuote{Rgui} will now return
        the contents of an unsaved document, or return the unsaved changes of a
        saved document.}

      \item{Added \code{set.gui.path()} for implementing \code{sys.path()} for
        arbitrary GUIs.}
    }
  }
}
\section{CHANGES IN VERSION 2.1.0 (2023-09-12)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{\code{this.path()} is now compatible with function
        \code{shiny::runApp()}.}

      \item{\code{this.path()} is now compatible with function
        \code{plumber::plumb()}.}

      \item{Added \code{tryCatch3()} which accepts condition expressions
        instead of condition handlers. Added active binding
        \codeorsQuote{last.condition} which will be the last thrown-and-caught
        condition in \code{tryCatch3()}.}

      \item{When \code{tryCatch2()} and \code{tryCatch3()} catch a condition
        but the respective handler / / expression is missing, the next
        non-missing handler / / expression is used instead.}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{\code{set.env.path()} and \code{set.src.path()} now return their
        input instead of returning \code{NULL}.}

      \item{\code{set.src.path()} now accepts an expression vector containing a
        source file.}
    }
  }
}
\section{CHANGES IN VERSION 2.0.0 (2023-08-08)}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{\code{this.path()} was misleading users into believing it returns
        the path of the script in which it is written. In actuality, it returns
        the path of the executing script. As such, it was renamed to
        \code{sys.path()} in accordance with the other call stack inspection
        functions \code{sys.call()}, \code{sys.frame()}, \code{sys.nframe()},
        and \code{sys.function()}. Related functions that were renamed:
        \tabular{rll}{
               \tab old                   \tab new                  \cr
          [1,] \tab this.dir              \tab sys.dir              \cr
          [2,] \tab here                  \tab sys.here             \cr
          [3,] \tab this.proj             \tab sys.proj             \cr
          [4,] \tab rel2here              \tab rel2sys.dir          \cr
          [5,] \tab LINENO                \tab sys.LINENO           \cr
          [6,] \tab try.this.path         \tab try.sys.path         \cr
          [7,] \tab set.this.path         \tab set.sys.path         \cr
          [8,] \tab unset.this.path       \tab unset.sys.path       \cr
          [9,] \tab set.this.path.jupyter \tab set.sys.path.jupyter
        }

        \code{this.path()} was replaced by a function that returns the path of
        the script in which it is written. It does this by looking for a source
        reference with an appropriate source file, or inspects the environment
        in which it is called for an associated path, or returns the path of
        the executing script, as before. Related functions that were replaced:
        \tabular{rllll}{
          [1] \tab this.dir  \tab here       \tab ici           \tab this.proj  \cr
          [5] \tab rel2here  \tab LINENO     \tab try.this.path \tab check.path \cr
          [9] \tab check.dir \tab check.proj \tab               \tab
        }}
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Added \code{env.path()} for determining the path associated with
        the top level environment. Also added \code{env.dir()},
        \code{env.here()}, \code{env.proj()}, \code{rel2env.dir()},
        \code{env.LINENO()}, and \code{try.env.path()}.}

      \item{Added \code{src.path()} for determining the path associated with
        its source reference. Also added \code{src.dir()}, \code{src.here()},
        \code{src.proj()}, \code{rel2src.dir()}, \code{src.LINENO()}, and
        \code{try.src.path()}.}

      \item{Added \code{set.env.path()} and \code{set.src.path()} to work along
        side \code{set.sys.path()}.}

      \item{\code{this.path()} now accepts arguments \codeorsQuote{n},
        \codeorsQuote{envir}, \codeorsQuote{matchThisEnv}, and
        \codeorsQuote{srcfile} to finely control how the path will be
        retrieved.}

      \item{Added \code{rel2proj()} for turning absolute paths into relative
        paths against the script's project root. Also added
        \code{rel2sys.proj()}, \code{rel2env.proj()}, and
        \code{rel2src.proj()}.}

      \item{\code{set.sys.path()} now accepts argument \codeorsQuote{ofile}
        specifying the original file argument. This overwrites the value
        returned by \code{sys.path(original = TRUE)}.}

      \item{Added active bindings \codeorsQuote{FILE} and \codeorsQuote{LINE}
        linking to \code{try.this.path()} and \code{LINENO()} to be used in a
        similar manner to the macros \codeorsQuote{__FILE__} and
        \codeorsQuote{__LINE__} in C.}

      \item{Added \code{path.functions()} which accepts a file path and
        constructs a set of path-related functions, similar to
        \code{this.path()} and associated.}
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item{\code{inside.source()} and \code{set.this.path()} are defunct and
        should be replaced with \code{set.sys.path()}.}

      \item{\code{unset.this.path()} is defunct and should be replaced with
        \code{unset.sys.path()}.}

      \item{\code{set.this.path.jupyter()} is defunct and should be replaced
        with \code{set.sys.path.jupyter()}.}

      \item{\code{reset.this.proj()} is defunct and should be replaced with
        \code{reset.proj()}.}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{Fixed display issues in LaTeX and plain text documentation.}

      \item{\code{reset.proj()} now has environment
        \code{<environment: namespace:this.path>} as do all other exported
        functions.}

      \item{Fixed \code{sys.path(contents = TRUE)} in \sQuote{\RStudio} and
        \sQuote{\VSCode} having a trailing blank string.}

      \item{Fixed \code{sys.path(contents = TRUE)} in \sQuote{\Jupyter} if the
        executing script had yet to be determined.}

      \item{Fixed \code{sys.path(original = NA)} in \sQuote{\RStudio},
        \sQuote{\VSCode}, and \sQuote{\Jupyter} saying
        \code{missing value where TRUE/FALSE needed}.}

      \item{Fixed \code{relpath()} on Windows when comparing paths with network
        shares.}

      \item{Fixed \pkg{this.path} used in the site-wide startup profile file, a
        user profile, or the function \code{.First()} on Windows in
        \sQuote{\RStudio}.}
    }
  }
}
