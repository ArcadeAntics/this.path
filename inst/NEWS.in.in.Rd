\name{NEWS}
\title{\pkg{this.path} News}
\section{CHANGES IN VERSION devel}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{\code{this.path()} is now compatible with package
        \CRANpkg{targets}.}
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

      \item{Fixed \pkg{this.path} used in site-wide startup profile file, the
        user profile, or the function \code{.First()} on Windows in
        \sQuote{\RStudio}.}
    }
  }
}
\section{CHANGES IN VERSION 1.4.0 (2023-04-18)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{\code{this.path()} now accepts argument \codeorsQuote{contents}
        that returns the contents of the executing script.}

      \item{\code{try.this.path()} now accepts argument \codeorsQuote{contents}
        that returns the contents of the executing script, \codeorsQuote{NULL}
        if that fails.}
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item{\code{local.path()} is defunct and should be replaced with
        \code{this.path(local = TRUE)}.}

      \item{\code{Sys.path()} and \code{Sys.dir()} are defunct and should be
        replaced with \code{this.path(verbose = FALSE)} and
        \code{this.dir(verbose = FALSE)}.}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{\code{inside.source()} / / \code{set.this.path()} / /
        \code{this.path(local = TRUE)} more rigorously check that they are
        called from a valid context.}

      \item{Fixed display issues in HTML docs when using \sQuote{Run example}
        button.}
    }
  }
}
\section{CHANGES IN VERSION 1.3.0 (2023-04-08)}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{\code{this.path()} no longer throws an error when multiple
        \optionorsQuote{-f} \optionorsQuote{FILE}, \optionorsQuote{--file=FILE}
        arguments are provided to a shell, instead returning the last
        \optionorsQuote{FILE} (ignoring \optionorsQuote{-f} \optionorsQuote{-},
        \optionorsQuote{--file=-}).}
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Added \code{path.split()}, \code{path.split.1()}, and
        \code{path.unsplit()} for splitting and unsplitting paths into
        components.}

      \item{\code{this.path()} is now compatible with GUI \sQuote{\VSCode} with
        the \sQuote{\radian} console.}

      \item{\code{this.path()} is now compatible with GUI \sQuote{\Jupyter}.
        Added \code{set.this.path.jupyter()} for explicitly declaring the path
        of the \sQuote{\Jupyter} notebook.}

      \item{\code{this.path()} is now compatible with function
        \code{compiler::loadcmp()}.}

      \item{\code{this.path()} is now compatible with function
        \code{box::use()}.}

      \item{For logging purposes, \code{this.path(for.msg = TRUE)} will now
        return \string{Untitled} when appropriate.}

      \item{\code{inside.source()} / / \code{set.this.path()} now accept
        argument \codeorsQuote{Function} specifying the name of the function
        and package in which they are called.}

      \item{\code{this.path()} now accepts argument \codeorsQuote{local} to
        confine the search for the executing script to the local environment in
        which \code{inside.source()} / / \code{set.this.path()} was called.
        Also added \code{local.path()}, short for
        \code{this.path(local = TRUE)}.}

      \item{Added \code{unset.this.path()} to undo a call to
        \code{inside.source()} / / \code{set.this.path()}.}

      \item{Added \code{check.proj()} for checking that \code{this.path()} and
        \code{this.proj()} are working correctly.}

      \item{Added \code{reset.this.proj()} to reset the paths saved by
        \code{this.proj()}.}

      \item{Added \code{try.this.path()} and \code{try.shFILE()} to get the
        normalized path of the executing script, the original path if that
        fails, \codeorsQuote{NA_character_} if that fails as well.}
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item{\code{as.relative.path()} and \code{as.rel.path()} are defunct and
        should be replaced with \code{rel2here()}.}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{\code{shFILE(for.msg = TRUE, default = <...>)} does not evaluate
        \codeorsQuote{default}, as intended. By extension,
        \code{this.path(for.msg = TRUE)} returns \codeorsQuote{NA_character_}
        when running from a shell instead of incorrectly throwing an error.}
    }
  }
}
\section{CHANGES IN VERSION 1.2.0 (2023-01-16)}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{\code{shFILE()} parses the command line arguments in the exact same
        manner as \R itself, more accurately extracting \optionorsQuote{FILE}.}

      \item{A relative path returned by \code{relpath()} / /
        \code{as.rel.path()} will always start with \string{./} or
        \string{../}.}
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Added \code{set.this.path()}, an alias for \code{inside.source()}.}

      \item{Added \code{Sys.putenv()}, an alternate method for setting
        environment variables.}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{C code no longer uses \code{lang1()} through \code{lang6()} with
        multiple unprotected arguments, uses \code{allocList()} and
        \code{SETCAR()} instead.}

      \item{C code no longer uses \code{sprintf()}, uses \code{snprintf()}
        instead.}

      \item{\pkg{this.path} does a much better job of determining if the \R
        session is running from \sQuote{\VSCode}.}

      \item{\pkg{this.path} does a much better job of determining the path of
        the executing script from \sQuote{Rgui}.}
    }
  }
}
\section{CHANGES IN VERSION 1.1.0 (2022-12-01)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{\code{this.path()} is now compatible with function
        \code{knitr::knit()}.}

      \item{\code{this.path()} now accepts argument \codeorsQuote{original}
        that returns the executing script's original (unnormalized) path.}

      \item{\code{this.path()} now accepts argument \codeorsQuote{for.msg} that
        returns a character string that can be used in diagnostic messages / /
        warnings / / errors. It should be noted that
        \code{this.path(for.msg = TRUE)} should not be used for constructing
        file paths.}

      \item{\code{shFILE()} also now accepts argument \codeorsQuote{original}
        that returns the original \optionorsQuote{FILE} from the command line
        arguments.}

      \item{Added \code{splitext()}, \code{removeext()}, \code{ext()}, and
        \code{ext<-()} for manipulating paths with extensions.}

      \item{Added \code{progArgs()}, similar to \code{fileArgs()} but better,
        might remove \code{fileArgs()} later.}

      \item{Added \code{is.main()} for determining if an expression is run in a
        top-level code environment.}

      \item{Added \code{wrap.source()} and \code{inside.source()} for
        implementing \code{this.path()} with any \code{source()}-like
        function.}

      \item{Added \code{LINENO()} for determining the line number of the
        executing expression in the executing script.}

      \item{Added \code{relpath()}, a version of \code{as.rel.path()} with
        different default argument values.}

      \item{Added \codeorsQuote{OS.type}, a list of \codeorsQuote{TRUE} / /
        \codeorsQuote{FALSE} values giving more details about the operating
        system.}
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item{\code{normalized.shFILE()}, \code{this.path2()},
        \code{this.dir2()}, and \code{this.dir3()} are defunct and should be
        replaced with \code{shFILE()}, \code{this.path(default = NULL)},
        \code{this.dir(default = NULL)}, and
        \code{this.dir(default = getwd())}, respectively.}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{\code{check.path()} and \code{check.dir()} use \code{path.join()}
        instead of \code{file.path()}.}

      \item{\code{fileArgs()}, \code{progArgs()}, \code{from.shell()}, and
        \code{is.main()} no longer force the normalization of the executing
        script's path, but instead save it as a promise to be evaluated later
        if / / when desired.}
    }
  }
}
\section{CHANGES IN VERSION 1.0.2 (2022-09-23)}{
  \subsection{BUG FIXES}{
    \itemize{
      \item{\code{this.path()} works when called in a script from a shell on
        Ubuntu installed on an Android (where \code{.Platform$GUI} is
        \string{unknown}, normally \string{unix}).}
    }
  }
}
\section{CHANGES IN VERSION 1.0.1 (2022-09-22)}{
  \subsection{BUG FIXES}{
    \itemize{
      \item{Fixed \code{path.join()}, \code{basename2()}, and \code{dirname2()}
        not having a trailing nul-character.}
    }
  }
}
\section{CHANGES IN VERSION 1.0.0 (2022-09-21)}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{\code{this.dir()}, \code{here()}, and \code{this.proj()} all use
        \code{path.join()} and \code{dirname2()} instead of \code{file.path()}
        and \code{dirname()}, this may change behaviour for some users and
        cases, but should be an improvement overall.}
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Added \code{path.join()}, \code{basename2()}, and \code{dirname2()}
        for constructing and manipulating paths to files. These are based on
        \code{file.path()}, \code{basename()}, and \code{dirname()}, but behave
        differently with network shares and a few important edge cases.}
    }
  }
}
