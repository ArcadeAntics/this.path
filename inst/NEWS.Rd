\name{NEWS}
\title{\pkg{this.path} News}
\newcommand{\commandorsQuote}{\ifelse{text}{\sQuote{#1}}{\command{#1}}}
\newcommand{\codeorsQuote}{\ifelse{text}{\sQuote{#1}}{\code{#1}}}
\newcommand{\optionorsQuote}{\ifelse{text}{\sQuote{#1}}{\option{#1}}}
\section{CHANGES IN VERSION devel}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{\code{this.path()} now accepts argument \codeorsQuote{contents}
        that returns the contents of the executing script.}

      \item{\code{try.this.path()} now accepts argument \codeorsQuote{contents}
        to get the contents of the executing script, \codeorsQuote{NULL} if
        that fails.}
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
        arguments are provided to a shell, and instead returns the last
        \optionorsQuote{FILE} (ignoring \optionorsQuote{-f} \optionorsQuote{-},
        \optionorsQuote{--file=-}).}
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Added \code{path.split()}, \code{path.split.1()}, and
        \code{path.unsplit()} for splitting and unsplitting paths into
        components.}

      \item{\code{this.path()} now works when run in \sQuote{\VSCode} with the
        \sQuote{\radian} console.}

      \item{\code{this.path()} now works in \sQuote{\Jupyter}. Added
        \code{set.this.path.jupyter()} for explicitly declaring the path of the
        \sQuote{\Jupyter} notebook.}

      \item{\code{this.path()} now works inside \code{compiler::loadcmp()}.}

      \item{\code{this.path()} now works inside \code{box::use()}.}

      \item{For logging purposes, \code{this.path(for.msg = TRUE)} will now
        return \code{"Untitled"} when appropriate.}

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
      \item{\code{shFILE(for.msg = TRUE, default = <...>)} now does not
        evaluate \codeorsQuote{default}, as intended. By extension,
        \code{this.path(for.msg = TRUE)} now returns
        \codeorsQuote{NA_character_} when running from a shell instead of
        incorrectly throwing an error.}
    }
  }
}
\section{CHANGES IN VERSION 1.2.0 (2023-01-16)}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{\code{shFILE()} now parses the command line arguments in the exact
        same manner as \R itself, more accurately selecting
        \optionorsQuote{FILE}.}

      \item{A relative path returned by \code{relpath()} / /
        \code{as.rel.path()} will always start with \code{"./"} or
        \code{"../"}.}
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
      \item{\code{this.path()} now works inside \code{knitr::knit()}. By
        extension, \code{withArgs(knitr::knit())} now also works.}

      \item{\code{this.path()} now accepts argument \codeorsQuote{original}
        that returns the executing script's original (unnormalized) path.}

      \item{\code{this.path()} now accepts argument \codeorsQuote{for.msg} that
        returns a character string that can be used in diagnostic messages / /
        warnings / / errors. It should be noted that
        \code{this.path(for.msg = TRUE)} should not be used for constructing
        file paths.}

      \item{\code{shFILE()} also accepts argument \codeorsQuote{original} that
        returns the original \optionorsQuote{FILE} from the command line
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
      \item{\code{check.path()} and \code{check.dir()} now use
        \code{path.join()} instead of \code{file.path()}.}

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
      \item{\code{this.path()} now works when called in a script from a shell
        on Ubuntu installed on an Android (where
        \code{.Platform$GUI == "unknown"}, normally \code{"unix"}).}
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
\section{CHANGES IN VERSION 0.11.0 (2022-09-03)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Added \code{tryCatch2()}, introducing argument \codeorsQuote{else.}
        similar to \codeorsQuote{try except} in \sQuote{\Python}.}

      \item{Added \codeorsQuote{else.} argument to \code{this.path()},
        \code{this.dir()}, \code{shFILE()}, and \code{normalized.shFILE()}, a
        function to act on the return value.}
    }
  }
}
\section{CHANGES IN VERSION 0.10.0 (2022-07-17)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Added \code{asArgs()}, \code{fileArgs()}, and \code{withArgs()} to
        more easily provide arguments to a script.}

      \item{Added \code{from.shell()} to determine if a script was run from a
        shell.}

      \item{Added \code{this.proj(...)}, similar to \code{here::here(...)},
        except that \code{this.proj()} supports multiple projects in one \R
        session and is independent of working directory.}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{\code{this.path::here()} no longer uses \code{dirname()} on a UNC
        path.}
    }
  }
}
\section{CHANGES IN VERSION 0.9.0 (2022-07-15)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{\code{this.path()} and others now work in \sQuote{\VSCode}.}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{when \code{source()}-ing a file URL,
        \code{source("file:///path/to/file")}, \code{this.path()} no longer
        attempts to re-open a connection.}
    }
  }
}
\section{CHANGES IN VERSION 0.8.0 (2022-07-08)}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{All previously defunct functions were removed entirely.}
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item{\code{this.path()}, \code{this.dir()}, \code{shFILE()}, and
        \code{normalized.shFILE()} were given a new argument \codeorsQuote{default}
        that will be evaluated and returned if there is no executing script or
        command line argument \optionorsQuote{FILE} was not found.}
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item{\code{this.path2()}, \code{this.dir2()}, and \code{this.dir3()} are
        deprecated and should be replaced with
        \code{this.path(default = NULL)}, \code{this.dir(default = NULL)}, and
        \code{this.dir(default = getwd())}, respectively.}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{\code{normalized.shFILE()} no longer sets attribute
        \code{"this.path.from.shell"}, that is now handled by
        \code{this.path()}.}
    }
  }
}
