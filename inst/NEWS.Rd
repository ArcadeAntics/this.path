\name{NEWS}
\title{\pkg{this.path} News}
\encoding{UTF-8}
\section{CHANGES IN VERSION 1.2.0.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item
        \code{this.path()} now works inside \code{box::use()}

      \item
        Added \code{path.split()}, \code{path.split.1()}, and
        \code{path.unsplit()}.
    }
  }
}
\section{CHANGES IN VERSION 1.2.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item
        Added \code{set.this.path()}, an alias for
        \code{inside.source()}.

      \item
        Added \code{Sys.putenv()}, an alternate method for setting
        environment variables.

      \item
        \code{system.file(package = "this.path", "extdata", "main.R")}
        is the script used to determine the translations of
        \code{"Untitled"} and \code{"R Editor"} between different
        languages and different C runtimes (for use in \sQuote{Rgui}
        for Windows). This script no longer relies on \sQuote{Python}
        nor external \R packages, so anyone can run it.
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item
        \code{shFILE()} now parses the command line arguments in the exact same
        manner as \R itself, more accurately selecting argument \option{FILE}.

      \item
        A relative path returned by \code{as.rel.path()} / /
        \code{relpath()} will always begin with \code{"./"} or
        \code{"../"}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item
        C code no longer uses \code{lang1()} through \code{lang6()} with
        multiple unprotected arguments, uses \code{allocList()} and
        \code{SETCAR()} instead.

      \item
        C code no longer uses \code{sprintf()}, uses \code{snprintf()} instead.

      \item
        \pkg{this.path} does a much better job of determining if the \R session
        is running from \sQuote{VSCode}.

      \item
        \pkg{this.path} does a much better job of determining the path of the
        executing script from \sQuote{Rgui}.
    }
  }
}
\section{CHANGES IN VERSION 1.1.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item
        \code{this.path()} now works inside \code{knitr::knit()}, fixing
        \href{https://github.com/ArcadeAntics/this.path/issues/4}{issue #4 at BugReports URL}.
        By extension, \code{withArgs(knitr::knit())} now also works.

      \item
        \code{this.path()} now accepts arguments \code{original} and
        \code{for.msg} that return the original argument from \code{source} / /
        \code{knit} / / \code{Rscript} or return a character string that can be
        used in diagnostic messages / / warnings / / errors. It should be noted
        that \code{this.path(for.msg = TRUE)} should not be used for
        constructing file paths.

      \item
        \code{shFILE()} also accepts argument \code{original} that returns the
        original \option{FILE} argument from \code{Rscript}.

      \item
        Added \code{splitext()}, \code{removeext()}, \code{ext()}, and
        \code{ext<-()} for manipulating paths with extensions.

      \item
        Added \code{progArgs()}, similar to \code{fileArgs()} but better, might
        remove \code{fileArgs()} later.

      \item
        Added \code{is.main()} for determining if an expression is run in a
        top-level code environment.

      \item
        Added \code{wrap.source()} and \code{inside.source()} for implementing
        \code{this.path()} with any source-like function.

      \item
        Added \code{LINENO()} for determining the line number of the executing
        expression in the executing script.

      \item
        Added \code{relpath()}, a version of \code{as.rel.path()} with
        different default argument values.

      \item
        Added \code{OS.type}, a list of \code{TRUE}/\code{FALSE} values giving
        more details about the operating system.
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item
        \code{normalized.shFILE()}, \code{this.path2()}, \code{this.dir2()},
        and \code{this.dir3()} are defunct and should be replaced with
        \code{shFILE()}, \code{this.path(default = NULL)},
        \code{this.dir(default = NULL)}, and
        \code{this.dir(default = getwd())}, respectively.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item
        \code{check.path()} and \code{check.dir()} now use \code{path.join()}
        instead of \code{file.path()}.

      \item
        \code{fileArgs()}, \code{progArgs()}, \code{from.shell()}, and
        \code{is.main()} no longer force the normalization of the \code{source}
        / / \code{knit} / / \code{Rscript} path, but instead save it as a
        promise to be evaluated later if / / when desired.
    }
  }
}
\section{CHANGES IN VERSION 1.0.2}{
  \subsection{BUG FIXES}{
    \itemize{
      \item
        See \href{https://github.com/ArcadeAntics/this.path/issues/3}{issue #3 at BugReports URL}.
        Fixed \code{this.path()} when called in a script from a shell on Ubuntu
        installed on an Android (where \code{.Platform$GUI == "unknown"},
        normally "unix").
    }
  }
}
\section{CHANGES IN VERSION 1.0.1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item
        Fixed \code{path.join()}, \code{basename2()}, and \code{dirname2()} not
        having a trailing nul-character.
    }
  }
}
\section{CHANGES IN VERSION 1.0.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item
        Added \code{path.join()}, \code{basename2()}, and \code{dirname2()} for
        constructing and manipulating paths to files. These are based on
        \code{file.path()}, \code{basename()}, and \code{dirname()}, but behave
        differently with network shares and a few important edge cases.
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item
        \code{this.dir()}, \code{here()}, and \code{this.proj()} all use
        \code{path.join()} and \code{dirname2()} instead of \code{file.path()}
        and \code{dirname()}, this may change behaviour for some users and
        cases, but should be an improvement overall.
    }
  }
}
\section{CHANGES IN VERSION 0.11.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item
        Added \code{tryCatch2()}, introducing argument \code{else.} similar to
        \code{try except} in \sQuote{Python}.
      \item
        Added \code{else.} argument to \code{this.path()}, \code{this.dir()},
        \code{shFILE()}, and \code{normalized.shFILE()}, a function to act on
        the return value.
    }
  }
}
\section{CHANGES IN VERSION 0.10.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item
        Added \code{asArgs()}, \code{fileArgs()}, and \code{withArgs()} to more
        easily provide arguments to a script.
      \item
        Added \code{from.shell()} to determine if a script was run from a shell.
      \item
        Added \code{this.proj(...)}, similar to \code{here::here(...)}, except
        that \code{this.proj()} supports multiple projects in one \R session
        and is independent of working directory.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item
        \code{this.path::here()} no longer uses \code{dirname()} on a UNC path.
    }
  }
}
\section{CHANGES IN VERSION 0.9.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item
        \code{this.path()} and others now work in VSCode.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item
        when \code{source}-ing a file URL,
        \code{source("file:///path/to/file")}, \code{this.path()} no longer
        attempts to re-open a connection.
    }
  }
}
\section{CHANGES IN VERSION 0.8.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item
        All previously defunct functions were removed entirely.
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item
        \code{this.path()}, \code{this.dir()}, \code{shFILE()}, and
        \code{normalized.shFILE()} were given a new argument \code{default}
        that will be evaluated and returned if there is no executing script or
        command line argument \option{FILE} was not found.
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item
        \code{this.path2()}, \code{this.dir2()}, and \code{this.dir3()} are
        deprecated and should be replaced with
        \code{this.path(default = NULL)}, \code{this.dir(default = NULL)}, and
        \code{this.dir(default = getwd())}, respectively.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item
        \code{normalized.shFILE()} no longer sets attribute
        \code{"this.path.from.shell"}, that is now handled by
        \code{this.path()}.
    }
  }
}
