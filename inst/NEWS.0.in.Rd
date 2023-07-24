\name{NEWS}
\title{\pkg{this.path} News}
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
      \item{\code{this.path()} is now compatible with GUI \sQuote{\VSCode}.}
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
      \item{\code{normalized.shFILE()} does not set attribute
        \string{this.path.from.shell}, that is handled by \code{this.path()}.}
    }
  }
}
\section{CHANGES IN VERSION 0.7.0 (2022-04-24)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{\code{this.path()} supports \sQuote{Rgui} running with
        \string{ucrt} as the C runtime.}
    }
  }
}
\section{CHANGES IN VERSION 0.6.0 (2022-04-21)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Added \code{shFILE()} and \code{normalized.shFILE()} for extracting
        \optionorsQuote{FILE} from the command line arguments.}
    }
  }
}
\section{CHANGES IN VERSION 0.5.1 (2022-03-12)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Added \code{as.relative.path()} and \code{as.rel.path()} to turn
        absolute paths into relative paths.}
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item{Several functions were made defunct and moved to package
        \pkg{essentials}.}
    }
  }
}
\section{CHANGES IN VERSION 0.5.0 (2022-01-11)}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{\code{this.path()} supports \sQuote{Rgui} running in any of the
        languages listed by
        \code{list.dirs(system.file(package = "translations"),
        full.names = FALSE, recursive = FALSE)}.}
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Added \code{check.path()} and \code{check.dir()} to initialize
        \code{this.path()} and check that it is returning the expected path.}

      \item{Added \code{here()} and \code{ici()} for constructing paths to
        files starting with \code{this.dir()}.}

      \item{Added \code{this.path2()}, \code{this.dir2()}, and
        \code{this.dir3()}, variants of \code{this.path()} and
        \code{this.dir()} which return \codeorsQuote{NULL} or \code{getwd()}
        when there is no executing script.}

      \item{Added \code{withArgs()} to \code{source()} an \R script while
        providing arguments, similar to \sQuote{Rscript} but in the same
        session in an environment of your choosing.}

      \item{Added several other functions that were made defunct soon after.}
    }
  }
}
\section{CHANGES IN VERSION 0.4.4 (2021-03-21)}{
  \subsection{BUG FIXES}{
    \itemize{
      \item{Added tests to ensure \code{this.path()} behaves as expected when
        used in combination with \code{source()}, \code{sys.source()},
        \code{debugSource()} in \sQuote{\RStudio}, and
        \code{testthat::source_file()}.}

      \item{Improved readability of \code{example(this.path)}.}

      \item{Fixed LaTeX errors when creating PDF version of the manual.}
    }
  }
}
\section{CHANGES IN VERSION 0.4.3 (2021-03-18)}{
  \subsection{BUG FIXES}{
    \itemize{
      \item{\code{this.path()}, in relation to \code{sys.source()} and
        \code{testthat::source_file()}, throws an error with the correct call
        and a better message when \codeorsQuote{file = "clipboard"} or
        \codeorsQuote{file = "stdin"}.}
    }
  }
}
\section{CHANGES IN VERSION 0.4.2 (2021-03-17)}{
  \subsection{BUG FIXES}{
    \itemize{
      \item{Added \CRANpkg{testthat} to the Enhances field of the DESCRIPTION
        file.}
    }
  }
}
\section{CHANGES IN VERSION 0.4.1 (2021-03-16)}{
  \subsection{BUG FIXES}{
    \itemize{
      \item{\code{this.path()}, in relation to \code{source()} and
        \code{debugSource()} in \sQuote{\RStudio}, recognizes that
        \codeorsQuote{file = ""}, \codeorsQuote{file = "clipboard"}, and
        \codeorsQuote{file = "stdin"} do not refer to files and skips those
        calls. In relation to \code{sys.source()} and
        \code{testthat::source_file()}, it recognizes
        \codeorsQuote{file = "clipboard"} and \codeorsQuote{file = "stdin"} do
        not refer to files and throws an error.}

      \item{\code{this.path()}, in relation to \code{source()} and
        \code{debugSource()} in \sQuote{\RStudio}, recognizes file URIs and
        turns them into paths.}
    }
  }
}
\section{CHANGES IN VERSION 0.4.0 (2021-03-15)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{\code{this.path()} is now compatible with function
        \code{testthat::source_file()}.}
    }
  }
}
\section{CHANGES IN VERSION 0.3.0 (2021-03-04)}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{\code{this.path()}, in relation to \code{source()} and
        \code{sys.source()}, takes argument \codeorsQuote{chdir} into account,
        particularly \codeorsQuote{chdir = TRUE}.}

      \item{\code{this.path()} saves the normalized path within its appropriate
        environment the first time it is called within a script. This makes it
        faster when called subsequent times within the same script and
        independent of the working directory.}
    }
  }
}
\section{CHANGES IN VERSION 0.2.1 (2020-12-17)}{
  \subsection{BUG FIXES}{
    \itemize{
      \item{\code{this.path()} better determines the executing script when
        running from a shell.}
    }
  }
}
\section{CHANGES IN VERSION 0.2.0 (2020-12-03)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{\code{this.path()} is now compatible with GUI \sQuote{Rgui} and
        function \code{debugSource()} in \sQuote{\RStudio}.}
    }
  }
}
\section{CHANGES IN VERSION 0.1.0 (2020-11-11)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Initial release of \CRANpkg{this.path}. Added \code{this.path()}
        for determining the path of the executing script. Added
        \code{this.dir()} for determining the directory in which the executing
        script is located. Compatible with GUIs \sQuote{\RStudio} and
        \sQuote{Rscript}. Compatible with functions \code{source()} and
        \code{sys.source()}.}
    }
  }
}
