\name{NEWS}
\title{\pkg{this.path} News}
\encoding{UTF-8}
\section{CHANGES IN VERSION 0.10.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added \code{asArgs()}, \code{fileArgs()}, and
            \code{withArgs()} to more easily provide arguments to a
            script.
      \item Added \code{from.shell()} to determine if a script was run
            from a shell.
      \item Added \code{this.proj(...)}, similar to
            \code{here::here(...)}, except that \code{this.proj()}
            supports multiple projects in one \R session.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{this.path::here()} no longer uses \code{dirname()} on
            a UNC path.
    }
  }
}
\section{CHANGES IN VERSION 0.9.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{this.path()} and all its associated functions now
            work in VSCode.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item when \code{source}-ing a file URL,
            \code{source("file:///path/to/file")}, \code{this.path()}
            no longer attempts to re-open a connection.
    }
  }
}
\section{CHANGES IN VERSION 0.8.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item All previously defunct functions were removed entirely.
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{this.path()}, \code{this.dir()}, \code{shFILE()}, and
            \code{normalized.shFILE()} were given a new argument
            \code{default} that will be evaluated and returned if there
            is no executing script or command line argument
            \option{FILE} was not found.
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item \code{this.path2()}, \code{this.dir2()}, and
            \code{this.dir3()} are deprecated and should be replaced
            with \code{this.path(default = NULL)},
            \code{this.dir(default = NULL)}, and
            \code{this.dir(default = getwd())}, respectively.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{normalized.shFILE()} no longer sets attribute
            \code{"this.path.from.shell"}, that is now handled by
            \code{this.path()}.
    }
  }
}
