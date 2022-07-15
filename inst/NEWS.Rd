\name{NEWS}
\title{\pkg{this.path} News}
\encoding{UTF-8}
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
