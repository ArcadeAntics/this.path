\name{NEWS}
\title{\pkg{this.path} News}
\encoding{UTF-8}
\section{CHANGES IN VERSION devel}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{
\code{\link{this.path}()} no longer throws an error when multiple \option{-f}
\option{FILE}, \option{--file=FILE} arguments are provided to a shell, and
instead returns the last \option{FILE} (ignoring \option{-f} \option{-} and
\option{--file=-}).
      }
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item{
Added \code{\link{path.split}()}, \code{\link{path.split.1}()}, and
\code{\link{path.unsplit}()} for splitting and unsplitting paths into
components.
      }
      \item{
\code{\link{this.path}()} now works when run in \sQuote{\VSCode} with the
\sQuote{\radian} console.
      }
      \item{
\code{\link{this.path}()} now works in \sQuote{\Jupyter}. Added
\code{\link{set.this.path.jupyter}()} for explicitly declaring the path of the
\sQuote{\Jupyter} notebook.
      }
      \item{
\code{\link{this.path}()} now works inside \code{\link[box:use]{box::use}()}.
      }
      \item{
For logging purposes, \code{\link{this.path}(for.msg = TRUE)} will now return
\code{"Untitled"} when appropriate.
      }
      \item{
\code{\link{inside.source}()} / / \code{\link{set.this.path}()} now accept
argument \code{Function} specifying the name of the function and package in
which they are called.
      }
      \item{
\code{\link{this.path}()} now accepts argument \code{local} to confine the
search for the executing script to the local environment in which
\code{\link{inside.source}()} / / \code{\link{set.this.path}()} was called.
Also added \code{\link{local.path}()}, short for
\code{\link{this.path}(local = TRUE)}.
      }
      \item{
Added \code{\link{unset.this.path}()} to undo a call to
\code{\link{inside.source}()} / / \code{\link{set.this.path}()}.
      }
      \item{
Added \code{\link{check.proj}()} for checking that \code{\link{this.path}()}
and \code{\link{this.proj}()} are working correctly.
      }
      \item{
Added \code{\link{reset.this.proj}()} to reset the paths saved by
\code{\link{this.proj}()}.
      }
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item{
\code{\link{as.relative.path}()} and \code{\link{as.rel.path}()} are defunct
and should be replaced with \code{\link{rel2here}()}.
      }
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{
\code{\link{shFILE}(for.msg = TRUE, default = <...>)} now does not evaluate
\code{default}, as intended. By extension,
\code{\link{this.path}(for.msg = TRUE)} now returns
\code{\link[base:NA]{NA_character_}} when running from a shell instead of
incorrectly throwing an error.
      }
    }
  }
}
\section{CHANGES IN VERSION 1.2.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{
Added \code{\link{set.this.path}()}, an alias for
\code{\link{inside.source}()}.
      }
      \item{
Added \code{\link{Sys.putenv}()}, an alternate method for setting environment
variables.
      }
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{
\code{\link{shFILE}()} now parses the command line arguments in the exact same
manner as \R itself, more accurately selecting \option{FILE}.
      }
      \item{
A relative path returned by \code{\link{relpath}()} / /
\code{\link{as.rel.path}()} will always begin with \code{"./"} or \code{"../"}.
      }
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{
C code no longer uses \code{lang1()} through \code{lang6()} with multiple
unprotected arguments, uses \code{allocList()} and \code{SETCAR()} instead.
      }
      \item{
C code no longer uses \code{sprintf()}, uses \code{snprintf()} instead.
      }
      \item{
\pkg{this.path} does a much better job of determining if the \R session is
running from \sQuote{\VSCode}.
      }
      \item{
\pkg{this.path} does a much better job of determining the path of the executing
script from \sQuote{Rgui}.
      }
    }
  }
}
\section{CHANGES IN VERSION 1.1.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{
\code{\link{this.path}()} now works inside
\code{\link[knitr:knit]{knitr::knit}()}. By extension,
\code{\link{withArgs}(knitr::knit())} now also works.
      }
      \item{
\code{\link{this.path}()} now accepts arguments \code{original} and
\code{for.msg} that return the original argument from
\code{\link[base]{source}()} / / \code{\link[knitr:knit]{knitr::knit}()} / /
\code{\link[utils]{Rscript}} or return a character string that can be used in
diagnostic messages / / warnings / / errors. It should be noted that
\code{\link{this.path}(for.msg = TRUE)} should not be used for constructing
file paths.
      }
      \item{
\code{\link{shFILE}()} also accepts argument \code{original} that returns the
original \option{FILE} from \code{\link[utils]{Rscript}}.
      }
      \item{
Added \code{\link{splitext}()}, \code{\link{removeext}()}, \code{\link{ext}()},
and \code{\link{ext<-}()} for manipulating paths with extensions.
      }
      \item{
Added \code{\link{progArgs}()}, similar to \code{\link{fileArgs}()} but better,
might remove \code{\link{fileArgs}()} later.
      }
      \item{
Added \code{\link{is.main}()} for determining if an expression is run in a
top-level code environment.
      }
      \item{
Added \code{\link{wrap.source}()} and \code{\link{inside.source}()} for
implementing \code{\link{this.path}()} with any
\code{\link[base]{source}()}-like function.
      }
      \item{
Added \code{\link{LINENO}()} for determining the line number of the executing
expression in the executing script.
      }
      \item{
Added \code{\link{relpath}()}, a version of \code{\link{as.rel.path}()} with
different default argument values.
      }
      \item{
Added \code{\link{OS.type}}, a list of \code{TRUE}/\code{FALSE} values giving
more details about the operating system.
      }
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item{
\code{\link{normalized.shFILE}()}, \code{\link{this.path2}()},
\code{\link{this.dir2}()}, and \code{\link{this.dir3}()} are defunct and should
be replaced with \code{\link{shFILE}()},
\code{\link{this.path}(default = NULL)},
\code{\link{this.dir}(default = NULL)}, and
\code{\link{this.dir}(default = \link[base]{getwd}())}, respectively.
      }
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{
\code{\link{check.path}()} and \code{\link{check.dir}()} now use
\code{\link{path.join}()} instead of \code{\link[base]{file.path}()}.
      }
      \item{
\code{\link{fileArgs}()}, \code{\link{progArgs}()}, \code{\link{from.shell}()},
and \code{\link{is.main}()} no longer force the normalization of the
\code{\link[base]{source}()} / / \code{\link[knitr:knit]{knitr::knit}()} / /
\code{\link[utils]{Rscript}} path, but instead save it as a
\link[base:delayedAssign]{promise} to be evaluated later if / / when desired.
      }
    }
  }
}
\section{CHANGES IN VERSION 1.0.2}{
  \subsection{BUG FIXES}{
    \itemize{
      \item{
\code{\link{this.path}()} now works when called in a script from a shell on
Ubuntu installed on an Android (where
\code{\link[base:.Platform]{.Platform}$GUI == "unknown"}, normally
\code{"unix"}).
      }
    }
  }
}
\section{CHANGES IN VERSION 1.0.1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item{
Fixed \code{\link{path.join}()}, \code{\link{basename2}()}, and
\code{\link{dirname2}()} not having a trailing nul-character.
      }
    }
  }
}
\section{CHANGES IN VERSION 1.0.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{
Added \code{\link{path.join}()}, \code{\link{basename2}()}, and
\code{\link{dirname2}()} for constructing and manipulating paths to files.
These are based on \code{\link[base]{file.path}()},
\code{\link[base]{basename}()}, and \code{\link[base:basename]{dirname}()}, but
behave differently with network shares and a few important edge cases.
      }
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item{
\code{\link{this.dir}()}, \code{\link{here}()}, and \code{\link{this.proj}()}
all use \code{\link{path.join}()} and \code{\link{dirname2}()} instead of
\code{\link[base]{file.path}()} and \code{\link[base:basename]{dirname}()},
this may change behaviour for some users and cases, but should be an
improvement overall.
      }
    }
  }
}
\section{CHANGES IN VERSION 0.11.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{
Added \code{\link{tryCatch2}()}, introducing argument \code{else.} similar to
\code{try except} in \sQuote{\Python}.
      }
      \item{
Added \code{else.} argument to \code{\link{this.path}()},
\code{\link{this.dir}()}, \code{\link{shFILE}()}, and
\code{\link{normalized.shFILE}()}, a function to act on the return value.
      }
    }
  }
}
\section{CHANGES IN VERSION 0.10.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{
Added \code{\link{asArgs}()}, \code{\link{fileArgs}()}, and
\code{\link{withArgs}()} to more easily provide arguments to a script.
      }
      \item{
Added \code{\link{from.shell}()} to determine if a script was run from a shell.
      }
      \item{
Added \code{\link{this.proj}(...)}, similar to
\code{\link[here:here]{here::here}(...)}, except that \code{\link{this.proj}()}
supports multiple projects in one \R session and is independent of working
directory.
      }
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{
\code{\link[this.path:here]{this.path::here}()} no longer uses
\code{\link[base:basename]{dirname}()} on a UNC path.
      }
    }
  }
}
\section{CHANGES IN VERSION 0.9.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{
\code{\link{this.path}()} and others now work in \sQuote{\VSCode}.
      }
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{
when \code{\link[base]{source}()}-ing a file URL,
\code{source("file:///path/to/file")}, \code{\link{this.path}()} no longer
attempts to re-open a connection.
      }
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
      \item{
\code{\link{this.path}()}, \code{\link{this.dir}()}, \code{\link{shFILE}()},
and \code{\link{normalized.shFILE}()} were given a new argument \code{default}
that will be evaluated and returned if there is no executing script or command
line argument \option{FILE} was not found.
      }
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item{
\code{\link{this.path2}()}, \code{\link{this.dir2}()}, and
\code{\link{this.dir3}()} are deprecated and should be replaced with
\code{\link{this.path}(default = NULL)},
\code{\link{this.dir}(default = NULL)}, and
\code{\link{this.dir}(default = \link[base]{getwd}())}, respectively.
      }
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item{
\code{\link{normalized.shFILE}()} no longer sets attribute
\code{"this.path.from.shell"}, that is now handled by
\code{\link{this.path}()}.
      }
    }
  }
}
