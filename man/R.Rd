\name{R}
\alias{R}
\alias{Rcmd}
\alias{Rscript}
\alias{Rterm}
\title{
Start a New R Session
}
\description{
Start a new \R session with the specified options.
}
\usage{
R(options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE,
    intern = FALSE, wait = TRUE, mustWork = FALSE, ...,
    quiet = Sys.getenv("R_THIS_PATH_QUIET", intern))

Rcmd(options = NULL, command = "", args = NULL,
    intern = FALSE, wait = TRUE, mustWork = FALSE, ...,
    quiet = Sys.getenv("R_THIS_PATH_QUIET", intern))

Rscript(options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE,
    intern = FALSE, wait = TRUE, mustWork = FALSE, ...,
    quiet = Sys.getenv("R_THIS_PATH_QUIET", intern))

Rterm(options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE,
    intern = FALSE, wait = TRUE, mustWork = FALSE, ...,
    quiet = Sys.getenv("R_THIS_PATH_QUIET", intern))
}
\arguments{
  \item{options}{
character vector. The options to be provided for the new \R session. For
\code{Rcmd} on Windows, \code{options} will not do anything.
  }
  \item{file}{
character string. The filename of the \R script to run in the new \R session,
provided to option \option{--file=}.
  }
%   \item{exprs}{
% any \R object. The expressions to be run in the new \R session, provided to
% option \option{-e}.
%   }
  \item{exprs}{
character vector. The expressions to be run in the new \R session, provided to
option \option{-e}.
  }
%   \item{exprs.literal}{
% logical. If \code{TRUE} \emph{and} \code{exprs} is a character vector, then
% \code{exprs} is \emph{not} deparsed before being provided to option \option{-e}.
%   }
  \item{args}{
any \R object. The command-line arguments to be provided \emph{after}
\option{--args}, or for \code{Rcmd}, arguments provided after \code{command}.
Converted to character in a similar manner to \code{\link{withArgs}}.
  }
%   \item{width.cutoff}{
% integer, passed to \code{\link[base]{deparse}()} which is used (only) when there
% are no source references.
%   }
%   \item{deparseCtrl}{
% \code{\link[base]{character}} vector, passed as \code{control} to
% \code{\link[base]{deparse}()}, see also \code{\link[base]{.deparseOpts}}.
%   }
  \item{chdir}{
logical; if \code{TRUE} and \code{file} is a pathname, the \R working directory
is temporarily changed to the directory containing \code{file} for evaluating.
  }
%   \item{keep.source}{
% logical: should the source formatting be retained, if possible?
%   }
  \item{intern}{
logical, indicates whether to make the output of the command an \R object.
  }
  \item{wait}{
should the \R interpreter wait for the command to finish? The default is to
wait, and the interpreter will always wait if \code{intern = TRUE}.
  }
  \item{mustWork}{
logical; if \code{TRUE} failure to run the command will give an \R error, if
\code{FALSE} a warning, and if \code{NA}, no \R message.
  }
  \item{\dots}{
further arguments passed to \code{\link[base]{system}()}.
  }
  \item{quiet}{
logical. Should the command-line / / terminal prompt and exit message be printed
before and after the \R session?
  }
  \item{command}{
character string. Naming one of the commands to execute from \command{R CMD}
usage message.
  }
}
\details{
\sQuote{Rterm} is an executable only available on Windows, but it is preferable
because it has more intuitive quoting rules than \sQuote{R} (see
\code{\link{commandQuote}}). If \code{Rterm} is called on a Unix-alike OS,
\code{R} is called instead.
}
\value{
The value of \code{\link[base]{system}()}. If \code{intern = FALSE}, it is
returned invisibly.
}
\examples{
# if you're on Windows, you should notice that the quoting rules for 'Rterm' are
# far more intuituve than for 'R'
#
# if you're under a Unix-alike, you should notice that 'Rterm' and 'R' do the
# same thing


R      (exprs = r"{cat(commandArgs(), sep = "\n")}")
Rterm  (exprs = r"{cat(commandArgs(), sep = "\n")}")
Rscript(exprs = r"{cat(commandArgs(), sep = "\n")}")
Rscript(exprs = r"{cat(commandArgs(), sep = "\n")}", quiet = TRUE)


Rcmd(command = "INSTALL", args = "--help")
}
