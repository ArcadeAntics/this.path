\name{@R_PACKAGE_NAME@-package}
\alias{@R_PACKAGE_NAME@-package}
\docType{package}
\title{Get Script's Path}
\description{
  Determine the path of the executing script. Compatible with a few
  popular GUIs: \sQuote{Rgui}, \sQuote{\RStudio}, \sQuote{\VSCode},
  \sQuote{\Jupyter}, \sQuote{\Emacs}, and
  \ifelse{html}{\sQuote{\code{\link[utils]{Rscript}}}}{\ifelse{text}{\code{Rscript}}{\sQuote{\code{Rscript}}}}
  (shell). Compatible with several functions and packages:
  \sourcesandlistlinks.
}
\details{
  The most important functions from \pkg{package:@R_PACKAGE_NAME@} are
  \code{\link{this.path}()}, \code{\link{this.dir}()}, \code{\link{here}()},
  and \code{\link{this.proj}()}:

  \itemize{
    \item{\code{\link{this.path}()} returns the normalized path of the script
      in which it is written.}

    \item{\code{\link{this.dir}()} returns the directory of
      \code{this.path()}.}

    \item{\code{\link{here}()} constructs file paths against
      \code{this.dir()}.}

    \item{\code{\link{this.proj}()} constructs file paths against the project
      root of \code{this.dir()}.}
  }

  New additions include:

  \itemize{
    \item{\code{\link{LINENO}()} returns the line number of the executing
      expression.}

    \item{\code{\link{shFILE}()} looks through the command line arguments,
      extracting \option{FILE} from either of the following: \option{-f}
      \option{FILE} or \option{--file=FILE}}

    \item{\code{\link{set.sys.path}()} implements \code{\link{this.path}()} for
      any \sourcelink-like functions outside of the builtins.}

    \item{\code{\link{with_init.file}()} allows \code{this.path()} and related
      to be used in a user profile.}
  }

  \pkg{package:@R_PACKAGE_NAME@} also provides functions for constructing and
  manipulating file paths:

  \itemize{
    \item{\code{\link{path.join}()}, \code{\link{basename2}()}, and
      \code{\link{dirname2}()} are drop in replacements for
      \codelink3{base}{file.path}{()}, \codelink3{base}{basename}{()}, and
      \codelink3{base:basename}{dirname}{()} which better handle drives and
      network shares.}

    \item{\code{\link{splitext}()}, \code{\link{removeext}()},
      \code{\link{ext}()}, and \code{\link{ext<-}()} split a path into root and
      extension, remove a file extension, get an extension, or set an extension
      for a file path.}

    \item{\code{\link{path.split}()}, \code{\link{path.split.1}()}, and
      \code{\link{path.unsplit}()} split the path to a file into components.}

    \item{\code{\link{relpath}()}, \code{\link{rel2here}()}, and
      \code{\link{rel2proj}()} turn absolute paths into relative paths.}
  }
}
\note{
  This package started from a stack overflow posting:

  \Sexpr[results=rd,stage=install]{if (getRversion() >= "3.0.0") "\\\\href{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649#64129649}{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649#64129649}" else "\\\\ifelse{latex}{\\\\href{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649}{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649}}{\\\\href{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649#64129649}{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649#64129649}}"}

  If you like this package, please consider upvoting my answer so that more
  people will see it! If you have an issue with this package, please use
  \codelink3{utils:bug.report}{bug.report}{(package = "@R_PACKAGE_NAME@")} to
  report your issue.
}
\author{
  @R_PACKAGE_AUTHOR@

  Maintainer: @R_PACKAGE_MAINTAINER@
}
\keyword{package}
