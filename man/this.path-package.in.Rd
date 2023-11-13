\name{this.path-package}
\alias{this.path-package}
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
  The most important functions from \pkg{this.path} are
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

  \pkg{this.path} also provides functions for constructing and manipulating
  file paths:

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

  New additions to \pkg{this.path} include:

  \itemize{
    \item{\code{\link{with_site.file}()} and \code{\link{with_init.file}()}
      allow \code{this.path()} and related to be used in the site-wide startup
      profile file or a user profile.}

    \item{\code{\link{LINENO}()} returns the line number of the executing
      expression.}

    \item{\code{\link{set.sys.path}()} implements \code{\link{this.path}()} for
      any \sourcelink-like functions outside of \sourcesandlistlinks.}

    \item{\code{\link{shFILE}()} looks through the command line arguments,
      extracting \option{FILE} from either of the following: \option{-f}
      \option{FILE} or \option{--file=FILE}}
  }
}
\note{
  This package started from a stack overflow posting, found at:

  \href{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script}{https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script}

  If you like this package, please consider upvoting my answer so that more
  people will see it! If you have an issue with this package, please use
  \codelink3{utils:bug.report}{utils::bug.report}{(package = "this.path")} to
  report your issue.
}
\author{
  \packageAuthor{this.path}

  Maintainer: \packageMaintainer{this.path}
}
\keyword{ package }
