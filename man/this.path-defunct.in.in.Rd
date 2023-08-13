\name{this.path-defunct}
\alias{this.path-defunct}
\alias{this.path2}
\alias{this.dir2}
\alias{this.dir3}
\alias{normalized.shFILE}
\alias{as.rel.path}
\alias{as.relative.path}
\alias{local.path}
\alias{Sys.path}
\alias{Sys.dir}
\alias{reset.this.proj}
\alias{set.this.path.jupyter}
\alias{inside.source}
\alias{set.this.path}
\alias{unset.this.path}
\title{Defunct Functions in Package \pkg{this.path}}
\description{The functions or variables listed here are no longer part of
  \pkg{this.path} as they are no longer needed.}
\usage{
# Defunct in 1.1.0
this.path2(...)
this.dir2(...)
this.dir3(...)
normalized.shFILE(default, else.)

# Defunct in 1.3.0
as.rel.path(path)
as.relative.path(path)

# Defunct in 1.4.0
local.path(verbose = getOption("verbose"), original = FALSE,
    for.msg = FALSE, contents = FALSE, default, else.)
Sys.path()
Sys.dir()

# Defunct in 2.0.0
reset.this.proj()
set.this.path.jupyter(...)
inside.source(file, path.only = FALSE, character.only = path.only,
    file.only = path.only, conv2utf8 = FALSE, allow.blank.string = FALSE,
    allow.clipboard = !file.only, allow.stdin = !file.only, allow.url = !file.only,
    allow.file.uri = !path.only, allow.unz = !path.only, allow.pipe = !file.only,
    allow.terminal = !file.only, allow.textConnection = !file.only,
    allow.rawConnection = !file.only, allow.sockconn = !file.only,
    allow.servsockconn = !file.only, allow.customConnection = !file.only,
    ignore.all = FALSE, ignore.blank.string = ignore.all, ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all, ignore.url = ignore.all, ignore.file.uri = ignore.all,
    Function = NULL, ofile)
set.this.path(file, path.only = FALSE, character.only = path.only,
    file.only = path.only, conv2utf8 = FALSE, allow.blank.string = FALSE,
    allow.clipboard = !file.only, allow.stdin = !file.only, allow.url = !file.only,
    allow.file.uri = !path.only, allow.unz = !path.only, allow.pipe = !file.only,
    allow.terminal = !file.only, allow.textConnection = !file.only,
    allow.rawConnection = !file.only, allow.sockconn = !file.only,
    allow.servsockconn = !file.only, allow.customConnection = !file.only,
    ignore.all = FALSE, ignore.blank.string = ignore.all, ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all, ignore.url = ignore.all, ignore.file.uri = ignore.all,
    Function = NULL, ofile)
unset.this.path()
}
\details{
  \code{this.path2()}, \code{this.dir2()}, and \code{this.dir3()} were used
  before \code{\link{sys.path}()} and \code{\link{sys.dir}()} were given an
  argument \code{default}. Should be replaced by
  \code{\link{sys.path}(default = NULL)},
  \code{\link{sys.dir}(default = NULL)}, and
  \ifelse{html}{\code{\link{sys.dir}(default = \link[base]{getwd}())}}{\code{\link{sys.dir}(default = getwd())}}
  , respectively.

  \code{normalized.shFILE()} was used before \code{\link{shFILE}()} was given
  an argument \code{original}. Should be replaced by \code{\link{shFILE}()}.

  \code{as.rel.path()} and \code{as.relative.path()} were poorly chosen names
  given their functionality. Should be replaced by \code{\link{rel2here}()}.

  \code{local.path()} was used when it called a different C function than
  \code{\link{sys.path}()}. Should be replaced by
  \code{\link{sys.path}(local = TRUE)}.

  \code{Sys.path()} and \code{Sys.dir()} are worse names for \code{sys.path()}
  and \code{sys.dir()}. Functions that start with \string{Sys} are related to
  the operating system, while functions that start with \string{sys} are
  related to the R system; the call stack. These new names are much better.

  \code{reset.this.proj()} was used when \code{this.proj()} was the only
  project related function. Now that \code{sys.proj()} and \code{env.proj()}
  exist, the function was renamed to \code{reset.proj()}.

  \code{this.path()} was renamed to \code{sys.path()}, so
  \code{set.this.path.jupyter()}, \code{set.this.path()}, and
  \code{unset.this.path()} were accordingly renamed to
  \code{set.sys.path.jupyter()}, \code{set.sys.path()}, and
  \code{unset.sys.path()}. Since \code{set.this.path()} was already being made
  defunct, \code{inside.source()} was also made defunct.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
