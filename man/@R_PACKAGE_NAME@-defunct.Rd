\name{@R_PACKAGE_NAME@-defunct}
\alias{@R_PACKAGE_NAME@-defunct}
\alias{this.path2}
\alias{this.dir2}
\alias{this.dir3}
\alias{normalized.shFILE}
\alias{as.rel.path}
\alias{as.relative.path}
\alias{local.path}
\alias{reset.this.proj}
\alias{set.this.path.jupyter}
\alias{inside.source}
\alias{set.this.path}
\alias{unset.this.path}
\alias{set.sys.path.jupyter}
\alias{fileArgs}
\title{Defunct Functions in Package '@R_PACKAGE_NAME@'}
\description{
  The functions or variables listed here are no longer part of
  \pkg{@R_PACKAGE_NAME@} as they are no longer needed.
}
\usage{
# Defunct in this.path 1.1.0 (2022-12-01)
this.path2(...)
this.dir2(...)
this.dir3(...)
normalized.shFILE(default, else.)

# Defunct in this.path 1.3.0 (2023-04-09)
as.rel.path(path)
as.relative.path(path)

# Defunct in this.path 1.4.0 (2023-04-18)
local.path(verbose = getOption("verbose"), original = FALSE,
    for.msg = FALSE, contents = FALSE, default, else.)

# Defunct in this.path 2.0.0 (2023-08-09)
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

# Defunct in this.path 2.4.0 (2024-02-16)
set.sys.path.jupyter(...)
fileArgs()
}
\details{
  \code{this.path2()}, \code{this.dir2()}, and \code{this.dir3()} were used
  before \code{\link{this.path}()} and \code{\link{this.dir}()} were given an
  argument \code{default}. Should be replaced by
  \code{\link{this.path}(default = NULL)},
  \code{\link{this.dir}(default = NULL)}, and
  \ifelse{html}{\code{\link{this.dir}(default = \link[base]{getwd}())}}{\code{this.dir(default = getwd())}},
  respectively.

  \code{normalized.shFILE()} was used before \code{\link{shFILE}()} was given
  an argument \code{original}. Should be replaced by \code{\link{shFILE}()}.

  \code{as.rel.path()} and \code{as.relative.path()} were poorly chosen names
  given their functionality. Should be replaced by \code{\link{rel2here}()}.

  \code{local.path()} was used when it called a different C function than
  \code{\link{this.path}()}. Should be replaced by
  \code{\link{this.path}(local = TRUE)}.

  \code{reset.this.proj()} was used when \code{\link{this.proj}()} was the only
  project related function. Now that \code{\link{src.proj}()},
  \code{\link{env.proj}()}, and \code{\link{sys.proj}()} exist, the function
  was renamed to \code{\link{reset.proj}()}.

  In \pkg{this.path 2.0.0}, the original implementation of
  \code{\link{this.path}()} was renamed to \code{\link{sys.path}()}, so
  \code{set.this.path.jupyter()}, \code{set.this.path()}, and
  \code{unset.this.path()} were accordingly renamed to
  \code{\link{set.jupyter.path}()}, \code{\link{set.sys.path}()}, and
  \code{\link{unset.sys.path}()}. Since \code{set.this.path()} was already
  being made defunct, \code{inside.source()} was also made defunct.

  \code{set.sys.path.jupyter()} was renamed to
  \code{\link{set.jupyter.path}()}.

  \code{fileArgs()} should be replaced by \code{\link{progArgs}()}.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
