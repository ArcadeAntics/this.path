\name{Enhances}
\alias{with_script_path}
\alias{make_fix_file}
\title{Functions That Enhance the Use of Other Packages}
\description{
  These functions improve the user experience of other packages.
}
\usage{
## enchances 'package:box'
with_script_path(expr, file, local = FALSE, n = 0, envir = parent.frame(n + 1),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0)

## enchances 'package:rprojroot'
make_fix_file(criterion, local = FALSE, n = 0, envir = parent.frame(n + 1),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0)
}
\arguments{
  \item{expr}{an expression to evaluate after setting the current script in
    \pkg{package:box}; most commonly a call to
    \codelink3{box:use}{box::use}{()}.}

  \item{file}{a character string giving the pathname of the file.}

  \item{criterion}{argument passed to
    \codelink3{rprojroot:find_root}{rprojroot::find_root}{()}.}

  \item{local, n, envir, matchThisEnv, srcfile}{See
    \code{?\link{this.path}()}.}
}
\details{
  \code{with_script_path()} improves the experience of \CRANpackage{box}; it
  sets the current script in \pkg{package:box} to \code{file} or
  \code{\link{this.path}()} using
  \codelink3{box:script_path}{box::set_script_path}{()}, then evaluates its
  argument, most commonly a \pkg{package:box} import statement.

  \code{make_fix_file()} improves the experience of \CRANpackage{rprojroot}; it
  looks for a project root starting with \code{\link{this.dir}()}, then makes a
  function that constructs file paths against said project root.
}
\value{
  for \code{with_script_path()}, the result of evaluating \code{expr}.

  for \code{make_fix_file()}, a function with formals \code{(..., .. = 0)} that
  returns a character vector.
}
\examples{
# @R_PACKAGE_NAME@::with_script_path(
# box::use(
#     <import 1>,
#     <import 2>,
#     <...>
# )
# )


# ## replace 'rprojroot::is_r_package' with desired criterion
#
# fix_file <- @R_PACKAGE_NAME@::make_fix_file(rprojroot::is_r_package)
}
