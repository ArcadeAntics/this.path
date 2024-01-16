\name{Enhances}
\alias{Enhances}
\alias{with_script_path}
\alias{make_fix_file}
\title{Functions That Enhance the Use of Other Packages}
\description{
  The functions here improve the user experience of other packages.
}
\usage{
## enchances 'package:box'
with_script_path(expr, local = FALSE, n = 0, envir = parent.frame(n + 1),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0)

## enchances 'package:rprojroot'
make_fix_file(criterion, local = FALSE, n = 0, envir = parent.frame(n + 1),
    matchThisEnv = getOption("topLevelEnvironment"),
    srcfile = if (n) sys.parent(n) else 0)
}
\arguments{
  \item{expr}{an expression to evaluate after setting the current script in
    \CRANpkg{box}; most commonly a call to \codelink3{box:use}{box::use}{()}.}

  \item{criterion}{argument passed to
    \codelink3{rprojroot:find_root}{rprojroot::find_root}{()}.}

  \item{local, n, envir, matchThisEnv, srcfile}{See
    \code{?\link{this.path}()}.}
}
\details{
  \code{with_script_path()} improves the experience of \pkg{package:box} by
  setting the current script to \code{\link{this.path}()} using
  \codelink3{box:script_path}{box::set_script_path}{()} then evaluating a
  provided expression, most commonly a box import statement.

  \code{make_fix_file()} improves the experience of \pkg{package:rprojroot} by
  looking for a project root starting with \code{\link{this.dir}()}.
}
\value{
  for \code{with_script_path()}, the result of evaluating \codeorsQuote{expr}.

  for \code{make_fix_file()}, a function with formals \code{(..., .. = 0)} that
  constructs file paths, starting with the project root.
}
\examples{
# this.path::with_script_path(
# box::use(
#     <import 1>,
#     <import 2>,
#     <...>
# )
# )


# ## substitute 'rprojroot::is_r_package' with desired criterion
#
# fix_file <- this.path::make_fix_file(rprojroot::is_r_package)
}
