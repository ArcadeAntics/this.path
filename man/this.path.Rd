\name{this.path}
\alias{env.path}
\alias{env.dir}
\alias{src.path}
\alias{src.dir}
\alias{this.path}
\alias{this.dir}
\title{Determine Script's Filename}
\description{
  \code{env.path()} returns the
  \ifelse{html}{\codelink2{base:normalizePath}{normalized}}{normalized} path
  associated with the top level environment (see
  \ifelse{html}{\code{?\link[base:ns-topenv]{topenv}}}{\code{?topenv}}).

  \code{env.dir()} returns the directory of \code{env.path()}.

  \code{src.path()} returns the normalized path associated with its source
  reference.

  \code{src.dir()} returns the directory of \code{src.path()}.

  \code{this.path()} returns the normalized path of the script in which it was
  written.

  \code{this.dir()} returns the directory of \code{this.path()}.
}
\usage{
env.path(verbose = getOption("verbose"), original = FALSE,
         for.msg = FALSE, contents = FALSE, n = 0,
         envir = parent.frame(n + 1),
         matchThisEnv = getOption("topLevelEnvironment"),
         default, else.)
env.dir(verbose = getOption("verbose"), n = 0,
        envir = parent.frame(n + 1),
        matchThisEnv = getOption("topLevelEnvironment"),
        default, else.)

src.path(verbose = getOption("verbose"), original = FALSE,
         for.msg = FALSE, contents = FALSE, n = 0,
         srcfile = sys.call(if (n) sys.parent(n) else 0),
         default, else.)
src.dir(verbose = getOption("verbose"), n = 0,
        srcfile = sys.call(if (n) sys.parent(n) else 0),
        default, else.)

this.path(verbose = getOption("verbose"), original = FALSE,
          for.msg = FALSE, contents = FALSE, local = FALSE,
          n = 0, envir = parent.frame(n + 1),
          matchThisEnv = getOption("topLevelEnvironment"),
          srcfile = sys.call(if (n) sys.parent(n) else 0),
          default, else.)
this.dir(verbose = getOption("verbose"), local = FALSE,
         n = 0, envir = parent.frame(n + 1),
         matchThisEnv = getOption("topLevelEnvironment"),
         srcfile = sys.call(if (n) sys.parent(n) else 0),
         default, else.)
}
\arguments{
  \item{verbose, original, for.msg, contents, local, default, else.}{See
    \code{?\link{sys.path}()}.}

  \item{n}{the number of additional generations to go back. By default,
    \code{this.path()} will look for a path based on the call to
    \code{this.path()} and the environment in which \code{this.path()} was
    called. This can be changed to be based on the call \code{n} levels up the
    call stack. See section \strong{Argument 'n'} for more details.}

  \item{envir, matchThisEnv}{arguments passed to
    \codelink3{base:ns-topenv}{topenv}{()} to determine the top level
    environment in which to search for an associated path.}

  \item{srcfile}{source file in which to search for a pathname, or an object
    containing a source file. This includes a source reference, a call, an
    expression object, or a closure.}
}
\details{
  There are two ways in which \code{env.path()} will find a path associated
  with the top level environment:

  \enumerate{
    \item{from a \CRANpkg{box} module's namespace.}

    \item{from an attribute \string{path}.}
  }

  If \code{env.path()} does not find an associated path, it will throw an
  error.

  \code{src.path()} will look for a source file in its argument. It will look
  at the bindings \code{filename} and \code{wd} to determine the associated
  file path. Filenames such as \blankstring, \string{clipboard}, and
  \string{stdin} will be ignored as they do not refer to files. A source file
  of class \string{srcfilecopy} in which binding \code{isFile} is \code{FALSE}
  will also be ignored. A source file of class \string{srcfilealias} will use
  the aliased \code{filename} in determining the associated path.

  If \code{src.path()} does not find an associated path, it will throw an
  error.

  \code{this.path()} determines the path of the script in which it is written
  by:

  \enumerate{
    \item{examining its \code{srcfile} argument, looking for an associated
      path, the same as \code{src.path()}.}

    \item{examining the top level environment, looking for an associated path,
      the same as \code{env.path()}.}

    \item{examining the call stack, looking for the path of the executing
      script, the same as \code{\link{sys.path}()}.}
  }

  \code{this.path()} and \code{this.dir()} are likely the functions you want to
  use. \code{env.path()}, \code{env.dir()}, \code{src.path()}, and
  \code{src.dir()} are provided for completeness / / convenience but are less
  general purpose. If you need to know the path of the executing script,
  perhaps for logging purposes, then you should use \code{sys.path()} and
  \code{sys.dir()}.
}
\value{
  character string.
}
\section{Argument 'n'}{
  By default, \code{this.path()} will look for a path based on the call to
  \code{this.path()} and the environment in which \code{this.path()} was
  called. For example: \preformatted{{
#line 1 "file1.R"
fun <- function() this.path::this.path(original = TRUE)
fun()
}


{
#line 1 "file2.R"
fun()
}}

  Both of these will return \string{file1.R} because that is where the call to
  \code{this.path()} is written.

  Consider another scenario in which you instead want the path where the call
  to \code{fun()} is written. Pass argument \code{n = 1} to do so: \preformatted{{
#line 1 "file1.R"
fun <- function() this.path::this.path(original = TRUE, n = 1)
fun()
}


{
#line 1 "file2.R"
fun()
}}

  These will return \string{file1.R} and \string{file2.R}, respectively,
  because those are where the calls to \code{fun()} are written.

  If you have multiple interconnected functions that you would like to all
  behave in the described manner, add a formal argument \code{n = 0} and pass
  \code{n = n + 1} to each associated function: \preformatted{{
#line 1 "file1.R"
fun1 <- function(n = 0) {
    this.path::this.path(original = TRUE, n = n + 1)
}
fun1()
}


{
#line 1 "file2.R"
fun2 <- function(n = 0) fun1(n = n + 1)
list(fun1 = fun1(), fun2 = fun2())
}


{
#line 1 "file3.R"
fun3 <- function(n = 0) fun2(n = n + 1)
list(fun1 = fun1(), fun2 = fun2(), fun3 = fun3())
}}

  Within each file, all these functions will return the path in which they are
  called.
}
\note{
  If you are using \code{this.path()} without modularizing your code, i.e.
  relying on source references, you should be aware of how \R stores source
  references. Only top level expressions will be given a source reference which
  includes expressions directly inside braces; sub-expressions will not receive
  a source reference. For example: \preformatted{fun <- function ()
{
    ## this one will have a source reference
    ## because it is a top level expression
    x <- this.path::this.path(verbose = TRUE)
    print(x)
    ## this one will not have a source reference
    ## because it is just a sub-expression
    ## `print()` is the top level expression
    print(this.path::this.path(verbose = TRUE))
    ## this one will have a source reference
    ## note the surrounding braces
    print({ this.path::this.path(verbose = TRUE) })
    ## this one will have a source reference because `return()`
    ## and `list()` are not "closure" type functions, they are
    ## "special" and "builtin" type functions
    return(list(this.path::this.path(verbose = TRUE)))
}}

  One (somewhat extreme) attitude of defensive programming is to always wrap
  braces around your calls to \code{this.path()} as well as all related
  functions: \code{this.dir()}, \code{\link{here}()}, \code{\link{ici}()},
  \code{\link{this.proj}()}, \code{\link{rel2here}()},
  \code{\link{rel2proj}()}, \code{\link{LINENO}()},
  \code{\link{try.this.path}()}, \code{\link{path.functions}()} (if \code{file}
  is missing), \code{\link{check.path}()}, \code{\link{check.dir}()}, and
  \code{\link{check.proj}()}, as well as the active bindings \code{\link{FILE}}
  and \code{\link{LINE}}.
}
\seealso{
  \code{\link{shFILE}()}
}
\examples{
## the important difference between 'this.path()' and 'sys.path()'


FILE1.R <- tempfile("FILE1-", fileext = ".R")
this.path:::.write.code({
    fun <- function() {
        cat("\\n> this.path()\\n")
        ## note the braces around 'this.path()'
        ## to ensure it is a top level expression
        print({ this.path::this.path(verbose = TRUE) })
        cat("\\n> sys.path()\\n")
        print({ this.path::sys.path(verbose = TRUE) })
    }
    ## 'this.path()' and 'sys.path()' should be identical because the
    ## executing script is the same as the script of the source file
    fun()
}, FILE1.R)
source(FILE1.R, verbose = FALSE, keep.source = TRUE)


FILE2.R <- tempfile("FILE2-", fileext = ".R")
this.path:::.write.code({
    ## 'this.path()' and 'sys.path()' should no longer be identical
    ## since FILE2.R is now the executing script, and FILE1.R is not
    fun()
}, FILE2.R)
source(FILE2.R, verbose = FALSE)


unlink(c(FILE1.R, FILE2.R))
}
