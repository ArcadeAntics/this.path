# this.path

## Description

`package:this.path` provides a mechanism for an **R** script to
retrieve its own path without the explicit need to write it elsewhere.

## Installation

Install it from CRAN:
```R
install.packages("this.path")
```

Install the development version from GitHub:
```R
install.packages("this.path",
    repos = "https://raw.githubusercontent.com/ArcadeAntics/PACKAGES")
```

## Details

The most important functions from `package:this.path` are
`this.path()`, `this.dir()`, `here()`, and `this.proj()`:

*   `this.path()` returns the normalized path of the script in which it
    is written.

*   `this.dir()` returns the directory of `this.path()`.

*   `here()` constructs file paths against `this.dir()`.

*   `this.proj()` constructs file paths against the project root of
    `this.dir()`.

New additions include:

*   `LINENO()` returns the line number of the executing expression.

*   `shFILE()` looks through the command line arguments, extracting
    *FILE* from either of the following: `-f FILE` or `--file=FILE`

*   `set.sys.path()` implements `this.path()` for any `source()`-like
    functions outside of the builtins.

*   `with_site.file()` and `with_init.file()` allow `this.path()` to be
    used in the site-wide startup profile file or a user profile.

`package:this.path` also provides functions for constructing and
manipulating file paths:

*   `path.join()`, `basename2()`, and `dirname2()` are drop in
    replacements for `file.path()`, `basename()`, and `dirname()` which
    better handle drives and network shares.

*   `splitext()`, `removeext()`, `ext()`, and `ext<-()` split a path
    into root and extension, remove a file extension, get an extension,
    or set an extension for a file path.

*   `path.split()`, `path.split.1()`, and `path.unsplit()` split the
    path to a file into components.

*   `relpath()`, `rel2here()`, and `rel2proj()` turn absolute paths
    into relative paths.

## Note

This package started from a stack overflow posting:

[https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649#64129649](https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/64129649#64129649)

If you like this package, please consider upvoting my answer so that
more people will see it! If you have an issue with this package, please
use `bug.report(package = "this.path")` to report your issue.

## Alternatives

If you are unhappy with the performance of `package:this.path`, or
would otherwise like to know some other solutions, here are 4
alternatives:

### Alternative 1: Other Packages That Determine Current **R** Script

There are a few other packages and functions that provide the ability
to retrieve the path of the current **R** script:

*   [`package:envDocument`](https://CRAN.R-project.org/package=envDocument)
    specifically `envDocument::getScriptPath()`

*   [`package:funr`](https://CRAN.R-project.org/package=funr)
    specifically `funr::get_script_path()` and `funr::sys.script()`

*   [`package:scriptName`](https://CRAN.R-project.org/package=scriptName)
    specifically `scriptName::current_filename()`

*   [`package:whereami`](https://CRAN.R-project.org/package=whereami)
    specifically `whereami::thisfile()`

These are lacking in functionality compared to `package:this.path`:

*   `this.path()` is compatible with GUIs 'Rgui',
    '[RStudio](https://posit.co/download/rstudio-desktop/)',
    '[VSCode](https://code.visualstudio.com/)',
    '[Jupyter](https://jupyter.org/)', and
    '[Emacs](https://www.gnu.org/software/emacs/)' +
    '[ESS](https://ess.r-project.org/)', as well as functions and
    packages `sys.source()`, `debugSource()` in 'RStudio',
    `compiler::loadcmp()`, `utils::Sweave()`,
    [`box::use()`](https://CRAN.R-project.org/package=box),
    [`knitr::knit()`](https://CRAN.R-project.org/package=knitr),
    [`plumber::plumb()`](https://CRAN.R-project.org/package=plumber),
    [`shiny::runApp()`](https://CRAN.R-project.org/package=shiny),
    [`package:targets`](https://CRAN.R-project.org/package=targets),
    and
    [`testthat::source_file()`](https://CRAN.R-project.org/package=testthat).

*   Related to `source()`, `this.path()`:

    *   takes argument `chdir` into account.

    *   recognizes that `file = ""`, `file = "clipboard"`, and
        `file = "stdin"` are not referring to files and skips them.

    *   accounts for `file` being a URL pathname.

    *   accounts for `file` being a connection instead of a character
        string.

    *   skips calls in which `file` is missing and `exprs` is used
        instead.

*   `this.path()` does a better job of extracting *FILE* from the
    command line arguments `-f FILE` and `--file=FILE`.

*   `this.path()` saves all normalized paths within their appropriate
    environments, making it much faster subsequent times within the
    same script and independent of working directory.

*   If `this.path()` does not find an executing script, it throws an
    error. This is better than returning `NULL`; if the executing
    script cannot be found, obviously there is a problem that needs to
    be addressed, so the script should not continue.

These packages also have some objectively incorrect coding issues such
as:

*   searching only the first call on the stack

*   failing to check the command line arguments

*   imposing unnecessary restrictions on pathnames

*   searching the call stack in the wrong order

*   returning the directory instead of the path of the executing script

*   searching for a path in the wrong order

*   searching for a source call by name instead of by value

*   returning a non-normalzied path

*   changing global options without explicit user permission

*   loading unnecessary additional packages without explicit user
    permission

### Alternative 2: Packages That Determine Project Root

[`package:here`](https://CRAN.R-project.org/package=here) provides
function `here::here()` with the ability to retrieve the project root,
but it lacks functionality:

*   `here::here()` returns the initial working directory when it cannot
    find the project's directory. If the project's directory cannot be
    found, there is something wrong and an error should be thrown, but
    it doesn't, and this leads to incorrect / / unexpected behaviour.

*   `here::here()` does not work when the initial working directory is
    set outside the context of the project. Occasionally, I would set
    the working directory to a temporary directory where my **R**
    script would create a log file of the details of the script. This
    leads to `package:here` not being able to find the project root and
    incorrectly returning the initial working directory.

*   `here::here()` does not work when multiple projects are in use nor
    for projects containing sub-projects. In my scenario, I had a
    project "A" and a project "B". I would run a script in "B" which
    runs a script in "A", but the project root is already set to the
    root of "B", so the script in "A" fails.

[`package:rprojroot`](https://CRAN.R-project.org/package=rprojroot) is
the **R** package upon which `package:here` is built. It provides a
list of project root criteria `rprojroot::criteria`. It is not useful
on its own since, like `here::here()`, it does not work when the
working directory is set outside the context of the project. However,
you can combine it with `package:this.path` to get the best results:

```R
## substitute 'rprojroot::criteria$is_r_package' with your criterion

fix_file <- this.path::path.functions(
    rprojroot::criteria$is_r_package$find_file(
        path = this.path::here()
    )
)$here

## or

fix_file <- this.path::path.functions(
    rprojroot::find_root(
        rprojroot::criteria$is_r_package,
        this.path::here()
    )
)$here
```

If the default criteria are not sufficient for your needs, you can make
your own using:

*   `rprojroot::has_basename()`

*   `rprojroot::has_dir()`

*   `rprojroot::has_file()`

*   `rprojroot::has_file_pattern()`

*   `rprojroot::root_criterion()`

`package:rprojroot` recommends using
`<criterion>$find_file(path = whereami::thisfile())` for this purpose,
but as mentioned in section **Alternative 1**, `whereami::thisfile()`
is seriously lacking compared to `this.path::this.path()`.

### Alternative 3: Always Change Working Directory

The working directory could always be changed to the directory of the
executing script before running it. This would be:

```bash
cd /path/to
Rscript ./file.R
```

or:

```R
source("/path/to/file.R", chdir = TRUE)
```

This fails when moving amongst files in different directories.
Additionally, it is sometimes convenient to have the working directory
set elsewhere. This means that **R** scripts cannot be made to act like
executables. If a script needs to call other scripts in the same
directory, it could not do so without the its own path.

### Alternative 4: Source References

`utils::getSrcFilename()` provides the ability to retrieve the filename
of a source reference. Everywhere `this.path()` would be used, write 
`utils::getSrcFilename(function() NULL, full.names = TRUE)` instead
(yes, it is quite lengthy), and everywhere `this.dir()` would be used,
write `utils::getSrcDirectory(function() NULL)` instead (again, quite
lengthy).

This fails in interactive use since scripts must be run with
`source()`. Also, it means option `keep.source` must be set to `TRUE`;
this may not be a big deal, but something to be aware of. This means
**R** scripts could not be run from a shell ever again, making it an
incredibly inconvenient substitute.

## Closing

If you think I have overlooked some of the alternatives, or think there
are any improvements I could make to `package:this.path`, please let me
know, I am open to suggestions! I hope this package serves you well!
