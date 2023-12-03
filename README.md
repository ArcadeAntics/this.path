# this.path

## Description

`package:this.path` provides a mechanism for retrieving the path of an
R script within itself without the explicit need to write its path
elsewhere. Additionally, it provides a mechanism for a script to refer
to files relative to its directory, independent of working directory.

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

  * `this.path()` returns the normalized path of the script in which it
    is written.

  * `this.dir()` returns the directory of `this.path()`.

  * `here()` constructs file paths against `this.dir()`.

  * `this.proj()` constructs file paths against the project root of
    `this.dir()`.

`package:this.path` also provides functions for constructing and
manipulating file paths:

  * `path.join()`, `basename2()`, and `dirname2()` are drop in
    replacements for `file.path()`, `basename()`, and `dirname()` which
    better handle drives and network shares.

  * `splitext()`, `removeext()`, `ext()`, and `ext<-()` split a path
    into root and extension, remove a file extension, get an extension,
    or set an extension for a file path.

  * `path.split()`, `path.split.1()`, and `path.unsplit()` split the
    path to a file into components.

  * `relpath()`, `rel2here()`, and `rel2proj()` turn absolute paths
    into relative paths.

New additions to `package:this.path` include:

  * `LINENO()` returns the line number of the executing expression.

  * `wrap.source()`, `set.sys.path()`, and `unset.sys.path()` implement
    `this.path()` for any `source()`-like functions outside of the
    builtins.

  * `shFILE()` looks through the command line arguments, extracting
    *FILE* from either of the following: `-f FILE` or `--file=FILE`

## Alternatives

  * There are a few other packages that provide the ability to retrieve
    the path of the current R script:
    [`package:envDocument`](https://CRAN.R-project.org/package=envDocument),
    [`package:funr`](https://CRAN.R-project.org/package=funr),
    [`package:scriptName`](https://CRAN.R-project.org/package=scriptName),
    and
    [`package:whereami`](https://CRAN.R-project.org/package=whereami).
    Specifically, the functions are `envDocument::getScriptPath()`,
    `funr::get_script_path()`, `funr::sys.script()`,
    `scriptName::current_filename()`, and `whereami::thisfile()`. These
    are lacking in functionality compared to `package:this.path`:

    1.  `this.path()` is compatible with GUIs 'Rgui',
        '[RStudio](https://posit.co/download/rstudio-desktop/)',
        '[VSCode](https://code.visualstudio.com/)',
        '[Jupyter](https://jupyter.org/)', and
        '[Emacs](https://www.gnu.org/software/emacs/)' +
        '[ESS](https://ess.r-project.org/)', as well as functions and
        packages `sys.source()`, `debugSource()` in 'RStudio',
        `compiler::loadcmp()`,
        [`box::use()`](https://CRAN.R-project.org/package=box),
        [`knitr::knit()`](https://CRAN.R-project.org/package=knitr),
        [`plumber::plumb()`](https://CRAN.R-project.org/package=plumber),
        [`shiny::runApp()`](https://CRAN.R-project.org/package=shiny),
        [`package:targets`](https://CRAN.R-project.org/package=targets),
        and
        [`testthat::source_file()`](https://CRAN.R-project.org/package=testthat).

    2.  Related to `source()`, `this.path()`:

        2.1. takes argument `chdir` into account.

        2.2. recognizes that `file = ""`, `file = "clipboard"`, and
             `file = "stdin"` are not referring to files and skips them.

        2.3. accounts for `file` being a URL pathname.

        2.4. accounts for `file` being a connection instead of a character
             string.

        2.5. skips calls in which `file` is missing and `exprs` is used
             instead.

    3.  `this.path()` does a better job of extracting *FILE* from the
        command line arguments `-f FILE` and `--file=FILE`.

    4.  `this.path()` saves all normalized paths within their appropriate
        environments, making it much faster subsequent times within the
        same script and independent of working directory.

    5.  If `this.path()` does not find an executing script, it throws an
        error. This is better than returning `NULL`; if the executing
        script cannot be found, obviously there is a problem that needs to
        be addressed, so the script should not continue.

    These packages also have some objectively incorrect coding issues such
    as:

      * searching only the first call on the stack

      * failing to check the command line arguments

      * imposing unnecessary restrictions on pathnames

      * searching the call stack in the wrong order

      * returning the directory instead of the path of the executing script

      * searching for a path in the wrong order

      * searching for a source call by name instead of by value

      * changing global options without explicit user permission

      * loading unnecessary additional packages without explicit user
        permission


  * [`package:rprojroot`](https://CRAN.R-project.org/package=rprojroot)
    and
    [`package:here`](https://CRAN.R-project.org/package=here) provide
    the ability to retrieve the project root. Specifically, the
    function most commonly used is `here::here()`, but it lacks
    functionality when compared to `this.path::this.proj()`:

    1.  `here::here()` returns the initial working directory when it
        cannot find the project's directory. If the project's directory
        cannot be found, there is something wrong and an error should
        be thrown, but it doesn't, and this leads to incorrect / /
        unexpected behaviour.

    2.  `package:here` does not work when the initial working directory
        is set outside the context of the project. Occasionally, I
        would set the working directory to a temporary directory where
        my R script would create a log file of the details of the
        script. This leads to `package:here` not being able to find the
        project root and incorrectly returning the initial working
        directory.

    3.  `package:here` does not work when multiple projects are in use
        nor for projects containing sub-projects. In my scenario, I had
        a project "A" and a project "B". I would run a script in "B"
        which runs a script in "A", but the project root is already set
        to the root of "B", so the script in "A" fails.

  * The working directory could always be changed to the directory of
    the executing script before running it. This would be:

    ```bash
    cd /path/to
    Rscript ./file.R
    ```

    or:

    ```R
    source("/path/to/file.R", chdir = TRUE)
    ```

    This fails when moving between files in different directories.
    Additionally, sometimes it is convenient to have the working
    directory set elsewhere. This means that R scripts cannot be made
    to act like executables. If a script needs to call other scripts in
    the same directory, it could not do so without the its own path.

  * `utils::getSrcFilename()` provides the ability to retrieve the
    filename of a source reference. Everywhere `this.path()` would be
    used, write
    `utils::getSrcFilename(function() NULL, full.names = TRUE)` instead
    (yes, it is quite lengthy), and everywhere `this.dir()` would be
    used, write `utils::getSrcDirectory(function() NULL)` instead
    (again, quite lengthy).

    This fails in interactive use since scripts must be run with
    `source()`. Also, it means option `keep.source` must be set to
    `TRUE`; this may not be a big deal, but something to be aware of.
    This means R scripts could not be run from a shell ever again,
    making it an incredibly inconvenient substitute.

## Closing

If you think I have overlooked some of the alternatives, or think there
are any improvements I could make to `package:this.path`, please let me
know, I am open to all suggestions! I hope this package serves you well!
