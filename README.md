# this.path

## Description

**this.path** aims to provide a mechanism for retrieving the path of an
R script within itself without needing to explicitly write its path
elsewhere. Additionally, it provides a mechanism for specifying file
paths against the executing script's directory (without changing the
working directory).

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

The most important functions from **this.path** are `this.path()`,
`this.dir()`, `here()`, and `this.proj()`:

*   `this.path()` returns the normalized path of the executing script.

*   `this.dir()` return the normalized path of the directory in which
    the executing script is located.

*   `here()` constructs file paths against the executing script's
    directory.

*   `this.proj()` constructs file paths against the executing project's
    directory instead of the executing script's directory.

**this.path** also provides functions for constructing and manipulating
file paths:

*  `path.join()`, `basename2()`, and `dirname2()` are drop in
    replacements for `file.path()`, `basename()`, and `dirname()` which
    better handle drives and network shares.

*   `splitext()`, `removeext()`, `ext()`, and `ext<-()` split a path
    into root and extension, remove a file extension, get an extension,
    or set an extension for a file path.

*   `path.split()`, `path.split.1()`, and `path.unsplit()` split the
    path to a file into components.

*   `relpath()` and `rel2here()` turn absolute paths into relative
    paths.

New additions to **this.path** include:

*   `LINENO()` returns the line number of the executing expression in
    the executing script.

*   `wrap.source()`, `inside.source()`, `set.this.path()`, and
    `unset.this.path()` implement `this.path()` for any `source()`-like
    functions outside of the builtins.

*   `shFILE()` looks through the command line arguments, extracting
    *FILE* from either of the following: `-f FILE` or `--file=FILE`

## **this.path** vs **whereami**

The only equivalent to `this.path()` (that I have seen) is
`whereami::thisfile()`. R package **whereami** has many issues that
**this.path** resolves:

1.  `this.path()` works with `sys.source()`, `debugSource()` in
    'RStudio', `testthat::source_file()`, `knitr::knit()`, and
    `box::use()`. It also works in interactive mode inside 'Rgui',
    'RStudio', 'VSCode', and 'Jupyter'.

2.  Related to `source()`, `this.path()`:

    2.1. takes argument `chdir` into account.

    2.2. recognizes that `file = ""`, `file = "clipboard"`, and
         `file = "stdin"` are not referring files and skips them.

    2.3. accounts for `file` being a URL pathname.

    2.4. accounts for `file` being a connection instead of a character
         string.

    2.5. skips calls in which `file` is missing and `exprs` is used
         instead.

3.  if an R script is being run from a shell, `this.path()` does a
    better job of extracting the command line argument `-f FILE` or
    `--file=FILE`.

4.  `this.path()` saves all normalized paths within their appropriate
    environments, making it much faster subsequent times within the
    same script, and independent of working directory.

5.  if `this.path()` does not find an executing script, it throws an
    error. This is better than `whereami::thisfile()` which returns
    `NULL` when it cannot find the executing script. If the executing
    script cannot be found, obviously there is a problem that needs to
    be addressed, so the script should not continue.

**whereami** also has some objectively incorrect coding issues:

1.  It treats R and Rscript as two separate applications to look for
    command line arguments. At least since R 2.5.0 (>= 15 years ago),
    Rscript directly calls R, so there should not be separate cases.
    Additionally, it does **NOT** take into account the differences
    between the command line applications on Windows and Unix-alikes.

2.  It checks for uses of `knitr::knit()` after checking the entire
    call stack for `source()` and after checking the command line
    arguments. This is incorrect, it should be checking for
    `knitr::knit()` at the same time as it checks for `source()`.

3.  When **whereami** is loaded or attached, it changes option
    `keep.source` to `TRUE`. A package should **never** be changing
    global options without asking / / informing the user beforehand.
    This may not harm your code, but it is still bad practice that
    should be avoided, especially for a CRAN package.

4.  When you call `whereami::thisfile()`, it forces **knitr** to be
    loaded as well. Again, it is changing your environment without
    permission. It should use something like
    `isNamespaceLoaded("knitr")` instead of `requireNamespace("knitr")`
    because, contrary to what the package claims, **whereami** enhances
    **knitr** rather than requires it.

Code aside, **whereami** suggests that an R script needing to know its
own path should only be done if absolutely necessary, and that it
should be set outside the context of the R script if possible. I find
this vague and unconvincing. Other scripting languages have methods of 
requesting a script's path without issue, so R should too. We should
not be scaring programmers into thinking that this is rule-breaking or
bad practice.

## **this.path** vs **here**

If you are not using **this.path** for the function `this.path()`, you
are probably using it for `this.path::here()`. The only equivalent
(that I know of) is R package **here** with its function
`here::here()`. **this.path** provides a mechanism for specifying a
path relative to the executing script's directory, while here provides
a mechanism for specifying a path relative to the project's directory.

As a long time **here** user, I appreciate all the functionality and
convenience this package offered, but it has some faults that I could
not ignore (which **this.path** fixes). I do not dislike **here**, it
just no longer fits my use-cases.

1.  `here::here()` returns the initial working directory when it cannot
    find the project's directory. If the project's directory cannot be
    found, there is something wrong and an error should be raised, but
    it doesn't, and this leads to incorrect / / unexpected behaviour.

2.  **here** does not work when the initial working directory is set
    outside the context of the project. Occasionally, I would set the
    working directory to a temporary directory where my R script would
    create a log file of the details of the script. This leads to
    **here** not being able to find the project's directory and
    incorrectly returning the initial working directory.

3.  **here** does not work for projects containing sub-projects. In my 
    scenario, I had a project "A", and later I had a project "B" of
    which "A" is a sub-project. I would run a script in "B" which runs
    another script in "A", but the project root is already set to the
    root of "B", so the script in "A" fails.

4.  **here** does not work when a project is stored on the internet.
    When I say on the internet, I am not referring to a project stored
    on a network share, that works correctly. I am talking about
    projects uploaded to a website. The work I do requires that all
    source code and input be publicly available through our website,
    and that the code should run as expected (slow as it may be, and
    only code that exclusively reads files and / / or produces
    graphics). Since the project root of **here** cannot be a URL
    pathname, I cannot use it.

It should be noted that if you prefer specifying files relative to the
project's directory instead of the executing script's directory, you
could use `this.path::this.proj()` instead. It behaves very similarly
to `here::here()`, but can handle multiple projects in use at once.

## Other methods

There are a few alternatives to `this.path()`, though they are very
limiting.

### Change working directory

You could always change the working directory to the directory of the
executing script before running it. This would be

```{bash}
cd /path/to
Rscript ./file.R
```

or

```{r}
source("/path/to/file.R", chdir = TRUE)
```

This works for a lot of use-cases, except for interactive use where you
probably are not changing the working directory as you move amongst
files, and it will not if your R script is stored on a website.
Sometimes it is convenient to have the working directory set elsewhere.
This also means that you cannot make R scripts that act like
executables. If you need to call other scripts in the same directory,
you could not call those other scripts without the original script's
path.

### `utils::getSrcFilename()`

You could use `utils::getSrcFilename()`. Everywhere you would use
`this.path()`, use
`utils::getSrcFilename(function() NULL, full.names = TRUE)` instead
(yes, it is quite lengthy), and everywhere you would use `this.dir()`,
use `utils::getSrcDirectory(function() NULL)` (again, quite lengthy).

While this will work for R scripts uploaded to a website, this will not
work in interactive use since you have to `source()` your scripts.
Also, it means you must set option `keep.source` to `TRUE`; once again
this may not be a big deal, but something that may cause issues. This
means you could not run your R scripts from a shell ever again, making
it an incredibly inconvenient substitute.

## Closing

If you think I have overlooked something in **whereami** or **here**,
or think there are any improvements I could make to **this.path**,
please let me know, I am open to all suggestions! And I hope this
package serves you well!
