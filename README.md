# {this.path}

## Installation

* From CRAN: `install.packages("this.path")`
* From GitHub: `install.packages("this.path", repos = "https://raw.githubusercontent.com/ArcadeAntics/PACKAGES")`

This is the README file for R package {this.path}. The goal of
{this.path} is to provide a mechanism for retrieving the absolute path
of an R script from within said R script, and without needing to
explicitly write down the path within said R script or anywhere outside
the context of the R script. Additionally, it provides a mechanism for
specifying absolute file paths relative to the script's directory
(without changing the working directory).

This is something that Python has natively. When a Python script is
run, it makes a variable `__file__` that is the absolute path of the
script in which it was created. This package seeks to provide the same.

`this.path()` will return the absolute path of the R script/Rmd file in
which it is written. this.dir() will return the absolute path of the
directory of the R script/Rmd file in which it is written. here() is a
method of specifying file paths in a similar manner to file.path(),
prepended by the directory of the executing R script.

This package also provides functions for constructing and manipulating
file paths. `path.join()`, `basename2()`, and `dirname2()` are drop in
replacements for `file.path()`, `basename()`, and `dirname()` which better
handle drives and network shares.

`splitext()`, `removeext()`, `ext()`, and
`ext<-()` split a path into root and extension, remove a file extension,
get an extension, or set an extension for a file path.

## {this.path} vs {whereami}

The only equivalent to {this.path} (that I've seen) is
`whereami::thisfile()`. R package {whereami} has many issues that
{this.path} resolves:

  1. `this.path()` works with `sys.source()`, `debugSource()` in RStudio, and
     `testthat::source_file()`. It also works in interactive mode inside
     RStudio, VSCode, and Rgui for Windows. 'whereami' does not.

  2. Related to `source()`, `this.path()`:

     2.1. takes argument `chdir` into account

     2.2. recognizes that `file = ""`, `file = "clipboard"`, and
          `file = "stdin"` are not opening files and skips them

     2.3. accounts for `file` being a URL path

     2.4. accounts for `file` being a connection instead of a character
          string

     2.5. skips calls in which `file` is missing and `exprs` is used
          instead

     `whereami` does not take any of this into consideration.

  3. if an R script is being run from a shell, `this.path()` does a much
     better job of extracting the command line argument `-f FILE` or
     `--file=FILE`

  4. `this.path()` saves all normalized paths within their appropriate
     environments, making it much faster subsequent times within the
     same script, and independent of working directory.

  5. if `this.path()` does not find an executing script, it throws an
     error. This is better than `whereami` which returns `NULL` when it
     cannot find the executing script. If the executing script cannot
     be found, obviously there is a problem that needs to be addressed,
     so the script should not continue.

Ignoring the direct comparison of {this.path} and {whereami},
{whereami} has some objectively incorrect coding issues and poor
recommendations for how you should structure your projects.

  1. It treats R and Rscript as two separate applications to look for
     command line arguments. At least since R 2.5.0 (15 years ago),
     Rscript directly calls R, so there should not be separate cases.
     Additionally, it does NOT take into account the differences
     between the command line applications on Windows and on Unix.

  2. It checks for uses of `knitr::knit()` after checking the entire call
     stack for `source()` and checking the command line arguments. This
     is 100% incorrect, it should be checking for `knitr::knit()` at the
     same time as it checks for `source()`.

  3. When {whereami} is loaded or attached, it changes option
     `keep.source` to `TRUE`. A package should __never__ be changing global
     options, especially not without informing the user. While that might not be
     a bad thing for you (it may be completely irrelevant whether
     `keep.source` is `FALSE` or `TRUE`), it is still bad practice that
     should be avoided, especially for being a CRAN package.

  4. When you call `whereami::thisfile()`, it forces {knitr} to
     be loaded as well. Again, this is such a problem with this
     package, it changes your environment without asking you. It should
     use something like `isNamespaceLoaded("knitr")` instead of
     `requireNamespace("knitr")` because, contrary to what the package
     claims, {whereami} should enhance rather than require {knitr}.

  5. Code aside, {whereami} suggests that an R script needing to know
     its own path should only be done if absolutely necessary, and that
     it should be set outside the context of the R script if possible.
     This is a giant load of shit. No elaboration is offered, they
     simply assert that an R script knowing its own path is bad.
     Windows batch scripts, bash scripts, C scripts, and Python scripts
     all have methods of requesting their own path without issue, so R
     should too. We shouldn't be scaring programmers into thinking that
     this is rule-breaking or bad practice.

## {this.path} vs {here}

If you're not using {this.path} for the function `this.path()`, you're
probably using it for `this.path::here()`. The only equivalent (that I
know of) is R package {here} with its function `here::here()`.
{this.path} provides a mechanism for specifying a path relative to the
executing scripts directory, while {here} provides a mechanism for
specifying a path relative to the project's directory.

As a long time {here} user, I appreciate all the functionality and
convenience this package offered, but it has some faults that I could
not ignore (which {this.path} fixes). It's not that I dislike {here},
but that I could no longer use it because of my use-case.

  1. {here} does not work when two projects rely on each other. In my
     scenario, I had project A and B, both of which rarely relied on
     the other (so I did not want to combine them into one project),
     but had once instance in which they ran a script from the other
     project. However, the project root was already set to the root of
     project A, so when I tried to run the script in project B from a
     script in project A, the project root did not update and the
     script in project B would fail.

  2. {here} does not work when a project is stored on the internet.
     When I say on the internet, I am not referring to a project stored
     on a network share, that works correctly. I'm talking about
     projects uploaded to a website. The work I do requires that all
     source code and input be publicly available through our website,
     and that the code should run as expected (slow as it may be, and
     only code that exclusively reads files and / or produces
     graphics). Since the project root of {here} cannot be a URL, I
     can't use it.

It should be noted that if you prefer specifying files relative to the
projects' directory instead of the executing scripts' directory, you
could use `this.path::this.proj()` instead. It behaves very similarly to
`here::here()`, but can handle multiple projects in use at once, and can
run projects stored on the internet.

## Other methods

There are a few alternatives to `this.path()`, though they are very
limiting.

### Change working directory

You could always change the working directory to the directory of
the executing R script before running it. This would be

```{bash}
cd /path/to
Rscript file
```

or

```{r}
source("/path/to/file", chdir = TRUE)
```

This works for just about every use-case, except for interactive
use where you probably aren't changing the working directory as
you move from file to file, and it won't if your R script is
stored on a website. This also means that you cannot make R
scripts that act like executables.

For example, the Rscript executable needs to know its own path so
that it can invoke the R executable. It would be bad practice for
Rscript to change the working directory before invoking R, the
working directory is __your__ directory, not Rscript's directory.
Similarly, if you made an R script that acts like an executable,
calling other scripts in the same directory, you could not call
those other scripts without the original script knowing its path.

### `utils::getSrcFilename()`

You could use `utils::getSrcFilename()`. Everywhere you would use
`this.path()`, use `utils::getSrcFilename(function() NULL, full.names = TRUE)`
instead (yes it is quite lengthy), and everywhere you would use
`this.dir()`, use `utils::getSrcDirectory(function() NULL)`
(again, quite lengthy).

While this will work for R scripts uploaded to a website, this
will not work in interactive use since you have to source your
scripts. Also, it means you must have option 'keep.source' set to
TRUE, once again probably not a big deal, but something that may
cause issues. This means you couldn't run your R scripts from a
shell ever again, making it an incredibly inconvenient substitute.

## Closing

If you think I've overlooked something in {whereami} or {here}, or think there
are any improvements I could make to {this.path}, please let me know, I'm open
to all suggestions! And I hope this package serves you well!
