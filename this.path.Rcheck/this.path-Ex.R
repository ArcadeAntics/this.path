pkgname <- "this.path"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "this.path-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('this.path')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("check.path")
### * check.path

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check.path
### Title: Check 'this.path()' is Functioning Correctly
### Aliases: check.path check.dir

### ** Examples

# this.path::check.path("EOAdjusted/code/provrun.R")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check.path", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("here")
### * here

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: here
### Title: Construct Path to File, Beginning with 'this.dir()'
### Aliases: here ici

### ** Examples

this.path:::write.code(file = FILE <- tempfile(), {


    this.path::here()
    this.path::here(.. = 1)
    this.path::here(.. = 2)


    # use 'here' to read input from a file located nearby
    this.path::here(.. = 1, "input", "file1.csv")


    # or maybe to run another script
    this.path::here("script2.R")


})


source(FILE, echo = TRUE, verbose = FALSE)
## Don't show: 
unlink(FILE)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("here", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("this.path")
### * this.path

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: this.path
### Title: Determine Executing Script's Filename
### Aliases: this.path this.dir

### ** Examples
## Don't show: 
.unload <- !isNamespaceLoaded("testthat")
## End(Don't show)
this.path:::write.code(file = FILE <- tempfile(), {

    withAutoprint({


        cat(sQuote(this.path::this.path(verbose = TRUE)), "\n\n")


    }, verbose = FALSE)

})


source(FILE, verbose = FALSE)
sys.source(FILE, envir = environment())
if (.Platform$GUI == "RStudio")
    get("debugSource", "tools:rstudio", inherits = FALSE)(FILE)
if (requireNamespace("testthat"))
    testthat::source_file(FILE, chdir = FALSE, wrap = FALSE)


this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE))


# this.path also works when source-ing a URL
# (included tryCatch in case an internet connection is not available)
tryCatch({
    source("https://raw.githubusercontent.com/ArcadeAntics/this.path/main/tests/this.path_w_URLs.R")
}, condition = base::message)
## Don't show: 
unlink(FILE) ; if (.unload) unloadNamespace("testthat")
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("this.path", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("this.path2")
### * this.path2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: this.path2
### Title: Determine Executing Script's Filename
### Aliases: this.path2 this.dir2 this.dir3

### ** Examples

this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", "-e", "this.path::this.path()" ))
this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", "-e", "this.path::this.path2()"))

this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", "-e", "this.path::this.dir()"  ))
this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", "-e", "this.path::this.dir2()" ))
this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", "-e", "this.path::this.dir3()" ))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("this.path2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
