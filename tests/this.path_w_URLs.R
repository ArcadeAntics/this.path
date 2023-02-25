cat("Executing script's path:\n")
cat(sQuote(this.path::this.path(verbose = TRUE)), "\n\n", sep = "")


cat("Executing script's directory:\n")
cat(sQuote(this.path::this.dir(verbose = TRUE)), "\n\n", sep = "")


cat("Testing 'here':\n")
(if (getRversion() < "3.4.0" || as.numeric_version(getNamespaceVersion("this.path")) < "1.2.0.11") this.path:::withAutoprint else withAutoprint)({
    this.path::here("test.R")
    this.path::here(.. = 1, "R", "this.path.R")
}, spaced = TRUE, verbose = FALSE, width.cutoff = 60L)


cat("\n\n\n\n")
