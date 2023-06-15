cat("\nexecuting script's path:\n")
cat(sQuote(this.path::this.path(verbose = TRUE)), "\n\n", sep = "")


cat("\nexecuting script's directory:\n")
cat(sQuote(this.path::this.dir(verbose = TRUE)), "\n\n", sep = "")


cat("\ntesting 'here()':\n")
cat('\n> this.path::here("test.R")\n')
print(   this.path::here("test.R"))
cat('\n> this.path::here(.. = 1, "R", "thispath.R")\n')
print(   this.path::here(.. = 1, "R", "thispath.R"))


cat("\n\n\n\n")
