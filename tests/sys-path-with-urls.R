cat("\nexecuting script's path:\n")
cat(sQuote(this.path::sys.path(verbose = TRUE)), "\n\n", sep = "")


cat("\nexecuting script's directory:\n")
cat(sQuote(this.path::sys.dir(verbose = TRUE)), "\n\n", sep = "")


cat("\ntesting 'sys.here()':\n")
cat('\n> this.path::sys.here("test.R")\n')
print(   this.path::sys.here("test.R"))
cat('\n> this.path::sys.here(.. = 1, "R", "thispath.R")\n')
print(   this.path::sys.here(.. = 1, "R", "thispath.R"))


cat("\n\n\n\n")
