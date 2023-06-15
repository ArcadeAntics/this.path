fun <- function ()
{
    cat("\n> env.path()\n");                                 print(env.path())
    cat("\n> env.dir()\n");                                  print(env.dir())
    cat('\n> env.here("script2.R")\n');                      print(env.here("script2.R"))
    cat('\n> env.here(.. = 1, "input", "file1.csv")\n');     print(env.here(.. = 1, "input", "file1.csv"))
    cat("\n> env.proj()\n");                             try(print(env.proj()))
    cat("\n> env.LINENO()\n");                               print(env.LINENO())
    cat("\n> sys.path()\n");                             try(print(sys.path()))
    cat("\n> this.path()\n");                                print(this.path())
    cat("\n> this.dir()\n");                                 print(this.dir())
    cat('\n> here("script2.R")\n');                          print(here("script2.R"))
    cat('\n> here(.. = 1, "input", "file1.csv")\n');         print(here(.. = 1, "input", "file1.csv"))
    cat("\n> this.proj()\n");                            try(print(this.proj()))
    cat("\n> LINENO()\n");                                   print(LINENO())
}


fun()
