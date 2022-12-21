# eval.with.message <- function (sym, val)
# {
#     oindent <- indent
#     on.exit(indent <<- oindent)
#     writeLines(sprintf("%sevaluating '%s'", strrep(" ", indent), sym))
#     indent <<- indent + if (indent < 16L) 4L else 2L
#     val
# }
# evalq(envir = environment(eval.with.message) <- new.env(), {
#     indent <- 0L
# })
#
#
# tmp <- function (envir = parent.frame(), bindings)
# {
#     if (!is.environment(envir))
#         envir <- as.environment(envir)
#     lapply(as.character(bindings), function(sym) {
#         expr <- call("substitute", as.symbol(sym))
#         expr <- eval(expr, envir)
#         expr <- call("eval.with.message", sym, expr)
#         expr <- call("delayedAssign", sym, expr)
#         eval(expr, envir)
#     })
#     invisible()
# }
#
#
# tmp(bindings = c(
#     "shINFO",
#     "os.unix", "os.windows",
#     "gui.aqua", "gui.rgui", "gui.tk",
#     "gui.rstudio", "gui.vscode",
#     "maybe.os.unix.in.shell", "maybe.os.windows.in.shell", "maybe.in.shell",
#     "os.unix.in.shell", "os.windows.in.shell", "in.shell",
#     "unrecognized.manner", "initwd", "ucrt", "utf8",
#
#     "has.shFILE",
#     "r.editor", "untitled", "nchar_r.editor", "identical2"
# ))
#
#
# envir <- environment(.shFILE)
# tmp(envir, grep("this.path::", names(envir), value = TRUE, fixed = TRUE, useBytes = TRUE))
# rm(envir)
#
#
# rm(tmp)
