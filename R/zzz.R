# if (!identical(environment(), globalenv())) {
#     envir <- environment()
#     promise.names <- c(
#         "os.unix", "os.windows",
#         "gui.aqua", "gui.rgui", "gui.rstudio", "gui.tk", "gui.vscode",
#         "tools.rstudio", ".rs.api.getActiveDocumentContext", ".rs.api.getSourceEditorContext", "debugSource",
#         "os.unix.in.shell", "os.windows.in.shell", "in.shell",
#         "unrecognized.manner", "initwd", "ucrt", "utf8",
#
#         "has.shFILE",
#         "r.editor", "untitled", "nchar_r.editor", "identical2"
#     )
#     exprs <- lapply(promise.names, function(x) {
#         expr <- call("substitute", as.symbol(x))
#         expr <- eval(expr, envir)
#         expr <- call("{", call("writeLines", sprintf("evaluating '%s'", x)), expr)
#         expr <- call("delayedAssign", x, expr)
#         expr
#     })
#     lapply(exprs, eval, envir = envir)
#     rm(envir, promise.names, exprs)
# }
#
#
# envir <- environment(.shFILE)
# promise.names <- grep("this.path::", names(envir), value = TRUE, fixed = TRUE, useBytes = TRUE)
# exprs <- lapply(promise.names, function(x) {
#     expr <- call("substitute", as.symbol(x))
#     expr <- eval(expr, envir)
#     expr <- call("{", call("writeLines", sprintf("evaluating '%s'", x)), expr)
#     expr <- call("delayedAssign", x, expr)
#     expr
# })
# lapply(exprs, eval, envir = envir)
# rm(envir, promise.names, exprs)
