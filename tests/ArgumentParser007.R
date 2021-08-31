# x <- this.path::ArgumentParser()
# `x group1` <- x$add.argument.group("stdin", mutually.exclusive = TRUE, type = "none")
# `x group1`$add.argument("-f", "--file", metavariable = "file")
# `x group1`$add.argument("-e", metavariable = "expression")
# `x group2` <- x$add.argument.group("suppress installation",
#     description = this.path::dedent("
#         suppress installation of the specified part of the
#         package for testing or other special purposes"),
#     wrap = FALSE, type = "together")
# `x group2`$add.argument("--no-R"   , action = "store_false", destination = "R"   )
# `x group2`$add.argument("--no-libs", action = "store_false", destination = "libs")
# `x group2`$add.argument("--no-data", action = "store_false", destination = "data")
# `x group2`$add.argument("--no-help", action = "store_false", destination = "help")
# `x group2`$add.argument("--no-demo", action = "store_false", destination = "demo")
# `x group2`$add.argument("--no-exec", action = "store_false", destination = "exec")
# `x group2`$add.argument("--no-inst", action = "store_false", destination = "inst")
#
# `x group3` <- x$add.argument.group("for Unix")
# `x group3`$add.argument("--configure-args", metavariable = "ARGS")
# `x group3`$add.argument("--configure-vars", metavariable = "VARS")
#
# `x group3 group1` <- `x group3`$add.argument.group("for macOS")
# `x group3 group1`$add.argument("--dsym", help = "(macOS only) generate dSYM directory")
#
# # x$print.help()
# .self <- x
# group <- .self
#
#
# ID <- function(x) {
#     if (inherits(x, "ArgumentParser"))
#         integer()
#     else if (inherits(x, "ArgumentGroup"))
#         c(x$parent.IDs, x$ID)
#     else x$group
# }
# ID2 <- function(x) paste(ID(x), collapse = "/")
# grouptypes <- function(x) {
#     value <- vapply(x$argument.groups, "[[", "type", FUN.VALUE = "")
#     names(value) <- vapply(x$argument.groups, "ID2", FUN.VALUE = "")
#     value
# }
# getGroup <- function(x, ID) {
#     ids <- vapply(x$argument.groups, "[[", "ID", FUN.VALUE = 0L)
#     x$argument.groups[[match(ID, ids, 0L)]]
# }
# organize1 <- function(args, group, indent = 0) {
#     types <- grouptypes(group)
#     value <- list()
#     gid <- ID(group)
#     gid2 <- ID2(group)
#     n <- length(gid)
#     while (length(args)) {
#         arg <- args[[1L]]
#         args <- args[-1L]
#         id <- ID(arg)
#         print(ID2(arg))
#         print(ID2(group))
#
#
#         # if the ID of the argument and the ID of the group are the same
#         if (length(id) == length(gid)) {
#             if (is.null(arg$help))
#                 next
#             if (!is.na(arg$name)) {
#                 value <- c(value, list(list(
#                     short.tag = "",
#                     long.tag  = arg$name,
#                     help      = arg$help,
#                     wrap      = arg$wrap.help)))
#             }
#             else if (arg$action %in% c("store_const", "store_true", "store_false", "count", "help", "version")) {
#                 value <- c(value, list(list(
#                     short.tag = if (!is.na(arg$short.flag)) sprintf("-%s" , arg$short.flag) else "",
#                     long.tag  = if (!is.na(arg$long.flag )) sprintf("--%s", arg$long.flag ) else "",
#                     help      = arg$help,
#                     wrap      = arg$wrap.help)))
#             }
#             else {
#                 value <- c(value, list(list(
#                     short.tag  = if (!is.na(arg$short.flag)) sprintf("-%s"    , arg$short.flag)                   else "",
#                     short.tag2 = if (!is.na(arg$short.flag)) sprintf("-%s %s" , arg$short.flag, arg$metavariable) else "",
#                     long.tag   = if (!is.na(arg$long.flag )) sprintf("--%s=%s", arg$long.flag , arg$metavariable) else "",
#                     long.tag2  = if (!is.na(arg$long.flag )) sprintf("--%s %s", arg$long.flag , arg$metavariable) else "",
#                     help       = arg$help,
#                     wrap       = arg$wrap.help)))
#             }
#         }
#
#
#         # if the argument belongs to a sub-group of type '
#         else if (TRUE) {
#             args2 <- c(list(arg), args)
#             print(args2)
#             print(lengths(lapply(args2, "ID")) == n + 1)
#         }
#         cat("\n")
#     }
#     value
# }
#
#
# # figure out the primary group of each argument
# arg.groups <- vapply(.self$formal.command.args$value, function(x) x$group[1L], FUN.VALUE = 0L)
#
# # get the ID of each argument group
# ids <- vapply(.self$argument.groups, "[[", "ID", FUN.VALUE = 0L)
#
# # get the IDs of argument groups that are printed separately
# sep.ids <- ids[grouptypes(.self) == "separate"]
#
# # get the index of the arguments that belong to each separate argument group
# k <- lapply(sep.ids, function(x) which(x == arg.groups))
#
# # remove the groups that have no arguments (will not be printed)
# sep.ids <- sep.ids[lengths(k) > 0]
# k <- k[lengths(k) > 0]
#
# # get the arguments that belong to each group
# args <- lapply(k, function(x) .self$formal.command.args$value[x])
#
# # get the argument group for each separate argument group
# groups <- lapply(match(sep.ids, ids), function(x) .self$argument.groups[[x]])
#
# # attach the arguments that are printed at the top level
# args <- c(args, list(.self$formal.command.args$value[setdiff(seq_along(arg.groups), unlist(k))]))
# groups <- c(groups, list(.self))
#
#
# .mapply(organize1, list(args, groups), NULL)
