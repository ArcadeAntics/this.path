# rm(list = ls())
#
#
# p <- this.path::ArgumentParser()
# `p Group 1` <- p$add.argument.group("Group 1")
# `p Group 1`$add.argument("--alpha")
# `p Group 1`$add.argument("--beta")
# `p Group 2` <- p$add.argument.group()
# `p Group 2`$add.argument("--gamma")
# `p Group 2`$add.argument("--delta")
# `p Group 3` <- `p Group 2`$add.argument.group("Group 3")
# `p Group 3`$add.argument("--eta")
# `p Group 3`$add.argument("--zeta")
# p$add.argument("--epsilon")
#
#
#
#
#
# ID <- function(x) {
#     if (inherits(x, "ArgumentGroup"))
#         c(x$parent.IDs, x$ID)
#     else integer()
# }
#
#
#
#
#
# y <- p$formal.command.args$value
# groups <- sapply(y, function(y) y[["group"]][1L])
# groups2 <- sapply(groups, function(group) {
#     if (is.na(group) || is.na(p$argument.groups[[group]]$title))
#         0L
#     else group
# })
# groups3 <- unique(groups2)
# m <- match(groups2, groups3)
# contexts <- lapply(groups3, function(group) {
#     if (group)
#         p$argument.groups[[group]]
#     else p
# })
#
# .mapply(function(group, args, context) {
#     args2 <- args
#     tags <- list()
#     more.tags <- list()
#     helps <- character()
#     wraps <- logical()
#     while (length(args2)) {
#         arg <- args2[[1L]]
#         tags <- c(tags, list(c(
#             if (!is.na(arg$short.flag))
#                 paste0("-", arg$short.flag)
#             else "",
#             if (!is.na(arg$name))
#                 paste0("  ", arg$name)
#             else if (!is.na(arg$long.flag))
#                 paste0("--", arg$long.flag)
#             else "  "
#         )))
#         helps <- c(helps, arg$help)
#         wraps <- c(wraps, arg$wrap)
#         if (identical(arg[["group"]], ID(context))) {
#             more.tags
#         }
#         more.tags <- c(more.tags, list(character()))
#         print(arg[["group"]])
#         print(ID(context))
#         cat("\n")
#         print(arg)
#         args2 <- args2[-1]
#     }
# }, list(groups3, split(y, m), contexts), NULL)
