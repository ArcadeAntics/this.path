# parser <- this.path::ArgumentParser(description = "Process some integers.", style = 2)
# parser$add.argument("integers", metavariable = "N", type = "integer", nargs = "+",
#     help = "An integer for the accumulator")
# parser$add.argument("--sum", destination = "accumulate", action = "store_const",
#     constant = base::sum, default = base::max,
#     help = "Sum the integers (default: find the max)")
#
#
# parser$print.help()
#
#
# pargs <- parser$parse.args(c("1", "2", "3", "4"))
# print(pargs$accumulate(pargs$integers))
#
#
# pargs <- parser$parse.args(c("1", "2", "3", "4", "--sum"))
# print(pargs$accumulate(pargs$integers))
#
#
# pargs <- parser$parse.args(c("a", "b", "c"))
# print(pargs$accumulate(pargs$integers))
#
#
# pargs <- parser$parse.args(c("--sum", "7", "-1", "42"))
# print(pargs)
