# p <- this.path::ArgumentParser(
#
#     description = "A description for this script",
#
#     epilogue = "   bonkque ---- testing ----------- without wrapping",
#     wrap.epilogue = FALSE,
#
#     style = 2
# )
#
#
# p$add.argument("testing", help = "testing positional arguments")
#
#
# p$add.argument("-a", action = "store_true", help = "Help message for argument '-%(SHORTFLAG)s' (default: %(DEFAULT)s)")
# p$add.argument("--beta", action = "store_false", help = "Help message for argument '--%(LONGFLAG)s'")
# p$add.argument("-d", "--delta", action = "count", help = "Help message for argument '--%(LONGFLAG)s'")
#
#
# p$add.argument("-f", help = "Take input from '%(METAVARIABLE)s'", metavariable = "FILE")
# p$add.argument("--encoding", help = "Specify encoding to be used for stdin", metavariable = "enc")
# p$add.argument("-n", "--number", action = "append", type = "numeric", help = "\n  A bunch of\nnumbers to sum \n  ")
#
#
# p$add.argument("-e", action = "append", type = "expression", help = "Any syntactically valid R expression", metavariable = "EXPR")
#
#
# p$add.argument("--with-keep.source",
#     action = "store_true",
#     help = "Use 'keep.source' for R code",
#     destination = "keep.source")
# p$add.argument("--without-keep.source",
#     action = "store_false",
#     help = "Do not use 'keep.source' for R code",
#     destination = "keep.source")
#
#
# p$print.help()
