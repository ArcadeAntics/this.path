# p <- this.path::ArgumentParser(style = 2)
# p$add.argument("--foo", help = "foo help")
# p$print.help()
#
#
# p <- this.path::ArgumentParser(program = "myprogram")
# p$print.help()
#
#
# p <- this.path::ArgumentParser(program = "myprogram", style = 2)
# p$add.argument("--foo", help = "foo of the %(PROGRAM)s program")
# p$print.help()
#
#
# p <- this.path::ArgumentParser(program = "PROGRAM", style = 2)
# p$add.argument("--foo", nargs = "?", help = "foo help")
# p$add.argument("bar", nargs = "+", help = "bar help")
# p$print.help()
#
#
# p <- this.path::ArgumentParser(description = "A foo that bars")
# p$print.help()
#
#
# p <- this.path::ArgumentParser(
#     description = "A foo that bars",
#     epilogue = "And that's how you'd foo a bar"
# )
# p$print.help()
#
#
#
#
#
#
#
# p <- this.path::ArgumentParser(
#     program = "PROGRAM",
#     description = "this description
#         was indented weird
#             but that is okay",
#     epilogue = "
#             likewise for this epilogue whose whitespace wil
#         be cleaned up and whose words will be wrapped
#         across a couple lines"
# )
# p$print.help()
#
#
# p <- this.path::ArgumentParser(
#     program = "PROGRAM",
#     wrap.description = FALSE,
#     description = this.path::dedent("
#         Please do not mess up this text!
#         --------------------------------
#             I have indented it
#             exactly the way
#             I want it
#             "))
# p$print.help()
