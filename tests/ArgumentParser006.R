parser <- this.path::ArgumentParser()


parser$add.version(exit = utils::packageVersion("this.path"))
parser$add.argument("--alpha")
parser$add.skip()
parser$add.argument("--beta")


parser.args <- parser$parse.args(
    c("--alpha", "testing", "--args", "--beta", "more")
)
