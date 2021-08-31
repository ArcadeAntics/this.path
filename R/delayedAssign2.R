delayedAssign2 <- function (x, value, eval.env = parent.frame(1), assign.env = parent.frame(1),
    evaluated = FALSE)
{
    eval(substitute(delayedAssign(x, value, eval.env, assign.env),
        list(x = x, value = if (evaluated) value else substitute(value),
            eval.env = eval.env, assign.env = assign.env)))
}
