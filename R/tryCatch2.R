tryCatch2 <- function (expr, ..., else., finally)
{
    if (!missing(finally))
        on.exit(finally)


    # if 'else.' is not provided, do original code, don't want to slow down existing code
    if (missing(else.))
        tryCatch(expr = expr, ...)


    else if (...length() <= 0L)
        # if we don't want to generate an error here, could instead do:
        # tryCatch({
        #     expr
        #     else.
        # })
        stop("'tryCatch' with 'else.' but no condition handlers makes no sense")


    else {
        # default is to not evaluate 'else.' expression
        do_else <- FALSE


        # if we catch a condition, we need to know whether it would
        # automatically print
        x <- withVisible(tryCatch(expr = {
            expr
            do_else <- TRUE
        }, ...))


        if (do_else)
            # if a condition was caught, 'do_else' will be FALSE
            # if an error was raised but not caught, execution will stop before
            # here either way, 'else.' is not evaluated, as expected
            #
            # also, I would argue that 'else.' should be returned because that
            # would be the most natural transition from
            # not using 'else.' to using 'else.'. example:
            #
            # tryCatch({
            #     piece.of.code.that.should.be.protected.from.conditions
            #     other.code.that.follows.that.shouldnt.be.protected.from.conditions
            # }, <handlers>)
            #
            # would become:
            #
            # tryCatch({
            #     piece.of.code.that.should.be.protected.from.conditions
            # }, <handlers>, else. = {
            #     other.code.that.follows.that.shouldnt.be.protected.from.conditions
            # })
            #
            # and in both cases, we would want
            # 'other.code.that.follows.that.shouldnt.be.protected.from.conditions'
            # to be returned
            else.


        # we end up here when a condition was caught
        else if (x[[2L]])        # else if (x$visible)
            x[[1L]]              #     x$value
        else invisible(x[[1L]])  # else invisible(x$value)
    }
}
