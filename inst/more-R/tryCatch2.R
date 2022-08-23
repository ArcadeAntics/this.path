tryCatch2 <- function (expr, ..., else., finally)
{
    tryCatchList <- function(expr, names, parentenv, handlers) {
        nh <- length(names)
        if (nh > 1L)
            tryCatchOne(tryCatchList(expr, names[-nh], parentenv,
                handlers[-nh]), names[nh], parentenv, handlers[[nh]])
        else if (nh == 1L)
            tryCatchOne(expr, names, parentenv, handlers[[1L]])
        else expr
    }
    tryCatchOne <- function(expr, name, parentenv, handler) {
        doTryCatch <- function(expr, name, parentenv, handler) {
            .Internal(.addCondHands(name, list(handler), parentenv,
                environment(), FALSE))
            expr
        }
        value <- doTryCatch(return(expr), name, parentenv, handler)
        if (is.null(value[[1L]])) {
            msg <- .Internal(geterrmessage())
            call <- value[[2L]]
            cond <- simpleError(msg, call)
        }
        else if (is.character(value[[1L]])) {
            msg <- value[[1L]]
            call <- value[[2L]]
            cond <- simpleError(msg, call)
        }
        else cond <- value[[1L]]
        value[[3L]](cond)
    }
    if (!missing(finally))
        on.exit(finally)
    handlers <- list(...)
    classes <- names(handlers)
    parentenv <- parent.frame()
    if (length(classes) != length(handlers))
        stop("condition handlers must be specified with a condition class")


    # if 'else.' is not provided, do original code, don't want to slow down existing code
    if (missing(else.))
        tryCatchList(expr, classes, parentenv, handlers)
    else if (length(handlers) <= 0L)
        # if we don't want to generate an error here, could instead do:
        # tryCatchList({
        #     expr
        #     else.
        # }, classes, parentenv, handlers)
        stop("'tryCatch' with 'else.' but no handlers makes no sense")
    else {
        # default is to not evaluate 'else.' expression
        do_else <- FALSE


        # if we catch a condition, we need to know whether it would
        # automatically print
        x <- withVisible(tryCatchList({
            expr
            # if we evaluated 'expr' without raising a condition
            # of class 'classes', we should evaluate 'else.'
            #
            # but we must evalaute 'else.' outside of tryCatchList
            # so as not to catch any conditions raised by 'else.'
            do_else <- TRUE
        }, classes, parentenv, handlers))


        if (do_else)
            # if a condition was caught, 'do_else' will be FALSE
            # if an error was raised but not caught, execution will stop before here
            # either way, 'else.' is not evaluated, as expected
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
environment(tryCatch2) <- getNamespace("base")
