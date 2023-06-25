tryCatch2 <- function (expr, ..., else., finally)
{
    if (!missing(finally))
        on.exit(finally)


    ## if 'else.' is not provided, do original code,
    ## don't want to slow down existing code
    if (missing(else.))
        tryCatch(expr = expr, ...)


    else if (...length() <= 0L)
        # ## if we don't want to generate an error here, could instead do:
        # tryCatch({
        #     expr
        #     else.
        # })
        stop("'tryCatch' with 'else.' but no condition handlers makes no sense")


    else {
        ## default is to not evaluate 'else.' expression
        do_else <- FALSE


        ## if we catch a condition, we need to know whether it would
        ## automatically print
        x <- withVisible(tryCatch(expr = {
            expr
            do_else <- TRUE
        }, ...))


        if (do_else)
            ## if a condition was caught, 'do_else' will be FALSE
            ## if an error was thrown but not caught, execution will stop
            ## before here either way, 'else.' is not evaluated, as expected
            ##
            ## also, I would argue that 'else.' should be returned because that
            ## would be the most natural transition from
            ## not using 'else.' to using 'else.'. example:
            ##
            ## tryCatch({
            ##     piece.of.code.that.should.be.protected.from.conditions
            ##     other.code.that.follows.that.should.not.be.protected.from.conditions
            ## }, <handlers>)
            ##
            ## would become:
            ##
            ## tryCatch({
            ##     piece.of.code.that.should.be.protected.from.conditions
            ## }, <handlers>, else. = {
            ##     other.code.that.follows.that.shouldnt.be.protected.from.conditions
            ## })
            ##
            ## and in both cases, we would want
            ## 'other.code.that.follows.that.shouldnt.be.protected.from.conditions'
            ## to be returned
            else.


        ## we end up here when a condition was caught
        else if (x[[2L]])        ## else if (x$visible)
            x[[1L]]              ##     x$value
        else invisible(x[[1L]])  ## else invisible(x$value)
    }
}


# .last.condition <- NULL
# tryCatch3 <- function (expr, ..., else., finally)
# {
#     fun <- function(c) {
#         .last.condition <<- c
#         NULL
#     }
#     syms <- lapply(paste0("..", seq_len(...length())), as.symbol)
#     env <- environment()
#     missing <- vapply(syms, function(sym) eval(call("missing", sym), env), NA)
#     return(missing)
#     funs <- lapply(paste0("..", seq_len(...length())), function(x) {
#         x <- as.symbol(x)
#         if (eval(call("missing", x)))
#
#         body(fun)[[3L]] <- x
#         fun
#     })
#     call <- lapply(seq_along(funs), function(x) {
#         call("[[", as.symbol("funs"), x)
#     })
#     names(call) <- ...names()
#     if (!missing(finally))
#         call <- c(call, finally = as.symbol("finally"))
#     call <- c(expr = as.symbol("expr"), call)
#     call <- as.call(c(as.symbol("tryCatch"), call))
#     if (missing(else.)) {
#         eval(call("delayedAssign", "call", call))
#         call
#     }
#     else {
#         do_else <- FALSE
#         call[[2L]] <- quote({ expr; do_else <- TRUE})
#         call <- call("withVisible", call)
#         eval(call("delayedAssign", "call", call))
#         x <- call
#         if (do_else)
#             else.
#         else if (x[[2L]])
#             x[[1L]]
#         else invisible(x[[1L]])
#     }
# }
#
#
# tryCatch3(message("testing"), error = , warning = 6, finally = writeLines("finally"), else. = 5)
