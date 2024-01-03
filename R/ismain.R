is.main <- function ()
{
    n <- .External2(.C_getframenumber)
    if (is.na(n))
        NA
    else if (n)
        FALSE
    else TRUE
}


delayedAssign(".has_shINPUT", { .in_shell && .shINFO[["has_input"]] })


from.shell <- function ()
{
    n <- .External2(.C_getframenumber)
    if (is.na(n))
        NA
    else if (n)
        FALSE
    else .has_shINPUT
}


.toplevel.nframe <- function ()
{
    if (.GUI_jupyter) {
        if (.isJupyterLoaded())
            sys.frame(1L)$kernel$executor$nframe + 1L
        else 0L
    }
    else 0L
}


# .pragma_once <- evalq(envir = new.env(), {
#     x <- character(0)
# function (path)
# {
#     if (indx <- match(path, x, 0L))
#         FALSE
#     else {
#         value <- all(.relpath(x, path, FALSE, FALSE) != ".")
#         x[[length(x) + 1L]] <<- path
#         value
#     }
# }
# })
#
#
# pragma_once <- function (expr)
# ## forceAndCall was introduced in R 3.2.0
# # forceAndCall(1L, .pragma_once, .External2(.C_this_path))
# {
#     path <- .External2(.C_this_path)
#     if (.pragma_once(path)) {
#         rm(path)
#         expr
#     }
# }
