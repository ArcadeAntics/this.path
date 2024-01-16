.toplevel.nframe <- function ()
{
    if (.GUI_jupyter) {
        if (.isJupyterLoaded())
            sys.frame(1L)$kernel$executor$nframe + 1L
        else 0L
    }
    else 0L
}


sys.srcref <- function (n = 1L, which = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    .External2(.C_sys.srcref, which)
}


sys.whiches <- function (n = 1L, which = if (n) sys.parent(n) else 0L)
{
    n <- .External2(.C_asIntegerGE0, n)
    .External2(.C_sys.whiches, which)
}
