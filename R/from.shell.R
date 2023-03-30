from.shell <- function ()
{
    n <- get.frame.number()
    if (is.na(n))
        NA
    else if (n)
        FALSE
    else has.shFILE
}


is.main <- function ()
{
    n <- get.frame.number()
    if (is.na(n))
        NA
    else if (n) {
        if (n == 1L || (n == 2L && identical(sys.function(1L), withArgs))) {
            if (unrecognized.manner)
                NA
            else !has.shFILE
        }
        else FALSE
    }
    else if (unrecognized.manner)
        NA
    else TRUE
}


toplevel.context.number <- function ()
{
    if (gui.jupyter) {
        if (isJupyterLoaded())
            sys.frame(1L)[["kernel"]][["executor"]][["nframe"]] + 1L
        else 0L
    }
    else 0L
}
