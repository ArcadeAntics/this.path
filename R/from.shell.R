from.shell <- function ()
{
    if (.this.path(get.frame.number = TRUE))
        FALSE
    else has.shFILE
}


is.main <- function ()
{
    n <- .this.path(get.frame.number = TRUE)
    if (n) {
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
