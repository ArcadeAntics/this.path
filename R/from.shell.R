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
