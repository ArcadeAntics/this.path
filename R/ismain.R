is.main <- function ()
# {
#     n <- .External2(.C_getframenumber)
#     if (is.na(n))
#         NA
#     else if (n)
#         FALSE
#     else TRUE
# }
!.External2(.C_getframenumber)


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
