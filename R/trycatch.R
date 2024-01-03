tryCatch2 <- function (expr, ..., else., finally)
.External2(.C_tryCatch2)


.last.condition <- function (c)
.External2(.C_last_condition, c)


last.condition <- function ()
.External2(.C_last_condition)


tryCatch3 <- function (expr, ..., else., finally)
.External2(.C_tryCatch3)


# tryCatch3(message("testing"), message = , error = writeLines("caught error"), warning = 6, test = , finally = writeLines("finally"), else. = writeLines("else."))
