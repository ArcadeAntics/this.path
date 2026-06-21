##
## this.path : Get Executing Script's Path
## Copyright (C) 2026   Iris Simmons
##


encode_string <- function (x, width = 0L, quote = "", na.encode = TRUE,
    justify = c("left", "right", "centre", "none"))
{
    at <- attributes(x)
    x <- as.character(x)
    attributes(x) <- at
    oldClass(x) <- NULL
    justify <- match(
        match.arg(justify),
        c("left", "right", "centre", "none")
    ) - 1L
    .External2(.C_encode_string, x, width, quote, justify, na.encode)
}


URL_encode <- function (URL, reserved = FALSE, repeated = FALSE)
.External2(.C_URL_encode, URL, reserved, repeated)


URL_decode <- function (URL)
.External2(.C_URL_decode, URL)
