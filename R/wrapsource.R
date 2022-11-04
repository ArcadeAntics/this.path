wrap.source <- function (expr, file, character.only = FALSE, file.only = FALSE)
{
    if (missing(file))
        .External2(C_wrapsource, character.only, file.only)
    else .External2(C_wrapsource, file, character.only, file.only)
}


inside.source <- function (file, character.only = FALSE, file.only = FALSE)
.External2(C_insidesource, file, character.only, file.only)
