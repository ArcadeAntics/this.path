if (getRversion() < "3.6.0") {


errorCondition <- function (message, ..., class = NULL, call = NULL)
structure(list(message = as.character(message), call = call, ...),
    class = c(class, "error", "condition"))
environment(errorCondition) <- .BaseNamespaceEnv


}


if (getRversion() < "3.3.0") {


strrep <- function (x, times)
{
    if (!is.character(x))
        x <- as.character(x)
    .External2(C_strrep, x, as.integer(times))
}


startsWith <- function (x, prefix)
.External2(C_startsWith, x, prefix)


endsWith <- function (x, suffix)
.External2(C_endsWith, x, suffix)


}


if (getRversion() < "3.2.0") {


dir.exists <- function (paths)
.External2(C_direxists, paths)


lengths <- function (x, use.names = TRUE)
.External2(C_lengths, x, use.names)


}


if (getRversion() < "3.1.0") {


anyNA <- function (x, recursive = FALSE)
.External2(C_anyNA, x, recursive)


}


if (getRversion() < "3.0.0") {


delayedAssign("C_mapply", getNativeSymbolInfo("do_mapply", PACKAGE = "base"))


.mapply <- function (FUN, dots, MoreArgs)
.Call(C_mapply, match.fun(FUN), dots, MoreArgs, environment())


setprseen2 <- function (ptr)
.External2(C_setprseen2, ptr)


}
