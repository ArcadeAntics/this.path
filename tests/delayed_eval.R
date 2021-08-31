x <- list(
    list(
        tags        = character(),
        short.flags = c("a", "b"),
        long.flags  = c("alpha", "beta"),
        action      = "store_true",
        type        = "logical",
        # default     = quote(expr = ),
        default     = quote(TRUE),
        has.default = TRUE,
        choices     = NULL,
        value       = NULL
    ),
    list(
        tags        = character(),
        short.flags = "c",
        long.flags  = c("gamma", "delta"),
        action      = "store_true",
        type        = "logical",
        default     = quote(`--alpha`),
        # default     = quote(expr = ),
        has.default = TRUE,
        choices     = NULL,
        value       = NULL
    )
)


# x <- list(
#     list(
#         short.flags = "-clean",
#         default     = quote(TRUE),
#         value       = NULL
#     ),
#     list(
#         short.flags = "-clean.input",
#         default     = quote(`-clean`),
#         value       = NULL
#     ),
#     list(
#         short.flags = "-clean.output",
#         default     = quote(`-clean`),
#         value       = NULL
#     )
# )


type_conv <- function (name, default, action, type, choices)
{
    default
    switch(action, store_true = , store_false = {
        default <- as.vector(default, type)[1L]
        if (is.na(default))
            stop("missing value where TRUE/FALSE needed")
        default
    }, count = {
        as.vector(default, type)[1L]
    }, append = {
        as.vector(default, type)
    }, store_const = {
        as.vector(default, type)[1L]
    }, help = , exit = , skip = {
        stop(gettextf("cannot request default from argument of action '%s'",
            action))
    }, {
        as.vector(default, type)[1L]
    })
}


envir <- new.env()
nams <- lapply(x, function(xx) {
    c(xx$tags, paste0("-", xx$short.flags), paste0("--", xx$long.flags))
})
nms1 <- vapply(nams, "[[", 1L, FUN.VALUE = "")
provided <- vapply(x, function(xx) !is.null(xx$value), NA)


for (n in seq_along(x)) {
    name <- nms1[[n]]
    if (provided[[n]])
        assign       (name, x[[n]]$value  , envir = envir, inherits = FALSE)
    else
        # this.path::delayedAssign2(name, x[[n]]$default, envir, envir, evaluated = TRUE)
        # this.path::delayedAssign2(name, substitute(this.path:::type_conv(default, action, type, choices), x[[n]]), envir, envir, evaluated = TRUE)
        this.path::delayedAssign2(name, substitute(if (has.default)
            type_conv(., default, action, type, choices)
        else quote(expr = ), c(x[[n]], . = name)), envir, envir, evaluated = TRUE)
    sym <- as.symbol(name)
    for (name in nams[[n]][-1L]) {
        this.path::delayedAssign2(name, sym, envir, envir, evaluated = TRUE)
    }
}


attach(envir)


# force all the arguments
for (n in which(!provided)) x[[n]]$default <- envir[[nms1[[n]]]]


detach("envir")
