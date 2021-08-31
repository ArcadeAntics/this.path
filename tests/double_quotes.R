args <- c(
    paste("test", c(
        r"{"}",
        r"{\"}",
        r"{\\"}",
        r"{\\\"}",
        r"{\\\\"}",
        r"{\\\\\"}",
        r"{\\\\\\"}",
        r"{\\\\\\\"}"
    ), "dbl quot w bslash"),
    paste("test trailing bslash", c(
        r"{\}",
        r"{\\}",
        r"{\\\}",
        r"{\\\\}",
        r"{\\\\\}",
        r"{\\\\\\}",
        r"{\\\\\\\}",
        r"{\\\\\\\\}"
    )),
    "test `trailing` squote '"
)


stopifnot(vapply(args, function(arg) {
    identical(
        this.path::Rscript(exprs = r"{writeLines(commandArgs(TRUE))}", args = arg, intern = TRUE),
        arg
    )
}, NA))
