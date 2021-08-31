install <- function (x)
{
    unlist(lapply(x, function(xx) {
        lapply(xx, function(xxx) {
            as.character(as.symbol(xxx))
        })
    }), use.names = FALSE)
}


STYLES <- c(
    DFLTSTYLE    <- 1L,
    WODITTOSTYLE <- 2L,
    INLINESTYLE  <- 3L
)


Refcharacter <- methods::setRefClass(
    Class  = "Refcharacter",
    fields = c(value = "character")
)


Refinteger <- methods::setRefClass(
    Class  = "Refinteger",
    fields = c(value = "integer")
)


subparsers <- methods::setRefClass(
    Class  = "subparsers",
    fields = c(
        style           = "integer"     ,  # fields copied from the argument parser
        parent.commands = "list"        ,
        commands        = "Refcharacter",


        title           = "character"   ,
        description     = "list"        ,
        program         = "character"   ,
        required        = "logical"     ,
        value           = "list"        ),
    validity = function (object)
{
    c(
        if (length(object$style) != 1L)
            gettextf("invalid 'style', must be of length 1, got %d", length(object$style))
        else if (!object$style %in% STYLES)
            gettextf("invalid 'style', must be one of %s", paste(STYLES, collapse = ", ")),
        if (!all(vapply(object$parent.commands, base::inherits, "Refcharacter", FUN.VALUE = NA)))
            gettext("invalid 'parent.commands', must be a list of \"Refcharacter\" objects")
    )
})


FormalCommandArgs <- methods::setRefClass(
    Class  = "FormalCommandArgs",
    fields = c(value = "list")
)


ArgumentGroup <- methods::setRefClass(
    Class  = "ArgumentGroup",
    fields = c(
        program                      = "character"        ,
        title                        = "character"        ,


        ID                           = "Refinteger"       ,


        required                     = "logical"          ,
        required.recursive           = "logical"          ,
        mutually.exclusive           = "logical"          ,
        mutually.exclusive.recursive = "logical"          ,
        parent.titles                = "character"        ,


        parent.IDs                   = "list"             ,


        type                         = "character"        ,
        description                  = "list"             ,
        argument.groups              = "list"             ,
        formal.command.args          = "FormalCommandArgs")
)


ArgumentParser <- methods::setRefClass(
    Class  = "this.path_ArgumentParser",
    fields = c(
        program             = "character"        ,
        usage               = "character"        ,
        description         = "list"             ,
        epilogue            = "list"             ,
        style               = "integer"          ,
        argument.groups     = "list"             ,
        formal.command.args = "FormalCommandArgs",
        subparsers          = "subparsers"       ,
        help.message        = "character"        ,


        parent.commands     = "list"             ,  # for subparsers only
        commands            = "Refcharacter"     ,
        help                = "list"             )
)


subparsers$methods(


add.parser = function (commands, program = NA, ..., help = NA,
    wrap = TRUE, wrap.help = wrap, style = NA,
    overwrite = getOption("this.path.overwrite", NA))
{
    progra2 <- as.character(program)[1L]
    if (is.na(progra2))
        progra2 <- .self$program


    styl2 <- as.integer(style)[1L]
    if (!styl2 %in% STYLES)
        styl2 <- .self$style


    valu2 <- ArgumentParser(program = progra2, style = styl2, wrap = wrap, ...)


    command2 <- install(commands)
    if (!length(command2))
        stop(gettext("invalid 'commands', must provide at least one"))
    if (any(i <- !isName(command2))) {
        stop(sprintf(ngettext(sum(i), "invalid command %s, does not match 'name.pattern'",
            "invalid commands %s, do not match 'name.pattern'"),
            paste(sQuote(command2[i]), collapse = ", ")))
    }


    valu2$parent.commands <- c(.self$parent.commands, list(.self$commands))
    valu2$commands <- Refcharacter(value = command2)
    valu2$help <- as.description(help, wrap = wrap.help)


    otags <- lapply(.self$value, function(xx) xx[["commands"]][["value"]])
    ids <- rep(seq_along(otags), lengths(otags))
    i <- unlist(lapply(lengths(otags), "seq_len"))
    otags <- unlist(otags)
    N <- match(command2, otags, nomatch = 0L)
    if (any(N)) {
        if (is.na(overwrite))
            warning(sprintf(ngettext(sum(N > 0L), "overwriting definition of command %s",
                "overwriting definitions of commands %s"), paste0("'", command2[N > 0L], "'", collapse = ", ")))
        else if (!overwrite)
            stop(sprintf(ngettext(sum(N > 0L), "invalid command %s, already in use",
                "invalid commands %s, already in use"), paste0("'", command2[N > 0L], "'", collapse = ", ")))
        N <- split(i[N], ids[N])
        i <- as.integer(names(N))
        for (j in seq_along(i)) {
            .self$value[[i[[j]]]][["commands"]][["value"]] <-
                .self$value[[i[[j]]]][["commands"]][["value"]][-N[[j]]]

        }
        i <- lengths(lapply(.self$value, function(xx) xx[["commands"]][["value"]])) > 0L
        if (!all(i))
            .self$value <- .self$value[i]
    }


    valu2$add.subparsers()
    .self$value <- c(.self$value, list(valu2))
    valu2
},


reorder = function (first = character(), last = character())
{
    process <- function(x) {
        if (is.character(x)) {
            otags <- lapply(.self$value, function(xx) xx[["commands"]][["value"]])
            ids <- rep(seq_len(otags), lengths(otags))
            otags <- unlist(otags)
            x <- ids[match(x, otags)]
        }
        else x <- as.integer(x)
        x <- unique(x)
        x[!is.na(x) & x >= 1L & x <= length(.self$value)]
    }
    first <- process(first)
    last <- process(last)
    last <- setdiff(last, first)
    middle <- setdiff(seq_along(.self$value), c(first, last))
    .self$value <- .self$value[c(first, middle, last)]
    invisible(.self)
}


)


as.description <- function (x = NA, wrap = TRUE, indent = 0, exdent = 0)
{
    x <- format.help(x)
    wrap <- as.logical(wrap)[1L]
    if (is.na(wrap))
        wrap <- TRUE
    indent <- as.integer(indent)[1L]
    if (is.na(indent) || indent < 0L)
        indent <- 0L
    exdent <- as.integer(exdent)[1L]
    if (is.na(exdent) || exdent < 0L)
        exdent <- 0L
    list(x = x, wrap = wrap, indent = indent, exdent = exdent)
}


# command.args <- function (x, trailingOnly = FALSE)
# if (trailingOnly) x$trailingArgs else x$args


commands <- function (x)
c(vapply(x$parent.commands[-1L], function(xx) {
    xx[["value"]][[1L]]
}, ""), if (length(x$commands[["value"]]))
    x$commands[["value"]][[1L]]
else character())


makeHelpMessage <- function (x, style = NULL)
{
    value <- character(0)


    sub.commands <- vapply(x$subparsers$value, function(xx) {
        paste(xx[["commands"]][["value"]], collapse = ", ")
    }, "")
    required <- x$subparsers$required
    usag2 <- x$usage
    if (is.na(usag2) || !nzchar(usag2))
        usag2 <- paste(c("Usage:", x$program, commands(x), "[arguments]", if (length(sub.commands)) {
            c(if (required)
                "command"
            else "[command]", "...")
        }), collapse = " ")
    value <- c(value, usage = usag2)


    d <- x$description
    if (nzchar(d$x) && d$wrap)
        d$x <- paste(strwrap(d$x, indent = d$indent, exdent = d$exdent), collapse = "\n")
    value <- c(value, description = d$x)


    styl2 <- as.integer(style)[1L]
    if (!styl2 %in% STYLES)
        styl2 <- x$style
    if (styl2 == DFLTSTYLE) {

        tags <- character()
        helps <- character()
        wraps <- logical()
        groups <- character()
        for (y in x$formal.command.args$value) {
            if (is.null(y$help))
                next
            helps <- c(helps, y$help)
            wraps <- c(wraps, y$wrap.help)
            if (length(y$tags)) {
                tags <- c(tags, paste(y$tags, collapse = ", "))
            }
            else if (y$action %in% c("store_const", "store_true", "store_false", "count", "help", "exit", "skip")) {
                groups <- c(groups, NA_character_)
                tags <- c(tags, paste(c(
                    paste0("-", y$short.flags, recycle0 = TRUE),
                    paste0("--", y$long.flags, recycle0 = TRUE)
                ), collapse = ", "))
            }
            else {
                groups <- c(groups, NA_character_)
                tags <- c(tags, paste(c(
                    paste0("-", y$short.flags, " ", y$metavariable, recycle0 = TRUE),
                    paste0("--", y$long.flags, "=", y$metavariable, recycle0 = TRUE)
                ), collapse = ", "))
            }
        }
    }
    else if (styl2 == 2) {
        short.tags <- character()
        long.tags <- character()
        helps <- character()
        wraps <- logical()
        groups <- character()
        for (y in x$formal.command.args$value) {
            groups <- c(groups, if (!is.na(y$name)) "Positional Arguments" else "Arguments")
            helps <- c(helps, y$help)
            wraps <- c(wraps, y$wrap.help)
            if (!is.na(y$name)) {
                groups <- ""
                short.tags <- c(short.tags, "")
                long.tags <- c(long.tags, y$name)
            }
            else if (y$action %in% c("store_const", "store_true", "store_false", "count", "help", "exit")) {
                short.tags <- c(short.tags, if (!is.na(y$short.flag))
                    sprintf("-%s", y$short.flag)
                else "")
                long.tags <- c(long.tags, if (!is.na(y$long.flag))
                    sprintf("--%s", y$long.flag)
                else "")
            }
            else if (!is.na(y$long.flag)) {
                short.tags <- c(short.tags, if (!is.na(y$short.flag))
                    sprintf("-%s", y$short.flag)
                else "")
                long.tags <- c(long.tags, sprintf("--%s=%s", y$long.flag, y$metavariable))
            }
            else {
                short.tags <- c(short.tags, sprintf("-%s %s", y$short.flag, y$metavariable))
                long.tags <- c(long.tags, "")
            }
        }
        i <- nzchar(short.tags) & nzchar(long.tags)
        short.tags <- format(short.tags, justify = "left")
        short.tags[ i] <- paste0(short.tags[ i], ", ")
        short.tags[!i] <- paste0(short.tags[!i], "  ")
        tags <- paste0(short.tags, long.tags)
    }
    else stop("invalid 'style'; should never happen, please report!")
    if (length(tags)) {
        nc <- nchar(tags, type = "width")
        tags <- paste0("  ", tags, strrep(" ", max(nc) - nc), "  ")
        indent <- max(nchar(tags, type = "width"))
        helps[wraps] <- vapply(strwrap(helps[wraps], width = max(10, getOption("width") - indent - 1), simplify = FALSE), "paste", collapse = "\n", FUN.VALUE = "")
        helps <- gsub("\n", paste0("\n", strrep(" ", indent)), helps)


        arguments <- paste0(tags, helps, collapse = "\n")
        arguments <- paste0("Arguments:\n", arguments)
        value <- c(value, arguments = arguments)
    }


    # if (length(i)) {
    #     cat("Positional Arguments:\n")
    #     for (j in i) cat(tags[[j]], helps[[j]], "\n", sep = "")
    #     cat("\n")
    # }
    # i <- setdiff(seq_along(tags), i)


    if (length(sub.commands)) {
        helps <- lapply(x$subparsers$value, "[[", "help")
        wraps <- vapply(helps, "[[", "wrap", FUN.VALUE = NA)
        helps <- vapply(helps, "[[", "x", FUN.VALUE = "")


        tags <- sub.commands
        tags <- sprintf("  %s  ", format(tags, justify = "left"))
        indent <- max(nchar(tags, type = "width"))
        helps[wraps] <- vapply(strwrap(helps[wraps],
            width = getOption("width") - indent - 1, exdent = 2,
            simplify = FALSE), "paste", collapse = "\n", FUN.VALUE = "")
        helps <- gsub("\n", paste0("\n", strrep(" ", indent)), helps, fixed = TRUE)


        title <- x$subparsers$title
        if (nzchar(title))
            title <- paste0(title, "\n")


        sub.commands <- paste0(title, paste0(tags, helps, collapse = "\n"))
    }
    else sub.commands <- ""
    value <- c(value, sub.commands = sub.commands)


    e <- x$epilogue
    if (nzchar(e$x) && e$wrap)
        e$x <- paste(strwrap(e$x, indent = e$indent, exdent = e$exdent), collapse = "\n")
    value <- c(value, epilogue = e$x)
    return(value)
}


default.help <- function (x)
{
    switch(x, help = {
        if (.Platform$OS.type == "windows") {
            "Print usage message and exit"
        } else "Print short help message and exit"
    }, skip = {
        "Skip the rest of the command line"
    }, version = {
        "Print version info and exit"
    }, stop("invalid 'x'; should never happen, please report!"))
}


sharedMethods <- list(


add.argument = function (..., action = NULL, nargs = NULL, constant, default,
    type = "any", choices = NULL, required = NA, help = NA,
    metavariable = NA, destination = NA, exit = NA, wrap = TRUE, wrap.help = wrap, wrap.exit = wrap,
    overwrite = getOption("this.path.overwrite", NA))
{
    x <- install(list(...))
    if (!length(x))
        stop("... must not be empty")
    tags <- shorts <- longs <- character()
    for (x in x) {
        if (isNameOrFlag(x)) {
            if (startsWith(x, "--"))
                longs <- c(longs, getTag(x))
            else if (startsWith(x, "-"))
                shorts <- c(shorts, getTag(x))
            else tags <- c(tags, getTag(x))
        }
        else stop(gettextf("unused argument '%s', is not a valid short flag, long flag, or name",
            x))
    }
    if (length(tags) && (length(shorts) || length(longs)))
        stop("... must be a set of names or flags, not both")


    destination <- as.character(destination)[1L]
    if (!nzchar(destination))
        stop("invalid 'destination' argument")
    if (is.na(destination))
        destination <- if (length(tags))
            tags[[1L]]
        else if (length(longs))
            longs[[1L]]
        else shorts[[1L]]
    destination <- as.character(as.symbol(destination))


    metavariable <- as.character(metavariable)[1L]
    if (is.na(metavariable))
        metavariable <- if (length(tags))
            tags[[1L]]
        else if (length(longs))
            longs[[1L]]
        else shorts[[1L]]


    hel2 <- if (!is.null(help))
        format.help(help)


    nargs <- parse.nargs(nargs)
    require2 <- as.logical(required)[1L]
    if (!is.null(nargs) && !is.na(require2)) {
        if (xor(require2, nargs[1L]))
            stop("specify one of 'nargs' and 'required'")                    # 'nargs' and 'required' are both specified and do not agree
        else warning("'nargs' and 'required' should not both be specified")  # 'nargs' and 'required' are both specified but agree
        require2 <- NA  # 'nargs' has precedence over 'required'
    }
    if      ( is.null(action) &&  is.null(nargs) &&  is.na(require2)) {
        action <- "store"
        require2 <- length(tags) > 0
        nargs <- parse.nargs(c(require2, 1))
    }
    else if ( is.null(action) &&  is.null(nargs) && !is.na(require2)) {
        action <- "store"
        nargs <- parse.nargs(c(require2, 1))
    }
    else if ( is.null(action) && !is.null(nargs) &&  is.na(require2)) {
        if (length(nargs) == 1)
            action <- if (nargs == 1) "store" else "append"
        else action <- if (all(nargs == c(0, 1))) "store" else "append"
    }
    else if (!is.null(action) &&  is.null(nargs) &&  is.na(require2)) {
        action <- match.action(action)
        nargs <- parse.nargs(c(
            switch(action, store_const = , store_true = , store_false = 0, length(tags) > 0),
            switch(action, append = , count = Inf, 1)
        ))
    }
    else if (!is.null(action) &&  is.null(nargs) && !is.na(require2)) {
        action <- match.action(action)
        if (require2 && action %in% c("store_const", "store_true", "store_false", "help", "exit"))
            warning(gettextf("'required = %s' does not make sense with 'action = \"%s\"'",
                require2, action))
        nargs <- parse.nargs(c(require2, switch(action, append = , count = Inf, 1)))
    }
    else if (!is.null(action) && !is.null(nargs) &&  is.na(require2)) {
        action <- match.action(action)
        if (length(nargs) == 1) {
            if (action %in% c("store_const", "store_true", "store_false", "count", "help", "exit") ||
                action == "store" && nargs > 1)
                warning(gettextf("'nargs = %.0f' does not make sense with 'action = \"%s\"'",
                    nargs, action))
        }
        else if (length(nargs) == 2) {
            if (nargs[1L] > 0 && action %in% c("store_const", "store_true", "store_false", "help", "exit") ||
                nargs[1L] > 1 && action == "store")
                warning(gettextf("'nargs >= %.0f' does not make sense with 'action = \"%s\"'",
                    nargs[1L], action))
        }
    }


    if (action %in% c("help", "exit", "skip"))
        destination <- NA_character_


    types <- c("logical", "integer", "numeric", "real", "double", "complex",
        "character", "raw", "list", "expression")
    typ2 <- match.arg(type, c("any", types))
    if (typ2 %in% c("real", "double"))
        typ2 <- "numeric"


    has.default <- !missing(default)


    switch(action, store_true = , store_false = {
        if (missing(default))
            default <- action != "store_true"
        default <- as.vector(default, typ2)[1L]
        if (typ2 == "any")
            typ2 <- typeof(default)
        default <- as.logical(default)
        if (is.na(default))
            stop("missing value where TRUE/FALSE needed")
        default <- as.vector(default, typ2)
        if (!is.null(choices))
            warning(gettextf("argument 'choices' does not make sense with 'action = \"%s\"'",
                action))
        choices <- NULL
    }, count = {
        if (missing(default))
            default <- 0L
        default <- as.vector(default, typ2)[1L]
        if (typ2 == "any")
            typ2 <- typeof(default)
        if (!is.null(choices))
            warning(gettextf("argument 'choices' does not make sense with 'action = \"%s\"'",
                action))
        choices <- NULL
    }, append = {
        if (missing(default)) {
            if (typ2 == "any")
                typ2 <- "character"
            default <- vector(typ2, length = 0L)
        }
        else {
            default <- as.vector(default, typ2)
            if (typ2 == "any")
                typ2 <- typeof(default)
        }
        if (!is.null(choices))
            choices <- as.vector(choices, typ2)
    }, store_const = {
        force(constant)
        force(default)
        if (typ2 != "any") {
            constant <- as.vector(constant, typ2)
            default <- as.vector(default, typ2)
        }
        typ2 <- "logical"
        if (!is.null(choices))
            warning(gettextf("argument 'choices' does not make sense with 'action = \"%s\"'",
                action))
        choices <- NULL
    }, {
        if (missing(default)) {
            if (is.null(choices)) {
                if (typ2 == "any")
                    typ2 <- "character"
            }
            else {
                choices <- as.vector(choices, typ2)
                if (typ2 == "any")
                    typ2 <- typeof(choices)
            }
            default <- quote(expr = )
        }
        else {
            default <- as.vector(default, typ2)[1L]
            if (typ2 == "any")
                typ2 <- typeof(default)
            if (!is.null(choices))
                choices <- as.vector(choices, typ2)
        }
    })


    exi2 <- format.help(exit)


    help.sub2 <- function(x) {
        suppressWarnings(sprintf(help.sub(x, value),
            .self$program, tags[1L], shorts[1L], longs[1L], action,
            if (length(nargs) == 1) nargs else sprintf("c(%.0f, %.0f)", nargs[1], nargs[2]),
            deparse1(value$constant, collapse = ""), deparse1(value$default, collapse = ""),
            typ2, deparse1(value$choices, collapse = ""),
            if (nargs[1] == 0) FALSE else TRUE,
            metavariable, destination))
    }


    value <- list(tags = tags, short.flags = shorts, long.flags = longs,
        action = action, nargs = nargs)
    if (action == "store_const")
        value <- c(value, list(constant = constant))


    # use 'environment()$default' instead of 'default' in case 'default' is the missing argument
    value <- c(value, list(default = environment()$default, type = typ2))
    if (action %in% c("store", "append"))
        value <- c(value, list(choices = choices))
    if (!is.null(hel2))
        value <- c(value, list(help = hel2))
    value <- c(value, list(metavariable = metavariable, destination = destination,
        exit = exi2, wrap.help = wrap.help, wrap.exit = wrap.exit))
    if (!is.null(hel2))
        value$help <- help.sub2(value$help)
    value$exit <- help.sub2(value$exit)
    if (inherits(.self, "ArgumentGroup"))
        value$group <- c(.self$parent.IDs, list(.self$ID))
    else value$group <- list()
    class(value) <- "formalCommandArg"


    if (length(tags)) {
        otags <- lapply(.self$formal.command.args$value, "[[", "tags")
        ids <- rep(seq_along(otags), lengths(otags))
        i <- unlist(lapply(lengths(otags), "seq_len"))
        otags <- unlist(otags)
        N <- match(tags, otags, nomatch = 0L)
        if (any(N)) {
            if (is.na(overwrite))
                warning(sprintf(ngettext(sum(N > 0L), "overwriting definition of positional argument %s",
                    "overwriting definitions of positional arguments %s"), paste0("'", tags[N > 0L], "'", collapse = ", ")))
            else if (!overwrite)
                stop(sprintf(ngettext(sum(N > 0L), "invalid positional argument %s, already in use",
                    "invalid positional arguments %s, already in use"), paste0("'", tags[N > 0L], "'", collapse = ", ")))
            N <- split(i[N], ids[N])
            i <- as.integer(names(N))
            for (j in seq_along(i)) {
                .self$formal.command.args$value[[i[[j]]]][["tags"]] <-
                    .self$formal.command.args$value[[i[[j]]]][["tags"]][-N[[j]]]
            }
        }
    }
    if (length(shorts)) {
        otags <- lapply(.self$formal.command.args$value, "[[", "short.flags")
        ids <- rep(seq_along(otags), lengths(otags))
        i <- unlist(lapply(lengths(otags), "seq_len"))
        otags <- unlist(otags)
        N <- match(shorts, otags, nomatch = 0L)
        if (any(N)) {
            if (is.na(overwrite))
                warning(sprintf(ngettext(sum(N > 0L), "overwriting definition of short flag %s",
                    "overwriting definitions of short flags %s"), paste0("'-", shorts[N > 0L], "'", collapse = ", ")))
            else if (!overwrite)
                stop(sprintf(ngettext(sum(N > 0L), "invalid short flag %s, already in use",
                    "invalid short flags %s, already in use"), paste0("'-", shorts[N > 0L], "'", collapse = ", ")))
            N <- split(i[N], ids[N])
            i <- as.integer(names(N))
            for (j in seq_along(i)) {
                .self$formal.command.args$value[[i[[j]]]][["short.flags"]] <-
                    .self$formal.command.args$value[[i[[j]]]][["short.flags"]][-N[[j]]]
            }
        }
    }
    if (length(longs)) {
        otags <- lapply(.self$formal.command.args$value, "[[", "long.flags")
        ids <- rep(seq_along(otags), lengths(otags))
        i <- unlist(lapply(lengths(otags), "seq_len"))
        otags <- unlist(otags)
        N <- match(longs, otags, nomatch = 0L)
        if (any(N)) {
            if (is.na(overwrite))
                warning(sprintf(ngettext(sum(N > 0L), "overwriting definition of long flag %s",
                    "overwriting definitions of long flags %s"), paste0("'--", longs[N > 0L], "'", collapse = ", ")))
            else if (!overwrite)
                stop(sprintf(ngettext(sum(N > 0L), "invalid long flag %s, already in use",
                    "invalid long flags %s, already in use"), paste0("'--", longs[N > 0L], "'", collapse = ", ")))
            N <- split(i[N], ids[N])
            i <- as.integer(names(N))
            for (j in seq_along(i)) {
                .self$formal.command.args$value[[i[[j]]]][["long.flags"]] <-
                    .self$formal.command.args$value[[i[[j]]]][["long.flags"]][-N[[j]]]
            }
        }
    }
    i <- lengths(lapply(.self$formal.command.args$value, "[[", "tags")) |
        lengths(lapply(.self$formal.command.args$value, "[[", "short.flags")) |
        lengths(lapply(.self$formal.command.args$value, "[[", "long.flags"))
    if (!all(i))
        .self$formal.command.args$value <- .self$formal.command.args$value[i]


    .self$formal.command.args$value <- c(.self$formal.command.args$value, list(value))
    invisible()
},


add.argument.group = function (title = NA, description = NA, wrap = TRUE, required = FALSE,
    mutually.exclusive = FALSE, recursive = FALSE, required.recursive = recursive,
    mutually.exclusive.recursive = recursive, type = NA)
{
    titl2 <- as.character(title)[1L]


    require2 <- as.logical(required)[1L]
    if (is.na(require2))
        require2 <- TRUE
    required.recursiv2 <- as.logical(required.recursive)[1L]
    if (is.na(required.recursiv2))
        required.recursiv2 <- TRUE


    mutually.exclusiv2 <- as.logical(mutually.exclusive)[1L]
    if (is.na(mutually.exclusiv2))
        mutually.exclusiv2 <- TRUE
    mutually.exclusive.recursiv2 <- as.logical(mutually.exclusive.recursive)[1L]
    if (is.na(mutually.exclusive.recursiv2))
        mutually.exclusive.recursiv2 <- TRUE


    types <- c("separate", "together", "none")
    if (is.character(type))
        typ2 <- types[pmatch(type, types)][1L]
    else typ2 <- types[type][1L]
    if (is.na(typ2))
        typ2 <- types[[1L]]


    I2 <- Refinteger(value = max(0L, vapply(.self$argument.groups, function(xx) {
        xx[["ID"]][["value"]]
    }, FUN.VALUE = 0L)) + 1L)
    value <- new("ArgumentGroup", program = .self$program, title = titl2,
        ID = I2, required = require2, required.recursive = required.recursiv2,
        mutually.exclusive = mutually.exclusiv2, mutually.exclusive.recursive = mutually.exclusive.recursiv2,
        type = typ2, description = as.description(description, wrap = wrap),
        formal.command.args = .self$formal.command.args)
    if (inherits(.self, "ArgumentGroup")) {
        value$parent.titles <- c(.self$parent.titles, .self$title)
        value$parent.IDs <- c(.self$parent.IDs, list(.self$ID))
    }
    x <- list(value)
    names(x) <- titl2
    .self$argument.groups <- c(.self$argument.groups, x)
    value
},


add.mutually.exclusive.group = function (...)
.self$add.argument.group(..., mutually.exclusive = TRUE),


add.help = function (names.or.flags = c("-h", "--help"), action = "help",
    help = default.help("help"), wrap = FALSE, ...)
.self$add.argument(names.or.flags, action = "help", help = help,
    wrap = wrap, ...),


add.skip = function (names.or.flags = "--args", action = "skip",
    help = default.help("skip"), wrap = FALSE, ...)
.self$add.argument(names.or.flags, action = "skip", help = help,
    wrap = wrap, ...),


add.version = function (names.or.flags = "--version", action = "exit",
    help = default.help("version"), wrap = FALSE, exit, ...)
.self$add.argument(names.or.flags, action = "exit", help = help,
    wrap = wrap, exit = exit, ...)


)


ArgumentParser$methods(sharedMethods)
ArgumentGroup$methods(sharedMethods)


terminhate <- function (..., save = "default", status = 0, runLast = TRUE, do_warning = TRUE)
{
    if (interactive())
        stop(errorCondition(...))
    else {
        if (do_warning) {
            oopt <- options(warn = 1L)
            on.exit(options(oopt))
            warning(warningCondition(...))
        }
        quit(save = save, status = status, runLast = runLast)
    }
}


ArgumentParser$methods(


add.parser = function (...)
.self$subparsers$add.parser(...),


print.help = function (message = "help requested", ..., do_terminhate = FALSE, style = NULL)
{
    if (length(.self$help.message))
        value <- .self$help.message
    else {
        value <- makeHelpMessage(.self, style = style)
        value <- value[nzchar(value)]
        value <- paste(value, collapse = "\n\n")
    }
    cat(value, "\n", sep = "")
    if (do_terminhate)
        terminhate(message = message, ...)
    else invisible(value)
},


add.subparsers = function (title = NA, description = NA, program = NA, required = FALSE, wrap = TRUE,
    indent = 0, exdent = 0)
{
    titl2 <- as.character(title)[1L]
    if (is.na(titl2))
        titl2 <- "Commands:"

    descriptio2 <- as.description(description, wrap = wrap,
        indent = indent, exdent = exdent)


    progra2 <- as.character(program)[1L]
    if (is.na(progra2))
        progra2 <- .self$program


    require2 <- as.logical(required)[1L]
    if (is.na(require2))
        require2 <- TRUE


    .self$subparsers <- new("subparsers", title = titl2, description = descriptio2,
        program = progra2, required = require2, style = .self$style,
        parent.commands = .self$parent.commands, commands = .self$commands)
    invisible(.self$subparsers)
},


parse.args = function (args = Args(), warnPartialMatchArgs = getOption("warnPartialMatchArgs", FALSE))
{
    if (!missing(args)) args <- asArgs(args)
    add.arg <- quote(x[[i]]$value <- c(x[[i]]$value, if (!is.null(val) && j == len) {  # hasValue(arg) && last.flag
        switch(x[[i]]$action, store_const = {
            if (!is.na(val <- as.logical(val)))
                val
            else FALSE
        }, store_true = {
            if (!is.na(val <- as.logical(val)))
                val
            else x[[i]]$default
        }, store_false = {
            if (!is.na(val <- as.logical(val)))
                !val
            else x[[i]]$default
        }, count = , help = , exit = , skip = {
            stop(gettextf("option '%s' does not accept an argument",
                arg))
        }, val)
    }
    else {                                                                          # !hasValue(arg) || !last.flag
        switch(x[[i]]$action, store_const = {
            TRUE
        }, store_true = {
            TRUE
        }, store_false = {
            FALSE
        }, count = {
            1L
        }, help = {
            print.help2(do_warning = FALSE)
        }, exit = {
            print.exit(exit = x[[i]]$exit, wrap = x[[i]]$wrap.exit,
                metavariable = x[[i]]$metavariable)
        }, skip = {
            do_break <- TRUE
        }, {
            if (j != len)  # all other actions require an argument
                stop(gettextf("only the last flag of '%s' may accept an argument",
                    arg))
            n <- n + 1L
            repeat {
                if (n > N)
                    stop(gettextf("option '%s' requires an argument",
                        arg))
                else if (startsWith(args[[n]], "@")) {
                    args <- from.file.substitute(args, n)
                    N <- length(args)
                }
                else break
            }
            if (args[[n]] == "-") {
                n <- n + 1L
                if (n > N)
                    stop(gettextf("invalid arguments, trailing hyphen"))
            }
            args[[n]]
        })
    }))
    init.context <- quote({
        subs <- lapply(context$subparsers$value, function(xx) {
            xx[["commands"]][["value"]]
        })
        sub.ids <- rep(seq_along(subs), lengths(subs))
        subs <- unlist(subs)


        x <- context$formal.command.args$value


        shorts <- lapply(x, "[[", "short.flags")
        short.ids <- rep(seq_along(shorts), lengths(shorts))
        shorts <- unlist(shorts)
        byte.ids <- nchar(shorts, type = "bytes") == 1L
        byte.flags <- shorts[byte.ids]
        byte.ids <- short.ids[byte.ids]


        longs <- lapply(x, "[[", "long.flags")
        long.ids <- rep(seq_along(longs), lengths(longs))
        longs <- unlist(longs)


        pos <- which(lengths(lapply(x, "[[", "tags")) > 0L)
        num.pos <- length(pos)
        cur.pos <- 1L
    })
    finalize.context <- quote({
        x <- check.args()
        check.groups()
        allArgs <- c(x, allArgs)
    })
    print.help2 <- function(..., save = "no", status = 10, runLast = FALSE) {
        if (length(.self$commands$value)) {
            temp1 <- context$parent.commands
            temp2 <- context$commands$value
            on.exit({
                context$parent.commands <- temp1
                context$commands$value  <- temp2
            })
            len <- length(.self$parent.commands) + 1L
            cmnd <- c(context$parent.commands, list(context$commands))
            cmnd <- cmnd[setdiff(
                seq_along(cmnd), seq_len(len)
            )]
            context$parent.commands <- cmnd[-length(cmnd)]
            context$commands$value  <- as.character(cmnd[length(cmnd)][1L][[1L]])
        }
        context$print.help(..., call = this.call, do_terminhate = TRUE)
    }
    print.exit <- function(exit, wrap, metavariable) {
        if (wrap)
            exit <- paste(strwrap(exit, width = getOption("width") - 1),
                collapse = "\n")
        cat(exit, "\n", sep = "")
        terminhate(message = paste(metavariable, "requested"),
            call = this.call, do_warning = FALSE)
    }
    check.groups <- function() {
        provided <- lengths(lapply(x, "[[", "value")) > 0L
        getIDs <- function(y) {
            paste(vapply(y, "[[", "value", FUN.VALUE = 0L), collapse = "/")
        }
        names(provided) <- vapply(x, function(xx) getIDs(xx[["group"]]), "")
        fun <- function(group) {
            ID <- getIDs(c(group$parent.IDs, list(group$ID)))
            sub.IDs <- paste(ID, vapply(group$argument.groups, function(xx) {
                xx[["ID"]][["value"]]
            }, FUN.VALUE = 0L), sep = "/")


            provided2 <- c(list(provided[names(provided) == ID]), lapply(sub.IDs, function(sub.ID) provided[startsWith(names(provided), sub.ID)]))
            names(provided2) <- c(ID, sub.IDs)


            num.formals <- lengths(provided2)
            num.provided <- vapply(provided2, "sum", FUN.VALUE = 0)


            title2 <- if (!is.na(group$title))
                encodeString(group$title, quote = "\"")
            else paste0("with ID ", ID)


            if (group$required) {
                if (!group$required.recursive) {
                    if (!num.formals[[1L]])
                        stop(gettextf("required argument group %s with no formal arguments",
                            title2))
                    else if (!num.provided[[1L]])
                        stop(gettextf("argument group %s requires at least 1 argument",
                            title2))
                }
                else {
                    if (!sum(num.formals))
                        stop(gettextf("required argument group %s with no formal arguments or sub-arguments",
                            title2))
                    else if (!sum(num.provided))
                        stop(gettextf("argument group %s requires at least 1 argument or sub-argument",
                            title2))
                }
            }
            if (group$mutually.exclusive) {
                if (!group$mutually.exclusive.recursive) {
                    if (num.provided[[1L]] > 1L)
                        stop(gettextf("argument group %s requires at most 1 argument",
                            title2))
                }
                else {
                    if (sum(num.provided) > 1L)
                        stop(gettextf("argument group %s requires at most 1 argument or sub-argument",
                            title2))
                }
            }
            lapply(group$argument.groups, "fun")
            invisible()
        }
        lapply(context$argument.groups, "fun")
        invisible()
    }
    check.args <- function() {
        for (n in seq_along(x)) {


            name <- if (length(x[[n]]$tags))
                x[[n]]$tags[[1L]]
            else if (length(x[[n]]$long.flags))
                paste0("--", x[[n]]$long.flags[[1L]])
            else paste0("-", x[[n]]$short.flags[[1L]])
            if (length(x[[n]]$nargs) == 1L && length(x[[n]]$value) != x[[n]]$nargs ||
                length(x[[n]]$nargs) == 2L && (
                    length(x[[n]]$value) < x[[n]]$nargs[1L] ||
                    length(x[[n]]$value) > x[[n]]$nargs[2L]
                ))
                stop(gettextf("expected %s for '%s', found %.0f",
                    nargs.description(x[[n]]$nargs, singular = "argument"),
                    name, length(x[[n]]$value)))


            if (!is.null(x[[n]]$value)) {
                switch(x[[n]]$action, count = {
                    x[[n]]$value <- sum(x[[n]]$value)
                }, append = {
                }, x[[n]]$value <- x[[n]]$value[length(x[[n]]$value)])
                switch(x[[n]]$type, logical = , integer = , numeric = , complex = {
                    x[[n]]$value[grepl("^\\s*(NA|<NA>|NA_integer_|NA_real_|NA_complex_|NA_character_)\\s*$", x[[n]]$value)] <- NA
                    x[[n]]$value <- as.vector(x[[n]]$value, x[[n]]$type)
                }, expression = {
                    x[[n]]$value <- str2expression(x[[n]]$value)
                }, x[[n]]$value <- as.vector(x[[n]]$value, x[[n]]$type))
                if (x[[n]]$action == "store_const") {
                    x[[n]]$value <- if (x[[n]]$value)
                        x[[n]]$constant
                    else x[[n]]$default
                }
                else if (!is.null(x[[n]]$choices)) {
                    switch(x[[n]]$type, character = {
                        k <- as.character(x[[n]]$choices)
                        i <- pmatch(x[[n]]$value, k, nomatch = 0L, duplicates.ok = TRUE)
                        if (!all(i > 0L))
                            stop(gettextf("'%s' should be one of %s", name, paste(dQuote(k),
                                collapse = ", ")))
                        x[[n]]$value <- k[i]
                    }, {
                        i <- x[[n]]$value %in% x[[n]]$choices
                        if (!all(i))
                            stop(gettextf("'%s' should be one of %s", name, paste(dQuote(x[[n]]$choices),
                                collapse = ", ")))
                    })
                }
            }
        }
        x
    }
    try.both <- !is.null(owd <- getwd()) && !is.null(alternate <- this.dir2(verbose = FALSE))
    from.file.substitute <- function(args, n) {
        N <- length(args)
        FILE <- args[[n]]
        FILE <- substr(FILE, 2L, 1000000L)  # remove the leading "@"
        if (grepl("^(ftp|http|https|file)://", FILE)) {
            FILE <- file(FILE, "r")
            on.exit(close(FILE))
        }
        else if (try.both) {
            FILE <- tryCatch({
                normalizePath(FILE, mustWork = TRUE)
            }, error = function(c) {
                on.exit(setwd(owd))
                setwd(alternate)
                normalizePath(FILE, mustWork = TRUE)
            })
        }
        else FILE <- normalizePath(FILE, mustWork = TRUE)
        x <- scan(file = FILE, what = "", sep = ",", quote = "\"",
            na.strings = NULL, quiet = TRUE, comment.char = "#",
            allowEscapes = TRUE, encoding = "UTF-8")
        c(args[seq_len(n - 1L)], x, args[seq.int(to = N, length.out = N - n)])
    }


    cmds <- character()
    oargs <- args
    this.call <- sys.call(sys.nframe())
    allArgs <- list()
    context <- .self
    eval(init.context)
    N <- length(args)
    n <- 0L
    while (n < N) {
        arg <- args[[n <- n + 1L]]
        if (isFlag(arg)) {
            tag <- getTag(arg)
            val <- if (hasValue(arg))
                getValue(arg)
            j <- len <- 1L
            do_break <- FALSE
            if (isLongFlag(arg)) {
                k <- charmatch(tag, longs)
                if (is.na(k))
                    stop(gettextf("unused argument '%s'", arg))
                if (k < 1L)
                    stop(gettextf("argument '--%s' matches multiple formal arguments",
                        tag))
                if (warnPartialMatchArgs && tag != longs[[k]])
                    warning(gettextf("partial argument match of '--%s' to '--%s'",
                        tag, longs[[k]]))
                i <- long.ids[[k]]
                eval(add.arg)
            }
            else {
                k <- match(tag, shorts)
                if (is.na(k)) {  # multiple single-byte flags


                    # tags <- strsplit(tag, NULL, useBytes = TRUE)[[1L]]
                    # K <- match(tags, byte.flags)
                    # I <- byte.ids[K]
                    I <- byte.ids[match(strsplit(tag, NULL, useBytes = TRUE)[[1L]], byte.flags)]


                    len <- length(I)
                    for (j in seq_along(I)) {
                        i <- I[[j]]
                        if (is.na(i))
                            stop(gettextf("unused argument '%s'", arg))
                        eval(add.arg)
                    }
                }
                else {           # one short flag
                    i <- short.ids[[k]]
                    eval(add.arg)
                }
            }
            if (do_break)
                break
        }
        else if (k <- match(arg, subs, nomatch = 0L)) {
            cmds <- c(cmds, context$subparsers$value[[sub.ids[[k]]]][["commands"]][["value"]][[1L]])
            eval(finalize.context)
            context <- context$subparsers$value[[sub.ids[[k]]]]
            eval(init.context)
        }
        else if (startsWith(arg, "@")) {
            args <- from.file.substitute(args, n)
            N <- length(args)
            n <- n - 1L
        }
        else if (cur.pos <= num.pos) {
            if (arg == "-") {
                n <- n + 1L
                if (n > N)
                    stop(gettextf("invalid arguments, trailing hyphen"))
                val <- args[[n]]
            }
            else if (arg == "--") {
                cur.pos <- cur.pos + 1L
                next
            }
            else val <- arg
            j <- len <- 1L
            do_break <- FALSE
            i <- pos[[cur.pos]]
            eval(add.arg)
            # x[[k]]$value <- c(x[[k]]$value, switch(x[[k]]$action, store_const = {
            #     if (!is.na(val <- as.logical(val)))
            #       val
            #     else FALSE
            # }, store_true = {
            #     if (!is.na(val <- as.logical(val)))
            #       val
            #     else x[[k]]$default
            # }, store_false = {
            #     if (!is.na(val <- as.logical(val)))
            #       !val
            #     else x[[k]]$default
            # }, count = {
            #     stop(gettextf("option '%s' does not accept an argument", oarg))
            # }, help = {
            #     print.help2(status = 0)
            # }, exit = {
            #     print.exit(exit = x[[k]]$exit, wrap = x[[k]]$wrap.exit,
            #       metavariable = x[[k]]$metavariable)
            # }, skip = {
            #    break
            # }, val))
            if (length(x[[i]]$value) == max(x[[i]]$nargs))
                cur.pos <- cur.pos + 1
        }
        else stop(gettextf("unused argument [%.0f]", n))
    }
    eval(finalize.context)
    if (context$subparsers$required)
        stop(gettextf("a sub-command is required"))
    trailing <- args[-seq_len(n)]
    x <- allArgs
    destinations <- vapply(x, "[[", "destination", FUN.VALUE = "")
    x <- x[!is.na(destinations)]
    destinations <- destinations[!is.na(destinations)]
    nm <- unique(destinations)
    value <- rep(list(quote(expr = )), length(nm))
    names(value) <- nm
    for (dest in rev(nm)) {
        vals <- x[destinations == dest]
        defs <- lapply(vals, "[[", "default")
        vals <- lapply(vals, "[[", "value")
        vals <- vals[!vapply(vals, "is.null", FUN.VALUE = NA)]
        vals <- c(vals, defs)
        for (n in seq_along(vals)) {
            if (!identical(vals[[n]], quote(expr = ))) {
                value[dest] <- list(vals[[n]])
                break
            }
        }
    }
    y <- as.environment(value)
    attr(y, "cmds") <- list(
        original = cmds,
        string   = paste(cmds, collapse = "/")
    )
    attr(y, "args") <- list(
        original     = oargs,
        all          = args,
        trailingOnly = trailing
    )
    class(y) <- c("ParsedArgs", "environment")
    y
},


show = function ()
{
    cat("An argument", if (length(command2 <- commands(.self)))
        sprintf("subparser (deriving from %s)\n",
            paste(command2, collapse = " "))
    else "parser\n")
    if (length(.self$description) && nzchar(.self$description$x)) {
        cat("with description:\n")
        print(.self$description$x)
    }
    else cat("with no description\n")
    if (length(.self$formal.command.args$value)) {
        cat("with formal arguments:\n")
        print(.self$formal.command.args$value)
    }
    else cat("with no formal arguments\n")
    if (length(.self$subparsers$value)) {
        tags <- vapply(.self$subparsers$value, function(xx) {
            paste(xx[["commands"]][["value"]], collapse = ", ")
        }, "")
        helps <- lapply(.self$subparsers$value, function(x) x$help)
        wraps <- vapply(helps, "[[", "wrap", FUN.VALUE = NA)
        helps <- vapply(helps, "[[", "x", FUN.VALUE = "")


        tags <- sprintf("  %s  ", format(tags, justify = "left"))
        indent <- max(nchar(tags, type = "width"))
        helps[wraps] <- vapply(strwrap(helps[wraps], width = getOption("width") - indent - 1, exdent = 2, simplify = FALSE), "paste", collapse = "\n", FUN.VALUE = "")
        helps <- gsub("\n", paste0("\n", strrep(" ", indent)), helps)


        cat("with sub-commands:\n")
        for (n in seq_along(tags)) cat(tags[[n]], helps[[n]], "\n", sep = "")
    }
    else cat("with no sub-commands\n")
    if (length(.self$epilogue) && nzchar(.self$epilogue$x)) {
        cat("with epilogue:\n")
        print(.self$epilogue$x)
    }
    else cat("with no epilogue\n")
    invisible()
}


)


`$.ParsedArgs` <- function (x, name)
get(x = name, envir = x, inherits = FALSE)


`$<-.ParsedArgs` <- function (x, name, value)
{
    if (!exists(x = name, envir = x, inherits = FALSE))
        get(x = name, envir = x, inherits = FALSE)
    assign(x = name, value = value, envir = x, inherits = FALSE)
    x
}


as.list.ParsedArgs <- function (x, all.names = TRUE, sorted = FALSE, ...)
as.list.environment(x = x, all.names = TRUE, sorted = sorted, ...)


print.ParsedArgs <- function (x, ...)
{
    xx <- as.list(x)
    keepAttrs <- setdiff(names(attributes(x)), c("class", "cmds", "args"))
    attributes(xx)[keepAttrs] <- attributes(x)[keepAttrs]
    cat("Object of class \"ParsedArgs\"\n")
    print(xx, ...)
    invisible(x)
}


Commands <- function (x, type = c("original", "string"))
attr(x, "cmds")[[match.arg(type)]]


Missing <- function (x, name)
{
    if (!inherits(x, "ParsedArgs"))
        stop("wrong class")
    name <- substitute(name)
    name <- switch (t <- typeof(name), symbol = {
        as.character(name)
    }, character = {
        .subset2(name, 1L)
    }, stop(gettextf("invalid subscript type '%s'", t)))
    if (!exists(x = name, envir = x, inherits = FALSE))
        get(x = name, envir = x, inherits = FALSE)
    identical(.subset2(x, name), quote(expr = ))
}


# print.subparsers <- function (x, ...)
# {
#
# }


print.this.path_ArgumentParser <- function (x, ...)
{
    x$show()
    invisible(x)
}


ArgumentParser <- function (program = NA, usage = NA, description = NA, epilogue = NA,
    add.help = TRUE, wrap = TRUE, indent = 0, exdent = 0, wrap.description = wrap,
    indent.description = indent, exdent.description = exdent,
    wrap.epilogue = wrap, indent.epilogue = indent, exdent.epilogue = exdent,
    style = NA, wrap.help = FALSE, help.help = default.help("help"), ...,
    help.message = NULL)
{
    .program <- as.character(program)[1L]
    if (is.na(.program))
        .program <- tryCatch(basename(this.path(verbose = FALSE)),
            error = function(c) "/path/to/script")
    .usage <- format.help(usage)
    .description <- as.description(description, wrap = wrap.description,
        indent = indent.description, exdent = exdent.description)
    .epilogue <- as.description(epilogue, wrap = wrap.epilogue,
        indent = indent.epilogue, exdent = exdent.epilogue)
    .style <- as.integer(style)[1L]
    if (!.style %in% 1:2)
        .style <- 1L
    if (is.null(help.message))
        .help.message <- character()
    else .help.message <- format.help(help.message)
    value <- new("this.path_ArgumentParser", program = .program, usage = .usage,
        description = .description, epilogue = .epilogue, style = .style,
        help.message = .help.message)
    if (add.help)
        value$add.help(wrap = wrap.help, help = help.help, ...)
    value$add.subparsers()
    return(value)
}


remove(sharedMethods)
