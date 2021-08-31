pattern <- function (x)
sprintf("((^|[^%%])(%%%%)*)%%\\(%s\\)s", x)


replacement <- function (x)
sprintf("\\1%%%d$s", switch(x, PROGRAM = 1, NAME = 2, SHORTFLAG = 3,
    LONGFLAG = 4, ACTION = 5, NARGS = 6, CONSTANT = 7, DEFAULT = 8,
    TYPE = 9, CHOICES = 10, REQUIRED = 11, METAVARIABLE = 12,
    DESTINATION = 13, stop("invalid 'x'; should never happen, please report!")))


cond <- function (x, value)
switch(x, NAME = length(value$name), SHORTFLAG = length(value$short.flag),
    LONGFLAG = length(value$long.flag), CONSTANT = "constant" %in% names(value),
    DEFAULT = !identical(value$default, quote(expr = )),
    CHOICES = "choices" %in% names(value), PROGRAM = , ACTION = ,
    NARGS = , TYPE = , REQUIRED = , METAVARIABLE = , DESTINATION = TRUE,
    stop("invalid 'x'; should never happen, please report!"))


arg <- function (x)
switch(x, NAME = "name", SHORTFLAG = "short flag", LONGFLAG = "long flag",
    ACTION = , NARGS = , CONSTANT = , DEFAULT = , TYPE = , CHOICES = ,
    REQUIRED = , METAVARIABLE = , DESTINATION = sprintf("'%s' argument", tolower(x)),
    PROGRAM = , stop("invalid 'x'; should never happen, please report!"))


help.sub <- function (help, value, which = c("PROGRAM", "NAME", "SHORTFLAG",
    "LONGFLAG", "ACTION", "NARGS", "CONSTANT", "DEFAULT", "TYPE",
    "CHOICES", "REQUIRED", "METAVARIABLE", "DESTINATION"))
{
    for (x in match.arg(which, several.ok = TRUE)) {
        if (cond(x, value))
            help <- gsub(pattern(x), replacement(x), help)
        else if (grepl(pattern(x), help))
            stop(gettextf("invalid 'help', cannot contain \"%%(%s)\" without a %s",
                x, arg(x)))
    }
    return(help)
}
