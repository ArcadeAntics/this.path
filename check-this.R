essentials:::check_this(  # this.path
    build = TRUE,

    check = TRUE, as.cran = TRUE,

    chdir = TRUE
)




# enumerate <- function (x, from = 1L)
# list(seq.int(from, along.with = x), x)
#
#
# mfor(i, xx, enumerate(letters), {
#     print(i)
#     print(xx)
# })





# add.attributes <- function (.Data, args = NULL)
# {
#     if (is.null(args))
#         return(.Data)
#     else if (!is.list(args))
#         stop("second argument must be a list")
#     if (length(args))
#         do.call(structure, c(list(.Data = .Data), args), quote = TRUE)
#     else .Data
# }
#
#
# scipy <- NULL
# fun <- function ()
# {
#     scipy <<- reticulate::import("scipy", delay_load = TRUE)
# }
# fun()





# x <- list(FALSE, 0L, 0, 0i, "", as.symbol("a"), quote(0L + 0L), ~FALSE)
# do.call(rbind, c(x[-6], list(deparse.level = 2)), quote = TRUE)
# (y <- do.call(rbind, c(lapply(x[-6], quoteExpression), list(deparse.level = 2))))
# methods::setClass("S4name", contains = "name")
# x <- methods::new("S4name", quote(a))





.regexps <- list()
.regexps$hexadecimal <- paste0(
    "([-+])?",
    "0[xX]",
    "(",
            "[[:xdigit:]]+(\\.[[:xdigit:]]*)?",
        "|",
            "\\.[[:xdigit:]]+",
    ")",
    "([Pp]([-+]?[[:digit:]]+))?"
)
.regexps$decimal <- paste0(
    "(",
            "[[:digit:]]+(\\.[[:digit:]]*)?",
        "|",
            "\\.[[:digit:]]+",
    ")",
    "([Ee]([-+]?[[:digit:]]+))?"
)
.regexps$numeric <- paste0(
    "(",
            .regexps$hexadecimal,
        "|",
            .regexps$decimal,
    ")"
)


num.choices <- list(
    sign  = c("", "-", "+"),
    start = c("0x", "0X"),
    num   = c("9AB", "9AB.", "9.AB", ".9AB")
)
exp.choices <- list(
    start = c("P", "p"),
    sign  = c("", "-", "+"),
    num   = c("123")
)
combinations <- function (x, lex.order = FALSE)
{
    lens <- lengths(x)
    length.out <- prod(lens)
    if (length.out <= 0L)
        return(list())
    each <- if (lex.order)
        rev(cumprod(c(1L, rev(lens)[-length(lens)])))
    else    cumprod(c(1L,      lens[-length(lens)]))
    essentials::plapply(list(
        x = x,
        each = each
    ), base::rep, length.out = length.out)
}
x <- combinations(num.choices)
y <- combinations(num.choices, lex.order = TRUE)
essentials::psapply(x, paste0, USE.NAMES = FALSE)
essentials::psapply(y, paste0, USE.NAMES = FALSE)


num.choices <- essentials::pvapply(combinations(num.choices), paste0, FUN.VALUE = "", USE.NAMES = FALSE)
exp.choices <- essentials::pvapply(combinations(exp.choices), paste0, FUN.VALUE = "", USE.NAMES = FALSE)


choices <- list(
    num.choices = num.choices,
    exp.choices = c("", exp.choices)  # the exponent is optional
)
choices <- essentials::pvapply(combinations(choices), paste0, FUN.VALUE = "", USE.NAMES = FALSE)


all(grepl(paste0("^(", .regexps$hexadecimal, ")$"), choices))
