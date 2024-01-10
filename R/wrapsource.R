wrap.source <- function (expr, path.only = FALSE, character.only = path.only,
    file.only = path.only, conv2utf8 = FALSE, allow.blank.string = FALSE,
    allow.clipboard = !file.only, allow.stdin = !file.only, allow.url = !file.only,
    allow.file.uri = !path.only, allow.unz = !path.only, allow.pipe = !file.only,
    allow.terminal = !file.only, allow.textConnection = !file.only,
    allow.rawConnection = !file.only, allow.sockconn = !file.only,
    allow.servsockconn = !file.only, allow.customConnection = !file.only,
    ignore.all = FALSE, ignore.blank.string = ignore.all, ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all, ignore.url = ignore.all, ignore.file.uri = ignore.all)
.External2(.C_wrap_source, character.only, conv2utf8, allow.blank.string,
    allow.clipboard, allow.stdin, allow.url, allow.file.uri,
    allow.unz, allow.pipe, allow.terminal, allow.textConnection,
    allow.rawConnection, allow.sockconn, allow.servsockconn,
    allow.customConnection, ignore.blank.string, ignore.clipboard,
    ignore.stdin, ignore.url, ignore.file.uri)


set.sys.path <- function (file, path.only = FALSE, character.only = path.only,
    file.only = path.only, conv2utf8 = FALSE, allow.blank.string = FALSE,
    allow.clipboard = !file.only, allow.stdin = !file.only, allow.url = !file.only,
    allow.file.uri = !path.only, allow.unz = !path.only, allow.pipe = !file.only,
    allow.terminal = !file.only, allow.textConnection = !file.only,
    allow.rawConnection = !file.only, allow.sockconn = !file.only,
    allow.servsockconn = !file.only, allow.customConnection = !file.only,
    ignore.all = FALSE, ignore.blank.string = ignore.all, ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all, ignore.url = ignore.all, ignore.file.uri = ignore.all,
    Function = NULL, ofile)
.External2(.C_set_sys_path, character.only, conv2utf8, allow.blank.string,
    allow.clipboard, allow.stdin, allow.url, allow.file.uri,
    allow.unz, allow.pipe, allow.terminal, allow.textConnection,
    allow.rawConnection, allow.sockconn, allow.servsockconn,
    allow.customConnection, ignore.blank.string, ignore.clipboard,
    ignore.stdin, ignore.url, ignore.file.uri, Function)


local({
    f1 <- formals(wrap.source)
    f2 <- formals(set.sys.path)
    stopifnot(
        length(f1) + 2L == length(f2),
        identical(f1[-1L], f2[2L:(length(f2) - 2L)]),
        TRUE
    )
    b1 <- body(wrap.source)
    b2 <- body(set.sys.path)
    stopifnot(
        length(b1) + 1L == length(b2),
        identical(b1[-2L], b2[-c(2L, length(b2))])
    )
})


unset.sys.path <- function ()
.External2(.C_unset_sys_path)


set.env.path <- function (envir, matchThisEnv = getOption("topLevelEnvironment"))
.External2(.C_set_env_path, envir, matchThisEnv)


set.src.path <- function (srcfile)
.External2(.C_set_src_path, srcfile)


set.sys.path.function <- function (fun)
.External2(.C_set_sys_path_function, fun)


with_sys.path <- function (file, expr, ...)
{
    if ((N <- sys.parent()) && typeof(sys.function(N)) == "closure")
        stop("'with_sys.path' cannot be used within a function, use 'set.sys.path' instead")
    set.sys.path(file = file, ..., Function = c("with_sys.path", "this.path"))
    expr
}


with_site.file <- function (expr)
{
    if ((N <- sys.parent()) && typeof(sys.function(N)) == "closure")
        stop("'with_site.file' cannot be used within a function")
    expr
}


with_init.file <- if (getRversion() >= "3.0.0") {
                  function (...)
{
    if ((N <- sys.parent()) && typeof(sys.function(N)) == "closure")
        stop("'with_init.file' cannot be used within a function")
    if (.getframenumber() == 0L)
        set.sys.path(init.file(), ofile = init.file(original = TRUE),
            Function = c("with_init.file", "this.path"))
    envir <- parent.frame()
    n <- ...length()
    ## if the last argument is missing, skip it
    if (n && .External2(.C_is_R_MissingArg, paste0("..", n)))
        n <- n - 1L
    for (i in seq_len(n)) {
        x <- withVisible(...elt(i))
        if (x$visible)
            .PrintValueEnv(x$value, envir)
    }
    invisible()
}
} else {
                  function (...)
{
    if ((N <- sys.parent()) && typeof(sys.function(N)) == "closure")
        stop("'with_init.file' cannot be used within a function")
    if (.getframenumber() == 0L)
        set.sys.path(init.file(), ofile = init.file(original = TRUE),
            Function = c("with_init.file", "this.path"))
    envir <- parent.frame()
    n <- ...length()
    ## if the last argument is missing, skip it
    if (n && .External2(.C_is_R_MissingArg, paste0("..", n)))
        n <- n - 1L
    ## silences NOTE:
    ## no visible binding for global variable 'sym'
    sym <- NULL
    for (i in seq_len(n)) {
        ## ...elt(i) does not propagate visibility in R < 3.0.0
        eval(call("delayedAssign", "sym", as.symbol(paste0("..", i))))
        x <- withVisible(sym)
        if (x$visible)
            .PrintValueEnv(x$value, envir)
    }
    invisible()
}
}


inside.source <- function (file, path.only = FALSE, character.only = path.only,
    file.only = path.only, conv2utf8 = FALSE, allow.blank.string = FALSE,
    allow.clipboard = !file.only, allow.stdin = !file.only, allow.url = !file.only,
    allow.file.uri = !path.only, allow.unz = !path.only, allow.pipe = !file.only,
    allow.terminal = !file.only, allow.textConnection = !file.only,
    allow.rawConnection = !file.only, allow.sockconn = !file.only,
    allow.servsockconn = !file.only, allow.customConnection = !file.only,
    ignore.all = FALSE, ignore.blank.string = ignore.all, ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all, ignore.url = ignore.all, ignore.file.uri = ignore.all,
    Function = NULL, ofile)
stop(.defunctError("set.sys.path", "this.path", old = "inside.source"))


set.this.path <- function (file, path.only = FALSE, character.only = path.only,
    file.only = path.only, conv2utf8 = FALSE, allow.blank.string = FALSE,
    allow.clipboard = !file.only, allow.stdin = !file.only, allow.url = !file.only,
    allow.file.uri = !path.only, allow.unz = !path.only, allow.pipe = !file.only,
    allow.terminal = !file.only, allow.textConnection = !file.only,
    allow.rawConnection = !file.only, allow.sockconn = !file.only,
    allow.servsockconn = !file.only, allow.customConnection = !file.only,
    ignore.all = FALSE, ignore.blank.string = ignore.all, ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all, ignore.url = ignore.all, ignore.file.uri = ignore.all,
    Function = NULL, ofile)
stop(.defunctError("set.sys.path", "this.path", old = "set.this.path"))


unset.this.path <- function ()
stop(.defunctError("unset.sys.path", "this.path", old = "unset.this.path"))
