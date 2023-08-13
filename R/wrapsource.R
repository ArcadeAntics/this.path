wrap.source <- function (expr, path.only = FALSE, character.only = path.only,
    file.only = path.only, conv2utf8 = FALSE, allow.blank.string = FALSE,
    allow.clipboard = !file.only, allow.stdin = !file.only, allow.url = !file.only,
    allow.file.uri = !path.only, allow.unz = !path.only, allow.pipe = !file.only,
    allow.terminal = !file.only, allow.textConnection = !file.only,
    allow.rawConnection = !file.only, allow.sockconn = !file.only,
    allow.servsockconn = !file.only, allow.customConnection = !file.only,
    ignore.all = FALSE, ignore.blank.string = ignore.all, ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all, ignore.url = ignore.all, ignore.file.uri = ignore.all)
.External2(.C_wrapsource, character.only, conv2utf8, allow.blank.string,
    allow.clipboard, allow.stdin, allow.url, allow.file.uri,
    allow.unz, allow.pipe, allow.terminal, allow.textConnection,
    allow.rawConnection, allow.sockconn, allow.servsockconn,
    allow.customConnection, ignore.blank.string, ignore.clipboard,
    ignore.stdin, ignore.url, ignore.file.uri)


set.sys.path <- wrap.source
names(formals(set.sys.path))[1L] <- "file"
formals(set.sys.path) <- c(formals(set.sys.path), alist(Function = NULL, ofile = ))
body(set.sys.path)[2L] <- alist(.C_setsyspath)
body(set.sys.path) <- as.call(c(as.list(body(set.sys.path)), alist(Function)))


unset.sys.path <- function ()
.External2(.C_unsetsyspath)


set.env.path <- function (envir, matchThisEnv = getOption("topLevelEnvironment"))
.External2(.C_setenvpath, envir, matchThisEnv)


set.src.path <- function (srcfile)
.External2(.C_setsrcpath, srcfile)


inside.source <- eval(call("function", as.pairlist(alist(file = , path.only = FALSE, character.only = path.only,
    file.only = path.only, conv2utf8 = FALSE, allow.blank.string = FALSE,
    allow.clipboard = !file.only, allow.stdin = !file.only, allow.url = !file.only,
    allow.file.uri = !path.only, allow.unz = !path.only, allow.pipe = !file.only,
    allow.terminal = !file.only, allow.textConnection = !file.only,
    allow.rawConnection = !file.only, allow.sockconn = !file.only,
    allow.servsockconn = !file.only, allow.customConnection = !file.only,
    ignore.all = FALSE, ignore.blank.string = ignore.all, ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all, ignore.url = ignore.all, ignore.file.uri = ignore.all,
    Function = NULL, ofile = )), bquote(
stop(.defunctError("set.sys.path", .(.pkgname), old = "inside.source"))
    )))


set.this.path <- `body<-`(inside.source, value = bquote(
stop(.defunctError("set.sys.path", .(.pkgname), old = "set.this.path"))
))


unset.this.path <- eval(call("function", NULL, bquote(
stop(.defunctError("unset.sys.path", .(.pkgname), old = "unset.this.path"))
)))
