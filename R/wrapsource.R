wrap.source <- function (expr, path.only = FALSE, character.only = path.only,
    file.only = path.only, conv2utf8 = FALSE, allow.blank.string = FALSE,
    allow.clipboard = !file.only, allow.stdin = !file.only, allow.url = !file.only,
    allow.file.uri = !path.only, allow.unz = !path.only, allow.pipe = !file.only,
    allow.terminal = !file.only, allow.textConnection = !file.only,
    allow.rawConnection = !file.only, allow.sockconn = !file.only,
    allow.servsockconn = !file.only, allow.customConnection = !file.only,
    ignore.all = FALSE, ignore.blank.string = ignore.all, ignore.clipboard = ignore.all,
    ignore.stdin = ignore.all, ignore.url = ignore.all, ignore.file.uri = ignore.all)
.External2(C_wrapsource, character.only, conv2utf8, allow.blank.string,
    allow.clipboard, allow.stdin, allow.url, allow.file.uri,
    allow.unz, allow.pipe, allow.terminal, allow.textConnection,
    allow.rawConnection, allow.sockconn, allow.servsockconn,
    allow.customConnection, ignore.blank.string, ignore.clipboard,
    ignore.stdin, ignore.url, ignore.file.uri)


inside.source <- wrap.source
names(formals(inside.source))[[1L]] <- "file"
formals(inside.source) <- c(formals(inside.source), alist(Function = NULL))
body(inside.source)[2L] <- alist(C_insidesource)
body(inside.source) <- as.call(c(as.list(body(inside.source)), alist(Function)))


set.this.path <- inside.source
body(set.this.path)[2L] <- alist(C_setthispath)


unset.this.path <- function ()
.External2(C_unsetthispath)
