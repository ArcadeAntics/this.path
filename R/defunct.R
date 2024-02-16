## Defunct in 1.3.0 (2023-04-08)


as.rel.path <- function (path)
stop(.defunctError("rel2here", "@R_PACKAGE_NAME@", old = "as.rel.path"))


as.relative.path <- function (path)
stop(.defunctError("rel2here", "@R_PACKAGE_NAME@", old = "as.relative.path"))


## Defunct in 1.4.0 (2023-04-18)


local.path <- function (verbose = getOption("verbose"), original = FALSE, for.msg = FALSE,
    contents = FALSE, default, else.)
stop(.defunctError("sys.path(..., local = TRUE)", "@R_PACKAGE_NAME@", old = "local.path(...)"))


Sys.path <- function ()
stop(.defunctError("sys.path(verbose = FALSE)", "@R_PACKAGE_NAME@", old = "Sys.path()"))


Sys.dir <- function ()
stop(.defunctError("sys.dir(verbose = FALSE)", "@R_PACKAGE_NAME@", old = "Sys.dir()"))


## Defunct in 2.0.0 (2023-08-08)


reset.this.proj <- function ()
stop(.defunctError("reset.proj", "@R_PACKAGE_NAME@", old = "reset.this.proj"))


set.this.path.jupyter <- function (...)
stop(.defunctError("set.jupyter.path", "@R_PACKAGE_NAME@", old = "set.this.path.jupyter"))


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
stop(.defunctError("set.sys.path", "@R_PACKAGE_NAME@", old = "inside.source"))


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
stop(.defunctError("set.sys.path", "@R_PACKAGE_NAME@", old = "set.this.path"))


unset.this.path <- function ()
stop(.defunctError("unset.sys.path", "@R_PACKAGE_NAME@", old = "unset.this.path"))


## Defunct in 2.4.0 (2024-02-16)


set.sys.path.jupyter <- function (...)
stop(.defunctError("set.jupyter.path", "@R_PACKAGE_NAME@", old = "set.sys.path.jupyter"))


fileArgs <- function ()
stop(.defunctError("progArgs", "@R_PACKAGE_NAME@", old = "fileArgs"))
