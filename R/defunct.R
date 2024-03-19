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
