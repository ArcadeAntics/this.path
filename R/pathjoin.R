.windows.path.join <- function (...)
.External2(.C_windows.path.join)


.unix.path.join <- function (...)
.External2(.C_unix.path.join)


path.join <- function (...)
.External2(.C_path.join)
