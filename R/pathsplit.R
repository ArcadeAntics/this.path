.windows.path.split <- function (path)
.External2(.C_windows.path.split, path)


.unix.path.split <- function (path)
.External2(.C_unix.path.split, path)


path.split <- function (path)
.External2(.C_path.split, path)


.windows.path.split.1 <- function (path)
.External2(.C_windows.path.split.1, path)


.unix.path.split.1 <- function (path)
.External2(.C_unix.path.split.1, path)


path.split.1 <- function (path)
.External2(.C_path.split.1, path)


.windows.path.unsplit <- function (...)
.External2(.C_windows.path.unsplit)


.unix.path.unsplit <- function (...)
.External2(.C_unix.path.unsplit)


path.unsplit <- function (...)
.External2(.C_path.unsplit)
