.windows.basename2 <- function (path)
.External2(.C_windows.basename2, path)


.unix.basename2 <- function (path)
.External2(.C_unix.basename2, path)


basename2 <- function (path)
.External2(.C_basename2, path)


.windows.dirname2 <- function (path)
.External2(.C_windows.dirname2, path)


.unix.dirname2 <- function (path)
.External2(.C_unix.dirname2, path)


dirname2 <- function (path)
.External2(.C_dirname2, path)


.windows.splitext <- function (path, compression = FALSE)
.External2(.C_windows.splitext, path, compression)


.unix.splitext <- function (path, compression = FALSE)
.External2(.C_unix.splitext, path, compression)


splitext <- function (path, compression = FALSE)
.External2(.C_splitext, path, compression)


.windows.removeext <- function (path, compression = FALSE)
.External2(.C_windows.removeext, path, compression)


.unix.removeext <- function (path, compression = FALSE)
.External2(.C_unix.removeext, path, compression)


removeext <- function (path, compression = FALSE)
.External2(.C_removeext, path, compression)


.windows.ext <- function (path, compression = FALSE)
.External2(.C_windows.ext, path, compression)


.unix.ext <- function (path, compression = FALSE)
.External2(.C_unix.ext, path, compression)


ext <- function (path, compression = FALSE)
.External2(.C_ext, path, compression)


`.windows.ext<-` <- function (path, compression = FALSE, value)
.External2(`.C_windows.ext<-`, path, compression, value)


`.unix.ext<-` <- function (path, compression = FALSE, value)
.External2(`.C_unix.ext<-`, path, compression, value)


`ext<-` <- function (path, compression = FALSE, value)
.External2(`.C_ext<-`, path, compression, value)


.windows.path.join <- function (...)
.External2(.C_windows.path.join)


.unix.path.join <- function (...)
.External2(.C_unix.path.join)


path.join <- function (...)
.External2(.C_path.join)


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
