.windows_basename2 <- function (path)
.External2(.C_windows_basename2, path)


.unix_basename2 <- function (path)
.External2(.C_unix_basename2, path)


basename2 <- function (path)
.External2(.C_basename2, path)


.windows_dirname2 <- function (path)
.External2(.C_windows_dirname2, path)


.unix_dirname2 <- function (path)
.External2(.C_unix_dirname2, path)


dirname2 <- function (path)
.External2(.C_dirname2, path)


.windows_splitext <- function (path, compression = FALSE)
.External2(.C_windows_splitext, path, compression)


.unix_splitext <- function (path, compression = FALSE)
.External2(.C_unix_splitext, path, compression)


splitext <- function (path, compression = FALSE)
.External2(.C_splitext, path, compression)


.windows_removeext <- function (path, compression = FALSE)
.External2(.C_windows_removeext, path, compression)


.unix_removeext <- function (path, compression = FALSE)
.External2(.C_unix_removeext, path, compression)


removeext <- function (path, compression = FALSE)
.External2(.C_removeext, path, compression)


.windows_ext <- function (path, compression = FALSE)
.External2(.C_windows_ext, path, compression)


.unix_ext <- function (path, compression = FALSE)
.External2(.C_unix_ext, path, compression)


ext <- function (path, compression = FALSE)
.External2(.C_ext, path, compression)


`.windows_ext<-` <- function (path, compression = FALSE, value)
.External2(`.C_windows_ext<-`, path, compression, value)


`.unix_ext<-` <- function (path, compression = FALSE, value)
.External2(`.C_unix_ext<-`, path, compression, value)


`ext<-` <- function (path, compression = FALSE, value)
.External2(`.C_ext<-`, path, compression, value)


.windows.path.join <- function (...)
.External2(.C_windows_path_join)


.unix.path.join <- function (...)
.External2(.C_unix_path_join)


path.join <- function (...)
.External2(.C_path_join)


.windows.path.split <- function (path)
.External2(.C_windows_path_split, path)


.unix.path.split <- function (path)
.External2(.C_unix_path_split, path)


path.split <- function (path)
.External2(.C_path_split, path)


.windows.path.split.1 <- function (path)
.External2(.C_windows_path_split_1, path)


.unix.path.split.1 <- function (path)
.External2(.C_unix_path_split_1, path)


path.split.1 <- function (path)
.External2(.C_path_split_1, path)


.windows.path.unsplit <- function (...)
.External2(.C_windows_path_unsplit)


.unix.path.unsplit <- function (...)
.External2(.C_unix_path_unsplit)


path.unsplit <- function (...)
.External2(.C_path_unsplit)
