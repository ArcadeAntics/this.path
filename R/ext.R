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
