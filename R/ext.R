windows.splitext <- function (path, compression = FALSE)
.External2(C_windowssplitext, path, compression)


unix.splitext <- function (path, compression = FALSE)
.External2(C_unixsplitext, path, compression)


splitext <- function (path, compression = FALSE)
.External2(C_splitext, path, compression)





windows.removeext <- function (path, compression = FALSE)
.External2(C_windowsremoveext, path, compression)


unix.removeext <- function (path, compression = FALSE)
.External2(C_unixremoveext, path, compression)


removeext <- function (path, compression = FALSE)
.External2(C_removeext, path, compression)





windows.ext <- function (path, compression = FALSE)
.External2(C_windowsext, path, compression)


unix.ext <- function (path, compression = FALSE)
.External2(C_unixext, path, compression)


ext <- function (path, compression = FALSE)
.External2(C_ext, path, compression)





`windows.ext<-` <- function (path, compression = FALSE, value)
.External2(C_windowsextgets, path, compression, value)


`unix.ext<-` <- function (path, compression = FALSE, value)
.External2(C_unixextgets, path, compression, value)


`ext<-` <- function (path, compression = FALSE, value)
.External2(C_extgets, path, compression, value)
