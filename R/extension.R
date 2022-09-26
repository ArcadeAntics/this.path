windows.splitext <- function (path, compression = FALSE)
.External2(C_windowssplitext, path, compression)


unix.splitext <- function (path, compression = FALSE)
.External2(C_unixsplitext, path, compression)


splitext <- function (path, compression = FALSE)
.External2(C_splitext, path, compression)





windows.extension <- function (path, compression = FALSE)
.External2(C_windowsextension, path, compression)


unix.extension <- function (path, compression = FALSE)
.External2(C_unixextension, path, compression)


extension <- function (path, compression = FALSE)
.External2(C_extension, path, compression)


ext <- extension





windows.removeext <- function (path, compression = FALSE)
.External2(C_windowsremoveext, path, compression)


unix.removeext <- function (path, compression = FALSE)
.External2(C_unixremoveext, path, compression)


removeext <- function (path, compression = FALSE)
.External2(C_removeext, path, compression)


rm.ext <- removeext
