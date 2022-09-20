windows.basename2 <- function (path)
.External2(C_windowsbasename2, path)


unix.basename2 <- function (path)
.External2(C_unixbasename2, path)


basename2 <- function (path)
.External2(C_basename2, path)


windows.dirname2 <- function (path)
.External2(C_windowsdirname2, path)


unix.dirname2 <- function (path)
.External2(C_unixdirname2, path)


dirname2 <- function (path)
.External2(C_dirname2, path)
