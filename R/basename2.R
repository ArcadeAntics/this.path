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
