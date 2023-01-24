windows.path.split <- function (path)
.External2(C_windowspathsplit, path)


unix.path.split <- function (path)
.External2(C_unixpathsplit, path)


path.split <- function (path)
.External2(C_pathsplit, path)


windows.path.split.1 <- function (path)
.External2(C_windowspathsplit1, path)


unix.path.split.1 <- function (path)
.External2(C_unixpathsplit1, path)


path.split.1 <- function (path)
.External2(C_pathsplit1, path)


windows.path.unsplit <- function (...)
.External2(C_windowspathunsplit)


unix.path.unsplit <- function (...)
.External2(C_unixpathunsplit)


path.unsplit <- function (...)
.External2(C_pathunsplit)
