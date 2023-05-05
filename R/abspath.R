.abspath <- function (path)
path.join(getwd(), path.expand(path))


.normalizeAbsPath <- function (path, ...)
normalizePath(path = if (.os.windows) path else .abspath(path), ...)
