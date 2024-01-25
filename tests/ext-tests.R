check <- function (path, root, ext, compression = FALSE)
{
    stopifnot(
        identical(splitext(path, compression), matrix(c(character(), root, ext), 2, byrow = TRUE, dimnames = list(c("root", "ext"), NULL))),
        identical(removeext(path, compression), root),
        identical(ext(path, compression), ext)
    )
}


splitext  <- @R_PACKAGE_NAME@:::.windows_splitext
removeext <- @R_PACKAGE_NAME@:::.windows_removeext
ext       <- @R_PACKAGE_NAME@:::.windows_ext


check(character(), character(), character())
check("", "", "")


check("d:", "d:", "")
check("//host/shar.e", "//host/shar.e", "")


check("d:/", "d:/", "")
check("//host/shar.e/", "//host/shar.e/", "")
check("/", "/", "")


check("d:/file.ext", "d:/file", ".ext")
check("//host/share/file.ext", "//host/share/file", ".ext")
check("d:file.ext", "d:file", ".ext")
check("file.ext", "file", ".ext")
check("/file.ext", "/file", ".ext")


check("d:/path/t.o/file", "d:/path/t.o/file", "")
check("//host/shar.e/file", "//host/shar.e/file", "")
check("//host/share/pa.th/to/file", "//host/share/pa.th/to/file", "")
check("/path/t.o/file", "/path/t.o/file", "")
check("path/t.o/file", "path/t.o/file", "")


check("file"    , "file", "")
check("file."   , "file.", "")
check(".file"   , ".file", "")
check("...ext"  , "...ext", "")
check("file.ext", "file", ".ext")


check("file.tar.gz" , "file"  , ".tar.gz" , compression = TRUE)
check("file.tar.bz2", "file"  , ".tar.bz2", compression = TRUE)
check("file.tar.xz" , "file"  , ".tar.xz" , compression = TRUE)
check("file.gz"     , "file"  , ".gz"     , compression = TRUE)
check("file..gz"    , "file." , ".gz"     , compression = TRUE)
check(".file.gz"    , ".file" , ".gz"     , compression = TRUE)
check("...ext.gz"   , "...ext", ".gz"     , compression = TRUE)
check("file.ext.gz" , "file"  , ".ext.gz" , compression = TRUE)


check(letters, letters, character(26))



splitext  <- @R_PACKAGE_NAME@:::.unix_splitext
removeext <- @R_PACKAGE_NAME@:::.unix_removeext
ext       <- @R_PACKAGE_NAME@:::.unix_ext


check(character(), character(), character())
check("", "", "")


check("//host/shar.e", "//host/shar.e", "")


check("//host/shar.e/", "//host/shar.e/", "")
check("/", "/", "")


check("//host/shar.e/file", "//host/shar.e/file", "")
check("/file", "/file", "")


check("file",     "file", "")
check("file.",    "file.", "")
check(".file",    ".file", "")
check("...ext",   "...ext", "")
check("file.ext", "file", ".ext")


check("file.tar.gz" , "file"  , ".tar.gz" , compression = TRUE)
check("file.tar.bz2", "file"  , ".tar.bz2", compression = TRUE)
check("file.tar.xz" , "file"  , ".tar.xz" , compression = TRUE)
check("file.gz"     , "file"  , ".gz"     , compression = TRUE)
check("file..gz"    , "file." , ".gz"     , compression = TRUE)
check(".file.gz"    , ".file" , ".gz"     , compression = TRUE)
check("...ext.gz"   , "...ext", ".gz"     , compression = TRUE)
check("file.ext.gz" , "file"  , ".ext.gz" , compression = TRUE)


check(letters, letters, character(26))


## windows replacement tests ----


ext     <- @R_PACKAGE_NAME@:::.windows_ext
`ext<-` <- @R_PACKAGE_NAME@:::`.windows_ext<-`


x   <- c(NA, "", ""    , "C:"  , "//host/share", "C:/" , "/path/to/fi", "/path/to/.", "/path/to/..", "/"   )
val <- c("", NA, ".png", ".png", ".png"        , ".png", ".png"       , ".png"      , ".png"       , ".png")
names(x) <- rev(LETTERS)[seq_along(x)]
ext(x) <- val
stopifnot(identical(x, c(Z = NA_character_, Y = NA_character_, X = "", W = "C:", V = "//host/share", U = "C:/", T = "/path/to/fi.png", S = "/path/to/.", R = "/path/to/..", Q = "/")))


x <- paste0("@R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.tar", c(".gz", ".bz2", ".xz"))
names(x) <- x
ext(x, compression = TRUE) <- ".png"
stopifnot(identical(x, c(@R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.tar.gz = "@R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.png", @R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.tar.bz2 = "@R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.png", @R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.tar.xz = "@R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.png")))


x   <- c("C:/path/to/file", "C:/path/to/.file", "C:/path/to/file.", "C:/path/to/.....", "C:/path/to/file.jpg", "C:/path/to/file.jpg")
val <- c(".png"           , ".png"            , ".png"            , ".png"            , ".png"               , ""                   )
names(x) <- rev(LETTERS)[seq_along(x)]
ext(x) <- val
stopifnot(identical(x, c(Z = "C:/path/to/file.png", Y = "C:/path/to/.file.png", X = "C:/path/to/file..png", W = "C:/path/to/.....", V = "C:/path/to/file.png", U = "C:/path/to/file")))


x <- "a"
ext(x) <- 5
stopifnot(identical(x, "a.5"))


## unix replacement tests ----


ext     <- @R_PACKAGE_NAME@:::.unix_ext
`ext<-` <- @R_PACKAGE_NAME@:::`.unix_ext<-`


x <- "C:.jpg"
ext(x) <- ".png"
stopifnot(identical(x, "C:.png"))


x   <- c(NA, "", ""    , "C:"  , "//host/share", "C:/" , "/path/to/fi", "/path/to/.", "/path/to/..", "/"   )
val <- c("", NA, ".png", ".png", ".png"        , ".png", ".png"       , ".png"      , ".png"       , ".png")
names(x) <- rev(LETTERS)[seq_along(x)]
ext(x) <- val
stopifnot(identical(x, c(Z = NA_character_, Y = NA_character_, X = "", W = "C:.png", V = "//host/share", U = "C:.png", T = "/path/to/fi.png", S = "/path/to/.", R = "/path/to/..", Q = "/")))


x <- paste0("@R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.tar", c(".gz", ".bz2", ".xz"))
names(x) <- x
ext(x, compression = TRUE) <- ".png"
stopifnot(identical(x, c(@R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.tar.gz = "@R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.png", @R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.tar.bz2 = "@R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.png", @R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.tar.xz = "@R_PACKAGE_NAME@_@R_PACKAGE_VERSION@.png")))


x   <- c("/path/to/file", "/path/to/.file", "/path/to/file.", "/path/to/.....", "/path/to/file.jpg", "/path/to/file.jpg")
val <- c(".png"         , ".png"          , ".png"          , ".png"          , ".png"             , ""                 )
names(x) <- rev(LETTERS)[seq_along(x)]
ext(x) <- val
stopifnot(identical(x, c(Z = "/path/to/file.png", Y = "/path/to/.file.png", X = "/path/to/file..png", W = "/path/to/.....", V = "/path/to/file.png", U = "/path/to/file")))


x <- "a"
ext(x) <- 5
stopifnot(identical(x, "a.5"))
