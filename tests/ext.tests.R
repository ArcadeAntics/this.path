check <- function (path, root, ext, compression = FALSE)
{
    stopifnot(
        identical(splitext(path, compression), matrix(c(character(), root, ext), 2, byrow = TRUE, dimnames = list(c("root", "ext"), NULL))),
        identical(removeext(path, compression), root),
        identical(ext(path, compression), ext)
    )
}


splitext  <- this.path:::windows.splitext
removeext <- this.path:::windows.removeext
ext       <- this.path:::windows.ext


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



splitext  <- this.path:::unix.splitext
removeext <- this.path:::unix.removeext
ext       <- this.path:::unix.ext


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
