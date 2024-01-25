check <- function (path, dirname, basename)
{
    stopifnot(
        identical(dirname2(path), dirname),
        identical(basename2(path), basename)
    )
}


dirname2  <- @R_PACKAGE_NAME@:::.windows_dirname2
basename2 <- @R_PACKAGE_NAME@:::.windows_basename2


check("", "", "")


check("d:", "d:.", "")
check("//host/share", "//host/share", "")


check("d:/", "d:/", "")
check("//host/share/", "//host/share/", "")
check("/", "/", "")


check("d:/path", "d:/", "path")
check("//host/share/path", "//host/share/", "path")
check("/path", "/", "path")


check("d:file", "d:.", "file")
check("file", ".", "file")
check("d:path/to/file", "d:path/to", "file")


check("d:/path/to/file", "d:/path/to", "file")
check("//host/share/path/to/file", "//host/share/path/to", "file")
check("/path/to/file", "/path/to", "file")
check("/path/to/file/", "/path/to", "file")


check(
    @R_PACKAGE_NAME@::path.join("/", "p1", "p2", "p3", c("file1", "file2")),
    rep("/p1/p2/p3", 2),
    c("file1", "file2")
)
check(
    @R_PACKAGE_NAME@::path.join("/", "p1", "p2", "p3", "filename"),
    "/p1/p2/p3",
    "filename"
)


check(
    c("/usr/lib", "/usr/", "usr", "/", ".", ".."),
    c("/usr"    , "/"    , "."  , "/", ".", "."),
    c("lib"     , "usr"  , "usr", "" , ".", "..")
)


dirname2  <- @R_PACKAGE_NAME@:::.unix_dirname2
basename2 <- @R_PACKAGE_NAME@:::.unix_basename2


check("", "", "")


check("//host/share", "//host/share", "")


check("//host/share/", "//host/share/", "")
check("/", "/", "")


check("//host/share/path", "//host/share/", "path")
check("/path", "/", "path")


check("file", ".", "file")


check("//host/share/path/to/file", "//host/share/path/to", "file")
check("/path/to/file", "/path/to", "file")
check("/path/to/file/", "/path/to", "file")


check(
    @R_PACKAGE_NAME@::path.join("/", "p1", "p2", "p3", c("file1", "file2")),
    rep("/p1/p2/p3", 2),
    c("file1", "file2")
)
check(
    @R_PACKAGE_NAME@::path.join("/", "p1", "p2", "p3", "filename"),
    "/p1/p2/p3",
    "filename"
)


check(
    c("/usr/lib", "/usr/", "usr", "/", ".", ".."),
    c("/usr"    , "/"    , "."  , "/", ".", "."),
    c("lib"     , "usr"  , "usr", "" , ".", "..")
)
