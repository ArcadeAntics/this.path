check <- function (path, dirname, basename)
{
    stopifnot(
        identical(dirname2(path), dirname),
        identical(basename2(path), basename)
    )
}


dirname2 <- this.path:::windows.dirname2
basename2 <- this.path:::windows.basename2


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
    this.path::path.join("/", "p1", "p2", "p3", c("file1", "file2")),
    rep("/p1/p2/p3", 2),
    c("file1", "file2")
)
check(
    this.path::path.join("/", "p1", "p2", "p3", "filename"),
    "/p1/p2/p3",
    "filename"
)


check(
    c("/usr/lib", "/usr/", "usr", "/", ".", ".."),
    c("/usr"    , "/"    , "."  , "/", ".", "."),
    c("lib"     , "usr"  , "usr", "" , ".", "..")
)


dirname2 <- this.path:::unix.dirname2
basename2 <- this.path:::unix.basename2


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
    this.path::path.join("/", "p1", "p2", "p3", c("file1", "file2")),
    rep("/p1/p2/p3", 2),
    c("file1", "file2")
)
check(
    this.path::path.join("/", "p1", "p2", "p3", "filename"),
    "/p1/p2/p3",
    "filename"
)


check(
    c("/usr/lib", "/usr/", "usr", "/", ".", ".."),
    c("/usr"    , "/"    , "."  , "/", ".", "."),
    c("lib"     , "usr"  , "usr", "" , ".", "..")
)
