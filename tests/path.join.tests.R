`%check%` <- function (x, y)
stopifnot(identical(x, y))


path.join <- this.path:::windows.path.join


# zero input gives zero output
path.join() %check% character()


# vector of length zero gives zero output
path.join(letters, character()) %check% character()


# binding to an empty string does nothing
path.join(letters, "") %check% letters


path.join("p1", "/p2", "p3/", "p4", "p5/") %check% "/p2/p3/p4/p5/"


path.join("C:", "p1") %check% "C:p1"
path.join("C:/", "p1") %check% "C:/p1"
path.join("C:/path/to/file1", "/path/to/file2") %check% "C:/path/to/file2"
path.join("//host/share/path/to/file1", "/path/to/file2") %check% "//host/share/path/to/file2"
path.join("C:p1", "C:/p2", "~", "~/p3", "//host",
    "//host/share", "//host/share/path/to/file", "p4") %check% "//host/share/path/to/file/p4"
path.join("p1", "c:/p2", "p3", "//host/share/p4", "p5", "c:p6", "p7", "C:p8", "p9") %check% "C:p6/p7/p8/p9"


path.join("C:/path/to/file1", "//host/share/path/to/file2") %check% "//host/share/path/to/file2"


path.join("//h/s", "p1", "C:p2", "p3/", "p4") %check% "C:p2/p3/p4"


path.join("//h/s/", "p1", "c:p2", "p3", "c:/p4", "p5", "C:p6", "p7/") %check% "C:/p4/p5/p6/p7/"


path.join("~/p1", "p2", "~/p3", "p4/") %check% "~/p3/p4/"


path.join("c:p1", "p2", "p3", "C:") %check% "C:p1/p2/p3"


# in 1.0.2-60 fixed mistake where forgot to
# nul terminate a string with no pathspec
path.join("//host/share") %check% "//host/share"


# in 1.1.0 fixed mistake where the non empty path spec was ignored
# because preceding path specs were empty
path.join("C:", "c:test") %check% "c:test"




path.join <- this.path:::unix.path.join


# zero input gives zero output
path.join() %check% character()


# vector of length zero gives zero output
path.join(letters, character()) %check% character()


# binding to an empty string does nothing
path.join(letters, "") %check% letters


path.join("p1", "/p2", "p3/", "p4", "p5/") %check% "/p2/p3/p4/p5/"


path.join("/path/to/file1", "//host/share/path/to/file2") %check% "//host/share/path/to/file2"


path.join("//h/s", "p1", "/p2", "p3/", "p4") %check% "/p2/p3/p4"


path.join("/testing", "~/", "p1", "p2", "p3/") %check% "~/p1/p2/p3/"
