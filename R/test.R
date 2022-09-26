if (FALSE) {


    test <- function(path) .External2(C_test, path)
    environment(test) <- getNamespace("this.path")


    test("path/to/file")
    test("/path/to/file")
    test("~andre/")
    test("~andrew/")
    test("~iris/")


}
