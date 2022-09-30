from.shell <- function ()
{
    tryCatch2({
        path <- .this.path(verbose = FALSE)
    }, `this.path::thisPathNotExistsError` = function(e) {
        FALSE
    }, `this.path::AQUAError` = function(e) {
        FALSE
    }, `this.path::thisPathUnrecognizedMannerError` = function(e) {
        FALSE
    }, else. = {
        isTRUE(attr(path, "this.path::from.shell"))
    })
}


is.main <- function ()
{
    tryCatch2({
        path <- .this.path(verbose = FALSE)
    }, `this.path::thisPathNotExistsError` = function(e) {
        TRUE
    }, `this.path::AQUAError` = function(e) {
        TRUE
    }, else. = {
        n <- attr(path, "this.path::n")
        is.null(n) || ((n == 1L || (n == 2L && identical(sys.function(1L), withArgs))) && is.na(.shFILE()))
    })
}
