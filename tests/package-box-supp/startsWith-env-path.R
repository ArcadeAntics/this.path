@R_PACKAGE_NAME@::env.path(verbose = FALSE)


#' @export
doc_cntxt <- get(
    ".@R_PACKAGE_NAME@::document.context",
    envir = sys.frame(@R_PACKAGE_NAME@:::.getframenumber()),
    inherits = FALSE
)


#' @export
fun_that_calls_src_path <- function (...)
{
    @R_PACKAGE_NAME@::src.path(...)
}


#' @export
fun_that_calls_env_path <- function (...)
{
    @R_PACKAGE_NAME@::env.path(...)
}
