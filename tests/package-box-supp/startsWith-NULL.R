##
## this.path : Get Executing Script's Path
## Copyright (C) 2024   Iris Simmons
##


NULL


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
