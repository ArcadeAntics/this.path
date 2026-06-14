##
## this.path : Get Executing Script's Path
## Copyright (C) 2022-2026   Iris Simmons
##


x <- c("clipboard", "clipboard-128", "X11_primary", "X11_secondary", "X11_clipboard")
expectation_windows <- c(
              TRUE,            TRUE,         FALSE,           FALSE,           FALSE
)
expectation_unix <- c(
              TRUE,           FALSE,          TRUE,            TRUE,            TRUE
)
stopifnot(identical(
    @R_PACKAGE_NAME@:::.is_clipboard_windows(x),
    expectation_windows
))
stopifnot(identical(
    @R_PACKAGE_NAME@:::.is_clipboard_unix(x),
    expectation_unix
))
stopifnot(identical(
    @R_PACKAGE_NAME@:::.is_clipboard(x),
    if (.Platform$OS.type == "windows") expectation_windows else expectation_unix
))
