stopifnot(identical(
    this.path:::is.clipboard(c("clipboard", "clipboard-128", "X11_primary", "X11_secondary", "X11_clipboard")),
    if (.Platform$OS.type == "windows")
        c(TRUE , TRUE , FALSE, FALSE, FALSE)
    else
        c(TRUE , FALSE, TRUE , TRUE , TRUE )
))
