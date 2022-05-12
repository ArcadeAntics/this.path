select.screen.res <- function ()
{
    info <- utils::read.csv(this.path::here("info.csv"),
        colClasses = c(scaling = "character"))
    rownames(info) <- info$name
    info$file <- this.path::here(info$file)
    if (length(args <- essentials::Args()))
        arg <- args[[1L]]
    else arg <- utils::select.list(info$name, title = "Select your screen resolution:", graphics = TRUE)
    i <- match.arg(arg, info$name)
    info[i, ]
}
