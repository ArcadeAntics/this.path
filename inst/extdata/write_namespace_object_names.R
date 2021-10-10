x <- c("datasets", "utils", "grDevices", "graphics", "stats", "methods", "base")
x <- sapply(x, function(x) {
    names(getNamespace(x))
}, simplify = FALSE)
FILE <- this.path::here(paste0(
    if (.Platform$OS.type == "windows")
        "windows"
    else if (capabilities("aqua"))
        "macOS"
    else "linux",
    "_namespace_object_names.rds"
))
saveRDS(x, FILE)
