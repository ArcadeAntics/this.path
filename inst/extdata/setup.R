path <- R.home("..")
pattern <- sprintf("^R-(%s)$", .standard_regexps()$valid_R_system_version)
paths <- list.files(path, pattern)
R.versions <- as.numeric_version(sub(pattern, "\\1", paths))
i <- order(R.versions, decreasing = TRUE)
paths <- normalizePath(file.path(path, paths[i], "bin", "Rscript.exe"), mustWork = TRUE)
R.versions <- R.versions[i]
nms <- paths[c(
    which(R.versions <  "4.2.0")[[1L]],
    which(R.versions >= "4.2.0")[[1L]]
)]
for (name in nms) {
    essentials::Rscript(
        options = c("--default-packages=NULL", "--vanilla"),
        file = this.path::here("write_r_editor_regexp.R"), chdir = TRUE,
        name = name, mustWork = TRUE
    )
}
