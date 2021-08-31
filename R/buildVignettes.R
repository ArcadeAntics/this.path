buildVignettes <- function (...)
{
    file <- here(...)
}


file <- getwd()
indir <- file.path(file, "vignettes")
outdir <- file.path(file, "docs")

infiles <- dir(indir, pattern = "\\.Rmd$", all.files = TRUE, ignore.case = TRUE)

outfiles <- sub("\\.Rmd", ".html", infiles, ignore.case = TRUE)
infiles <- file.path(indir, infiles)

