\newcommand{\debugwrstudio}{\href{https://support.posit.co/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE}{#1}}
\newcommand{\debugSource}{\debugwrstudio{\code{debugSource}}}
\newcommand{\sourcelinks}{\code{\link[base]{source}}, \code{\link[base]{sys.source}}, \debugSource{}, \code{\link[testthat:source_file]{testthat::source_file}}, \code{\link[knitr:knit]{knitr::knit}}}
\newcommand{\sourceorlist}{\code{\link[base]{source}}, \code{\link[base]{sys.source}}, \debugSource{} in \sQuote{RStudio}, \code{\link[testthat:source_file]{testthat::source_file}}, or \code{\link[knitr:knit]{knitr::knit}}}
\newcommand{\sourceandlist}{\code{\link[base]{source}}, \code{\link[base]{sys.source}}, \debugSource{} in \sQuote{RStudio}, \code{\link[testthat:source_file]{testthat::source_file}}, and \code{\link[knitr:knit]{knitr::knit}}}
