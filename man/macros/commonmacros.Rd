\newcommand{\debugwrstudio}{\href{https://support.posit.co/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE}{#1}}
\newcommand{\sourcelink}{\code{\link[base]{source}}}
\newcommand{\syssourcelink}{\code{\link[base]{sys.source}}}
\newcommand{\debugSource}{\debugwrstudio{\code{debugSource}} in \sQuote{RStudio}}
\newcommand{\sourcefilelink}{\code{\link[testthat:source_file]{testthat::source_file}}}
\newcommand{\knitlink}{\code{\link[knitr:knit]{knitr::knit}}}
\newcommand{\sourcelinks}{\sourcelink{}, \syssourcelink{}, \debugSource{}, \sourcefilelink{}, \knitlink{}}
\newcommand{\sourceorlist}{\sourcelink{}, \syssourcelink{}, \debugSource{}, \sourcefilelink{}, or \knitlink{}}
\newcommand{\sourceandlist}{\sourcelink{}, \syssourcelink{}, \debugSource{}, \sourcefilelink{}, and \knitlink{}}
