\newcommand{\Jupyter}{\href{https://jupyter.org/}{Jupyter}}
\newcommand{\Python}{\href{https://www.python.org/}{Python}}
\newcommand{\radian}{\href{https://github.com/randy3k/radian}{radian}}
\newcommand{\RStudio}{\href{https://posit.co/products/open-source/rstudio/}{RStudio}}
\newcommand{\VSCode}{\href{https://code.visualstudio.com/}{VSCode}}

\newcommand{\DebuggingWithRStudio}{\href{https://support.posit.co/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE}{#1}}

\newcommand{\sourcelink}{\code{\link[base]{source}()}}
\newcommand{\syssourcelink}{\code{\link[base]{sys.source}()}}
% we have to use \command{} instead of \code{} because \command{} is LaTeX-like
% which supports macros in text, whereas \code{} is entered in R-like syntax in
% which only macros \var{} and \link{} are interpreted within text
%
% both are displayed using typewriter font where possible
\newcommand{\debugSourcelink}{\command{\DebuggingWithRStudio{debugSource}()} in \sQuote{\RStudio}}
\newcommand{\sourcefilelink}{\code{\link[testthat:source_file]{testthat::source_file}()}}
\newcommand{\knitlink}{\code{\link[knitr:knit]{knitr::knit}()}}
\newcommand{\boxuselink}{\code{\link[box:use]{box::use}()}}

\newcommand{\sourcenolink}{\code{source()}}
\newcommand{\syssourcenolink}{\code{sys.source()}}
\newcommand{\debugSourcenolink}{\code{debugSource()} in \sQuote{RStudio}}
\newcommand{\sourcefilenolink}{\code{testthat::source_file()}}
\newcommand{\knitnolink}{\code{knitr::knit()}}
\newcommand{\boxusenolink}{\code{box::use()}}

\newcommand{\sourceslistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, \knitlink{}, \boxuselink{}}
\newcommand{\sourcesorlistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, \knitlink{}, or \boxuselink{}}
\newcommand{\sourcesandlistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, \knitlink{}, and \boxuselink{}}

\newcommand{\sourceslistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, \knitnolink{}, \boxusenolink{}}
\newcommand{\sourcesorlistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, \knitnolink{}, or \boxusenolink{}}
\newcommand{\sourcesandlistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, \knitnolink{}, and \boxusenolink{}}

\newcommand{\sources1stargisfilelistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, \knitlink{}}
\newcommand{\sources1stargisfileorlistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, or \knitlink{}}
\newcommand{\sources1stargisfileandlistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, and \knitlink{}}

\newcommand{\sources1stargisfilelistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, \knitnolink{}}
\newcommand{\sources1stargisfileorlistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, or \knitnolink{}}
\newcommand{\sources1stargisfileandlistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, and \knitnolink{}}

\newcommand{\sourcescompatiblewithwithArgslistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, \knitlink{}}
\newcommand{\sourcescompatiblewithwithArgsorlistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, or \knitlink{}}
\newcommand{\sourcescompatiblewithwithArgsandlistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, and \knitlink{}}

\newcommand{\sourcescompatiblewithwithArgslistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, \knitnolink{}}
\newcommand{\sourcescompatiblewithwithArgsorlistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, or \knitnolink{}}
\newcommand{\sourcescompatiblewithwithArgsandlistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, and \knitnolink{}}
