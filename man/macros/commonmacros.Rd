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
\newcommand{\compilerloadcmplink}{\code{\link[compiler:compile]{compiler::loadcmp}()}}

\newcommand{\sourcenolink}{\code{source()}}
\newcommand{\syssourcenolink}{\code{sys.source()}}
\newcommand{\debugSourcenolink}{\code{debugSource()} in \sQuote{RStudio}}
\newcommand{\sourcefilenolink}{\code{testthat::source_file()}}
\newcommand{\knitnolink}{\code{knitr::knit()}}
\newcommand{\boxusenolink}{\code{box::use()}}
\newcommand{\compilerloadcmpnolink}{\code{compiler::loadcmp()}}

\newcommand{\sourceslistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, \knitlink{}, \boxuselink{}, \compilerloadcmplink{}}
\newcommand{\sourcesorlistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, \knitlink{}, \boxuselink{}, or \compilerloadcmplink{}}
\newcommand{\sourcesandlistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, \knitlink{}, \boxuselink{}, and \compilerloadcmplink{}}

\newcommand{\sourceslistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, \knitnolink{}, \boxusenolink{}, \compilerloadcmpnolink{}}
\newcommand{\sourcesorlistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, \knitnolink{}, \boxusenolink{}, or \compilerloadcmpnolink{}}
\newcommand{\sourcesandlistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, \knitnolink{}, \boxusenolink{}, and \compilerloadcmpnolink{}}

% only excludes box::use
\newcommand{\sources1stargisfilelistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, \knitlink{}, \compilerloadcmplink{}}
\newcommand{\sources1stargisfileorlistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, \knitlink{}, or \compilerloadcmplink{}}
\newcommand{\sources1stargisfileandlistlinks}{\sourcelink{}, \syssourcelink{}, \debugSourcelink{}, \sourcefilelink{}, \knitlink{}, and \compilerloadcmplink{}}

\newcommand{\sources1stargisfilelistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, \knitnolink{}, \compilerloadcmpnolink{}}
\newcommand{\sources1stargisfileorlistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, \knitnolink{}, or \compilerloadcmpnolink{}}
\newcommand{\sources1stargisfileandlistnolinks}{\sourcenolink{}, \syssourcenolink{}, \debugSourcenolink{}, \sourcefilenolink{}, \knitnolink{}, and \compilerloadcmpnolink{}}

% currently, all source()-like functions in which the first argument is the
% file are also compatible with withArgs(), so the commands are the same.
% this may change in the future
\newcommand{\sourcescompatiblewithwithArgslistlinks}{\sources1stargisfilelistlinks{}}
\newcommand{\sourcescompatiblewithwithArgsorlistlinks}{\sources1stargisfileorlistlinks{}}
\newcommand{\sourcescompatiblewithwithArgsandlistlinks}{\sources1stargisfileandlistlinks{}}

\newcommand{\sourcescompatiblewithwithArgslistnolinks}{\sources1stargisfilelistnolinks{}}
\newcommand{\sourcescompatiblewithwithArgsorlistnolinks}{\sources1stargisfileorlistnolinks{}}
\newcommand{\sourcescompatiblewithwithArgsandlistnolinks}{\sources1stargisfileandlistnolinks{}}
