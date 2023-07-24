%% does not work well with characters that have
%% special meaning in HTML or LaTeX or Rd
\newcommand{\string}{\ifelse{text}{"#1"}{\code{"#1"}}}
\newcommand{\blankstring}{\ifelse{text}{""}{\code{""}}}
\newcommand{\codelink2}{\ifelse{html}{\code{\link[#1]{#2}}}{\code{#2}}}
\newcommand{\codelink3}{\ifelse{html}{\code{\link[#1]{#2}#3}}{\code{#2#3}}}
\newcommand{\codelink4}{\ifelse{html}{\code{#1\link[#2]{#3}#4}}{\code{#1#3#4}}}


%% used in ./inst/NEWS.Rd
\newcommand{\commandorsQuote}{\ifelse{text}{\sQuote{#1}}{\command{#1}}}
\newcommand{\codeorsQuote}{\ifelse{text}{\sQuote{#1}}{\code{#1}}}
\newcommand{\optionorsQuote}{\ifelse{text}{\sQuote{#1}}{\option{#1}}}


\newcommand{\Jupyter}{\href{https://jupyter.org/}{Jupyter}}
\newcommand{\Python}{\href{https://www.python.org/}{Python}}
\newcommand{\radian}{\href{https://github.com/randy3k/radian}{radian}}
\newcommand{\RStudio}{\href{https://posit.co/products/open-source/rstudio/}{RStudio}}
\newcommand{\VSCode}{\href{https://code.visualstudio.com/}{VSCode}}

\newcommand{\sourcelink}{\codelink3{base}{source}{()}}
\newcommand{\syssourcelink}{\codelink3{base}{sys.source}{()}}
\newcommand{\debugSourceinRStudiolink}{\ifelse{text}{\code{debugSource()} in \sQuote{RStudio}}{\href{https://support.posit.co/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE}{\code{debugSource}}\code{()} in \sQuote{\RStudio}}}
\newcommand{\testthatsourcefilelink}{\codelink3{testthat:source_file}{testthat::source_file}{()}}
\newcommand{\knitrknitlink}{\codelink3{knitr:knit}{knitr::knit}{()}}
\newcommand{\compilerloadcmplink}{\codelink3{compiler:compile}{compiler::loadcmp}{()}}
\newcommand{\boxuselink}{\codelink3{box:use}{box::use}{()}}
\newcommand{\Rscriptlink}{\codelink2{utils}{Rscript}}

\newcommand{\sourcenolink}{\code{source()}}
\newcommand{\syssourcenolink}{\code{sys.source()}}
\newcommand{\debugSourceinRStudionolink}{\code{debugSource()} in \sQuote{RStudio}}
\newcommand{\testthatsourcefilenolink}{\code{testthat::source_file()}}
\newcommand{\knitrknitnolink}{\code{knitr::knit()}}
\newcommand{\compilerloadcmpnolink}{\code{compiler::loadcmp()}}
\newcommand{\boxusenolink}{\code{box::use()}}
\newcommand{\Rscriptnolink}{\code{Rscript}}

\newcommand{\sourceslistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \testthatsourcefilelink, \knitrknitlink, \compilerloadcmplink, \boxuselink}
\newcommand{\sourcesorlistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \testthatsourcefilelink, \knitrknitlink, \compilerloadcmplink, or \boxuselink}
\newcommand{\sourcesandlistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \testthatsourcefilelink, \knitrknitlink, \compilerloadcmplink, and \boxuselink}

\newcommand{\sourceslistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \testthatsourcefilenolink, \knitrknitnolink, \compilerloadcmpnolink, \boxusenolink}
\newcommand{\sourcesorlistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \testthatsourcefilenolink, \knitrknitnolink, \compilerloadcmpnolink, or \boxusenolink}
\newcommand{\sourcesandlistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \testthatsourcefilenolink, \knitrknitnolink, \compilerloadcmpnolink, and \boxusenolink}

%% only excludes box::use
\newcommand{\sources1stargisfilelistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \testthatsourcefilelink, \knitrknitlink, \compilerloadcmplink}
\newcommand{\sources1stargisfileorlistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \testthatsourcefilelink, \knitrknitlink, or \compilerloadcmplink}
\newcommand{\sources1stargisfileandlistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \testthatsourcefilelink, \knitrknitlink, and \compilerloadcmplink}

\newcommand{\sources1stargisfilelistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \testthatsourcefilenolink, \knitrknitnolink, \compilerloadcmpnolink}
\newcommand{\sources1stargisfileorlistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \testthatsourcefilenolink, \knitrknitnolink, or \compilerloadcmpnolink}
\newcommand{\sources1stargisfileandlistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \testthatsourcefilenolink, \knitrknitnolink, and \compilerloadcmpnolink}

%% currently, all source()-like functions in which the first argument is the
%% file are also compatible with withArgs(), so the commands are the same.
%% this may change in the future
\newcommand{\sourcescompatiblewithwithArgslistlinks}{\sources1stargisfilelistlinks}
\newcommand{\sourcescompatiblewithwithArgsorlistlinks}{\sources1stargisfileorlistlinks}
\newcommand{\sourcescompatiblewithwithArgsandlistlinks}{\sources1stargisfileandlistlinks}

\newcommand{\sourcescompatiblewithwithArgslistnolinks}{\sources1stargisfilelistnolinks}
\newcommand{\sourcescompatiblewithwithArgsorlistnolinks}{\sources1stargisfileorlistnolinks}
\newcommand{\sourcescompatiblewithwithArgsandlistnolinks}{\sources1stargisfileandlistnolinks}
