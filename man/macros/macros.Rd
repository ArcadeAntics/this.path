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


\newcommand{\Emacs}{\href{https://ess.r-project.org/}{Emacs}}
\newcommand{\Jupyter}{\href{https://jupyter.org/}{Jupyter}}
\newcommand{\Python}{\href{https://www.python.org/}{Python}}
\newcommand{\radian}{\href{https://github.com/randy3k/radian}{radian}}
\newcommand{\RStudio}{\href{https://posit.co/products/open-source/rstudio/}{RStudio}}
\newcommand{\VSCode}{\href{https://code.visualstudio.com/}{VSCode}}


\newcommand{\sourcelink}{\codelink3{base}{source}{()}}
\newcommand{\syssourcelink}{\codelink3{base}{sys.source}{()}}
\newcommand{\debugSourceinRStudiolink}{\ifelse{text}{\code{debugSource()} in \sQuote{RStudio}}{\href{https://support.posit.co/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE}{\code{debugSource}}\code{()} in \sQuote{\RStudio}}}
\newcommand{\compilerloadcmplink}{\codelink3{compiler:compile}{compiler::loadcmp}{()}}
\newcommand{\utilsSweavelink}{\codelink3{utils:Sweave}{utils::Sweave}{()}}
\newcommand{\boxuselink}{\codelink3{box:use}{box::use}{()}}
\newcommand{\knitrknitlink}{\codelink3{knitr:knit}{knitr::knit}{()}}
\newcommand{\plumberplumblink}{\codelink3{plumber:plumb}{plumber::plumb}{()}}
\newcommand{\shinyrunApplink}{\codelink3{shiny:runApp}{shiny::runApp}{()}}
\newcommand{\targetslink}{\ifelse{text}{\sQuote{package:targets}}{\CRANpkg{targets}}}
\newcommand{\testthatsourcefilelink}{\codelink3{testthat:source_file}{testthat::source_file}{()}}
\newcommand{\Rscriptlink}{\codelink2{utils}{Rscript}}

\newcommand{\sourcenolink}{\code{source()}}
\newcommand{\syssourcenolink}{\code{sys.source()}}
\newcommand{\debugSourceinRStudionolink}{\code{debugSource()} in \sQuote{RStudio}}
\newcommand{\compilerloadcmpnolink}{\code{compiler::loadcmp()}}
\newcommand{\utilsSweavenolink}{\code{utils::Sweave()}}
\newcommand{\boxusenolink}{\code{box::use()}}
\newcommand{\knitrknitnolink}{\code{knitr::knit()}}
\newcommand{\plumberplumbnolink}{\code{plumber::plumb()}}
\newcommand{\shinyrunAppnolink}{\code{shiny::runApp()}}
\newcommand{\targetsnolink}{\code{targets}}
\newcommand{\testthatsourcefilenolink}{\code{testthat::source_file()}}
\newcommand{\Rscriptnolink}{\code{Rscript}}


\newcommand{\sourceslistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \compilerloadcmplink, \utilsSweavelink, \boxuselink, \knitrknitlink, \plumberplumblink, \shinyrunApplink, \targetslink, \testthatsourcefilelink}
\newcommand{\sourcesorlistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \compilerloadcmplink, \utilsSweavelink, \boxuselink, \knitrknitlink, \plumberplumblink, \shinyrunApplink, \targetslink, or \testthatsourcefilelink}
\newcommand{\sourcesandlistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \compilerloadcmplink, \utilsSweavelink, \boxuselink, \knitrknitlink, \plumberplumblink, \shinyrunApplink, \targetslink, and \testthatsourcefilelink}

\newcommand{\sourceslistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \compilerloadcmpnolink, \utilsSweavenolink, \boxusenolink, \knitrknitnolink, \plumberplumbnolink, \shinyrunAppnolink, \targetsnolink, \testthatsourcefilenolink}
\newcommand{\sourcesorlistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \compilerloadcmpnolink, \utilsSweavenolink, \boxusenolink, \knitrknitnolink, \plumberplumbnolink, \shinyrunAppnolink, \targetsnolink, or \testthatsourcefilenolink}
\newcommand{\sourcesandlistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \compilerloadcmpnolink, \utilsSweavenolink, \boxusenolink, \knitrknitnolink, \plumberplumbnolink, \shinyrunAppnolink, \targetsnolink, and \testthatsourcefilenolink}

%% excludes box::use, plumber::plumb, shiny::runApp, and package:targets
\newcommand{\sourcesfirstargisfilelistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \compilerloadcmplink, \utilsSweavelink, \knitrknitlink, \testthatsourcefilelink}
\newcommand{\sourcesfirstargisfileorlistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \compilerloadcmplink, \utilsSweavelink, \knitrknitlink, or \testthatsourcefilelink}
\newcommand{\sourcesfirstargisfileandlistlinks}{\sourcelink, \syssourcelink, \debugSourceinRStudiolink, \compilerloadcmplink, \utilsSweavelink, \knitrknitlink, and \testthatsourcefilelink}

\newcommand{\sourcesfirstargisfilelistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \compilerloadcmpnolink, \utilsSweavenolink, \knitrknitnolink, \testthatsourcefilenolink}
\newcommand{\sourcesfirstargisfileorlistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \compilerloadcmpnolink, \utilsSweavenolink, \knitrknitnolink, or \testthatsourcefilenolink}
\newcommand{\sourcesfirstargisfileandlistnolinks}{\sourcenolink, \syssourcenolink, \debugSourceinRStudionolink, \compilerloadcmpnolink, \utilsSweavenolink, \knitrknitnolink, and \testthatsourcefilenolink}

%% currently, all source()-like functions in which the first argument is the
%% file are also compatible with withArgs(), so the commands are the same.
%% this may change in the future
\newcommand{\sourcescompatiblewithwithArgslistlinks}{\sourcesfirstargisfilelistlinks}
\newcommand{\sourcescompatiblewithwithArgsorlistlinks}{\sourcesfirstargisfileorlistlinks}
\newcommand{\sourcescompatiblewithwithArgsandlistlinks}{\sourcesfirstargisfileandlistlinks}

\newcommand{\sourcescompatiblewithwithArgslistnolinks}{\sourcesfirstargisfilelistnolinks}
\newcommand{\sourcescompatiblewithwithArgsorlistnolinks}{\sourcesfirstargisfileorlistnolinks}
\newcommand{\sourcescompatiblewithwithArgsandlistnolinks}{\sourcesfirstargisfileandlistnolinks}
