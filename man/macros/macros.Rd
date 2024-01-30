%% used in ./inst/NEWS.Rd
\newcommand{\codeorsQuote}{\ifelse{text}{\sQuote{#1}}{\code{#1}}}
\newcommand{\commandorsQuote}{\ifelse{text}{\sQuote{#1}}{\command{#1}}}
\newcommand{\optionorsQuote}{\ifelse{text}{\sQuote{#1}}{\option{#1}}}
\newcommand{\pkgorsQuote}{\ifelse{text}{\sQuote{#1}}{\pkg{#1}}}


%% does not work well with characters that have
%% special meaning in HTML or LaTeX or Rd
\newcommand{\string}{\ifelse{text}{"#1"}{\code{"#1"}}}
\newcommand{\blankstring}{\ifelse{text}{""}{\code{""}}}
\newcommand{\codelink2}{\ifelse{html}{\code{\link[#1]{#2}}}{\codeorsQuote{#2}}}
\newcommand{\codelink3}{\ifelse{html}{\code{\link[#1]{#2}#3}}{\codeorsQuote{#2#3}}}
\newcommand{\codelink4}{\ifelse{html}{\code{#1\link[#2]{#3}#4}}{\codeorsQuote{#1#3#4}}}


\newcommand{\CRANpackage}{\href{https://CRAN.R-project.org/package=#1}{\pkgorsQuote{package:#1}}}


\newcommand{\Emacs}{\href{https://ess.r-project.org/}{Emacs}}
\newcommand{\Jupyter}{\href{https://jupyter.org/}{Jupyter}}
\newcommand{\Python}{\href{https://www.python.org/}{Python}}
\newcommand{\radian}{\href{https://github.com/randy3k/radian}{radian}}
\newcommand{\RStudio}{\href{https://posit.co/products/open-source/rstudio/}{RStudio}}
\newcommand{\VSCode}{\href{https://code.visualstudio.com/}{VSCode}}


\newcommand{\sourcelink}{\codelink3{base}{source}{()}}
\newcommand{\syssourcelink}{\codelink3{base}{sys.source}{()}}
\newcommand{\debugSourceinRStudiolink}{\ifelse{text}{\sQuote{debugSource()} in \sQuote{RStudio}}{\href{https://support.posit.co/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE}{\code{debugSource}}\code{()} in \sQuote{\RStudio}}}
\newcommand{\compilerloadcmplink}{\codelink3{compiler:compile}{compiler::loadcmp}{()}}
\newcommand{\utilsSweavelink}{\codelink3{utils:Sweave}{utils::Sweave}{()}}
\newcommand{\boxuselink}{\codelink3{box:use}{box::use}{()}}
\newcommand{\knitrknitlink}{\codelink3{knitr:knit}{knitr::knit}{()}}
\newcommand{\plumberplumblink}{\codelink3{plumber:plumb}{plumber::plumb}{()}}
\newcommand{\shinyrunApplink}{\codelink3{shiny:runApp}{shiny::runApp}{()}}
\newcommand{\targetslink}{\CRANpackage{targets}}
\newcommand{\targetstarcallrinnertrylink}{\codelink3{targets:tar_callr_inner_try}{targets::tar_callr_inner_try}{()}}
\newcommand{\targetstarloadglobalslink}{\codelink3{targets:tar_load_globals}{targets::tar_load_globals}{()}}
\newcommand{\targetstarsourcelink}{\codelink3{targets:tar_source}{targets::tar_source}{()}}
\newcommand{\targetstarworkspacelink}{\codelink3{targets:tar_workspace}{targets::tar_workspace}{()}}
\newcommand{\testthatsourcefilelink}{\codelink3{testthat:source_file}{testthat::source_file}{()}}
\newcommand{\Rscriptlink}{\codelink2{utils}{Rscript}}

\newcommand{\sourcenolink}{\codeorsQuote{source()}}
\newcommand{\syssourcenolink}{\codeorsQuote{sys.source()}}
\newcommand{\debugSourceinRStudionolink}{\codeorsQuote{debugSource()} in \sQuote{RStudio}}
\newcommand{\compilerloadcmpnolink}{\codeorsQuote{compiler::loadcmp()}}
\newcommand{\utilsSweavenolink}{\codeorsQuote{utils::Sweave()}}
\newcommand{\boxusenolink}{\codeorsQuote{box::use()}}
\newcommand{\knitrknitnolink}{\codeorsQuote{knitr::knit()}}
\newcommand{\plumberplumbnolink}{\codeorsQuote{plumber::plumb()}}
\newcommand{\shinyrunAppnolink}{\codeorsQuote{shiny::runApp()}}
\newcommand{\targetsnolink}{\pkgorsQuote{package:targets}}
\newcommand{\targetstarcallrinnertrynolink}{\codeorsQuote{targets::tar_callr_inner_try()}}
\newcommand{\targetstarloadglobalsnolink}{\codeorsQuote{targets::tar_load_globals()}}
\newcommand{\targetstarsourcenolink}{\codeorsQuote{targets::tar_source()}}
\newcommand{\targetstarworkspacenolink}{\codeorsQuote{targets::tar_workspace()}}
\newcommand{\testthatsourcefilenolink}{\codeorsQuote{testthat::source_file()}}
\newcommand{\Rscriptnolink}{\codeorsQuote{Rscript}}


\newcommand{\syspathcompatible}{\itemize{\item{\source#1} \item{\syssource#1} \item{\debugSourceinRStudio#1} \item{\compilerloadcmp#1} \item{\utilsSweave#1} \item{\boxuse#1} \item{\knitrknit#1} \item{\plumberplumb#1} \item{\shinyrunApp#1} \item{\targets#1} \item{\testthatsourcefile#1}}}
\newcommand{\syspathcompatiblelinks}{\syspathcompatible{link}}
\newcommand{\syspathcompatiblenolinks}{\syspathcompatible{nolink}}


\newcommand{\sources}{\itemize{\item{\source#1} \item{\syssource#1} \item{\debugSourceinRStudio#1} \item{\compilerloadcmp#1} \item{\utilsSweave#1} \item{\boxuse#1} \item{\knitrknit#1} \item{\plumberplumb#1} \item{\shinyrunApp#1} \item{\targetstarcallrinnertry#1 \cr \targetstarloadglobals#1 \cr \targetstarsource#1 \cr \targetstarworkspace#1} \item{\testthatsourcefile#1}}}
\newcommand{\sourceslinks}{\sources{link}}
\newcommand{\sourcesnolinks}{\sources{nolink}}
