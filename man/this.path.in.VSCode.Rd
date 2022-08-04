\name{this.path.in.VSCode}
\alias{this.path.in.VSCode}
\title{
\code{this.path()} in \sQuote{VSCode}
}
\description{
\code{this.path()} will not work with a fresh installation of \sQuote{VSCode},
some other packages / / applications are needed.
}
\details{
You will need:
\enumerate{
  \item to install \R packages \pkg{jsonlite} and \pkg{rlang}. On platforms
        without pre-built binaries, these packages will need compilation, you
        need to have \command{make}, \command{gcc}, and \command{g++}
        installed. They can be installed from the terminal like:\verb{
}\command{sudo} \command{apt} \option{install} \option{make}\verb{
}\command{sudo} \command{apt} \option{install} \option{gcc}\verb{
}\command{sudo} \command{apt} \option{install} \option{g++}
  \item to install \R package \pkg{rstudioapi}. This package does not require
        compilation. Note that you do not need to install \sQuote{RStudio}.
  \item to install the \R extension for \sQuote{VSCode}; a prompt to install it
        will appear upon opening an \R script in \sQuote{VSCode}.
}
}
