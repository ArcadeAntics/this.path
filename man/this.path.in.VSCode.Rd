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
        installed. They can be installed from the terminal like:

\itemize{
  \item \verb{sudo apt install make}
  \item \verb{sudo apt install gcc}
  \item \verb{sudo apt install g++}
}
  \item to install \R package \pkg{rstudioapi}. This package does not require
        compilation. Note that you do not need to install \sQuote{RStudio}.
  \item to install the \R extension for \sQuote{VSCode}; a prompt to install it
        will appear upon opening an \R script in \sQuote{VSCode}.
}

Furthermore, you must ensure that \sQuote{VSCode} has been attached properly.
If you look at the bar on the bottom of your \sQuote{VSCode} window and see
this:

\strong{R: (not attached)}

then \sQuote{VSCode} is not attached to your \R session and \code{this.path()}
will fail. Click the button \strong{R: (not attached)} and \sQuote{VSCode} will
attempt to attach itself. If clicking the button throws this error:

\strong{Error in .vsc.attach() : could not find function ".vsc.attach"}

then \sQuote{VSCode} was unable to attach itself, so you will need to restart
your \R session. Hover your cursor over \strong{R Interactive} on the right
side and click the trash bin labelled \strong{Kill (Delete)} to kill your \R
session. After closing all of your \R sessions, click the button
\strong{R: (not attached)} and \sQuote{VSCode} should now attach itself
successfully. You should see something like this:

\strong{R \Sexpr{getRversion()}}

in the place of \strong{R: (not attached)}.
}
