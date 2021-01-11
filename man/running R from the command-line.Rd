\name{Running R from the command-line / / terminal}
\alias{Running R from the command-line / / terminal}
\title{Running R from the command-line / / terminal
}
\description{
How to run \R from the Windows command-line / / Unix terminal.
}
\details{
When you download \R, you are given four executable files that can be used from
the command-line / / terminal. These are \sQuote{R}, \sQuote{Rcmd},
\sQuote{Rscript}, and \sQuote{Rterm}. For the purposes of running R scripts,
only \sQuote{Rterm} is needed. Explanations for the other executables can be
found in \sQuote{Examples}.

Suppose you wanted to run an R script with a filename \sQuote{input R script}
from the command-line / / terminal. You would write and execute one of the
following commands:

\command{Rterm} \option{-f "input R script"}

\command{Rterm} \option{--file="input R script"}

\pkg{this.path} is capable of recognizing either of these, so use whichever you
prefer. I prefer the second option, I feel it is more R like, though of course
this is just personal preference.

There are plenty of options that \sQuote{Rterm} accepts, you can find all of
them by executing the following command:

\command{Rterm} \option{--help}

The few I use the most are:

\option{--quiet} do not print the R startup message

\option{--no-echo} make R run as quietly as possible (includes \option{--quiet})

\option{--verbose} print information about progress, includes setting option
\sQuote{verbose} to TRUE

\option{--args} indicates that arguments following this argument are for the R
script itself, which can be accessed in R with the following command:
\code{commandArgs(trailingOnly = TRUE)}

If, when you execute one of the previous commands, you see the following error
message: \dQuote{\sQuote{Rterm} is not recognized as an internal or external
command, operable program or batch file.}, see \sQuote{Ease of Use on Windows}.

The R executable can be used to call the Rcmd executable or the Rterm executable,
so it's unnecessary to call this executable directly. Additionally, the Rscript
executable is used as a wrapper for the Rterm executable, so it's also unnecessary
to call this executable directly (See \sQuote{Examples}). The Rcmd executable is
mostly for preparing R packages, the only command to Rcmd that isn't for packages
specifically is \command{Rcmd} \command{BATCH} \option{...}. \command{Rcmd} \command{BATCH}
\option{...} is also a wrapper for the Rterm executable, so it's also unnecessary
to call this executable directly (See \sQuote{Examples}).
output file is always made (which may or may not be desired), and three other command-line
options are set by default
}
\section{Ease of Use on Windows}{
On a Unix-alike OS (including macOS), it is easy to invoke an \R session from
the command-line / / terminal by simply typing the name of the R executable file
you wish to run. On Windows, you should see that typing the name of the
executable file you wish to run does not run that application but instead throws
an error. Instead, you will have to type the full path of the directory where
your R executable files are located (See \sQuote{Where are my R executable files
located?}), followed by the name of the executable file you wish to run. This is
not very convenient to type everytime something needs to be run from the
command-line / / terminal, plus it has another issue of being computer
dependent. The solution is to add the path of the directory where your R
executable files are located to the \env{Path} environment variable. In short,
the \env{Path} environment variable is a list of directories where executable
programs are located. When you type the name of an executable program you wish
to run, Windows looks for that program through each directory in the \env{Path}
environment variable. When you add the full path of the directory where your R
executable files are located to your \env{Path} environment variable, you should
be able to run any of those executable programs by their basenames (\sQuote{R},
\sQuote{Rcmd}, \sQuote{Rscript}, and \sQuote{Rterm}) instead of their full
paths.

To add a new path to your \env{Path} environment variable, first open the
Control Panel. You should be able to do this by pressing \strong{Windows},
\strong{Windows}+\strong{R}, or opening a file explorer window and typing
\strong{Control Panel} in the prompt. From there, open the category
\strong{System and Security}, then open the category \strong{System}, then open
\strong{Advanced system settings} (should appear near the top left), then open
\strong{Environment Variables...} (should appear near the bottom right). You
should see a new window split in two sections, environment variables for the
current user and environment variables for the system. If all users of this
machine are using the same R executable files, you can add the path to the
system environment variables (this is what I did), otherwise you can add it to
your user environment variables. Click the variable \env{Path}, then click
\strong{Edit...}, then click \strong{New}, then type (or paste) the full path of
the directory where your R executable files are located.

To check that this worked correctly, open the command-line / / terminal and
execute the following commands:

\command{Rterm} \option{--help}

\command{Rterm} \option{--version}

You should see that the first prints the usage message for the \sQuote{Rterm}
executable file while the second prints information about the version of R
currently being run. Make sure this is the version of R you wish to run.
}
\section{Where are my R executable files located?}{
In an R session, you can find the location of your R executable files with the
following R command:

\code{cat(normalizePath(R.home(component = "bin")))}

For me, this is:

\file{C:\\Program Files\\R\\R-4.0.2\\bin\\x64}
}
\examples{
## the following shows that 'Rscript' is just a wrapper for 'Rterm' with a few
## extra options. This makes 'Rscript' less desirable in my opinion.
system("Rterm   -e \"commandArgs()\"")  # "Rterm" "-e" "commandArgs()"
system("Rscript -e \"commandArgs()\"")  # "Rterm" "--no-echo" "--no-restore" "-e" "commandArgs()"


## it's hard to show here but 'Rcmd BATCH' is also just a wrapper for 'Rterm'
## with a few extra options.

tmp.R.script <- tempfile

}
