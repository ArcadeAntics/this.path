\name{Running.R.from.command-line}
\alias{Running.R.from.command-line}
\title{Running R from the Windows Command-Line / / Unix Terminal
}
\description{
How to run \R from the Windows command-line / / Unix terminal.
}
\details{
For the purposes of running \R scripts, there are four ways to do it. Suppose
our \R script is named \option{file3f4c31de2e51.R}, we could write any of:

\verb{R -f file3f4c31de2e51.R
R --file=file3f4c31de2e51.R
R CMD BATCH file3f4c31de2e51.R
Rscript file3f4c31de2e51.R}

The first two are different ways of writing equivalent statements. The third
statement is the first statement plus options \option{--restore --save} (plus
option \option{--no-readline} under Unix-alikes). The fourth statement is the
second statement plus options \option{--no-echo --no-restore}. You can try

\verb{R --help
R CMD BATCH --help
Rscript --help}

for a help message that describes what these options mean. In general,
\command{Rscript} is the one you want to use. It should be noted that
\command{Rscript} has some exclusive
\code{\link[base:EnvVar]{environment variables}} (not used by the other
executables) that will make its behaviour different from \command{R}.


If, when you execute one of the previous commands, you see the following error
message: \dQuote{\sQuote{R} is not recognized as an internal or external
command, operable program or batch file.}, see section \strong{Ease of Use on
Windows}.
}
\section{At the \R Level}{
If you are already in an \R session, you can invoke one of the commands above
with \code{\link{R}}, \code{\link{Rcmd}}, \code{\link{Rscript}}, and
\code{\link{Rterm}}. Each of these will invoke a corresponding command, but hide
the details of quoting arguments to be passed to a shell.
}
\section{Ease of Use on Windows}{
Under Unix-alikes, it is easy to invoke an \R session from the terminal by
typing the name of the \R executable you wish to run. On Windows, you should see
that typing the name of the \R executable you wish to run does not run that
application, but instead signals an error. Instead, you will have to type the
full path of the directory where your \R executables are located (see section
\strong{Where are my R executable files located?}), followed by the name of the
\R executable you wish to run.

This is not very convenient to type everytime something needs to be run from the
command-line / / terminal, plus it has another issue of being computer
dependent. The solution is to add the path of the directory where your \R
executables are located to the \env{Path} environment variable. The \env{Path}
environment variable is a list of directories where executable programs are
located. When you type the name of an executable program you wish to run,
Windows looks for that program through each directory in the \env{Path}
environment variable. When you add the full path of the directory where your \R
executables are located to your \env{Path} environment variable, you should be
able to run any of those executable programs by their basenames (\sQuote{R},
\sQuote{Rcmd}, \sQuote{Rscript}, and \sQuote{Rterm}) instead of their full
paths.

To add a new path to your \env{Path} environment variable:
\enumerate{
  \item{Open the \strong{Control Panel}}
  \item{Open category \strong{User Accounts}}
  \item{Open category \strong{User Accounts} (again)}
  \item{Open \strong{Change my environment variables}}
  \item{Click the variable \env{Path}}
  \item{Click the button \strong{Edit...}}
  \item{Click the button \strong{New}}
  \item{Type (or paste) the full path of the directory where your \R executables
are located, and press \strong{OK}}
}
This will modify your environment variable \env{Path}, not the systems. If
another user wishes to run \R from the command-line / / terminal, they will have
to add the directory to their \env{Path} environment variable as well.

If you wish to modify the system environment variable \env{Path} (you will need
admin permissions):
\enumerate{
  \item{Open the \strong{Control Panel}}
  \item{Open category \strong{System and Security}}
  \item{Open category \strong{System}}
  \item{Open \strong{Advanced system settings}}
  \item{Click the button \strong{Environment Variables...}}
  \item{Modify \env{Path} same as before, just select \env{Path} in
\strong{System variables} instead of \strong{User variables}}
}

To check that this worked correctly, open the command-line and execute the
following commands:

\verb{R --help
R --version}

You should see that the first prints the usage message for the \R executable
while the second prints information about the version of \R currently being run.
If you have multiple versions of \R installed, make sure this is the version of
\R you wish to run.
}
\section{Where are my R executable files located?}{
In an \R session, you can find the location of your \R executable files with the
following command:

\verb{cat(sQuote(normalizePath(R.home("bin"))), "\n")}

On Windows, for me, this is:

\file{\verb{C:\Program Files\R\R-4.1.1\bin\x64}}

On Linux, for me, this is:

\file{/usr/lib/R/bin}
}
