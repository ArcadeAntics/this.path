\name{R.from.shell}
\alias{R.from.shell}
\title{
Using R From a Shell
}
\description{
How to use \R from a shell (including the Windows command-line / / Unix
terminal).
}
\details{
For the purpose of running \R scripts, there are four ways to do it. Suppose
our \R script has filename \option{script1.R}, we could write any of:

\itemize{
  \item \verb{R -f script1.R}
  \item \verb{R --file=script1.R}
  \item \verb{R CMD BATCH script1.R}
  \item \verb{Rscript script1.R}
}

The first two are different ways of writing equivalent statements. The third
statement is the first statement plus options \option{--restore}
\option{--save} (plus option \option{--no-readline} under Unix-alikes), and it
also saves the \code{\link[base:showConnections]{stdout}} and
\code{\link[base:showConnections]{stderr}} in a file of your choosing. The
fourth statement is the second statement plus options \option{--no-echo}
\option{--no-restore}. You can try:

\itemize{
  \item \verb{R --help}
  \item \verb{R CMD BATCH --help}
  \item \verb{Rscript --help}
}

for a help message that describes what these options mean. In general,
\command{Rscript} is the one you want to use. It should be noted that
\command{Rscript} has some exclusive
\code{\link[base:EnvVar]{environment variables}} (not used by the other
executables) that will make its behaviour different from \command{R}.

For the purpose of making packages, \command{R CMD} is what you will need. Most
commonly, you will use:

\itemize{
  \item \verb{R CMD build}
  \item \verb{R CMD INSTALL}
  \item \verb{R CMD check}
}

\command{R CMD build} will turn an \R package (specified by a directory) into
tarball. This allows for easy sharing of \R packages with other people,
including
\href{https://cran.r-project.org/submit.html}{submitting a package to CRAN}.
\command{R CMD INSTALL} will install an \R package (specified by a directory
or tarball), and is used by
\code{\link[utils:install.packages]{utils::install.packages}()}.
\command{R CMD check} will check an \R package (specified by a tarball) for
possible errors in code, documentation, tests, and much more.

#ifdef windows
If, when you execute one of the previous commands, you see the following error
message: \dQuote{\sQuote{R} is not recognized as an internal or external
command, operable program or batch file.}, see section \strong{Ease of Use on
Windows}.
#endif
}
#ifdef windows
\section{Ease of Use on Windows}{
Under Unix-alikes, it is easy to invoke an \R session from a shell by typing
the name of the \R executable you wish to run. On Windows, you should see
that typing the name of the \R executable you wish to run does not run that
application, but instead signals an error. Instead, you will have to type the
full path of the directory where your \R executables are located (see section
\strong{Where are my R executable files located?}), followed by the name of the
\R executable you wish to run.

This is not very convenient to type everytime something needs to be run from a
shell, plus it has another issue of being computer dependent. The solution is
to add the path of the directory where your \R executables are located to the
\env{Path} environment variable. The \env{Path} environment variable is a list
of directories where executable programs are located. When you type the name of
an executable program you wish to run, Windows looks for that program through
each directory in the \env{Path} environment variable. When you add the full
path of the directory where your \R executables are located to your \env{Path}
environment variable, you should be able to run any of those executable
programs by their basenames (\sQuote{R}, \sQuote{Rcmd}, \sQuote{Rscript}, and
\sQuote{Rterm}) instead of their full paths.

To add a new path to your \env{Path} environment variable:
\enumerate{
  \item Open the \strong{Control Panel}
  \item Open category \strong{User Accounts}
  \item Open category \strong{User Accounts} (again)
  \item Open \strong{Change my environment variables}
  \item Click the variable \env{Path}
  \item Click the button \strong{Edit...}
  \item Click the button \strong{New}
  \item{
Type (or paste) the full path of the directory where your \R executables are
located, and press \strong{OK}
  }
}
This will modify your environment variable \env{Path}, not the systems. If
another user wishes to run \R from a shell, they will have to add the directory
to their \env{Path} environment variable as well.

If you wish to modify the system environment variable \env{Path} (you will need
admin permissions):
\enumerate{
  \item Open the \strong{Control Panel}
  \item Open category \strong{System and Security}
  \item Open category \strong{System}
  \item Open \strong{Advanced system settings}
  \item Click the button \strong{Environment Variables...}
  \item{
Modify \env{Path} same as before, just select \env{Path} in
\strong{System variables} instead of \strong{User variables}
  }
}

To check that this worked correctly, open a shell and execute the following
commands:

\itemize{
  \item \verb{R --help}
  \item \verb{R --version}
}

You should see that the first prints the usage message for the \R executable
while the second prints information about the version of \R currently being run.
If you have multiple versions of \R installed, make sure this is the version of
\R you wish to run.
}
#endif
\section{Where are my R executable files located?}{
In an \R session, you can find the location of your \R executable files with the
following command:

#ifdef unix
\verb{cat(sQuote(R.home("bin")), "\n")}
#endif
#ifdef windows
\verb{cat(sQuote(normalizePath(R.home("bin"))), "\n")}
#endif

For me, this is:

#ifdef unix
\file{\verb{/usr/lib/R/bin}}
#endif
#ifdef windows
\file{\verb{C:\Program Files\R\R-}\Sexpr{getRversion()}\verb{\bin\x64}}
#endif
}
