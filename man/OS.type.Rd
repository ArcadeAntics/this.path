\name{OS.type}
\alias{OS.type}
\title{
Detect the Operating System Type
}
\description{
\code{OS.type} is a list of \code{TRUE} / / \code{FALSE} values dependent on
the platform under which this package was built.
}
\usage{
OS.type
}
\value{
A list with at least the following components:

\item{AIX}{
Built under IBM AIX.
}
%
%\item{BSD}{
%Built under BSD (DragonFly BSD, FreeBSD, OpenBSD, NetBSD).
%}

\item{HPUX}{
Built under Hewlett-Packard HP-UX.
}

\item{linux}{
Built under some distribution of Linux.
}

\item{darwin}{
Built under Apple OSX and iOS (Darwin).
}

\item{iOS.simulator}{
Built under iOS in Xcode simulator.
}

\item{iOS}{
Built under iOS on iPhone, iPad, etc.
}

\item{macOS}{
Built under OSX.
}

\item{solaris}{
Built under Solaris (SunOS).
}

\item{cygwin}{
Built under Cygwin POSIX under Microsoft Windows.
}

\item{windows}{
Built under Microsoft Windows.
}

\item{win64}{
Built under Microsoft Windows (64-bit).
}

\item{win32}{
Built under Microsoft Windows (32-bit).
}

\item{UNIX}{
Built under a UNIX-style OS.
}
%
%\item{POSIX}{
%Built under a POSIX compliant OS.
%}
}
\source{
\href{http://web.archive.org/web/20191012035921/http://nadeausoftware.com/articles/2012/01/c_c_tip_how_use_compiler_predefined_macros_detect_operating_system}{http://web.archive.org/web/20191012035921/http://nadeausoftware.com/articles/2012/01/c_c_tip_how_use_compiler_predefined_macros_detect_operating_system}
}
