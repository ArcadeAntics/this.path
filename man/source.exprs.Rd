%%
%% this.path : Get Executing Script's Path
%% Copyright (C) 2023-2024   Iris Simmons
%%


\name{source.exprs}
\alias{source.exprs}
\title{Evaluate and Print Expressions}
\description{
  \code{source.exprs()} evaluates and auto-prints expressions as if in a
  toplevel context.
}
\usage{
source.exprs(exprs, evaluated = FALSE, envir = parent.frame(),
             echo = TRUE, print.eval = TRUE)
}
\arguments{
  \item{exprs, evaluated, envir, echo, print.eval}{See
    \codelink3{base:source}{withAutoprint}{()}.}
}
\keyword{internal}
