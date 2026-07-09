%%
%% this.path : Get Executing Script's Path
%% Copyright (C) 2026   Iris Simmons
%%


\name{URL_encode}
\alias{URL_encode}
\alias{URL_decode}
\title{Encode or Decode (partial) URLs}
\description{
  Functions to percent-encode or decode characters in URLs.
}
\usage{
URL_encode(URL, reserved = FALSE, repeated = FALSE)
URL_decode(URL)
}
\arguments{
  \item{URL}{character vector.}

  \item{reserved}{\code{TRUE} or \code{FALSE}; should \sQuote{reserved}
    characters be percent-encoded?}

  \item{repeated}{\code{TRUE} or \code{FALSE}; should apparently
    already-encoded URLs be encoded again?}
}
\details{
  Characters in a URL other than the English alphanumeric characters and
  \samp{- _ . ~} should be encoded as \samp{\%} plus a two-digit hexadecimal
  representation, and any single-byte character can be so encoded. Multi-byte
  characters are encoded byte-by-byte. The standard refers to this as
  \sQuote{percent-encoding}.

  In addition, \samp{! $ & ' ( ) * + , ; = : / ? @ # [ ]} are reserved
  characters, and should be encoded unless used in their reserved sense, which
  is scheme specific.  The default in \code{URLencode} is to leave them alone,
  which is appropriate for \samp{file://} URLs, but probably not for
  \samp{http://} ones.

  An \sQuote{apparently already-encoded URL} is one containing \samp{\%xx} for
  two hexadecimal digits.
}
\value{
  A character vector.
}
