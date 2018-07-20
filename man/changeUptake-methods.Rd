\name{changeUptake-methods}

\docType{methods}
\encoding{utf8}

\alias{changeUptake}
\alias{changeUptake-methods}
\alias{changeUptake,modelorg-method}

\title{Change Uptake Reactions}

\description{
  Switch uptake reactions in metabolic networks on and off.
}

\usage{
\S4method{changeUptake}{modelorg}(object, off = NULL, on = NULL,
                                  rate = SYBIL_SETTINGS("MAXIMUM") * -1)
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "modelorg")}}{
      method to use with objects of class \code{\linkS4class{modelorg}}.
    }
  }
}

\arguments{
  \item{object}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{off}{
    A numeric or character vector or an object of class
    \code{\linkS4class{reactId_Exch}} containing the metabolite id's of
    metabolites to not use for uptake. If they have an exchange reaction with
    a lower bound less than zero, this lower bound is set to \code{0}.
    If \code{off} is set to \code{NULL}, all uptake reactions will be
    deactivated. If \code{off} is set to \code{FALSE}, no uptake reaction will
    be deactivated. If you just want to add an uptake reaction, set \code{off}
    to \code{FALSE}.\cr
    Default: \code{NULL}.
  }
  \item{on}{
    A numeric or character vector or an object of class
    \code{\linkS4class{reactId_Exch}} containing the metabolite id's of
    metabolites to use for uptake.\cr
    Default: \code{NULL}.
  }
  \item{rate}{
    A numeric vector containing the uptake rates for metabolites given in
    \code{on}.\cr
    Default: \code{SYBIL_SETTINGS("MAXIMUM") * -1}.
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\value{
  An object of class \code{\linkS4class{modelorg}}.
}

\seealso{
  Class \code{\linkS4class{modelorg}}
}

\keyword{methods}
