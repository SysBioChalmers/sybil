\name{singletonMetabolites-methods}

\docType{methods}
\encoding{utf8}

\alias{singletonMetabolites}
\alias{singletonMetabolites-methods}
\alias{singletonMetabolites,modelorg-method}

\title{Identify Singleton Metabolites}

\description{
  Search a metabolic network for metabolites, which appear only once in the
  stoichiometric matrix.
}

\usage{
\S4method{singletonMetabolites}{modelorg}(object,tol,retIds)
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "modelorg")}}{
      method to use with class \code{\linkS4class{modelorg}}.
    }
  }
}

\arguments{
  \item{object}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{tol}{
    A numeric tolerance value: an entry of the stoichiometric matrix \eqn{s_ij}
    is considered to be non-zero if \eqn{abs(s_ij) > tol} is \code{TRUE}.\cr
    Default: \code{SYBIL_SETTINGS("TOLERANCE")}.
  }
  \item{retIds}{
    Boolean. If set to \code{TRUE}, a list containing metabolite id's will be
    returned, otherwise a list of logical vectors.\cr
    Default: \code{TRUE}.
  }
}

\value{
  A list will be returned:
    \item{smet}{singleton metabolites}
    \item{sreact}{reactions containing singleton metabolites}
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  Class \code{\linkS4class{modelorg}} and \code{\link{readTSVmod}}.
}

\keyword{methods}
