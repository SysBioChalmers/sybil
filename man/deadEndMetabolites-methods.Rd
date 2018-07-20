\name{deadEndMetabolites-methods}

\docType{methods}
\encoding{utf8}

\alias{deadEndMetabolites}
\alias{deadEndMetabolites-methods}
\alias{deadEndMetabolites,modelorg-method}

\title{Identify Dead End Metabolites}

\description{
  Search a metabolic network for metabolites, which are produced, but not
  consumed and vice versa.
}

\usage{
\S4method{deadEndMetabolites}{modelorg}(object,retIds)
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
  \item{retIds}{
    Boolean. If set to \code{TRUE}, a list containing metabolite id's will be
    returned, otherwise a list of logical vectors.\cr
    Default: \code{TRUE}.
  }
}

\value{
  A list will be returned:
    \item{dem}{dead end metabolites}
    \item{der}{reactions containing dead end metabolites}
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  Class \code{\linkS4class{modelorg}} and \code{\link{readTSVmod}}.
}

\keyword{methods}
