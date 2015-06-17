\name{sensitivityAnalysis-methods}

\docType{methods}
\encoding{utf8}

\alias{sensitivityAnalysis}
\alias{sensitivityAnalysis-methods}

\alias{sensitivityAnalysis,optObj_cplexAPI-method}
\alias{sensitivityAnalysis,optObj_glpkAPI-method}

\title{Sensitivity Analysis}

\description{
  Perform sensitivity analysis.
}

\usage{
\S4method{sensitivityAnalysis}{optObj_cplexAPI}(lp, ...)

\S4method{sensitivityAnalysis}{optObj_glpkAPI}(lp, ...)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_cplexAPI")}}{
      method to use with package \pkg{optObj_cplexAPI}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{...}{
    Further arguments passed to the initialization function of the solver
    package.
  }
}

\value{
  The \pkg{glpkAPI} method generates a file \code{"sar.txt"} and the
  \pkg{cplexAPI} method returns a list.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
}

\keyword{methods}
\keyword{optimize}
