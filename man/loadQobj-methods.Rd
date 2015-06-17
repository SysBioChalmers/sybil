\name{loadQobj-methods}

\docType{methods}
\encoding{utf8}

\alias{loadQobj}
\alias{loadQobj-methods}

\alias{loadQobj,optObj_cplexAPI,Matrix-method}
\alias{loadQobj,optObj_cplexAPI,numeric-method}

\title{Load Quadratic Part of the Objective Function to the Optimization Problem}

\description{
 load quadratic part of the objective function to the optimization problem.
}

\usage{
\S4method{loadQobj}{optObj_cplexAPI,Matrix}(lp, mat)
\S4method{loadQobj}{optObj_cplexAPI,numeric}(lp, mat)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_cplexAPI", mat = "Matrix")}}{
      method to use with package \pkg{optObj_cplexAPI} and if \code{mat} is
      of class \code{\linkS4class{Matrix}}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", mat = "numeric")}}{
      method to use with package \pkg{optObj_cplexAPI} and if \code{mat} is
      a \code{numeric} vector.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{mat}{
    An object of class \code{\link{Matrix}} or a \code{numeric} vector
    containing the quadratic objective Matrix \eqn{Q}.
  }
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
