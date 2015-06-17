\name{getSolverParm-methods}

\docType{methods}
\encoding{utf8}

\alias{getSolverParm}
\alias{getSolverParm-methods}

\alias{getSolverParm,optObj_clpAPI-method}
\alias{getSolverParm,optObj_cplexAPI-method}
\alias{getSolverParm,optObj_glpkAPI-method}
\alias{getSolverParm,optObj_lpSolveAPI-method}

\title{Retrieve Current Parameter Settings Used By The Optimization Software}

\description{
  Retrieve current parameter settings used by the optimization software.
}

\usage{
\S4method{getSolverParm}{optObj_clpAPI}(lp)

\S4method{getSolverParm}{optObj_cplexAPI}(lp)

\S4method{getSolverParm}{optObj_glpkAPI}(lp)

\S4method{getSolverParm}{optObj_lpSolveAPI}(lp)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI")}}{
      method to use with package \pkg{optObj_clpAPI}. This method is currently
      unused. It is not possible to provide parameters for package \pkg{clpAPI}.
      Always \code{FALSE} will be returned.
    }
    \item{\code{signature(lp = "optObj_cplexAPI")}}{
      method to use with package \pkg{optObj_cplexAPI}. This method writes
      the current parameter settings to the file \code{"cplex_parameters.prm"}.
      The method returns zero if successfull, otherwise non-zero.
    }
    \item{\code{signature(lp = "optObj_glpkAPI")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI")}}{
      method to use with package \pkg{optObj_lpSolveAPI}.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
}

\value{
  Returns a list containing the current parameter settings or zero/non-zero.
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
