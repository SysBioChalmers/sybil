\name{changeObjCoefs-methods}

\docType{methods}
\encoding{utf8}

\alias{changeObjCoefs}
\alias{changeObjCoefs-methods}

\alias{changeObjCoefs,optObj_clpAPI-method}
\alias{changeObjCoefs,optObj_cplexAPI-method}
\alias{changeObjCoefs,optObj_glpkAPI-method}
\alias{changeObjCoefs,optObj_lpSolveAPI-method}

\title{
  Change Column (Variable) Objective Coefficients in the Optimization Problem
}

\description{
  Change column (variable) objective coefficients in the optimization problem.
}

\usage{
\S4method{changeObjCoefs}{optObj_clpAPI}(lp, j, obj_coef)

\S4method{changeObjCoefs}{optObj_cplexAPI}(lp, j, obj_coef)

\S4method{changeObjCoefs}{optObj_glpkAPI}(lp, j, obj_coef)

\S4method{changeObjCoefs}{optObj_lpSolveAPI}(lp, j, obj_coef)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI")}}{
      method to use with package \pkg{optObj_clpAPI}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI")}}{
      method to use with package \pkg{optObj_cplexAPI}.
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
  \item{j}{
    A numeric vector containing the column indices of the variables to change.
  }
  \item{obj_coef}{
    A numeric vector of the same length as \code{j} containing the objective
    coefficients of the variables to change.
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
