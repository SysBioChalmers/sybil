\name{changeColsBndsObjCoefs-methods}

\docType{methods}
\encoding{utf8}

\alias{changeColsBndsObjCoefs}
\alias{changeColsBndsObjCoefs-methods}

\alias{changeColsBndsObjCoefs,optObj_clpAPI-method}
\alias{changeColsBndsObjCoefs,optObj_cplexAPI-method}
\alias{changeColsBndsObjCoefs,optObj_glpkAPI-method}
\alias{changeColsBndsObjCoefs,optObj_lpSolveAPI-method}

\title{
  Change Column (Variable) Bounds and Objective Coefficients in the
  Optimization Problem
}

\description{
  Change column (variable) bounds and objective coefficients in the
  optimization problem.
}

\usage{
\S4method{changeColsBndsObjCoefs}{optObj_clpAPI}(lp, j, lb, ub, obj_coef)

\S4method{changeColsBndsObjCoefs}{optObj_cplexAPI}(lp, j, lb, ub, obj_coef)

\S4method{changeColsBndsObjCoefs}{optObj_glpkAPI}(lp, j, lb, ub, obj_coef)

\S4method{changeColsBndsObjCoefs}{optObj_lpSolveAPI}(lp, j, lb, ub, obj_coef)
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
  \item{lb}{
    A numeric vector of the same length as \code{j} containing the lower bounds
    of the variables to change.
  }
  \item{ub}{
    A numeric vector of the same length as \code{j} containing the upper bounds
    of the variables to change.
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
