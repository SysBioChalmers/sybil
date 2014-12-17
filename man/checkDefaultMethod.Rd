\name{checkDefaultMethod}
\alias{checkDefaultMethod}

\encoding{utf8}

\title{
  Validate Solver and Method
}

\description{
  The function \code{checkDefaultMethod} returns the default method
  for a desired solver, or a default solver -- method pair. A \dQuote{solver}
  is always the name of a R package offering facilities for solving
  optimization problems.
}

\usage{
  checkDefaultMethod(solver, method, probType, loadPackage = TRUE)
}

\arguments{
  \item{solver}{
    A single character string, containing the solver name (must be identical to
    the name of an R-package), see \code{\link{SYBIL_SETTINGS}}.
  }
  \item{method}{
    A single character string, containing the method name, see
    \code{\link{SYBIL_SETTINGS}}.
  }
  \item{probType}{
    A single character string, containing the problem type, see
    \code{\link{optObj}}.
  }
  \item{loadPackage}{
    A single Boolean value. If set to \code{TRUE}, load the given solver
    package via \code{\link{require}}.
  }
}

\details{
  In order to run simulations (optimizations) with sybil, additional software
  offering facilities for solving optimization problems is required. Supported
  R packages are described in \code{\link{SYBIL_SETTINGS}}.
  At first, the function checks if argument \code{solver} contains a valid
  solver. If that is not the case, a corresponding library will be loaded, if
  one exists (this library must have the same name as given in \code{solver}).
  If this failes too, the default solver will be returned
  (see \code{\link{SYBIL_SETTINGS}}). Next the same is done for the argument
  \code{method}, regarding the current value of \code{solver}.
  Additionally, it will be checked, wether or not the given problem type can be
  solved using the given method and solver.
}

\value{
  \item{sol}{Validated solver name.}
  \item{met}{Validated method name.}
  \item{parm}{Default prarmeter set for the validated method.}
}

\note{
  Arguments \code{"glpk"}, \code{"cplex"} and \code{"clp"} not used anymore;
  valid arguments must be the name of the desired solver package like
  \code{"glpkAPI"}, \code{"cplexAPI"} and \code{"cplAPI"}.
}

\author{
  Gabriel Gelius-Dietrich

  Maintainer: Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>
}

\seealso{
  \code{\link{SYBIL_SETTINGS}} and \code{\link{getsybilenv}}
}

\keyword{ optimize }
