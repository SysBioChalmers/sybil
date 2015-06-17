\name{loadLPprob-methods}

\docType{methods}
\encoding{utf8}

\alias{loadLPprob}
\alias{loadLPprob-methods}
\alias{loadLPprob,optObj_clpAPI-method}
\alias{loadLPprob,optObj_cplexAPI-method}
\alias{loadLPprob,optObj_glpkAPI-method}
\alias{loadLPprob,optObj_lpSolveAPI-method}

\title{Load Data to Optimization Problem}

\description{
  Load data to the problem object (extending class
  \code{\linkS4class{optObj}}). Use this method to generate problem
  objects.
}

\usage{
\S4method{loadLPprob}{optObj_clpAPI}(lp,
           nCols, nRows, mat, ub, lb, obj, rlb, rtype,
           lpdir = "max", rub = NULL, ctype = NULL,
           cnames = NULL, rnames = NULL, pname = NULL,
           defLowerBnd = SYBIL_SETTINGS("MAXIMUM") * -1,
           defUpperBnd = SYBIL_SETTINGS("MAXIMUM")
)

\S4method{loadLPprob}{optObj_cplexAPI}(lp,
           nCols, nRows, mat, ub, lb, obj, rlb, rtype,
           lpdir = "max", rub = NULL, ctype = NULL,
           cnames = NULL, rnames = NULL, pname = NULL)

\S4method{loadLPprob}{optObj_glpkAPI}(lp,
           nCols, nRows, mat, ub, lb, obj, rlb, rtype,
           lpdir = "max", rub = NULL, ctype = NULL,
           cnames = NULL, rnames = NULL, pname = NULL)

\S4method{loadLPprob}{optObj_lpSolveAPI}(lp,
           nCols, nRows, mat, ub, lb, obj, rlb, rtype,
           lpdir = "max", rub = NULL, ctype = NULL,
           cnames = NULL, rnames = NULL, pname = NULL)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI")}}{
      method to use with package \pkg{clpAPI}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI")}}{
      method to use with package \pkg{cplexAPI}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI")}}{
      method to use with package \pkg{glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI")}}{
      method to use with package \pkg{lpSolveAPI}.
    }
  }
}

\arguments{
  \item{lp}{
    An object of class
    \code{\linkS4class{optObj_clpAPI}},
    \code{\linkS4class{optObj_cplexAPI}},
    \code{\linkS4class{optObj_glpkAPI}} or
    \code{\linkS4class{optObj_lpSolveAPI}}.
  }
  \item{nCols}{
    Number of columns (variables) of the constraint matrix.
  }
  \item{nRows}{
    Number of rows (constraints) of the constraint matrix.
  }
  \item{mat}{
    An object of class \code{\link{Matrix}}. The constraint matrix of the
    problem object. The number of columns in \code{mat} must be \code{nCols}
    and the number of rows in \code{mat} must be \code{nRows}.
  }
  \item{ub}{
    A numeric vector of length \code{nCols} giving the upper bounds of the
    variables of the problem object.
  }
  \item{lb}{
    A numeric vector of length \code{nCols} giving the lower bounds of the
    variables of the problem object.
  }
  \item{obj}{
    A numeric vector of length \code{nCols} giving the objective coefficients
    of the variables of the problem object.
  }
  \item{rlb}{
    A numeric vector of length \code{nRows} giving the right hand side of the
    problem object. If argument \code{rub} is not \code{NULL}, \code{rlb}
    contains the lower bounds of the constraints of the problem object.
    See Details.
  }
  \item{rtype}{
    A character vector of length \code{nRows} giving the constraint type:
    \tabular{lll}{
      \code{"F"}: \tab free constraint (GLPK only)          \tab \eqn{-\infty < x < \infty}{-INF < x < INF}               \cr
      \code{"L"}: \tab constraint with lower bound          \tab \eqn{\mathrm{lb} \leq x < \infty}{lb <= x < INF}         \cr
      \code{"U"}: \tab constraint with upper bound          \tab \eqn{-\infty < x \leq \mathrm{ub}}{-INF < x <= ub}       \cr
      \code{"D"}: \tab double-bounded (ranged) constraint   \tab \eqn{\mathrm{lb} \leq x \leq \mathrm{ub}}{lb <= x <= ub} \cr
      \code{"E"}: \tab fixed (equality) constraint          \tab \eqn{\mathrm{lb} = x = \mathrm{ub}}{lb = x = ub}         \cr
    }
    If \code{rtype[i]} is not one of \code{"F"}, \code{"L"}, \code{"U"},
    \code{"D"} or \code{"E"}, the value of \code{rtype[i]} will be set to
    \code{"E"}. See Details.
  }
  \item{lpdir}{
    Single character string containing the direction of optimization.
    Can be set to \code{"min"} or \code{"max"}.\cr
    Default: \code{"max"}.
  }
  \item{rub}{
    A numeric vector of length \code{nRows} giving the right hand side of the
    problem object. If not \code{NULL}, it contains the upper bounds of the
    constraints of the problem object. See Details.\cr
    Default: \code{NULL}.
  }
  \item{ctype}{
    A character vector of length \code{nCols} giving the variable type. If set
    to \code{NULL}, no specific variable type is set, which usually means, all
    variables are treated as continuous variables.\cr
    Default: \code{NULL}.
    \tabular{lll}{
      \code{"C"}: \tab continuous variable  \cr
      \code{"B"}: \tab binary variable     \cr
      \code{"I"}: \tab integer variable    \cr
      \code{"S"}: \tab semi-continuous variable  \cr
      \code{"N"}: \tab semi-integer variable    \cr
    }
    Values \code{"S"} and \code{"N"} are not available for every solver
    software. Check documentation of the solver software if semi-continuous and
    semi-integer variables are supported.
    If \code{ctype[j]} is not \code{"C"}, \code{"B"}, \code{"I"}, \code{"S"},
    or \code{"N"}, the value of \code{ctype[j]} will be set to \code{"C"}.
  }
  \item{cnames}{
    A character vector of length \code{nCols} containing symbolic names for
    the variable of the problem object.\cr
    Default: \code{NULL}.
  }
  \item{rnames}{
    A character vector of length \code{nRows} containing symbolic names for
    the constraints of the problem object.\cr
    Default: \code{NULL}.
  }
  \item{pname}{
    A single character string containing a name for the problem object.\cr
    Default: \code{NULL}.
  }
  \item{defLowerBnd}{
    For the \code{\linkS4class{optObj_clpAPI}} method only: a single numeric
    value containing a default value for an lower bound to a constraint in an
    optimization problem.\cr
    Default: \code{SYBIL_SETTINGS("MAXIMUM") * -1}.
  }
  \item{defUpperBnd}{
    For the \code{\linkS4class{optObj_clpAPI}} method only: a single numeric
    value containing a default value for an upper bound to a constraint in an
    optimization problem.\cr
    Default: \code{SYBIL_SETTINGS("MAXIMUM")}.
  }
}

\details{
  Method \code{loadLPprob} can be used any time after a problem object is
  initialized by \code{\link{initProb}}.
  
  In order so set constraints, usually only parameter \code{rlb} is required
  and parameter \code{rub} can be left at \code{NULL} (which is the default).
  If \code{rub} is not \code{NULL}, \code{rlb} and \code{rub} must have the same
  length. Parameter \code{rub} is required, if a particular constraint is a
  ranged or double bounded constraint. The general idea is, for any constraint
  \code{i}, the value in \code{rlb[i]} gives the lower bound and the value in
  \code{rub[i]} gives the upper bound. If the constraints of the optimization
  problem do only have one bound (type \code{"L"}, \code{"U"} and \code{"E"}),
  all bounds can be set via \code{rlb} and \code{rub} is not required. If any
  constraint is of type \code{"D"} (a double-bounded or ranged constraint)
  additionally \code{rub} is required. It is of course also possible to use
  \code{rlb} strictly for all lower bounds and \code{rub} for all upper bounds.
  Again, if both \code{rlb} and \code{rub} are given (not \code{NULL}), they
  must have the same length. For equality constraints (type \code{"E"}), allways
  the value in \code{rlb} is used.
  
  For the \code{\linkS4class{optObj_cplexAPI}} method: CPLEX uses so called
  ranged constraints for double bounded constraints. The values in \code{rlb}
  and \code{rub} will be transformed into range values for ranged constraints.
  The range for a ranged constraint \eqn{i} is given as
  \code{abs(rub[i] - rlb[i])}, so that the valid interval is denoted as
  \code{[rlb[i], rlb[i] + range]}.
  
  For the \code{\linkS4class{optObj_glpkAPI}} method: if \code{cnames} or
  \code{rnames} is not \code{NULL}, an index will be created.

  For the \code{\linkS4class{optObj_clpAPI}} method: if \code{cnames} is
  not \code{NULL}, \code{rnames} must be also not \code{NULL} and vice versa.

  For the \code{\linkS4class{optObj_lpSolveAPI}} method: if \code{cnames} is
  not \code{NULL}, \code{rnames} must be also not \code{NULL} and vice versa.
  Round brackets (\code{"("} and \code{")"}) will be replaced by underscores
  \code{"_"}.
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
