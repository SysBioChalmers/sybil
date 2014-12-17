\name{optObj-class}

\Rdversion{1.1}
\encoding{utf8}
\docType{class}

\alias{optObj-class}

\alias{lpExtPtr-class}
\alias{pointerToProb-class}
\alias{pointerToProb}

\alias{clpPtr-class}
\alias{cplexPtr-class}
\alias{glpkPtr-class}
\alias{cplexPointer-class}

\alias{dim,optObj-method}
\alias{method,optObj-method}
\alias{solver,optObj-method}
\alias{probType,optObj-method}
\alias{probType}

\alias{checkSolStat}
\alias{getMeanReturn}
\alias{getMeanStatus}
\alias{wrong_type_msg}
\alias{wrong_solver_msg}


\title{Class \code{"optObj"}}

\description{
  Structure of the class \code{"optObj"}. Objects extending \code{optObj}
  returned by the constructor function \code{optObj}. These objects are used
  as part of class \code{\linkS4class{sysBiolAlg}}.
}

\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}

\section{Slots}{
  \describe{
    \item{\code{oobj}:}{
      Object of class \code{"pointerToProb"} containing a pointer to a problem
      object (see section Note).
    }
    \item{\code{solver}:}{
      Object of class \code{"character"} containing the name of the solver
      software (see \code{\link{SYBIL_SETTINGS}} for suitable values).
    }
    \item{\code{method}:}{
      Object of class \code{"character"} containing the method (algorithm) used
      by the solver software (see \code{\link{SYBIL_SETTINGS}} for suitable
      values).
    }
    \item{\code{probType}:}{
      Object of class \code{"character"} giving the problem type (see
      \code{\link{optObj}} argument \code{pType} for suitable values).
    }
  }
}

\section{Methods}{
  \describe{
    \item{\code{dim}}{\code{signature(x = "optObj")}:
      returns a vector \code{d} of length two with d[1] and d[2] containing the
      number of rows and columns of the constraint matrix.
    }
    \item{\code{method}}{\code{signature(object = "optObj")}:
      gets the \code{method} slot.
    }
    \item{\code{probType}}{\code{signature(object = "optObj")}:
      gets the \code{probType} slot.
    }
    \item{\code{solver}}{\code{signature(object = "optObj")}:
      gets the \code{solver} slot.
    }
  }
}

\section{Further usefull Functions}{
  \describe{
    \item{\code{checkSolStat}:}{
      \code{checkSolStat(stat, solver = SYBIL_SETTINGS("SOLVER"))}
      \cr
      Returns the indices of problems with a non-optimal solution status, or
      \code{NA} if it is not possible to retrieve a solution status.
      \describe{
        \item{\code{stat}}{
          Vector of integer values containing the solution status.
        }
        \item{\code{solver}}{
          Single character string specifying the used solver
          (see \code{\link{SYBIL_SETTINGS}}).
        }
      }
    }
    \item{\code{getMeanReturn}:}{
      \code{getMeanReturn(code, solver = SYBIL_SETTINGS("SOLVER"))}
      \cr
      Translates the return value (\code{code}) of a solver in a human
      readable string. Returns \code{NA} if hte translation is not possible.
    }
    \item{\code{getMeanStatus}:}{
      \code{getMeanStatus(code, solver = SYBIL_SETTINGS("SOLVER"), env = NULL)}
      \cr
      Translates the soluton status value (\code{code}) of a solver in a
      human readable string. Returns \code{NA} if hte translation is not
      possible. Argument \code{env} is for use with IBM ILOG CPLEX holding an
      object of class \code{cplexPtr} pointing to a IBM ILOG CPLEX environment.
    }
    \item{\code{wrong_type_msg}:}{
      \code{wrong_type_msg(lp)}
      \cr
      prints a warning message, if slot \code{oobj} from \code{lp} (an instance
      of class \code{optObj}) does not contain a pointer to a valid solver. See
      also \code{\link{SYBIL_SETTINGS}} for possible solvers.
    }
    \item{\code{wrong_solver_msg}:}{
      \code{wrong_solver_msg(lp, method, printOut = TRUE)}
      \cr
      if \code{printOut == TRUE}, it will print a warning message,
      if \code{method} is not available for \code{solver} in \code{lp}.
    }
  }
}

\section{Additional methods used by classes extending class \code{optObj}}{
  \describe{
    \item{\code{\link{addCols}}:}{
      add columns to the problem object.
    }
    \item{\code{\link{addRows}}:}{
      add rows to the problem object.
    }
    \item{\code{\link{addRowsCols}}:}{
      add rows and columns to the problem object.
    }
    \item{\code{\link{addColsToProb}}:}{
      add new columns (variables) to the problem object.
    }
    \item{\code{\link{addRowsToProb}}:}{
      add new rows (constraints) to the problem object.
    }
    \item{\code{\link{backupProb}}:}{
      copies a problem object into a new problem object.
    }
    \item{\code{\link{changeColsBnds}}:}{
      change column (variable) bounds in the problem object.
    }
    \item{\code{\link{changeColsBndsObjCoefs}}:}{
      change column (variable) bounds and objective coefficients in the
      problem object.
    }
    \item{\code{\link{changeMatrixRow}}:}{
      change a row in the constraint matrix of the problem object.
    }
    \item{\code{\link{changeObjCoefs}}:}{
      change objective coefficients in the problem object.
    }
    \item{\code{\link{changeRowsBnds}}:}{
        change row bounds in the problem object.
    }
    \item{\code{\link{delProb}}:}{
      delete (free) memory associated to the pointer to the problem object.
    }
    \item{\code{\link{getColPrim}}:}{
      get primal value of variables after optimization.
    }
    \item{\code{\link{getColsLowBnds}}:}{
      get lower bounds of variables.
    }
    \item{\code{\link{getColsUppBnds}}:}{
      get upper bounds of variables.
    }
    \item{\code{\link{getFluxDist}}:}{
      get all primal values of variables after optimization (resulting flux
      distribution).
    }
    \item{\code{\link{getNumCols}}:}{
      get number of columns in the problem object.
    }
    \item{\code{\link{getNumNnz}}:}{
      get number of non zero elements in the constraint matrix of the problem
      object.
    }
    \item{\code{\link{getNumRows}}:}{
      get number of rows in the problem object.
    }
    \item{\code{\link{getObjCoefs}}:}{
      get objective coefficients in the problem object.
    }
    \item{\code{\link{getObjDir}}:}{
      get direction of optimization.
    }
    \item{\code{\link{getObjVal}}:}{
      get value of the objective function after optimization.
    }
    \item{\code{\link{getRedCosts}}:}{
      get reduced costs of all variables after optimization.
    }
    \item{\code{\link{getRowsLowBnds}}:}{
      get lower row bounds of the problem object.
    }
    \item{\code{\link{getRowsUppBnds}}:}{
      get lower bounds of the rows (constraints) of the problem object.
    }
    \item{\code{\link{getSolStat}}:}{
      get solution status after optimization.
    }
    \item{\code{\link{getSolverParm}}:}{
      get current parameter settings of the used solver.
    }
    \item{\code{\link{initProb}}:}{
      initialize problem object.
    }
    \item{\code{\link{loadLPprob}}:}{
      load data to the problem object. Use this method to generate problem
      objects.
    }
    \item{\code{\link{loadQobj}}:}{
      load quadratic part of the objective function to the problem object.
    }
    \item{\code{\link{readProb}}:}{
        read problem object from file (e.g. lp formated).
    }
    \item{\code{\link{scaleProb}}:}{
      scaling of the constraint matrix.
    }
    \item{\code{\link{sensitivityAnalysis}}:}{
      perform sensitivity analysis.
    }
    \item{\code{\link{setObjDir}}:}{
      set direction of optimization.
    }
    \item{\code{\link{setRhsZero}}:}{
      set right hand side of the problem object to zero:
      \eqn{\mbox{\boldmath$Sv$\unboldmath} = 0}{Sv = 0}.
    }
    \item{\code{\link{setSolverParm}}:}{
      set parameters for the used solver.
    }
    \item{\code{\link{solveLp}}:}{
      run optimization with the solver mentioned in slot \code{\link{solver}} and with
      the method given by slot \code{\link{method}}.
    }
    \item{\code{\link{writeProb}}:}{
        write problem object to file (e.g. in lp format).
    }
  }
}

\details{
  The intention of class \code{optObj} is, to provide a flexible
  user interface to several optimization software products. The
  methods here working on the slot \code{oobj} are interface functions
  to low level functions invoking corresponding C functions.
  Basically, the user has not to care about the nature of the solver,
  or solver-specific functions. That is done by the class.
}

\note{
  The class \code{pointerToProb} contains an external pointer to a problem
  object (usually a C/C++ pointer). This is for \pkg{glpkAPI} an object of class
  \code{\link[glpkAPI]{glpkPtr}}, for \pkg{clpAPI} an object of class
  \code{\linkS4class{externalptr}}, for \pkg{lpSolveAPI} an object
  of class \code{lpExtPtr} and for \pkg{cplexAPI} an object of class
  \code{cplexPointer}.
  
  The class \code{cplexPointer} has two slots \code{env} and \code{lp}, each of
  class \code{cplexPtr}. %\code{\link[cplexAPI]{cplexPtr}}.
  To access for example the environment
  pointer from an object of class \code{optObj}, one can write
  \code{lp@oobj@env}.
}

\author{Gabriel Gelius-Dietrich}

\seealso{
  The constructor function \code{\link{sysBiolAlg}} for objects extending
  class \code{\linkS4class{sysBiolAlg}};
  The constructor function \code{\link{optObj}}; \code{\link{SYBIL_SETTINGS}}
  and \code{\link{checkDefaultMethod}}.
}

\examples{
  showClass("optObj")
}

\keyword{classes}
