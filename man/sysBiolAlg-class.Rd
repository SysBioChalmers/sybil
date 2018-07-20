\name{sysBiolAlg-class}

\Rdversion{1.1}
\encoding{utf8}

\docType{class}

\alias{sysBiolAlg-class}
\alias{algorithm,sysBiolAlg-method}
\alias{algorithm<-,sysBiolAlg-method}
\alias{fldind,sysBiolAlg-method}
\alias{fldind<-,sysBiolAlg-method}
\alias{nc,sysBiolAlg-method}
\alias{nc}
\alias{nc<-,sysBiolAlg-method}
\alias{nc<-}
\alias{nr,sysBiolAlg-method}
\alias{nr}
\alias{nr<-,sysBiolAlg-method}
\alias{nr<-}
\alias{alg_par,sysBiolAlg-method}
\alias{alg_par}
\alias{alg_par<-,sysBiolAlg-method}
\alias{alg_par<-}
\alias{problem,sysBiolAlg-method}
\alias{problem}

\title{Class \code{"sysBiolAlg"}}

\description{
  The class \code{sysBiolAlg} holds an object of class
  \code{\linkS4class{optObj}} which is generated concerning a particular
  algorithm, e.g. FBA or ROOM. This class is extended by other classes and will
  not be used as is. The representation of class \code{sysBiolAlg} is used as
  superclass.
}

\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}

\section{Slots}{
  \describe{
    \item{\code{problem}:}{Object of class \code{"optObj"}
      containing the problem object.
    }
    \item{\code{algorithm}:}{Object of class \code{"character"}
      containing the name of the algorithm.
    }
    \item{\code{nr}:}{Object of class \code{"integer"}
      containing the number of rows of the problem object.
    }
    \item{\code{nc}:}{Object of class \code{"integer"}
      containing the number of columns of the problem object
    }
    \item{\code{fldind}:}{Object of class \code{"integer"}
      pointers to columns (variables) representing a flux (reaction) in the
      original network. The variable \code{fldind[i]} in the problem object
      represents reaction \code{i} in the original network.
    }
    \item{\code{alg_par}:}{Object of class \code{"list"}
      containing a named list of algorithm specific parameters.
    }
  }
}

\section{Methods}{
  \describe{
    \item{algorithm}{\code{signature(object = "sysBiolAlg")}:
      gets the \code{algorithm} slot.
    }
    \item{algorithm<-}{\code{signature(object = "sysBiolAlg")}:
      sets the \code{algorithm} slot.
    }
    \item{alg_par}{\code{signature(object = "sysBiolAlg")}:
      gets the \code{alg_par} slot.
    }
    \item{alg_par<-}{\code{signature(object = "sysBiolAlg")}:
      sets the \code{alg_par} slot.
    }
    \item{fldind}{\code{signature(object = "sysBiolAlg")}:
      gets the \code{fldind} slot.
    }
    \item{fldind<-}{\code{signature(object = "sysBiolAlg")}:
      sets the \code{fldind} slot.
    }
    \item{nc}{\code{signature(object = "sysBiolAlg")}:
      gets the \code{nc} slot.
    }
    \item{nc<-}{\code{signature(object = "sysBiolAlg")}:
      sets the \code{nc} slot.
    }
    \item{nr}{\code{signature(object = "sysBiolAlg")}:
      gets the \code{nr} slot.
    }
    \item{nr<-}{\code{signature(object = "sysBiolAlg")}:
      sets the \code{nr} slot.
    }
    \item{optimizeProb}{\code{signature(object = "sysBiolAlg")}:
      runs optimization on the given problem object
      (see \code{\link{optimizeProb}} for details).
    }
    \item{problem}{\code{signature(object = "sysBiolAlg")}:
      gets the \code{problem} slot.
    }
    \item{initialize}{\code{signature(object = "sysBiolAlg")}:
      default constructor method for objects inheriting from class
      \code{sysBiolAlg}. It gets all data structures necessary to built a
      problem object (object of class \code{\linkS4class{optObj}})
      representing a particular algorithm. This method can be used in
      constructor methods for subclasses of \code{sysBiolAlg} via
      \code{\link{callNextMethod}}. In this case, the constructor has to
      generate all the data structures, pass them to
      \code{\link{callNextMethod}} and let the constructor of the superclass do
      all the work in generating the problem object and interacting with the
      solver software. See also the Details section.
    }
  }
}

\details{
  The \code{initialize} method has the following arguments:
  \describe{
    \item{solver}{
      Single character string giving the solver package to use.  See
      \code{\link{SYBIL_SETTINGS}} for possible values.\cr
      Default: \code{SYBIL_SETTINGS("SOLVER")}.
    }
    \item{method}{
      Single character string giving the method the desired solver has to use.
      \code{\link{SYBIL_SETTINGS}} for possible values.\cr
      Default: \code{SYBIL_SETTINGS("METHOD")}.
    }
    \item{solverParm}{
      A named data frame or list containing parameters for the specified
      solver.  Parameters can be set as data frame or list:
      \code{solverParm = list(parm1 = val1, parm2 = val2)} with \code{parm1} and
      \code{parm2} being the names of two different parameters and \code{val1}
      and \code{val2} the corresponding values. For possible parameters and
      values see the documentation of the used solver package
      (e.g. \pkg{glpkAPI}).\cr
      Default: \code{SYBIL_SETTINGS("SOLVER_CTRL_PARM")}.
    }
    \item{termOut}{
      A single boolean, numeric or character value, controling the amount of
      terminal output of the solver software. See also \code{\link{initProb}}
      (argument \code{to}) for more details.\cr
      Default: \code{NULL}.
    }
    \item{sbalg}{
      Single character string containing the name of the algorithm to use.
    }
    \item{pType}{
      Single character string containing the type of the problem object. Can be
      \code{"lp"}: linear program, \code{mip}: mixed integer program or
      \code{"qp"}: quadratic program.\cr
      Default: \code{"lp"}.
    }
    \item{scaling}{
      Scaling options used to scale the constraint matrix. If set to
      \code{NULL}, no scaling will be performed
      (see \code{\link{scaleProb}}).\cr
      Default: \code{NULL}.
    }
    \item{fi}{
      Pointers to columns (variables) representing a flux (reaction) in the
      original network. The variable \code{fldind[i]} in the problem object
      represents reaction \code{i} in the original network. 
    }
    \item{nCols}{
      Number of columns (variables) of the problem object.
    }
    \item{nRows}{
      Number of rows (constraints) of the problem object.
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
    }
    \item{rtype}{
      A character vector of length \code{nRows} giving the constraint type.
      See \code{\link{loadLPprob}} for details.
    }
    \item{lpdir}{
      Single character string containing the direction of optimization.
      Can be set to \code{"min"} or \code{"max"}.\cr
      Default: \code{"max"}.
    }
    \item{rub}{
      A numeric vector of length \code{nRows} giving the right hand side of the
      problem object. If not \code{NULL}, it contains the upper bounds of the
      constraints of the problem object.\cr
      Default: \code{NULL}.
    }
    \item{ctype}{
      A character vector of length \code{nCols} giving the variable type. If set
      to \code{NULL}, no specific variable type is set, which usually means, all
      variables are treated as continuous variables.
      See \code{\link{loadLPprob}} for details.\cr
      Default: \code{NULL}.
    }
    \item{cnames}{
      A character vector of length \code{nCols} giving the variable names.
      If set to \code{NULL}, no specific variable names are set.\cr
      Default: \code{NULL}.
    }
    \item{rnames}{
      A character vector of length \code{nRows} giving the constraint names.
      If set to \code{NULL}, no specific constraint names are set.\cr
      Default: \code{NULL}.
    }
    \item{pname}{
      A single character string containing a name for the problem object.\cr
      Default: \code{NULL}.
    }
    \item{retAlgPar}{
      A single boolean flag, if algorithm specific parameters should be saved
      in the object extending class \code{sysBiolAlg}.\cr
      Default: \code{TRUE}.
    }
    \item{algPar}{
      A named list containing algorithm specific parameters.\cr
      Default: \code{NULL}.
    }
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  The general constructor function \code{\link{sysBiolAlg}},
  and classes
  \code{\linkS4class{sysBiolAlg_fba}},
  \code{\linkS4class{sysBiolAlg_fv}},
  \code{\linkS4class{sysBiolAlg_mtf}},
  \code{\linkS4class{sysBiolAlg_lmoma}},
  \code{\linkS4class{sysBiolAlg_moma}} and
  \code{\linkS4class{sysBiolAlg_room}}.
}

\examples{
  showClass("sysBiolAlg")
}

\keyword{classes}
\keyword{optimize}
