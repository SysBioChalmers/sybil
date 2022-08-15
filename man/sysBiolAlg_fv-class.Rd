\name{sysBiolAlg_fv-class}

\Rdversion{1.1}
\encoding{utf8}

\docType{class}

\alias{sysBiolAlg_fv-class}
\alias{sysBiolAlg_fv}
\alias{fv}

\title{Class \code{"sysBiolAlg_fv"}}

\description{
  The class \code{sysBiolAlg_fv} holds an object of class
  \code{\linkS4class{optObj}} which is generated to meet the
  requirements of the flux variance algorithm.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  
  \code{sysBiolAlg(model, algorithm = "fv", ...)}.
  
  Arguments to \code{...} which are passed to method \code{initialize} of class
  \code{sysBiolAlg_fv} are described in the Details section.
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
      containing a named list containing algorithm specific parameters.
    }
  }
}

\section{Extends}{
  Class \code{"\linkS4class{sysBiolAlg}"}, directly.
}

\section{Methods}{
  No methods defined with class "sysBiolAlg_fv" in the signature.
}

\details{
  The \code{initialize} method has the following arguments:
  \describe{
    \item{model}{
      An object of class \code{\linkS4class{modelorg}}.
    }
    \item{percentage}{
      Consider solutions with \code{x} percent of the optimal solution.\cr
      Default: \code{100}.
    }
    \item{Zopt}{
      A single numeric value giving the optimal value to be fixed during all
      other optimizations (see argument \code{fixObjVal}). If \code{Zopt} is set
      to \code{NULL} and \code{model} has an objective function, a default value
      is computed based on FBA. If given, arguments \code{solver}, \code{method}
      and \code{solverParm} are used during FBA.\cr
      Default: \code{NULL}.
    }
    \item{fixObjVal}{
      A single Boolean value. If set to \code{TRUE} and if the model contains
      an objective function, an optimal value of this objective function will be
      fixed during all other optimizations. The optimal value can be controlled
      by argument \code{Zopt}.\cr
      Default: \code{TRUE}.
    }
    \item{tol}{
      Single numeric value giving the tolerance value.\cr
      Default: \code{SYBIL_SETTINGS("TOLERANCE")}.
    }
    \item{lpdir}{
      Single character string containing the direction of optimization.
      Can be set to \code{"min"} or \code{"max"}.\cr
      Default: \code{SYBIL_SETTINGS("OPT_DIRECTION")}.
    }
    \item{useNames}{
      A single boolean value. If set to \code{TRUE}, variables and constraints
      will be named according to \code{cnames} and \code{rnames}. If set to
      \code{NULL}, no specific variable or constraint names are set.\cr
      Default: \code{SYBIL_SETTINGS("USE_NAMES")}.
    }
    \item{cnames}{
      A character vector giving the variable names. If set to \code{NULL},
      the reaction id's of \code{model} are used.\cr
      Default: \code{NULL}.
    }
    \item{rnames}{
      A character vector giving the constraint names. If set to \code{NULL},
      the metabolite id's of \code{model} are used. If an objective value has
      to be fixed (see argument \code{fixObjVal}), the corresponding constrained
      is named \code{"Z"}.\cr
      Default: \code{NULL}.
    }
    \item{pname}{
      A single character string containing a name for the problem object.\cr
      Default: \code{NULL}.
    }
    \item{scaling}{
      Scaling options used to scale the constraint matrix. If set to
      \code{NULL}, no scaling will be performed
      (see \code{\link{scaleProb}}).\cr
      Default: \code{NULL}.
    }
    \item{writeProbToFileName}{
      A single character string containing a file name to which the problem
      object will be written in LP file format.\cr
      Default: \code{NULL}.
    }
    \item{...}{
      Further arguments passed to the initialize method of
      \code{\linkS4class{sysBiolAlg}}. They are \code{solver},
      \code{method} and \code{solverParm}.
    }
  }

  The problem object is built to be capable to perform the flux variance
  algorithm with a given model, which is basically the solution of a linear
  program
  \deqn{%
      \begin{array}{rll}%
          \max \textrm{ or } \min & v_i                                 \\[1ex]
          \mathrm{s.\,t.} & Z = Z_{\mathrm{opt}}                        \\[1ex]
                          & \mbox{$Sv$} = 0         \\[1ex]
                          & \alpha_i \leq v_i \leq \beta_i
                            & \quad \forall i \in \{1, \ldots, n\}      \\[1ex]
      \end{array}%
  }{
      max or min v[i]
      s.t. Z = Z[opt]
           Sv = 0
           a_i <= v_i <= b_i  for i = 1, ..., n
  }
  with \eqn{\bold{S}}{S} being the stoichiometric matrix, \eqn{\alpha_i}{a_i}
  and \eqn{\beta_i}{b_i} being the lower and upper bounds for flux (variable)
  \eqn{i}.  The total number of variables of the optimization problem is denoted
  by \eqn{n}.  
  The optimization can be executed by using \code{\link{optimizeProb}}.
}

\references{
  Becker, S. A., Feist, A. M., Mo, M. L., Hannum, G., Palsson, B. Ø.  and
  Herrgard, M. J. (2007) Quantitative prediction of cellular metabolism with
  constraint-based models: the COBRA Toolbox. \emph{Nat Protoc} \bold{2},
  727--738.

  Schellenberger, J., Que, R., Fleming, R. M. T., Thiele, I., Orth, J. D.,
  Feist, A. M., Zielinski, D. C., Bordbar, A., Lewis, N. E., Rahmanian, S.,
  Kang, J., Hyduke, D. R. and Palsson, B. Ø. (2011) Quantitative prediction of
  cellular metabolism with constraint-based models: the COBRA Toolbox v2.0.
  \emph{Nat Protoc} \bold{6}, 1290--1307.

  Bernhard Ø. Palsson (2006).
  \emph{Systems Biology: Properties of Reconstructed Networks}.
  Cambridge University Press.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  Constructor function \code{\link{sysBiolAlg}} and
  superclass \code{\linkS4class{sysBiolAlg}}.
}

\examples{
  showClass("sysBiolAlg_fv")
}

\keyword{classes}
