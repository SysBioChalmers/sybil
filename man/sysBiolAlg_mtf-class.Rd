\name{sysBiolAlg_mtf-class}

\Rdversion{1.1}
\encoding{utf8}

\docType{class}

\alias{changeMaxObj,sysBiolAlg_mtf-method}
\alias{changeMaxObj}

\alias{sysBiolAlg_mtf-class}
\alias{sysBiolAlg_mtf}
\alias{mtf}

\title{Class \code{"sysBiolAlg_mtf"}}

\description{
  The class \code{sysBiolAlg_mtf} holds an object of class
  \code{\linkS4class{optObj}} which is generated to meet the
  requirements of the minimize total flux algorithm: minimize the absolute sum
  of all fluxes given a previously calculated objective value.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  
  \code{sysBiolAlg(model, algorithm = "mtf", ...)}.
  
  Arguments to \code{...} which are passed to method \code{initialize} of class
  \code{sysBiolAlg_mtf} are described in the Details section.
}

\section{Slots}{
  \describe{
    \item{\code{maxobj}:}{Object of class \code{"numeric"}
      containing optimized objective values.
    }
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
  \describe{
    \item{changeMaxObj}{\code{signature(object = "sysBiolAlg_mtf")}:
      change current objective value to the \eqn{j}th value given in slot
      \code{maxobj}. Argument \code{j} must be in \code{[1:length(maxobj)]}.
    }
  }
}

\details{
  The \code{initialize} method has the following arguments:
  \describe{
    \item{model}{
      An object of class \code{\linkS4class{modelorg}}.
    }
    \item{wtobj}{
      A single numeric value giving the optimal value. If missing, a default
      value is computed based on FBA. If given, arguments \code{solver} and
      \code{method} are used, but \code{solverParm} is not.\cr
      Default: \code{NULL}.
    }
    \item{react}{
      Arguments \code{react}, \code{lb} and \code{ub} are used, if argument
      \code{wtobj} is \code{NULL}, meaning: no previous objective value is
      given. Objective values will be calculated via \code{\link{fba}} using
      the parameters given in \code{react}, \code{lb} and \code{ub}.\cr
      Default: \code{NULL}.
    }
    \item{lb}{
      See argument \code{react}.\cr
      Default: \code{NULL}.
    }
    \item{ub}{
      See argument \code{react}.\cr
      Default: \code{NULL}.
    }
    \item{costcoeffw}{
      A numeric vector containing cost coefficients for all variables (forward
      direction). If set to \code{NULL}, all cost coefficients are set to
      \code{1}, so that all variables have the same impact on the objective
      function.\cr
      Default: \code{NULL}.
    }
    \item{costcoefbw}{
      A numeric vector containing cost coefficients for all variables (backward
      direction). If set to \code{NULL}, all cost coefficients are set to the
      values given in \code{costcoeffw}.\cr
      Default: \code{NULL}.
    }
    \item{absMAX}{
      A single numerical value used as a maximum value for upper variable
      and contraint bounds.\cr
      Default: \code{SYBIL_SETTINGS("MAXIMUM")}.
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
      the metabolite id's of \code{model} are used.\cr
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

  The problem object is built to be capable to perform minimize total flux
  with a given model, which is basically the solution of a linear programming
  problem
  \deqn{%
      \begin{array}{rll}%
          \min            &  \sum\limits_{i=1}^n cost_i |v_i|            \\[2em]
          \mathrm{s.\,t.} & \mbox{$Sv$} = 0          \\[1ex]
                          & \alpha_i \leq v_i \leq \beta_i
                            & \quad \forall i \in \{1, \ldots, n\}       \\[1ex]
                          & \mbox{$c$}_{\mathrm{wt}} \geq
                            \mbox{$c$}^{\mathrm{T}}
                            \mbox{$v$}_{\mathrm{wt}} \\[1ex]
      \end{array}%
  }{
      min  sum cost_i abs(v_i)  for i = 1, ..., n
      s.t. Sv = 0
           a_i <= v_i <= b_i  for i = 1, ..., n
           c_wt >= c^T v_wt
  }
  with
  \eqn{
      \mbox{$c$}^{\mathrm{T}}
      \mbox{$v$}_{\mathrm{wt}}
  }{c^T v_wt}
  being the previously computed optimized value of the objective function
  (argument \code{wtobj}).
  The variable \eqn{\bold{S}}{S} denotes the stoichiometric matrix,
  \eqn{\alpha_i}{a_i} and \eqn{\beta_i}{b_i} being the lower and upper bounds
  for flux (variable) \eqn{i}.  The total number of variables of the
  optimization problem is denoted by \eqn{n}.
  The optimization can be executed by using \code{\link{optimizeProb}}.
}

\references{
  Edwards, J. S., Covert, M and Palsson, B. Ø. (2002) Metabolic modelling of
  microbes: the flux-balance approach. \emph{Environ Microbiol} \bold{4},
  133--140.
  
  Edwards, J. S., Ibarra, R. U. and Palsson, B. Ø. (2001) In silico predictions
  of \emph{Escherichia coli} metabolic capabilities are consistent with
  experimental data. \emph{Nat Biotechnol} \bold{19}, 125--130.
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
  showClass("sysBiolAlg_mtf")
}

\keyword{classes}
