\name{sysBiolAlg_moma-class}

\Rdversion{1.1}
\encoding{utf8}

\docType{class}

\alias{sysBiolAlg_moma-class}
\alias{sysBiolAlg_moma}
\alias{moma}

\title{Class \code{"sysBiolAlg_moma"}}

\description{
  The class \code{sysBiolAlg_moma} holds an object of class
  \code{\linkS4class{optObj}} which is generated to meet the
  requirements of the MOMA algorithm.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  
  \code{sysBiolAlg(model, algorithm = "moma", ...)}.
  
  Arguments to \code{...} which are passed to method \code{initialize} of class
  \code{sysBiolAlg_moma} are described in the Details section.
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
  No methods defined with class "sysBiolAlg_moma" in the signature.
}

\details{
  The \code{initialize} method has the following arguments:
  \describe{
    \item{model}{
      An object of class \code{\linkS4class{modelorg}}.
    }
    \item{wtflux}{
      A numeric vector holding an optimal wild type flux distribution for the
      given model. If set to \code{NULL}, a default value is computed based on
      flux-balance analysis. If given, arguments \code{solver} and \code{method}
      are used, but \code{solverParm} is not.
      Default: \code{NULL}.
    }
    \item{Qmat}{
      A numeric vector or matrix (of class \code{\linkS4class{Matrix}}) holding
      the quadratic part of the objective function.  If set to \code{NULL}, a
      quadratic unity matrix with number of columns and rows equal to the number
      of reactions given in the model is used.
      Default: \code{NULL}.
    }
    \item{scaleDist}{
      A numeric vector containing scaling factors for each reaction in the
      objective function. If \code{scaleDist[j]} is set to \code{0}, reaction
      \code{j} will be ignored. The quadratic and the linear part of the
      objective function are multiplied by this factor. If set to \code{NULL},
      the reactions are not scaled.
      Default: \code{NULL}.
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

  The problem object is built to be capable to perform the MOMA algorithm with
  a given model, which is basically the solution of a quadratic programming
  problem
  \deqn{%
    \begin{array}{rll}%
      \min            & \sum\limits_{j=1}^n
                          %\bigl(v_{j,\mathrm{del}} - v_{j,\mathrm{wt}}\bigr)^2
                        \left(\left(v_{j,\mathrm{del}} - v_{j,\mathrm{wt}}\right) \cdot sd_j \right)^2 \\[2em]
          \mathrm{s.\,t.} & \mbox{$Sv$} = 0         \\[1ex]
                          & \alpha_j \leq v_j \leq \beta_j
                            & \quad \forall j \in \{1, \ldots, n\}      \\[1ex]
    \end{array}%
  }{
     min  sum ( (v_j,del - v_j,wt) * sd[j] )^2  for j = 1, ..., n
      s.t. Sv = 0
           a_j <= v_j <= b_j  for j = 1, ..., n
  }
  with \eqn{\bold{S}}{S} being the stoichiometric matrix, \eqn{\alpha_j}{a_j}
  and \eqn{\beta_j}{b_j} being the lower and upper bounds for flux (variable)
  \eqn{j} and \eqn{sd_j}{sd[j]} being the scaling factor for reaction \eqn{j}
  (default: \eqn{sd_j = 1,\ \forall j}{sd[j] = 1, for j = 1, ..., n}).
  The total number of variables of the optimization problem is denoted by
  \eqn{n}. Here,
  \eqn{
    \mbox{$v$}_{\mathrm{wt}}
  }{v_wt}
  is the optimal wild type flux distribution. This can be set via the argument
  \code{wtflux}.  If \code{wtflux} is \code{NULL} (the default), the
  wild type flux distribution will be calculated by a standard FBA.
  The optimization can be executed by using \code{\link{optimizeProb}}.
}


\references{
  Segrè, D., Vitkup, D. and Church, G. M. (2002) Analysis or optimality in
  natural and pertubed metabolic networks. \emph{PNAS} \bold{99}, 15112--15117.
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
  showClass("sysBiolAlg_moma")
}

\keyword{classes}
