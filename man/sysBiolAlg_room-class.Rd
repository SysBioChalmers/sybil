\name{sysBiolAlg_room-class}

\Rdversion{1.1}
\encoding{utf8}

\docType{class}

\alias{sysBiolAlg_room-class}
\alias{sysBiolAlg_room}
\alias{room}

\title{Class \code{"sysBiolAlg_room"}}

\description{
  The class \code{sysBiolAlg_room} holds an object of class
  \code{\linkS4class{optObj}} which is generated to meet the
  requirements of the ROOM algorithm.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  
  \code{sysBiolAlg(model, algorithm = "room", ...)}.
  
  Arguments to \code{...} which are passed to method \code{initialize} of class
  \code{sysBiolAlg_room} are described in the Details section.
}

\section{Slots}{
  \describe{
    \item{\code{wu}:}{Object of class \code{"numeric"}
      containing the upper threshold for a significant flux change,
      see Details below.
    }
    \item{\code{wl}:}{Object of class \code{"numeric"}
      containing the lower threshold for a significant flux change,
      see Details below.
    }
    \item{\code{fnc}:}{Object of class \code{"integer"}
      containing the number of reactions in the entire metabolic network
      (argument \code{model} to the constructor function
      \code{\link{sysBiolAlg}}).
    }
    \item{\code{fnr}:}{Object of class \code{"integer"}
      containing the number of metabolites in the entire metabolic network
      (argument \code{model} to the constructor function
      \code{\link{sysBiolAlg}}).
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
    \item{optimizeProb}{\code{signature(object = "sysBiolAlg_room")}:
      runs optimization on the given problem object
      (see \code{\link{optimizeProb}} for details).
    }
  }
}

\details{
  The \code{initialize} method has the following arguments:
  \describe{
    \item{model}{
      An object of class \code{\linkS4class{modelorg}}.
    }
    \item{wtflux}{
      A numeric vector holding an optimal wild type flux distribution for the
      given model. If missing, a default value is computed based on FBA.
      If given, arguments \code{solver} and \code{method} are used to calculate
      the dafault, but \code{solverParm} is not.
    }
    \item{delta}{
      A single numeric value giving the relative range of tolerance, see
      Details below.\cr
      Default: \code{0.03}.
    }
    \item{epsilon}{
      A single numeric value giving the absolute range of tolerance, see
      Details below.\cr
      Default: \code{0.001}.
    }
    \item{LPvariant}{
      Boolean. If \code{TRUE}, the problem object is formulated as linear
      program. See Details below.\cr
      Default: \code{FALSE}.
    }
    \item{LPvariant}{
      Boolean. If \code{TRUE}, the problem object is formulated as linear
      program. See Details below.\cr
      Default: \code{FALSE}.
    }
    \item{absMAX}{
      A single numerical value used as a maximum value for upper variable
      and contraint bounds.\cr
      Default: \code{SYBIL_SETTINGS("MAXIMUM")}.
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

  The problem object is built to be capable to perform the ROOM algorithm with
  a given model, which is basically the solution of a mixed integer programming
  problem
  \deqn{%
    \begin{array}{rll}%
      \min & \sum\limits_{i=1}^n y_i
                                                                       \\[2em]
          \mathrm{s.\,t.} & \mbox{$Sv$} = 0         \\[1ex]
                          & \alpha_i \leq v_i \leq \beta_i
                            & \quad \forall i \in \{1, \ldots, n\}      \\[1ex]
                          & v_i - y(\beta_i - w_i^u) \leq w_i^u        \\[1ex]
                          & v_i - y(\alpha_i - w_i^l) \geq w_i^l       \\[1ex]
                          & y_i \in \{0, 1\}                           \\[1ex]
                          & w_i^u = w_i + \delta |w_i| + \epsilon      \\[1ex]
                          & w_i^l = w_i - \delta |w_i| - \epsilon      \\[1ex]
    \end{array}%
  }{
     max  sum (v_j,del - v_i,wt)^2  for i,j = 1, ..., n
      s.t. Sv = 0
           a_i <= v_i <= b_i  for i = 1, ..., n
  }
  with \eqn{\bold{S}}{S} being the stoichiometric matrix, \eqn{\alpha_i}{a_i}
  and \eqn{\beta_i}{b_i} being the lower and upper bounds for flux (variable)
  \eqn{i}.
  The total number of fluxes of the optimization problem is denoted by \eqn{n}.
  Here, \eqn{w} is the optimal wild type flux distribution. This can be set via
  the argument \code{wtflux}. If \code{wtflux} is \code{NULL} (the default), the
  wild type flux distribution will be calculated by a standard FBA.
  All variables \eqn{y_i} are binary, with \eqn{y_i = 1} for a significant flux
  change in \eqn{v_i} and \eqn{y_i = 0} otherwise. Thresholds determining the
  significance of a flux change are given in \eqn{w^u} and \eqn{w^l}, with
  \eqn{\delta}{delta} and \eqn{\epsilon}{epsilon} specifying absolute and
  relative ranges in tolerance [Shlomi et al. 2005].
  
  The Boolean argument \code{LPvariant} relax the binary contraints to
  \eqn{0 \leq y_i \leq 1}{0 <= y_i <= 1} so that the problem becomes a linear
  program.
  The optimization can be executed by using \code{\link{optimizeProb}}.
}

\note{
  If using \pkg{glpkAPI} as MIP solver, consider to set parameter
  \code{PRESOLVE} to \code{GLP_ON}.
}

\references{
  Shlomi, T., Berkman, O. and Ruppin, E. (2005) Regulatory on/off minimization
  of metabolic flux changes after genetic pertubations. \emph{PNAS} \bold{102},
  7695--7700.
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
  showClass("sysBiolAlg_room")
}

\keyword{classes}
