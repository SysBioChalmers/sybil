\name{sysBiolAlg_lmoma-class}

\Rdversion{1.1}
\encoding{utf8}

\docType{class}

\alias{sysBiolAlg_lmoma-class}
\alias{sysBiolAlg_lmoma}
\alias{lmoma}

\title{Class \code{"sysBiolAlg_lmoma"}}

\description{
  The class \code{sysBiolAlg_lmoma} holds an object of class
  \code{\linkS4class{optObj}} which is generated to meet the
  requirements of a lineraized versoin of the MOMA algorithm.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  
  \code{sysBiolAlg(model, algorithm = "lmoma", ...)}.
  
  Arguments to \code{...} which are passed to method \code{initialize} of class
  \code{sysBiolAlg_lmoma} are described in the Details section.
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
  No methods defined with class "sysBiolAlg_lmoma" in the signature.
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
      If given, arguments \code{solver} and \code{method} are used, but
      \code{solverParm} is not.
    }
    \item{COBRAflag}{
      Boolean, prepare problem object in order to perform minimization of
      metabolic adjustment as in COBRA Toolbox.\cr
      Default: \code{FALSE}.
    }
    \item{wtobj}{
      Only used if argument \code{COBRAflag} is set to \code{TRUE}:
      A single numeric value giving the optimized value of the objective
      function of the wild type problem. If missing, a default
      value is computed based on FBA. If given, arguments \code{solver} and
      \code{method} are used, but \code{solverParm} is not.
    }
    \item{wtobjLB}{
      Only used if argument \code{COBRAflag} is set to \code{TRUE}:
      Boolean. If set to \code{TRUE}, the value of argument \code{wtobj} is
      treated as lower bound. If set to \code{FALSE}, \code{wtobj} serves as
      an upper bound.\cr
      Default: \code{TRUE}.
    }
    \item{obj_coefD}{
      A numeric vector of length two times the number of reactions in the model
      containing the non-zero part of the objective function. If set to
      \code{NULL}, the vector is filled with ones.\cr
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

  The problem object is built to be capable to perform a linearized version of
  the MOMA algorithm with a given model, which is basically the solution of a
  linear programming problem
  \deqn{%
    \begin{array}{rll}%
      \min            & \begin{minipage}[b]{5em}
                        \[ 
                          \sum_{i,j=1}^n
                          \bigl|v_{j,\mathrm{del}} - v_{i,\mathrm{wt}}\bigr|
                        \]
                        \end{minipage}                                 \\[2em]
      \mathrm{s.\,t.} & \mbox{\boldmath$Sv$\unboldmath}_{\mathrm{del}} = 0
                                                                       \\[1ex]
                        & v_i = v_{i,\mathrm{wt}}
                          & \quad \forall i \in \{1, \ldots, n\}       \\[1ex]
                        & \alpha_j \leq v_{j,\mathrm{del}} \leq \beta_j
                          & \quad \forall j \in \{1, \ldots, n\}       \\[1ex]
    \end{array}%
  }{
     min  sum |v_j,del - v_i,wt|  for i,j = 1, ..., n
     s.t. Sv_del = 0
          v_i = vi_i,wt  for i = 1, ..., n
          a_j <= v_j,del <= b_j  for j = 1, ..., n
  }
  Here,
  \eqn{
    \mbox{\boldmath$v$\unboldmath}_{\mathrm{wt}}
  }{v_wt}
  is the optimal wild type flux distribution. This can be set via the argument
  \code{wtflux}.  If \code{wtflux} is \code{NULL} (the default), the
  wild type flux distribution will be calculated by a standard FBA.

  If argument \code{COBRAflag} is set to \code{TRUE}, the linear programm is
  formulated differently. Wild type and knock-out strain will be computed
  simultaneously.
  \deqn{%
    \begin{array}{rll}%
      \min            & \begin{minipage}[b]{5em}
                        \[ 
                          \sum_{i,j=1}^n
                          \bigl|v_{j,\mathrm{del}} - v_{i,\mathrm{wt}}\bigr|
                        \]
                        \end{minipage}                                 \\[2em]
      \mathrm{s.\,t.} & \mbox{\boldmath$Sv$\unboldmath}_{\mathrm{wt}} = 0
                                                                       \\[1ex]
                        & \alpha_i \leq v_{i,\mathrm{wt}} \leq \beta_i
                          & \quad \forall i \in \{1, \ldots, n\}       \\[1ex]

                      & \mbox{\boldmath$Sv$\unboldmath}_{\mathrm{del}} = 0
                                                                       \\[1ex]
                        & \alpha_j \leq v_{j,\mathrm{del}} \leq \beta_j
                          & \quad \forall j \in \{1, \ldots, n\}       \\[1ex]
                          & \mbox{$\mu$}_{\mathrm{wt}} =
                            \mbox{\boldmath$c$\unboldmath}^{\mathrm{T}}
                            \mbox{\boldmath$v$\unboldmath}_{\mathrm{wt}} \\[1ex]
    \end{array}%
  }{
     min  sum |v_j,del - v_i,wt|  for i,j = 1, ..., n
     s.t. Sv_wt = 0
          v_i = vi_i,wt  for i = 1, ..., n
          Sv_del = 0
          a_j <= v_j,del <= b_j  for j = 1, ..., n
          mu_wt = c^T v_wt
  }
  with \eqn{\bold{S}}{S} being the stoichiometric matrix, \eqn{\alpha_i}{a_i}
  and \eqn{\beta_i}{b_i} being the lower and upper bounds for flux (variable)
  \eqn{i} (\eqn{j} for the deletion strain).
  The total number of variables of the optimization problem is denoted
  by \eqn{n}.
  Here,
  \eqn{
    \mu_{\mathrm{wt}}
  }{mu_wt}
  is the optimal wild type growth rate. This can be set via the argument
  \code{wtobj}.  If \code{wtobj} is \code{NULL} (the default), the
  wild type growth rate will be calculated by a standard FBA.
  The optimization can be executed by using \code{\link{optimizeProb}}.
}

\references{
  Becker, S. A., Feist, A. M., Mo, M. L., Hannum, G., Palsson, B. Ø.  and
  Herrgard, M. J. (2007) Quantitative prediction of cellular metabolism with
  constraint-based models: the COBRA Toolbox. \emph{Nat Protoc} \bold{2},
  727--738.

  Edwards, J. S., Covert, M and Palsson, B. Ø. (2002) Metabolic modelling of
  microbes: the flux-balance approach. \emph{Environ Microbiol} \bold{4},
  133--140.
  
  Edwards, J. S., Ibarra, R. U. and Palsson, B. Ø. (2001) In silico predictions
  of \emph{Escherichia coli} metabolic capabilities are consistent with
  experimental data. \emph{Nat Biotechnol} \bold{19}, 125--130.

  Schellenberger, J., Que, R., Fleming, R. M. T., Thiele, I., Orth, J. D.,
  Feist, A. M., Zielinski, D. C., Bordbar, A., Lewis, N. E., Rahmanian, S.,
  Kang, J., Hyduke, D. R. and Palsson, B. Ø. (2011) Quantitative prediction of
  cellular metabolism with constraint-based models: the COBRA Toolbox v2.0.
  \emph{Nat Protoc} \bold{6}, 1290--1307.

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
  showClass("sysBiolAlg_lmoma")
}

\keyword{classes}
