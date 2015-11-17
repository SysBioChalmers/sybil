\name{sysBiolAlg_fbaEasyConstraint-class}

\Rdversion{1.1}
\encoding{utf8}

\docType{class}

\alias{sysBiolAlg_fbaEasyConstraint-class}
\alias{sysBiolAlg_fbaEasyConstraint}
\alias{sysBiolAlg_mtfEasyConstraint-class}
\alias{sysBiolAlg_mtfEasyConstraint}
\alias{mtfEasyConstraint}
\alias{fbaEasyConstraint}
\alias{EasyConstraint}

\title{Class \code{"sysBiolAlg_fbaEasyConstraint"} and \cr Class \code{"sysBiolAlg_mtfEasyConstraint"}}

\description{
  The classes \code{sysBiolAlg_fbaEasyConstraint} \code{sysBiolAlg_mtfEasyConstraint} hold an object of class
  \code{\linkS4class{optObj}} which is generated to meet the
  requirements of the FBA/MTF algorithm. \cr
  In Addition to this, it is very easy to add
  additional linear constraints to that linear problem. Each constraints is 
  defined by the affected reaction, the coefficient, lower and upper bounds, and
  the constraint type.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  
  \code{sysBiolAlg(model, algorithm = "fbaEasyConstraint", ...)}.
  
  Arguments to \code{...} which are passed to method \code{initialize} of class
  \code{sysBiolAlg_fba} are described in the Details section.
}

\section{Slots}{
  \describe{
    Slots are the same as in the original MTF/FBA classes. In addition, this 
    slot is implemented:\cr
    
    \item{\code{easyConstraint}}{
    	Named list holding the information for the constraints (see details):\cr
		\itemize{
			\item{\code{react}}{
				List of numeric vectors. Values indicate, to which reaction the 
				constraint applys.
			}
			\item{\code{x}}{
				List of numeric vectors. Values indicate coefficients of the 
				constraint. Lengths have to be equal to \code{react}-field.
			}
			\item{\code{lb}}{
				Numeric vector of lower bounds for constraints. If not given, a 
				default bound of 0 will be used.
			}
			\item{\code{ub}}{
				Numeric vector of lower bounds for constraints. If not given, a 
				default bound of 0 will be used. Only needed for constraints, 
				that need two bounds.
			}
			\item{\code{rtype}}{
				Character vector defining the type of constraint.
				
				\tabular{lll}{
					  \code{"F"}: \tab free constraint (GLPK only)          \tab \eqn{-\infty < x < \infty}{-INF < x < INF}               \cr
					  \code{"L"}: \tab constraint with lower bound          \tab \eqn{\mathrm{lb} \leq x < \infty}{lb <= x < INF}         \cr
					  \code{"U"}: \tab constraint with upper bound          \tab \eqn{-\infty < x \leq \mathrm{ub}}{-INF < x <= ub}       \cr
					  \code{"D"}: \tab double-bounded (ranged) constraint   \tab \eqn{\mathrm{lb} \leq x \leq \mathrm{ub}}{lb <= x <= ub} \cr
					  \code{"E"}: \tab fixed (equality) constraint          \tab \eqn{\mathrm{lb} = x = \mathrm{ub}}{lb = x = ub}         \cr
				}
				If \code{rtype[i]} is not one of \code{"F"}, \code{"L"}, \code{"U"},
				\code{"D"} or \code{"E"}, the value of \code{rtype[i]} will be set to
				\code{"E"}. See Details of \code{\link{loadLPprob}}.
			}
		}
    }
  }
}

\section{Extends}{
  Class \code{"\linkS4class{sysBiolAlg}"}, directly.
}

\section{Methods}{
  No methods defined with class "sysBiolAlg_fbaEasyConstraint" in the signature.
}

\details{
  The problem object is built to be capable to perform flux balance analysis
  (FBA) with a given model, which is basically the solution of a linear
  programming problem
  \deqn{%
      \begin{array}{rll}%
          \max            & \mbox{\boldmath$c$\unboldmath}^{\mathrm{T}}
                                         \mbox{\boldmath$v$\unboldmath} \\[1ex]
          \mathrm{s.\,t.} & \mbox{\boldmath$Sv$\unboldmath} = 0         \\[1ex]
                          & \alpha_i \leq v_i \leq \beta_i
                            & \quad \forall i \in \{1, \ldots, n\}      \\[1ex]
      \end{array}%
  }{
      max  c^T v
      s.t. Sv = 0
           a_i <= v_i <= b_i  for i = 1, ..., n
  }
  with \eqn{\bold{S}}{S} being the stoichiometric matrix, \eqn{\alpha_i}{a_i}
  and \eqn{\beta_i}{b_i} being the lower and upper bounds for flux (variable)
  \eqn{i} respectively.  The total number of variables of the optimization
  problem is denoted by \eqn{n}.  The solution of the optimization is a flux
  distribution maximizing the objective function
  \eqn{
      \mbox{\boldmath$c$\unboldmath}^{\mathrm{T}}
      \mbox{\boldmath$v$\unboldmath}
  }{c^T v} under the a given environment and the assumption of steady state.
  The optimization can be executed by using \code{\link{optimizeProb}}.
  
  The additional i-th EasyConstraint will be added as follows to the problem: \emph{to be checked.}
  \deqn{%
	\gamma_i \leq v_{r_i} * (x_i)^\mathrm{T} \leq \delta_i
  }{
  	gamma_i <= v_{r^i} * (x_i)^T <= delta_i
  }
  Here \eqn{r_i}{r_i} (= \code{easyConstraint$react[[i]]}) is a set of reaction
  indices and \eqn{x_i}{x_i} (= \code{easyConstraint$x[[i]]}) is the
  corresponding set of coefficients. \eqn{\gamma}{gamma} and \eqn{\delta}{delta}
  are the vectors of lower and upper bounds for the constraints, respectively.
  For the type of (in)equality (\eqn{\leq}{<=}, ...) see the text above for parameter 
  \code{rtype}.
  
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
 	Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  Constructor function \code{\link{sysBiolAlg}} and
  superclass \code{\linkS4class{sysBiolAlg}}.
}

\examples{
  showClass("sysBiolAlg_fbaEasyConstraint")
  
  # see package vignette for second example with more comments:
  #vignette("sybil")
  
  #load model
  data(Ec_core)
  
  # allow influx of Fumarate and restrict outflux of Fumarate and Glucose
  lowbnd(Ec_core)[react_id(Ec_core) \%in\% c("EX_fum(e)")] <- -1000
  uppbnd(Ec_core)[react_id(Ec_core) \%in\% c("EX_glc(e)", "EX_fum(e)")] <- 0
  
  # see result
  findExchReact(Ec_core)
  optimizeProb(Ec_core)
  
  # define easyConstraint to have the same influx for Glucose and Fumarate:
  # EX_glc(e) = EX_fum(e)
  # here we omit the upper and lower bound, hence they are set to zero.
  ec <- list(
  	react=list(which(react_id(Ec_core) \%in\% c("EX_glc(e)", "EX_fum(e)"))),
  	x=list(c(1, -1)),
  	rtype="E")
  
  # optimize
  opt <- optimizeProb(Ec_core, algorithm=("fbaEasyConstraint"), easyConstraint=ec)
  
  # check if fluxes are really the same:
  fluxes(opt)[react_id(Ec_core) \%in\% c("EX_glc(e)", "EX_fum(e)")]
  
  
  
  
  
  
  
}

\keyword{classes}
