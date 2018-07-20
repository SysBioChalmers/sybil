\name{optsol_blockedReact-class}

\Rdversion{1.1}
\docType{class}

\alias{optsol_blockedReact-class}

\alias{blocked,optsol_blockedReact-method}
\alias{blocked<-,optsol_blockedReact-method}
\alias{blocked}
\alias{blocked<-}
\alias{react,optsol_blockedReact-method}
\alias{react<-,optsol_blockedReact-method}
\alias{react<-}
\alias{maxSol,optsol_blockedReact-method}
\alias{minSol,optsol_blockedReact-method}
\alias{maxSol}
\alias{minSol}

\title{Class \code{"optsol_blockedReact"}}

\description{
  Structure of the class \code{"optsol_blockedReact"}. Objects of that class are
  returned by the function \code{\link{blockedReact}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("optsol_blockedReact", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{blocked}:}{Object of class \code{"logical"}
      indicating if a reaction is blocked, or not.
    }
    \item{\code{react}:}{Object of class \code{"reactId"}
      containing the reaction id's of checked reactions.
    }
    \item{\code{mod_id}:}{Object of class \code{"character"}
      containing the model id of the used model.
    }
    \item{\code{mod_key}:}{Object of class \code{"character"}
      containing the model key of the used model.
    }
    \item{\code{solver}:}{Object of class \code{"character"}
      indicating the used solver.
    }
    \item{\code{method}:}{Object of class \code{"character"}
      indicating the used method.
    }
    \item{\code{algorithm}:}{Object of class \code{"character"}
      containing the name of the algorithm used for optimizations.
    }
    \item{\code{num_of_prob}:}{Object of class \code{"integer"}
      indicating the number of optimization problems.
    }
    \item{\code{lp_num_cols}:}{Object of class \code{"integer"}
      indicating the number of columns.
    }
    \item{\code{lp_num_rows}:}{Object of class \code{"integer"}
      indicating the number of rows.
    }
    \item{\code{lp_obj}:}{Object of class \code{"numeric"}
      containing the optimal values of the objective function after
      optimization. If no flux distribution is available, slot \code{lp_obj}
      contains the cross-product of the objective coefficients in slot
      \code{obj_coef} and the part of the flux distribution in slot
      \code{fluxdist} containing the values representing fluxes in the entire
      metabolic network (slot \code{fldind}).
    }
    \item{\code{lp_ok}:}{Object of class \code{"integer"}
      containing the exit code of the optimization.
    }
    \item{\code{lp_stat}:}{Object of class \code{"integer"}
      containing the solution status of the optimization.
    }
    \item{\code{lp_dir}:}{Object of class \code{"character"}
      indicating the direction of optimization.
    }
    \item{\code{obj_coef}:}{Object of class \code{"numeric"}
      containing the objective coefficients of the used model (slot
      \code{obj_coef} of an object of class \code{\linkS4class{modelorg}}).
      These are not necessarily the objective coefficients of the used
      algorithm. 
    }
    \item{\code{obj_func}:}{Object of class \code{"character"}
      containing the objective function of the used model. Usually, it contains
      the return value of \code{\link{printObjFunc}}. 
    }
    \item{\code{fldind}:}{Object of class \code{"integer"}
      pointers to columns (variables) representing a flux (reaction) in the
      original network. The variable \code{fldind[i]} in the problem object
      represents reaction \code{i} in the original network. 
    }
    \item{\code{fluxdist}:}{Object of class \code{"fluxDistribution"}
      containing the solutions flux distributions.
    }
    \item{\code{alg_par}:}{Object of class \code{"list"}
      containing a named list containing algorithm specific parameters.
    }
  }
}

\section{Extends}{
  Class \code{"\linkS4class{optsol}"}, directly.
}

\section{Methods}{
  \describe{
    \item{\code{blocked}:}{
      \code{signature(object = "optsol_blockedReact")}: gets the \code{blocked} slot.
    }
    \item{\code{blocked<-}:}{
      \code{signature(object = "optsol_blockedReact")} sets the \code{blocked} slot.
    }
    \item{\code{react}:}{
      \code{signature(object = "optsol_blockedReact")}: gets the \code{react} slot.
    }
    \item{\code{react<-}:}{
      \code{signature(object = "optsol_blockedReact")} sets the \code{react} slot.
    }
    \item{\code{maxSol}:}{
      \code{signature(object = "optsol_blockedReact")}(\code{slot}): returns the
      values in the slot given in \code{slot} for optimizations in \dQuote{max}
      direction.
    }
    \item{\code{minSol}:}{
      \code{signature(object = "optsol_blockedReact")}(\code{slot}): returns the
      values in the slot given in \code{slot} for optimizations in \dQuote{min}
      direction.
    }
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\link{checkOptSol}} and \code{\linkS4class{optsol}}
}
      
\examples{
  showClass("optsol_blockedReact")
}

\keyword{classes}
