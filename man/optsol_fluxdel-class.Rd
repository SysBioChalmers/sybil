\name{optsol_fluxdel-class}

\docType{class}

\alias{optsol_fluxdel-class}

\alias{chlb,optsol_fluxdel-method}
\alias{chlb<-,optsol_fluxdel-method}
\alias{chlb}
\alias{chlb<-}
\alias{chub,optsol_fluxdel-method}
\alias{chub<-,optsol_fluxdel-method}
\alias{chub}
\alias{chub<-}
\alias{dels,optsol_fluxdel-method}
\alias{dels<-,optsol_fluxdel-method}
\alias{dels}
\alias{dels<-}
% \alias{ind2id,optsol_fluxdel-method}
% \alias{ind2id}
\alias{deleted,optsol_fluxdel-method}
\alias{deleted}
\alias{lethal,optsol_fluxdel-method}
\alias{lethal}
\alias{[,optsol_fluxdel,ANY,ANY,ANY-method}

\encoding{utf8}

\title{Class \code{"optsol_fluxdel"}}

\description{
  Structure of the class \code{"optsol_fluxdel"}. Objects of that class are
  returned by the function \code{\link{oneFluxDel}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("optsol_fluxdel", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{chlb}:}{Object of class \code{"numeric"}
      containing the new (changed) values for the columns lower bounds.
    }
    \item{\code{chub}:}{Object of class \code{"numeric"}
      containing the new (changed) values for the columns upper bounds.
    }
    \item{\code{dels}:}{
      Object of class \code{"matrix"} containing the reaction id's of
      constrained reactions. Each row of the matrix represents one set of
      simultaneously constrained reactions.
    }
    \item{\code{preProc}:}{Object of class \code{"ppProc"}
      containing the results of pre-processing.
      See also \code{\link{optimizeProb}}.
    }
    \item{\code{postProc}:}{Object of class \code{"ppProc"}
      containing the results of post-processing.
      See also \code{\link{optimizeProb}}.
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
  Class \code{"\linkS4class{optsol_optimizeProb}"}, directly.
  Class \code{"\linkS4class{optsol}"},
  by class "optsol_optimizeProb", distance 2.
}

\section{Methods}{
  \describe{
    \item{\code{react_id}:}{
      \code{signature(object = "optsol_fluxdel")}: gets the \code{react_id} slot.
    }
    \item{\code{react_id<-}:}{
      \code{signature(object = "optsol_fluxdel")} sets the \code{react_id} slot.
    }
    \item{\code{allGenes}:}{
      \code{signature(object = "optsol_fluxdel")}: gets the \code{allGenes} slot.
    }
    \item{\code{allGenes<-}:}{
      \code{signature(object = "optsol_fluxdel")} sets the \code{allGenes} slot.
    }
    \item{\code{chlb}:}{
      \code{signature(object = "optsol_fluxdel")}: gets the \code{chlb} slot.
    }
    \item{\code{chlb<-}:}{
      \code{signature(object = "optsol_fluxdel")} sets the \code{chlb} slot.
    }
    \item{\code{chub}:}{
      \code{signature(object = "optsol_fluxdel")}: gets the \code{chub} slot.
    }
    \item{\code{chub<-}:}{
      \code{signature(object = "optsol_fluxdel")}: sets the \code{chub} slot.
    }
    \item{\code{dels}:}{
      \code{signature(object = "optsol_fluxdel")}: gets the \code{dels} slot.
    }
    \item{\code{dels<-}:}{
      \code{signature(object = "optsol_fluxdel")} sets the \code{dels} slot.
    }
    \item{\code{algorithm}:}{
      \code{signature(object = "optsol_fluxdel")}: gets the \code{algorithm} slot.
    }
    \item{\code{algorithm<-}:}{
      \code{signature(object = "optsol_fluxdel")} sets the \code{algorithm} slot.
    }
%     \item{\code{ind2id}:}{
%       \code{signature(object = "optsol_fluxdel")}(\code{slotN}): translates the
%       indices in \code{slotN} into corresponding gene or reaction id's. Possible
%       values for \code{slotN} are: \code{"dels"}.
%     }
    \item{\code{lethal}:}{
      \code{signature(object = "optsol_fluxdel")}(\code{wt, tol}): returns
      a logical vector of length \code{num_of_prob(object)}. Argument \code{wt}
      is an optimal (wild type) growth rate, e.g. computed via FBA. If the
      absolute growth ratio (\code{mod_obj(object)/wt}) of knock-out \code{i}
      is less than \code{tol}, the deletion is considered as lethal.
      If \code{lethal(object)[i]} is \code{TRUE}, deletion [i] is lethal.
    }
    \item{\code{deleted}:}{
      \code{signature(object = "optsol_fluxdel")}(\code{i}): gets the \code{i}th
      element of the \code{dels} slot.
    }
    \item{\code{[}:}{
      \code{signature(x = "optsol_fluxdel")}: access like a vector.
      \code{x[i]} returns a new object of class \code{optsol_fluxdel} containing
      the \code{i}th deletion experiment.
    }
  }
}

\author{Gabriel Gelius-Dietrich}

\seealso{
  \code{\link{checkOptSol}},
  \code{\linkS4class{optsol}},
  \code{\linkS4class{optsol_genedel}} and
  \code{\linkS4class{optsol_optimizeProb}}
}
      
\examples{
  showClass("optsol_fluxdel")
}

\keyword{classes}
