\name{optsol-class}

\docType{class}

\alias{optsol-class}
\alias{optsol}

\alias{checkStat,optsol-method}
\alias{checkStat}

\alias{algorithm,optsol-method}
\alias{algorithm}
\alias{algorithm<-,optsol-method}
\alias{algorithm<-}
\alias{alg_par,optsol-method}
\alias{alg_par<-,optsol-method}
\alias{fldind,optsol-method}
\alias{fldind}
\alias{fldind<-,optsol-method}
\alias{fldind<-}
\alias{fluxdist,optsol-method}
\alias{fluxdist}
\alias{fluxdist<-,optsol-method}
\alias{fluxdist<-}
\alias{fluxes,optsol-method}
\alias{fluxes}
\alias{fluxes<-,optsol-method}
\alias{fluxes<-}
\alias{lp_dir,optsol-method}
\alias{lp_dir}
\alias{lp_dir<-,optsol,character-method}
\alias{lp_dir<-,optsol,factor-method}
\alias{lp_dir<-,optsol,numeric-method}
\alias{lp_dir<-}
\alias{lp_num_cols,optsol-method}
\alias{lp_num_cols}
\alias{lp_num_cols<-,optsol-method}
\alias{lp_num_cols<-}
\alias{lp_num_rows,optsol-method}
\alias{lp_num_rows}
\alias{lp_num_rows<-,optsol-method}
\alias{lp_num_rows<-}
\alias{lp_obj,optsol-method}
\alias{lp_obj}
\alias{lp_obj<-,optsol-method}
\alias{lp_obj<-}
\alias{lp_ok,optsol-method}
\alias{lp_ok}
\alias{lp_ok<-,optsol-method}
\alias{lp_ok<-}
\alias{lp_stat,optsol-method}
\alias{lp_stat}
\alias{lp_stat<-,optsol-method}
\alias{lp_stat<-}
\alias{method,optsol-method}
\alias{method}
\alias{method<-,optsol-method}
\alias{method<-}
\alias{mod_id,optsol-method}
\alias{mod_id<-,optsol-method}
\alias{mod_key<-,optsol-method}
\alias{mod_key,optsol-method}
\alias{mod_obj,optsol-method}
\alias{mod_obj}
\alias{nfluxes,optsol-method}
\alias{nfluxes}
\alias{num_of_prob,optsol-method}
\alias{num_of_prob}
\alias{num_of_prob<-,optsol-method}
\alias{num_of_prob<-}
\alias{obj_coef,optsol-method}
\alias{obj_coef<-,optsol-method}
\alias{obj_func,optsol-method}
\alias{obj_func}
\alias{obj_func<-,optsol-method}
\alias{obj_func<-}
\alias{react_id,optsol-method}
\alias{react_id<-,optsol-method}
\alias{show,optsol-method}
\alias{solver,optsol-method}
\alias{solver}
\alias{solver<-,optsol-method}
\alias{solver<-}
\alias{length,optsol-method}
\alias{plot,optsol,missing-method}

\encoding{utf8}

\title{Class optsol}

\description{
  The class \code{optsol} provides data structures to store and access the
  results of optimizations. This class is extended by other classes and will
  not be used as is. The representation of class \code{optsol} is used as
  superclass.
}

\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}

\section{Slots}{
  \describe{
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

\section{Methods}{
  \describe{
    \item{\code{algorithm<-}:}{\code{signature(object = "optsol")}:
      sets the \code{algorithm} slot.
    }
    \item{\code{algorithm}:}{\code{signature(object = "optsol")}:
      gets the \code{algorithm} slot.
    }
    \item{alg_par}{\code{signature(object = "optsol")}:
      gets the \code{alg_par} slot.
    }
    \item{alg_par<-}{\code{signature(object = "optsol")}:
      sets the \code{alg_par} slot.
    }
    \item{\code{checkStat}:}{\code{signature(opt = "optsol")}:
      returns the indices of problems with a non optimal solution status.
    }
    \item{\code{fldind<-}:}{\code{signature(object = "optsol")}:
      sets the \code{fldind} slot.
    }
    \item{\code{fldind}:}{\code{signature(object = "optsol")}:
      gets the \code{fldind} slot.
    }
    \item{\code{fluxdist<-}:}{\code{signature(object = "optsol")}:
      sets the \code{fluxdist} slot.
    }
    \item{\code{fluxdist}:}{\code{signature(object = "optsol")}:
      gets the \code{fluxdist} slot.
    }
    \item{\code{fluxes<-}:}{\code{signature(object = "optsol")}:
      sets the \code{fluxes} slot of slot \code{fluxdist}.
    }
    \item{\code{fluxes}:}{\code{signature(object = "optsol")}:
      gets the \code{fluxes} slot of slot \code{fluxdist}.
    }
    \item{\code{plot}:}{
      \code{signature(x = "optsol")}: plots a \code{\link{histogram}} of the
      values of the objective function given in the model in optimal state.
      Additional arguments can be passed to \code{\link{histogram}} via the
      \code{\dots} argument.
    }
    \item{\code{length}:}{\code{signature(x = "optsol")}:
      returns the number of optimizations.
    }
    \item{\code{lp_dir<-}:}{\code{signature(object = "optsol", value = "character")}:
      sets the \code{lp_dir} slot. Argument \code{value} can be \code{"min"}
      (minimization) or \code{"max"} (maximization).
    }
    \item{\code{lp_dir<-}:}{\code{signature(object = "optsol", value = "factor")}:
      sets the \code{lp_dir} slot.
    }
    \item{\code{lp_dir<-}:}{\code{signature(object = "optsol", value = "numeric")}:
      sets the \code{lp_dir} slot. Argument \code{value} can be \code{1}
      (minimization) or \code{-1} (maximization).
    }
    \item{\code{lp_dir}:}{\code{signature(object = "optsol")}:
      gets the \code{lp_dir} slot.
    }
    \item{\code{lp_num_cols<-}:}{\code{signature(object = "optsol")}:
      sets the \code{lp_num_cols} slot.
    }
    \item{\code{lp_num_cols}:}{\code{signature(object = "optsol")}:
      gets the \code{lp_num_cols} slot.
    }
    \item{\code{lp_num_rows<-}:}{\code{signature(object = "optsol")}:
      sets the \code{lp_num_rows} slot.
    }
    \item{\code{lp_num_rows}:}{\code{signature(object = "optsol")}:
      gets the \code{lp_num_rows} slot.
    }
    \item{\code{lp_obj<-}:}{\code{signature(object = "optsol")}:
      sets the \code{lp_obj} slot.
    }
    \item{\code{lp_obj}:}{\code{signature(object = "optsol")}:
      gets the \code{lp_obj} slot.
    }
    \item{\code{lp_ok<-}:}{\code{signature(object = "optsol")}:
      sets the \code{lp_ok} slot.
    }
    \item{\code{lp_ok}:}{\code{signature(object = "optsol")}:
      gets the \code{lp_ok} slot.
    }
    \item{\code{lp_stat<-}:}{\code{signature(object = "optsol")}:
      sets the \code{lp_stat} slot.
    }
    \item{\code{lp_stat}:}{\code{signature(object = "optsol")}:
      gets the \code{lp_stat} slot.
    }
    \item{\code{method<-}:}{\code{signature(object = "optsol")}:
      sets the \code{method} slot.
    }
    \item{\code{method}:}{\code{signature(object = "optsol")}:
      gets the \code{method} slot.
    }
    \item{\code{mod_id<-}:}{\code{signature(object = "optsol")}:
      sets the \code{mod_id} slot.
    }
    \item{\code{mod_id}:}{\code{signature(object = "optsol")}:
      gets the \code{mod_id} slot.
    }
    \item{\code{mod_key<-}:}{\code{signature(object = "optsol")}:
      sets the \code{mod_key} slot.
    }
    \item{\code{mod_key}:}{\code{signature(object = "optsol")}:
      gets the \code{mod_key} slot.
    }
    \item{\code{mod_obj}:}{\code{signature(object = "optsol_fluxdel")}:
      returns always the cross-product of the objective coefficients in slot
      \code{obj_coef} and the part of the flux distribution in slot
      \code{fluxdist} containing the values representing fluxes in the entire
      metabolic network (slot \code{fldind}). If slot \code{obj_coef} is
      \code{NA}, the content of slot \code{lp_obj} is returned.
       In contrast, method \code{lp_obj} always returns the value of the
       objective function of the used algorithm after optimization.
    }
    \item{\code{nfluxes}:}{\code{signature(object = "optsol")}:
      gets the number of elements in the flux distribution matrix.
    }
    \item{\code{num_of_prob<-}:}{\code{signature(object = "optsol")}:
      sets the \code{num_of_prob} slot.
    }
    \item{\code{num_of_prob}:}{\code{signature(object = "optsol")}:
      gets the \code{num_of_prob} slot.
    }
    \item{\code{obj_coef<-}:}{\code{signature(object = "optsol")}:
      sets the \code{obj_coef} slot.
    }
    \item{\code{obj_coef}:}{\code{signature(object = "optsol")}:
      gets the \code{obj_coef} slot.
    }
    \item{\code{obj_func<-}:}{\code{signature(object = "optsol")}:
      sets the \code{obj_func} slot.
    }
    \item{\code{obj_func}:}{\code{signature(object = "optsol")}:
      gets the \code{obj_func} slot.
    }
    \item{\code{react_id<-}:}{\code{signature(object = "optsol")}:
      sets the \code{react_id} slot.
    }
    \item{\code{react_id}:}{\code{signature(object = "optsol")}:
      gets the \code{react_id} slot.
    }
    \item{\code{show}:}{\code{signature(object = "optsol")}:
      prints a summary of the content of instance of class \code{optsol}.
    }
    \item{\code{solver<-}:}{\code{signature(object = "optsol")}:
      sets the \code{solver} slot.
    }
    \item{\code{solver}:}{\code{signature(object = "optsol")}:
      gets the \code{solver} slot.
    }
  }
}

\author{Gabriel Gelius-Dietrich}

\seealso{
  \code{\link{checkOptSol}},
  \code{\linkS4class{optsol_optimizeProb}}
  \code{\linkS4class{optsol_fluxdel}},
  \code{\linkS4class{optsol_genedel}},
  \code{\linkS4class{optsol_robAna}} and
  \code{\linkS4class{optsol_fluxVar}}
}

\examples{
  showClass("optsol")
}

\keyword{classes}
