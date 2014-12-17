\name{optsol_robAna-class}

\Rdversion{1.1}
\docType{class}

\alias{optsol_robAna-class}

\alias{ctrlfl,optsol_robAna-method}
\alias{ctrlfl<-,optsol_robAna-method}
\alias{ctrlfl}
\alias{ctrlfl<-}
\alias{ctrlr,optsol_robAna-method}
\alias{ctrlr<-,optsol_robAna-method}
\alias{ctrlr}
\alias{ctrlr<-}
\alias{plot,optsol_robAna,missing-method}

\title{Class \code{"optsol_robAna"}}

\description{
  Structure of the class \code{"optsol_robAna"}. Objects of that class are
  returned by the function \code{\link{robAna}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("optsol_robAna", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{ctrlr}:}{Object of class \code{"reactId"}
      containing the reaction id of the control reaction.
    }
    \item{\code{ctrlfl}:}{Object of class \code{"numeric"}
      containing the control flux values.
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
  Class \code{"\linkS4class{optsol}"}, by class "optsol_optimizeProb", distance 2.
}

\section{Methods}{
  \describe{
    \item{\code{ctrlfl}:}{\code{signature(object = "optsol_robAna")}:
      gets the \code{ctrlfl} slot.
    }
    \item{\code{ctrlfl<-}:}{\code{signature(object = "optsol_robAna")}:
      sets the \code{ctrlfl} slot.
    }
    \item{\code{ctrlr}:}{\code{signature(object = "optsol_robAna")}:
      gets the \code{ctrlr} slot.
    }
    \item{\code{ctrlr<-}:}{\code{signature(object = "optsol_robAna")}:
      sets the \code{ctrlr} slot.
    }
    \item{\code{plot}}{\code{signature(x = "optsol_robAna", y = "missing")}
      (\code{xlab = paste("Control Flux:", react_id(ctrlr(x))),
             ylab = paste("Objective Function:", obj_func(x)),
             type = "b", pch = 20, fillColorBg = "grey", fillBg = TRUE,
             absCtrl = TRUE, ...}):\cr
      plots the optimal values of the objective function vs. the control flux
      values.
      \describe{
        \item{\code{xlab}}{
          label of x-axis, see also \code{\link{par}}.
        }
        \item{\code{ylab}}{
          label of y-axis, see also \code{\link{par}}.
        }
        \item{\code{type}}{
          plot type, see also \code{\link{par}}.
        }
        \item{\code{pch}}{
          how to plot the points, see also \code{\link{par}}.
        }
        \item{\code{fillColorBg}}{
          color of the area below the curve.
        }
        \item{\code{fillBg}}{
          logical: color the area below the curve.
        }
        \item{\code{absCtrl}}{
          if set to \code{TRUE}, the control flux values (x axis) are plotted
          as absolute values.
        }
        \item{\code{...}}{
          further graphical parameters to the \code{\link{points}} function.
        }
      }
    }
  }
}

\author{Gabriel Gelius-Dietrich}

\seealso{
  \code{\link{robAna}}, \code{\link{checkOptSol}} and
  \code{\linkS4class{optsol}}
}
      
\examples{
  showClass("optsol_robAna")
}

\keyword{classes}
