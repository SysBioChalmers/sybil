\name{optsol_fluxVar-class}

\Rdversion{1.1}
\docType{class}

\alias{optsol_fluxVar-class}

\alias{react,optsol_fluxVar-method}
\alias{react<-,optsol_fluxVar-method}
\alias{maxSol,optsol_fluxVar-method}
\alias{minSol,optsol_fluxVar-method}
\alias{plot,optsol_fluxVar,missing-method}
\alias{plotRangeVar,optsol_fluxVar-method}
\alias{plotRangeVar}
\alias{blReact,optsol_fluxVar-method}
\alias{blReact}


\title{Class \code{"optsol_fluxVar"}}

\description{
  Structure of the class \code{"optsol_fluxVar"}. Objects of that class are
  returned by the function \code{\link{fluxVar}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("optsol_fluxVar", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{react}:}{Object of class \code{"reactId"}
      containing reaction id's for which ranges were calculated.
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
      optimization.
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
    \item{\code{react}:}{\code{signature(object = "optsol_fluxVar")}:
      gets the \code{react} slot.
    }
    \item{\code{react<-}:}{\code{signature(object = "optsol_fluxVar")}:
      sets the \code{react} slot.
    }
    \item{\code{maxSol}:}{
      \code{signature(object = "optsol_fluxVar")}(\code{slot}): returns the
      values in the slot given in \code{slot} for optimizations in \dQuote{max}
      direction.
    }
    \item{\code{minSol}:}{
      \code{signature(object = "optsol_fluxVar")}(\code{slot}): returns the
      values in the slot given in \code{slot} for optimizations in \dQuote{min}
      direction.
    }
    \item{\code{plot}}{
      \code{signature(x = "optsol_fluxVar", y = "missing")}
      (\code{ylim, xlab = "", ylab = "Value", pch = 20, col = "black",
             collower, colupper, pchupper, pchlower,
             dottedline = FALSE, baseline = 0, ...}):\cr
      plots the range of values each flux can have still giving an optimal
      objective function value.
      \describe{
        \item{\code{ylim}}{
          scaling of y-axis, if missing, the maximum and minimum value of
          all optimizations is used (rounded to the next smaller/larger
          integer value).
        }
        \item{\code{xlab}}{
          label of x-axis, see also \code{\link{par}}.
        }
        \item{\code{ylab}}{
          label of y-axis, see also \code{\link{par}}.
        }
        \item{\code{pch}}{
          how to plot the points, see also \code{\link{par}}.
        }
        \item{\code{col}}{
          color of the plot, see also \code{\link{par}}.
        }
        \item{\code{collower}}{
          color of the minimum range value. Default \code{col}.
        }
        \item{\code{colupper}}{
          color of the maximum range value. Default \code{col}.
        }
        \item{\code{pchupper}}{
          how to plot the point for the maximum range value. Default \code{pch}.
        }
        \item{\code{pchlower}}{
          how to plot the point for the minimum range value. Default \code{pch}.
        }
        \item{\code{dottedline}}{
          if set to \code{FALSE}, from each minimum range value a dotted line to
          the corresponding x-axis label will be plotted. Default \code{FALSE}.
        }
        \item{\code{baseline}}{
          plot a horizontal dashed line at the value of \code{baseline}. Default
          \code{0}. If set to \code{NA}, no baseline will be plotted.
        }
        \item{\code{connect}}{
          if set to \code{TRUE}, a solid connecting line will be drawn
          between the minimum and maximum value of one reaction.
          Default \code{TRUE}.
        }
        \item{\code{colconnect}}{
          color of the connecting line. Default \code{"black"}.
        }
        \item{\code{\dots}}{
          further arguments to the \code{\link{plot}} function.
        }
      }
    }
    \item{\code{plotRangeVar}}{\code{signature(object = "optsol_fluxVar")}
      (\code{...}): plot a histogram of the span of the minimum and maximum
      range values for each flux.
      \describe{
        \item{\code{...}}{
          further arguments to the \code{\link{hist}} function.
        }
      }
    }
    \item{\code{blReact}}{\code{signature(object = "optsol_fluxVar")}
      (\code{tol = SYBIL_SETTINGS("TOLERANCE")}): returns a logical vector of
      length equal to the number of reactions analyzed during flux variance
      analysis (number of optimizations divided by two).
      If \code{blReact(object)[j]} equals \code{TRUE}, reaction \code{j} is
      considered to be blocked (zero flux rate) given the used conditions.
      A reaction \eqn{j} is considered to be \sQuote{blocked}, if its calculated
      range of reaction rates does not exceed \code{0 +/- tol}.
      \describe{
        \item{\code{tol}}{
          limit of tolerance.
        }
      }
    }
  }
}

\author{Gabriel Gelius-Dietrich}

\seealso{
  \code{\link{checkOptSol}} and \code{\linkS4class{optsol}}
}
      
\examples{
  showClass("optsol_fluxVar")
}

\keyword{classes}
