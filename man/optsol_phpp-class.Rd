\name{optsol_phpp-class}

\Rdversion{1.1}
\docType{class}

\alias{optsol_phpp-class}

\alias{ctrlfl,optsol_phpp-method}
\alias{ctrlfl<-,optsol_phpp-method}
\alias{getRedCosts,optsol_phpp-method}
\alias{plot,optsol_phpp,character-method}
\alias{plot,optsol_phpp,missing-method}

\title{Class \code{"optsol_phpp"}}

\description{
  Structure of the class \code{"optsol_robAna"}. Objects of that class are
  returned by the function \code{\link{phpp}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("optsol_phpp", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{ctrlflm}:}{Object of class \code{"matrix"}
      containing the control flux values.
    }
    \item{\code{redCosts}:}{Object of class \code{"matrix"}
      containing the reduced costs of the two control flux values.
    }
    \item{\code{ctrlr}:}{Object of class \code{"reactId"}
      containing the reaction id of the control reaction.
    }
    \item{\code{ctrlfl}:}{Object of class \code{"numeric"}
      unused, see \code{ctrlflm}.
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
  Class \code{"\linkS4class{optsol_robAna}"}, directly.
  Class \code{"\linkS4class{optsol_optimizeProb}"}, by class "optsol_robAna", distance 2.
  Class \code{"\linkS4class{optsol}"}, by class "optsol_robAna", distance 3.
}

\section{Methods}{
  \describe{
    \item{ctrlfl}{\code{signature(object = "optsol_phpp")}:
      gets the \code{ctrlflm} slot.
    }
    \item{ctrlfl<-}{\code{signature(object = "optsol_phpp")}:
      sets the \code{ctrlflm} slot.
    }
    \item{getRedCosts}{\code{signature(lp = "optsol_phpp")}:
      gets the \code{ctrlflm} slot.
    }
    \item{plot}{\code{signature(x = "optsol_phpp", y = "character")}:
      (\code{main = paste("Reduced Costs:", y),
             xlab = react_id(ctrlr(x)[1]),
             ylab = react_id(ctrlr(x)[2]),
             shrink = c(0.95, 0.95),
             col.regions = colorRampPalette(c("#FFFFFF", "#F0F0F0",
                                    "#D9D9D9", "#BDBDBD", "#969696", "#737373",
                                    "#525252", "#252525", "#000000"))(100),
             ...}):\cr
      plots the reduced costs of the control fluxes as \code{\link{levelplot}}.
      \describe{
        \item{\code{y}}{
          reaction id of one control reaction.
        }
        \item{\code{main}}{
          plot title, see also \code{\link{levelplot}}.
        }
        \item{\code{xlab}}{
          label of x-axis, see also \code{\link{levelplot}}.
        }
        \item{\code{ylab}}{
          label of y-axis, see also \code{\link{levelplot}}.
        }
        \item{\code{shrink}}{
          scale of rectangles to plot, see \code{\link{levelplot}}.
        }
        \item{\code{col.regions}}{
          a vector of colors (default greyscale) see \code{\link{levelplot}}.
        }
        \item{\code{...}}{
          further graphical parameters to the \code{\link{levelplot}} function.
        }
      }
    }
    \item{plot}{\code{signature(x = "optsol_phpp", y = "missing")}:
      (\code{xlab = list(label = react_id(ctrlr(x)[1]), rot = 30, cex = 0.8),
             ylab = list(label = react_id(ctrlr(x)[2]), rot = -40, cex = 0.8),
             zlab = list(label = obj_func(x), rot = 90, cex = 0.8),
             scales = list(arrows = FALSE, cex = 0.6, font = 3, tck = 1, col = "black"),
             par.settings = list(axis.line = list(col = "transparent")),
             shade = TRUE,
             shade.colors = function(irr, ref, height, w = 0.75) {
                                grey(w * irr + (1 - w) * (1-(1-ref)^0.75))
             },
             ...}):\cr
      plots the optimal values of the objective function vs. the control flux
      values in a \code{\link{wireframe}} plot.
      \describe{
        \item{\code{xlab}}{
          label of x-axis, see also \code{\link{wireframe}}.
        }
        \item{\code{ylab}}{
          label of y-axis, see also \code{\link{wireframe}}.
        }
        \item{\code{zlab}}{
          label of z-axis, see also \code{\link{wireframe}}.
        }
        \item{\code{scales}}{
          parameters describing scales, see \code{\link{wireframe}}.
        }
        \item{\code{par.settings}}{
          additional parameters, see \code{\link{wireframe}}.
        }
        \item{\code{shade}}{
          enable/disable shading, see \code{\link{wireframe}}.
        }
        \item{\code{shade.colors}}{
          a function for the shading color (default greyscale),
          see \code{\link{wireframe}}.
        }
        \item{\code{...}}{
          further graphical parameters to the \code{\link{wireframe}} function.
        }
      }
    }
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  \code{\link{phpp}}, \code{\link{checkOptSol}} and \code{\linkS4class{optsol}}
}

\examples{
  showClass("optsol_phpp")
}

\keyword{classes}
