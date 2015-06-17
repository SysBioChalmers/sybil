\name{optimizer}
\alias{optimizer}

\encoding{utf8}

\title{Performs Series of Optimizations}

\description{
  The function \code{optimizer} is a wrapper to the
  \code{\linkS4class{sysBiolAlg}}-method \code{\link{optimizeProb}}.
  While \code{optimizeProb} runs one optimization, \code{optimizer} is designed
  to run a series of optimization by re-optimizing a given problem object
  (successive calls to \code{\link{optimizeProb}}.
}

\usage{
optimizer(model, react, lb, ub, obj_coef, lpdir,
          algorithm = SYBIL_SETTINGS("ALGORITHM"),
          mtfobj = NULL,
          setToZero = FALSE,
          rebuildModel = FALSE,
          fld = "none",
          prCmd = NA, poCmd = NA,
          prDIR = NULL, poDIR = NULL,
          verboseMode = 2,
          ...)
}

\arguments{
  \item{model}{
    An object of class \code{modelorg}.
  }
  \item{react}{
    A list of numeric vectors. Each value must point to a reaction id present
    in \code{model}. The length of the list in \code{react} determines the
    number of optimizations to run. Each list element can be used in conjunction
    with arguments \code{lb} and \code{ub} or \code{obj_coef} and \code{lpdir}.
    The parameters given in this arguments will be set temporarily for each
    optimization.
  }
  \item{lb}{
    A numeric vector or list of the same length as \code{react} or a matrix with
    the number of rows equal to the length of \code{react} containing the lower
    bounds for the reaction rates of reactions (variables) given in argument
    \code{react}. If set to \code{NULL}, no lower bounds will be changed.
    If \code{lb} is a vector, \code{lb[k]} is used as lower bound for all
    reactions given in \code{react[k]}. If \code{lb} is a list, \code{lb[k]}
    must have the same length as \code{react[k]}. If \code{lb} is a matrix, each
    row serves as lower bound for the reactions given in each element of
    \code{react} (all elements in \code{react} must have the same length).\cr
    Default: \code{NULL}.
  }
  \item{ub}{
    A numeric vector or list of the same length as \code{react} or a matrix with
    the number of rows equal to the length of \code{react} containing the upper
    bounds for the reaction rates of reactions (variables) given in argument
    \code{react}. If set to \code{NULL}, no upper bounds will be changed.
    If \code{ub} is a vector, \code{ub[k]} is used as upper bound for all
    reactions given in \code{react[k]}. If \code{ub} is a list, \code{ub[k]}
    must have the same length as \code{react[k]}. If \code{ub} is a matrix, each
    row serves as upper bound for the reactions given in each element of
    \code{react} (all elements in \code{react} must have the same length).\cr
    Default: \code{NULL}.
  }
  \item{obj_coef}{
    A numeric vector or list of the same length as \code{react} or a matrix with
    the number of rows equal to the length of \code{react} containing the
    objective coefficients for the reactions (variables) given in argument
    \code{react}. If set to \code{NULL}, no objective coefficients will be
    changed. If \code{obj_coef} is a vector, \code{obj_coef[k]} is used as
    objective coefficients for all reactions given in \code{react[k]}.
    If \code{obj_coef} is a list, \code{obj_coef[k]} must have the same length
    as \code{react[k]}. If \code{obj_coef} is a matrix, each row serves as
    objective coefficient for the reactions given in each element of
    \code{react} (all elements in \code{react} must have the same length).\cr
    Default: \code{NULL}.
  }
  \item{lpdir}{
    A character vector of the same length as \code{react} containing the
    direction of optimization for each optimization. Possible values are
    \code{"min"} for minimization, or \code{"max"} for maximization. If set to
    \code{NULL}, optimization direction will not change.\cr
    Default: \code{NULL}.
  }
  \item{algorithm}{
    A single character value giving the algorithm to compute genetic
    perturbations. Can be
    \code{"\link[=sysBiolAlg_fba-class]{fba}"}: flux-balance analysis,
    \code{"\link[=sysBiolAlg_mtf-class]{mtf}"}: minimization of absolute total
                                                flux (see Details below),
    \code{"\link[=sysBiolAlg_moma-class]{moma}"}: minimization of
                                                  metabolic adjustment (MOMA),
    \code{"\link[=sysBiolAlg_lmoma-class]{lmoma}"}: linear version of MOMA,
    \code{"\link[=sysBiolAlg_room-class]{room}"}: regulatory on/off
                                                  minimization (ROOM) or
    \code{"\link[=sysBiolAlg_fv-class]{fv}"}: flux variability analysis.\cr
    Default: \code{SYBIL_SETTINGS("ALGORITHM")}.
  }
  \item{mtfobj}{
    Only used, if argument \code{algorithm} is set to \code{"mtf"}.
    A numeric vector of the same length as \code{react} containing previously
    calculated optimized values of the objective function given in the model.
    The objective function of the model will be fixed to this values in each
    optimization. If set to \code{NULL}, they will be computed by means of the
    \code{"\link[=sysBiolAlg_fba-class]{fba}"} algorithm. If additionally
    arguments \code{solver} and \code{method} are set, they will be used here
    too.\cr
    Default: \code{NULL}.
  }
  \item{setToZero}{
    Logical: If the mathematical programming software returns a solution status
    which is not optimal, set the corresponding objective value to zero.\cr
    Default: \code{FALSE}.
  }
  \item{rebuildModel}{
    Logical.  If set to \code{TRUE}, the problem object will be rebuilt prior
    each round of optimization.
    Default: \code{FALSE}.
  }
  \item{fld}{
    Type of flux distribution to return. If set to \code{"none"}, no flux
    distribution will be returned. If set to \code{"fluxes"}, only the real
    flux distribution is returned, meaning all variable values after
    optimization representing a flux (reaction) in the model. If set to
    \code{"all"}, all variable values are returned. If \code{algorithm} is set
    to \code{"mtf"} and \code{fld} equals \code{"none"}, argument \code{fld}
    will be changed to \code{"fluxes"}.\cr
    Default: \code{"none"}.
  }
  \item{prCmd}{
    A list of preprocessing commands passed to \code{\link{optimizeProb}}.
    See there for details.\cr
    Default: \code{NA}.
  }
  \item{poCmd}{
    A list of postprocessing commands passed to \code{\link{optimizeProb}}.
    See there for details.\cr
    Default: \code{NA}.
  }
  \item{prDIR}{
    A numeric or character vector, indicating in which round of optimization
    the preprocessing command(s) will be executed. \code{prDIR = c(2, 5, 10)}
    executes the commands in prCmd before the second, 5th and 10th optimization.
    
    If \code{prDIR} is a character vector, for example
    \code{prDIR = c("10")}, the preprocessing commands given in \code{prCmd}
    will pe executed every 10th round of optimization. 
    
    If \code{prDIR} is character and has length \code{2}, the first element is
    an offset to the following elements. \code{prDIR = c("-2", "10")} will do
    the preprocessing on every 10th round of optimization, beginning in round
    number \code{10 - 2 = 8}.\cr
    Default: \code{NULL}.
  }
  \item{poDIR}{
    The same as \code{prDIR}, but for postprocessing.
    Default: \code{NULL}.
  }
  \item{verboseMode}{
    Single integer value, giving the amount of output to the console. Use
    \code{\link{sink}} to redirect output to a file. If \code{verboseMode == 1}
    status messages will be printed, if \code{verboseMode == 2} additionally
    a progress bar will be produced. If \code{verboseMode > 2}, intermediate
    results will be printed. Use \code{\link{suppressMessages}} to disable any
    output to the console.\cr
    Default: \code{2}.
  }
  \item{\dots}{
    Further arguments passed to \code{\link{sysBiolAlg}}.
  }
}

\value{
  A list containing the results of the optimization:
  \item{solver}{
    A single character string indicating the used mathematical programming
    software.
  }
  \item{method}{
    A single character string indicating the used optimization method by the
    mathematical programming software.
  }
  \item{algorithm}{
    A single character string indicating the used algorithm.
  }
  \item{lp_num_cols}{
    Number of columns (variables) in the problem object.
  }
  \item{lp_num_rows}{
    Number of rows (constraints) in the problem object.
  }
  \item{obj}{
    A numeric vector containing the values of the objective function after
    optimization.
  }
  \item{ok}{
    A numeric vector containing the return values of the optimizer
    (e.g. \dQuote{solution process was successful} or
    \dQuote{time limit exceeded}).
  }
  \item{stat}{
    A numeric vector containing the status value of the optimization (e.g.
    \dQuote{solution is optimal} or \dQuote{no feasible solution exists}).
  }
  \item{lp_dir}{
    A factor variable indicating the direction of optimization for each
    optimization.
  }
  \item{fldind}{
    Pointers to columns (variables) representing a flux (reaction) in the
    original network. The variable \code{fldind[i]} in the solution object
    represents reaction \code{i} in the original network. 
  }
  \item{fluxdist}{
    The resulting flux distribution.
  }
  \item{prAna}{
    An object of class \code{\link{ppProc}} if a preprocessing command
    was given.
  }
  \item{poAna}{
    An object of class \code{\link{ppProc}} if a postprocessing command
    was given.
  }
  \item{alg_par}{
    A named list of algorithm specific parameters.
  }
}

\references{
  Becker, S. A., Feist, A. M., Mo, M. L., Hannum, G., Palsson, B. Ø.  and
  Herrgard, M. J. (2007) Quantitative prediction of cellular metabolism with
  constraint-based models: the COBRA Toolbox. \emph{Nat Protoc} \bold{2},
  727--738.

  Schellenberger, J., Que, R., Fleming, R. M. T., Thiele, I., Orth, J. D.,
  Feist, A. M., Zielinski, D. C., Bordbar, A., Lewis, N. E., Rahmanian, S.,
  Kang, J., Hyduke, D. R. and Palsson, B. Ø. (2011) Quantitative prediction of
  cellular metabolism with constraint-based models: the COBRA Toolbox v2.0.
  \emph{Nat Protoc} \bold{6}, 1290--1307.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  Class \code{\linkS4class{sysBiolAlg}}, and constructor function
  \code{\link{sysBiolAlg}}, \code{\link{optimizeProb}}
  and \code{\link{SYBIL_SETTINGS}}.
}


\keyword{optimize}

