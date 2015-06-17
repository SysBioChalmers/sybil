\name{optimizeProb-methods}

\docType{methods}
\encoding{utf8}

\alias{optimizeProb}
\alias{optimizeProb-methods}
\alias{optimizeProb,modelorg-method}
\alias{optimizeProb,sysBiolAlg-method}

\title{Optimize Problem Object}

\description{
  The generic \code{optimizeProb} performs the optimization of a mathematical
  programming object.
}

\usage{
\S4method{optimizeProb}{modelorg}(object,
             algorithm = SYBIL_SETTINGS("ALGORITHM"),
             gene = NULL,
             react = NULL,
             lb = NULL,
             ub = NULL,
             retOptSol = TRUE,
             obj_coef = NULL,
             lpdir = NULL,
             mtfobj = NULL,
             fldind = TRUE,
             prCmd = NA,
             poCmd = NA,
             prCil = NA,
             poCil = NA,
             ...)

\S4method{optimizeProb}{sysBiolAlg}(object,
             react = NULL,
             lb = NULL,
             ub = NULL,
             obj_coef = NULL,
             lpdir = NULL,
             fldind = TRUE,
             resetChanges = TRUE,
             prCmd = NA,
             poCmd = NA,
             prCil = NA,
             poCil = NA)
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "modelorg")}}{
      Translates the object of class \code{\linkS4class{modelorg}} into an
      object of class \code{\linkS4class{sysBiolAlg}} and calls
      \code{optimizeProb} again.
    }
    \item{\code{signature(object = "sysBiolAlg")}}{
      Run optimization with the given problem object.
    }
  }
}

\arguments{
  \item{object}{
    An object of class \code{\linkS4class{modelorg}} or
    \code{\linkS4class{sysBiolAlg}}.
  }
  \item{algorithm}{
    A single character string giving the name of the algorithm to use. See
    parameter \code{"ALGORITHM"} in \code{\link{SYBIL_SETTINGS}} for possible
    values.\cr
    Default: \code{SYBIL_SETTINGS("ALGORITHM")}.
  }
  \item{gene}{
    A character or integer vector containing gene id's or indices of gene id's
    in \code{\link{allGenes}(model)}. If arguments \code{lb} and/or \code{ub}
    are additionally used (not \code{NULL}), upper and lower bounds will be
    applied to all fluxes on which the deletion of the genes given in
    \code{gene} have an effect. In this case, the first value in \code{lb} and
    \code{ub} is used.
    Default: \code{NULL}.
  }
  \item{react}{
    An object of class \code{\linkS4class{reactId}}, character or integer.
    Specifies the fluxes (variables) for which to change the upper and lower
    bound (see also arguments \code{lb} and \code{ub}) or objective coefficients
    (see also argument \code{obj_coef}). For class
    \code{\linkS4class{sysBiolAlg}}, it must be numeric. For class
    \code{\linkS4class{modelorg}}, setting \code{react} as no effect, if
    \code{gene} is also not \code{NULL}.\cr
    Default: \code{NULL}.
  }
  \item{lb}{
    Numeric vector, must have the same length as \code{react}.  Contains the new
    values for the lower bounds of fluxes (variables) mentioned in \code{react}.
    If set to \code{NULL}, lower bounds for variables in \code{react} will be
    left unchanged. For class \code{\linkS4class{modelorg}}: if \code{lb} is of length
    one, \code{lb} is used for all elements in \code{react}.\cr
    Default: \code{NULL}.
  }
  \item{ub}{
    Same functionality as \code{lb}, but for upper bounds.\cr
    Default: \code{NULL}.
  }
  \item{obj_coef}{
    Numeric vector, must have the same length as \code{react}.  Contains the new
    values for the objective coefficients of fluxes (variables) mentioned in
    \code{react}. All other objective coefficients stay untouched. If set to
    \code{NULL}, objective coefficients for variables in \code{react} will be
    left unchanged. For class \code{\linkS4class{modelorg}}: if \code{obj_coef} is of
    length one, \code{obj_coef} is used for all elements in \code{react}.\cr
    Default: \code{NULL}.
  }
  \item{lpdir}{
    Character value, direction of optimization.  Can be set to \code{"min"} for
    minimization or \code{"max"} for maximization.\cr
    Default: \code{SYBIL_SETTINGS("OPT_DIRECTION")}.
  }
  \item{mtfobj}{
    Only used, if argument \code{algorithm} is set to \code{"mtf"}.
    A single numeric value giving a previously calculated optimized value of the
    objective function given in the model.
    The objective function of the model will be fixed to this value during
    optimization. If set to \code{NULL}, it will be computed by means of the
    \code{"\link[=sysBiolAlg_fba-class]{fba}"} algorithm. If additionally
    arguments \code{solver} and \code{method} are set, they will be used here
    too.\cr
    Default: \code{NULL}.
  }
  \item{fldind}{
    Boolean value. If set to \code{TRUE}, (default) indices in \code{"react"} are used
    only for reactions. If set to \code{FALSE}, indices in \code{"react"} are used for
    all variables during optimization, e.g. also for additional variables introduced by
    the \code{\link{mtf}} algorithm. Currently unused by class
    \code{\linkS4class{sysBiolAlg_room}}.\cr
    Default: \code{TRUE}.
  }
  \item{resetChanges}{
    Boolean value. If set to \code{TRUE}, (default) modifications of the
    problem object will be reset to their original values (e.g. changing
    upper and lower bounds for certain reactions). If set to \code{FALSE},
    modifications will stay in the model.\cr
    Default: \code{TRUE}.
  }
  \item{prCmd}{
    A list of preprocessing commands.  See Details below.\cr
    Default: \code{NA}.
  }
  \item{poCmd}{
    A list of postprocessing commands.  See Details below.\cr
    Default: \code{NA}.
  }
  \item{prCil}{
    Can be used if \code{optimizeProb} is called several times (like in
    \code{\link{optimizer}}). The argument \code{prCil} gets the value of the
    loop variable and passes it to the preprocessing function. There, one
    can access it via the keyword \dQuote{LOOP_VAR}. See also
    \code{\link{optimizer}}.\cr
    Default: \code{NA}.
  }
  \item{poCil}{
    Same as \code{prCil}, but for postprocessing.\cr
    Default: \code{NA}.
  }
  \item{retOptSol}{
    Boolean.  Return an object of class
    \code{\linkS4class{optsol_optimizeProb}} or
    just a list containing the results.\cr
    Default: \code{TRUE}.
  }
  \item{\dots}{
    Only for the \code{modelorg-method}: further arguments passed to
    \code{\link{sysBiolAlg}}. See Details below.
  }
}

\details{
  The arguments \code{prCmd} and \code{poCmd} can be used to execute \R commands
  working on the problem object.  All commands in \code{prCmd} are executed
  immediately before solving the problem; all commands in \code{poCmd} are
  executed after the problem has been solved.  In all other aspects, the
  arguments work the same.  The value of \code{prCmd} or \code{poCmd} are
  lists of character vectors (each list element is one command).  Each command
  is a character vector and should be built as follows:
  \itemize{
      \item The first element is the name of the function to call.
      \item All other elements are arguments to the function named in the
            first element.
      \item If any argument is character, enclose it in single
            quotes \code{' '}.
      \item Use the keyword \kbd{LP_PROB} in order to refer to the variable name
            of the problem object (object of class
            \code{\linkS4class{optObj}}).
      \item If the length of the character vector is one, it is treated as a
            function call with the problem object
            (object of class \code{\linkS4class{optObj}}) as single
            argument.
  }
  The result will be an object of class \code{\link{ppProc}}.
  A few examples for arguments \code{prCmd} or \code{poCmd} (all arguments must
  be lists, see examples section below):
  \preformatted{
    sensitivityAnalysis
  }
  will be translated to the command
  \preformatted{
    sensitivityAnalysis(LP_PROB)
  }
  with \code{LP_PROB} being the placeholder for the variable name of the
  problem object. The vector
  \preformatted{
    c("writeProb", "LP_PROB", "'Ec_core.lp'", "'lp'")
  }
  will be translated to the command
  \preformatted{
    writeProb(LP_PROB, 'Ec_core.lp', 'lp')
  }
  The first element will be the function name and the others the arguments to
  that function. The list of commands
  \preformatted{
    list("sensitivityAnalysis",
         c("getDjCPLEX", "LP_PROB@oobj@env",
           "LP_PROB@oobj@lp", "0", "react_num(Ec_core)-1"
         )
    )
  }
  will be translated to the commands
  \preformatted{
    sensitivityAnalysis(LP_PROB)
    getDjCPLEX(LP_PROB@oobj@env, LP_PROB@oobj@lp,
               0, react_num(Ec_core)-1)
  }
  For more information on the usage of \code{prCmd} and \code{poCmd}, see the
  examples section below.

  The method \code{optimizeProb} for class \code{\linkS4class{modelorg}}
  generates a subclass of class \code{\linkS4class{sysBiolAlg}}
  and calls \code{optimizeProb} for that object again. Argument \code{MoreArgs}
  is used to transport arguments to the second \code{optimizeProb} call.
  Argument \code{\dots} instead is used to transport arguments to the constructor
  function \code{\link{sysBiolAlg}}, for example \code{algorithm},
  \code{solver}, \code{method} and \code{solverParm}.
  See \code{\link{SYBIL_SETTINGS}} for possible values.

  Arguments \code{gene}, \code{react}, \code{lb}, \code{ub} and \code{react}
  cause changes in the problem object (object of class
  \code{\linkS4class{optObj}}, slot \code{problem} of class
  \code{\linkS4class{sysBiolAlg}}). These changes will be reset
  immediately after optimization if argument \code{resetChanges} is set to
  \code{TRUE}, otherwise changes will persist.
}

\value{
  Calls to \code{optimizeProb} returns either an object of class
  \code{\linkS4class{optsol_optimizeProb}} of length one
  if argument \code{retOptSol} is set to \code{TRUE} and \code{object} is of
  class \code{\linkS4class{modelorg}}, or a list containing the results of the
  optimization:
  \item{ok}{
    Return value of the optimizer (e.g. \dQuote{solution process was
    successful} or \dQuote{time limit exceeded}).
  }
  \item{obj}{
    Value of the objective function after optimization.
  }
  \item{stat}{
    Status value of the optimization (e.g. \dQuote{solution is optimal} or
    \dQuote{no feasible solution exists}).
  }
  \item{fluxes}{
    The resulting flux distribution.
  }
  \item{fldind}{
    Pointers to columns (variables) representing a flux (reaction) in the
    original network. The variable \code{fldind[i]} in the solution object
    represents reaction \code{i} in the original network. 
  }
  \item{preP}{
    An object of class \code{\link{ppProc}} if a preprocessing command
    was given.
  }
  \item{postP}{
    An object of class \code{\link{ppProc}} if a postprocessing command
    was given.
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  \code{\link{modelorg}}, \code{\link{applyChanges}} and \code{\linkS4class{sysBiolAlg}}.
}

\examples{
\dontrun{
## The examples here require the package glpkAPI to be
## installed. If that package is not available, you have to set
## the argument 'solver' (the default is: solver = SYBIL_SETTINGS("SOLVER")).

## load the example data set
data(Ec_core)

## run optimizeProb(), Ec_sf will be an object of
## class optsol_optimizeProb
Ec_sf <- optimizeProb(Ec_core)

## run optimizeProb(), Ec_sf will be a list
Ec_sf <- optimizeProb(Ec_core, retOptSol = FALSE)

## do FBA, change the upper and lower bounds for the reactions
## "ATPM" and "PFK".
optimizeProb(Ec_core, react = c("ATPM", "PFK"),
             lb = c(3, -3), ub = c(5, 6))
          
## do FBA, perform sensitivity analysis after optimization
optimizeProb(Ec_core, poCmd = list("sensitivityAnalysis"))

## do FBA, write the problem object to file in lp-format
optimizeProb(Ec_core,
             poCmd = list(c("writeProb", "LP_PROB",
                            "'Ec_core.lp'", "'lp'")))

## do FBA, use "cplexAPI" as lp solver. Get all lower bounds before
## solving the problem. After solving, perform a sensitivity
## analysis and retrieve the reduced costs
opt <- optimizeProb(Ec_core, solver = "cplexAPI",
                    prCmd = list(c("getColsLowBnds", "LP_PROB", "1:77")),
                    poCmd = list("sensitivityAnalysis",
                                 c("getDjCPLEX",
                                 "LP_PROB@oobj@env",
                                 "LP_PROB@oobj@lp",
                                 "0", "react_num(Ec_core)-1")))
## get lower bounds
preProc(opt)
## get results of sensitivity analysis
postProc(opt)
}
}

\keyword{methods}
\keyword{optimize}
