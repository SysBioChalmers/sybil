%% sybil Version History

\name{NEWS}
\title{sybil News}
\encoding{UTF-8}

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.10 2015-03-30}{
  \itemize{
    \item \code{findExchangeReact} can now deal with real big matrices (>30.000 columns).
    \item additional example for readProb and writeProb.
    \item modified \code{multiDel} to not use \code{require}
    \item \code{doubleFluxDel} had a bug if \code{react1} or \code{react2} contained duplicated entries.
  }
}

% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.9 2014-11-19}{
  \itemize{
    \item fixed a bug in class \code{modelorg} and function \code{addReact} when
          the modelorg object is initially empty
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.8 2014-08-29}{
  \itemize{
    \item for method \code{optimizeProb} of class \code{modelorg}: if arguments
          \code{lb}, \code{ub} or \code{obj_coef} are of length one, they are used
          for all elements given in \code{react}
    \item function \code{geneDel} returns a more illustrative error message if a gene
          association is malformed
    \item fixed a bug in optimizer which returns the wrong objective value in rare cases
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.7 2014-06-05}{
  \itemize{
    \item added boolean parameter \code{fldind} to methods \code{optimizeProb}
          and \code{applyChanges} which controls the usage of parameter \code{react}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.6 2014-06-04}{
  \itemize{
    \item function \code{findExchReact} gives a propper warning, if no exchange
          reactions can be found and returns \code{NULL} in this case
    \item fixed a bug in function \code{addReact} which accours when more than
          one new gene per reaction is added to the model
    \item function \code{checkSolStat} recognizes return code
          \code{CPXMIP_OPTIMAL_TOL (102)}: \sQuote{integer optimal, tolerance}
          as \sQuote{successfully solved}
    \item added parameter \code{absMAX} to the constructor methods of classes
          \code{sysBiolAlg_lmoma}, \code{sysBiolAlg_mtf} and
          \code{sysBiolAlg_room}, which is used as a maximum value for upper variable
          and contraint bounds
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.5 2013-11-27}{
  \itemize{
    \item for method \code{optimizeProb} of class \code{modelorg}: if both
          arguments \code{gene} and \code{react} are not \code{NULL}, a warning
          will be given that \code{react} is ignored
    \item added file \file{CITATION}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.4 2013-09-08}{
  \itemize{
    \item added methods \code{singletonMetabolites} and
          \code{deadEndMetabolites} to class \code{modelorg} to find dead end
          or singleton metabolites in metabolic networks
    \item fixed a bug in function \code{findExchReact} which accours when a
          model is in irreversible format
    \item added methods \code{setRowsNames}, \code{getRowsNames},
          \code{setColsNames} and \code{getColsNames} to classes extending
          class \code{optObj}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.3 2013-08-22}{
  \itemize{
    \item added function \code{phpp} for phenotypic phase plane analysis
    \item added class \code{optsol_phpp}
    \item added argument \code{rng} to function \code{robAna}
    \item fixed a bug in function \code{optimizer} which accours when setting
          arguments \code{setToZero} to \code{TRUE} and \code{verboseMode} to
          a value larger than two
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.2 2013-07-18}{
  \itemize{
    \item updated function \code{promptSysBiolAlg}, including now a very basic
          example how to set column, row and problem names in problem objects of
          class \code{sysBiolAlg} and \code{optObj}
    \item fixed a bug in function \code{addReact} which accours if the model
          does not contain a GPR association
    \item some adjustments in classes extending class \code{optObj}:
          methods \code{loadLPprob}, \code{addRowsToProb} and
          \code{changeRowsBnds} works in the same way for each solver (see the
          corresponding manual pages for details)
    \item added parameters \code{writeProbToFileName} and \code{pname} to
          constructor methods of classes extending class \code{sysBiolAlg}
    \item fixed a bug in method \code{changeRowBnds} of class
          \code{optObj_lpSolveAPI}
    \item suggests on \CRANpkg{glpkAPI} (version \eqn{\geq}{>=} 1.2.8)
    \item suggests on \CRANpkg{clpAPI} (version \eqn{\geq}{>=} 1.2.4)
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.1 2013-07-02}{
  \itemize{
    \item added parameter \code{scaleDist} to the constructor method of class
          \code{sysBiolAlg_moma}, which provides a fine tuning method of the
          impact of particular reactions on the objective function of the
          MOMA algorithm
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.0 2013-05-31}{
  \itemize{
    \item suggests on \CRANpkg{glpkAPI} (version \eqn{\geq}{>=} 1.2.7)
    \item suggests on \CRANpkg{clpAPI} (version \eqn{\geq}{>=} 1.2.3)
    \item variable and constraint names can now be set in objects of class
          \code{sysBiolAlg} and \code{optObj}
    \item added a new parameter \code{USE_NAMES}
    \item added arguments \code{cnames} and \code{rnames} to method
          \code{loadLPprob} for classes extending class \code{optObj}
    \item added argument \code{rnames} to method \code{addRowsToProb} for
          classes extending class \code{optObj}
    \item added arguments \code{cnames} and \code{rnames} to method
          \code{initialize} of class \code{sysBiolAlg}
    \item added arguments \code{useNames}, \code{cnames} and \code{rnames} to
          method \code{initialize} of classes \code{sysBiolAlg_fba},
          \code{sysBiolAlg_fv},\code{sysBiolAlg_lmoma},\code{sysBiolAlg_moma},
          \code{sysBiolAlg_mtf} and \code{sysBiolAlg_room}.
    \item in method \code{printMetabolite} of class \code{modelorg}:
          metabolite id's beginning with a digit or period are prefixed by the
          letter \code{"r"} and reaction id's beginning with a digit or period
          are prefixed by the letter \code{"x"}
    \item function \code{readTSVmod} checks input files for empty fields
    \item added method \code{changeUptake} to class \code{modelorg} to change
          media conditions in metabolic networks
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.15 2013-04-18}{
  \itemize{
    \item fixed a bug in functions \code{.generateWT} and \code{doubleGeneDel}
          which accours if argument solver was given as variable name and not
          as character value directly
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.14 2013-04-15}{
  \itemize{
    \item added argument \code{termOut} to method \code{initialize} of class
          \code{sysBiolAlg} enabling or disabling terminal output of the
          mathematical programming software
    \item function \code{checkSolStat} no longer returns an optimal MILP
          solution generated by IBM ILOG CPLEX as non-optimal
    \item some refinements for the regular expressions in function
          \code{readTSVmod} to identify stoichiometric coefficients and
          compartment flags of metabolites
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.13 2013-01-29}{
  \itemize{
    \item added argument \code{linInd} to function \code{doubleReact}
    \item return value of function \code{doubleReact} is now \code{FALSE} if
          no identical reactions are found, otherwise a list of id's
          corresponding to the reaction id's of identical reactions
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.12 2013-01-22}{
  \itemize{
    \item default value for argument \code{wtflux} in method \code{initialize}
          of class \code{sysBiolAlg_moma} is \code{NULL}
    \item new parameter to the MOMA algorithm (quadratic version): argument
          \code{Qmat} in method \code{initialize} of class
          \code{sysBiolAlg_moma} gives the quadratic part of the objective
          function
    \item new parameter to the MOMA algorithm (linear version): argument
          \code{obj_coefD} in method \code{initialize} of class
          \code{sysBiolAlg_lmoma} contains the non-zero part of the objective
          function
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.11 2013-01-18}{
  \itemize{
    \item fixed a bug in function \code{doubleGeneDel} which appears if
          argument \code{exLethal} is set to \code{TRUE}
    \item removed old file \file{NEWS.Rd} from top level directory
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.10 2012-12-21}{
  \itemize{
    \item fixed a bug in the calculation of default objective values in the
          \code{mtf} algorithm
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.9 2012-12-21}{
  \itemize{
    \item method \code{histogram} of class \code{optsol} plots the return value
          of \code{mod_obj}, not \code{lp_obj}
    \item removed argument \code{MoreArgs} from method \code{optimizeProb} of
          class \code{modelorg}
    \item slots \code{dels} and \code{fluxdels} of class \code{optsol_fluxdel}
          and class \code{optsol_genedel} are matrices of class \code{character}
    \item class \code{sysBiolAlg} has new methods \code{applyChanges} and
          \code{resetChanges} for altering objects of class \code{sysBiolAlg}
          temporarily
    \item replaced method \code{optimizeProb} of class \code{sysBiolAlg_room} by
          methods \code{applyChanges} and \code{resetChanges}
    \item constructor method of class \code{sysBiolAlg} has new arguments
          \code{retAlgPar} and \code{algPar} used for saving algorithm specific
          parameters in objects extending class \code{sysBiolAlg}
    \item added slot \code{maxobj} and method \code{changeMaxObj} to class
          \code{sysBiolAlg_mtf}
    \item fixed a bug in algorithm \code{mtf}: modifications in the model (like
          gene knock-outs) were ignored during the calculation of the optimized
          objective value
    \item argument algorithm to function \code{optimizer} can now also be
          \code{"mtf"} or \code{"fv"}
    \item added class \code{netFlux} and constructor function \code{getNetFlux}
          to order exchange reaction rates according to their sign
    \item function \code{optimizer} returns a list and no longer an object
          extending class \code{optsol}.
    \item removed method \code{logOptimizationNE} of class \code{sybilLog}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.8 2012-12-03}{
  \itemize{
    \item requires R version 2.14.2 or higher
    \item method \code{getFluxDist} of class \code{optsol} get additional
          argument \code{opt} to access specific optimization results
    \item removed argument \code{needId} from function \code{checkReactId}
    \item removed slot \code{has_id} and method \code{hasId} from
          class \code{reactId}
    \item removed constructor function \code{reactId} for objects of class
          \code{reactId}, use function \code{checkReactId} instead
    \item added function \code{editEnvir}: the environment editor for metabolic
          networks
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.7 2012-11-30}{
  \itemize{
    \item method \code{shrinkMatrix} can get objects of class \code{reactId}
    \item removed deprecated functions \code{simpleFBA} and \code{prepProbObj}
    \item removed deprecated generics \code{loadProblemData},
          \code{loadProblemDataLM}, \code{loadProblemDataMTF}
          and \code{loadMatrix}
    \item removed deprecated class \code{optsol_simpleFBA}
    \item some updates in the vignette
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.6 2012-11-26}{
  \itemize{
    \item class \code{reactId} is reorganized: it has new slots \code{mod_id},
          \code{mod_key}, \code{react_num} and \code{has_id} and new methods
          \code{hasId} and \code{[}, also the implementation of method
          \code{show} has changed
    \item new class \code{reactId_Exch} extending class \code{react_id}
    \item function \code{findExchReact} now returns an object of class
          \code{reactId_Exch} when getting an object of class \code{modelorg}
          instead of a list
    \item class \code{modelorg}, class \code{reactId}, class \code{optsol} and
          class \code{optsolSummary} have new slot \code{mod_key} containing a
          character string functioning as a unique key to a model object
    \item fixed a small bug in method \code{printObjFunc} of class
          \code{modelorg}
    \item fixed a small bug in function \code{fluxVar} which has occurred when
          not all reactions given in a model where analyzed
    \item function \code{fluxVar} has new argument \code{exex} in order to
          exclude exchange reactions from the analysis
    \item class \code{optsol_fluxVar} has a new method \code{blReact} which does
          essentially the same as function \code{blockedReact}
    \item function \code{blockedReact} is now deprecated
    \item class \code{optsol} has a new slot \code{obj_func} containing the
          objective function of the used model as a character string
    \item method \code{optimizeProb} of class \code{modelorg} returns an object
          of class \code{optsol_optimizeProb} as default behaviour (argument
          \code{retOptSol} is \code{TRUE}, default)
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.5 2012-10-31}{
  \itemize{
    \item function \code{geneDel} now returns \code{NULL} instead of \code{NA}
          if a gene deletion has no effect
    \item default value for arguments \code{gene}, \code{react}, \code{lb},
          \code{ub} and \code{obj_coef} in methods \code{optimizeProb} is
          \code{NULL}
    \item in \code{optimizeProb} all changes to variable bounds must be set
          explicitly, there is no default anymore
    \item added method \code{shrinkMatrix} to class \code{modelorg} for
          retrieving subsets of the stoichiometric matrix
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.4 2012-10-24}{
  \itemize{
    \item changed argument \code{alg} in method \code{initialize} of class
          \code{sysBiolAlg} to \code{sbalg}
    \item changed argument \code{alg} in function \code{optimizer}
          to \code{algorithm}
  }
}

% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.3 2012-10-22}{
  \itemize{
    \item slot \code{lp_dir} of class \code{optsol} is now of type \code{factor}
    \item solver names \code{"glpk"}, \code{"cplex"} and \code{"clp"} are not
          used anymore, use \code{"glpkAPI"}, \code{"cplexAPI"} and
          \code{"clpAPI"} instead
    \item added method \code{readProb} to class \code{optObj}
    \item removed slot \code{rhs} from class \code{modelorg}, because this
          parameter is algorithm dependent and not model dependent
    \item added new class \code{optsolSummary}
    \item the function \code{printObjFunc} is now a generic and has a method in
          class \code{modelorg}
    \item the function \code{checkOptSol} is now a generic and has a method in
          class \code{optsol} and class \code{optsolSummary}
    \item added methods \code{printMetabolite} and \code{printReaction} to
          class \code{modelorg}
    \item added method \code{nnzero} to class \code{fluxDistribution}
  }
}

% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.2 2012-10-10}{
  \itemize{
    \item fixed a bug in backupProb methods
    \item fixed a small bug in checkDefaultMethod
    \item moved method \code{mod_obj} from class \code{optsol_fluxdel} to the
          base class \code{optsol}
    \item added deprecated class \code{optsol_simpleFBA} for compatibility
          with other packages depending on \pkg{sybil}
  }
}

% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.1 2012-10-08}{
  \itemize{
    \item fixed file \file{DESCRIPTION}
  }
}

% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.0 2012-10-08}{
  \itemize{
    \item some performance improvements in function \code{fluxVar}
    \item new function \code{modelorg2ExPA}, which writes input files for
          \href{http://gcrg.ucsd.edu/Downloads/ExtremePathwayAnalysis}{ExPA}
    \item some very minor improvements in sybilStack (stack [LIFO] and
          queue [FIFO])
    \item solver names \code{glpk}, \code{cplex} and \code{clp} are deprecated,
          use \code{glpkAPI}, \code{cplexAPI} and \code{clpAPI} instead
    \item classes \code{optObj_glpk}, \code{optObj_cplex} and \code{optObj_clp}
          are renamed to \code{optObj_glpkAPI}, \code{optObj_cplexAPI} and
          \code{optObj_clpAPI}
    \item fixed a small bug in \code{SYBIL_SETTINGS} appearing if parameters
          were set several times
    \item argument \code{solverParm} can be \code{list} or \code{data.frame}
    \item argument \code{fmt} to the generic \code{writeProb} is now \code{ff}  
    \item added support for mixed interger optimization problems with
          \pkg{glpkAPI} and \pkg{cplexAPI}
    \item added support for quadratic optimization problems with \pkg{cplexAPI}
    \item added methods \code{changeMatrixRow}, \code{loadLPprob} and
          \code{loadQobj} to class \code{optObj}
    \item \pkg{cplexAPI} must be version 1.2.3 or higher, if used
    \item added function \code{addSolver} in order to make additional solver
          packages available
    \item added argument \code{arrowlength} to function \code{readTSVmod}
    \item added arguments \code{probType} and \code{loadPackage} to function
          \code{checkDefaultMethod}
    \item class \code{optsol_doublefluxdel} and class
          \code{optsol_doublegenedel} are not available anymore, class
          \code{optsol_fluxdel} and class \code{optsol_genedel} are used instead
    \item added new method \code{mod_obj} to class \code{optsol\_fluxdel}, which
          returns
          \eqn{\mbox{\boldmath$c$\unboldmath}^{\mathrm{T}}\mbox{\boldmath$v$\unboldmath}}{c^T v}
          with \eqn{\mbox{\boldmath$c$\unboldmath}}{c} being the vector of
          objective coefficients in the model (object of class \code{modelorg})
          and \eqn{\mbox{\boldmath$v$\unboldmath}}{v} being the flux
          distribution as a result of the optimization. In contrast, method
          \code{lp_obj} always returns the value of the objective function
          of the used algorithm after optimization.
    \item added functions \code{addAlgorithm}, \code{checkAlgorithm} and
          \code{getsybilenv}
    \item added a new class \code{sysBiolAlg} including a general constructor
          function \code{sysBiolAlg} and subclasses \code{sysBiolAlg_fba},
          \code{sysBiolAlg_fv}, \code{sysBiolAlg_lmoma}, \code{sysBiolAlg_moma},
          \code{sysBiolAlg_mtf} and \code{sysBiolAlg_room}. These classes
          contain instances of class \code{optObj}, prepared for specific
          algorithms like ROOM or MOMA
    \item support for algorithms MOMA and ROOM in class \code{sysBiolAlg}
    \item added method \code{optimizeProb} as a replacement for function
          \code{simpleFBA}: unused argument \code{checkIds} is removed, new
          arguments \code{gene} and \code{MoreArgs} were added.
    \item added function \code{promptSysBiolAlg} which generates a skeletal
          structure for new classes inhereting from \code{sysBiolAlg}
    \item functions \code{prepProbObj} and \code{simpleFBA} are deprecated and
          will be removed soon, use \code{sysBiolAlg} and \code{optimizeProb}
          instead
    \item methods \code{loadMatrix}, \code{loadProblemData},
          \code{loadProblemDataLM} and \code{loadProblemDataMTF} are deprecated
          and will be removed soon, use \code{loadLPprob} instead
    \item class \code{optsol} makes now use of the package \pkg{lattice} to
          plot histograms (use \code{histogram()} instead of \code{hist()})
    \item updated package vignette
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.0.2 2012-04-20}{
  \itemize{
    \item renamed parameter \code{SOLVER_CTRL_PARAM} to \code{SOLVER_CTRL_PARM}
    \item renamed argument \code{param} in \code{SYBIL_SETTINGS()} to
          \code{parm}
    \item file \file{NEWS} is now in \file{inst/NEWS.Rd}
    \item added argument \code{absCtrl} to the \code{plot} method in
          class \code{optsol_robAna}
    \item in \code{readTSVmod}, if a model description file is missing, the
          basename of the reaction list is used as model id, not the complete
          path to the file if fpath is used, or the reaction list is not in
          the working directory
    \item removed arguments \code{verboseMode}, \code{loglevel}, \code{logfile}
          and \code{logfileEnc} from function \code{readTSVmod}; use function
          \code{sink} instead
    \item removed arguments \code{loglevel}, \code{logfile} and
          \code{logfileEnc} from function \code{optimizer}; use function
          \code{sink} instead
    \item added method \code{show} to class \code{sybilError}
    \item methods \code{logOptimization} and \code{logOptimizationNE} of class
          \code{sybilLog} need argument \code{i} giving a consecutive number
    \item methods \code{logComment}, \code{logOptimization},
          \code{logOptimizationNE} and \code{logOptimizationTH} of class
          \code{sybilLog} now also write to \code{stdout} (by \code{cat}) if
          \code{verblevel} is \code{> 2}
    \item fixed a bug in \code{mod2irrev()}: slots \code{met_de} and
          \code{react_de} of class \code{modelorg} (in the reversible model)
          are no longer ignored
    \item added function \code{addReact()} to add or change reactions in a
          given model
    \item added function \code{addExchReact()} to add exchange reactions for
          metabolites to a given model
    \item minor changes in \code{printObjFunc()}
    \item SYBIL_SETTINGS(parm) gives an error if \code{parm} is not a valid
          parameter name
    \item function \code{findExchReact()} now returns indices of metabolite id's
          having an exchange reaction
    \item compatible to \CRANpkg{clpAPI} (version \eqn{\geq}{>=} 1.2.0)
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.0.1 2012-03-16}{
  \itemize{
    \item removed \pkg{sybilSBML} from Suggests field and removed undocumented
          function \code{readModel()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.0.0 2012-03-16}{
  \itemize{
    \item added method \code{dim} to class \code{modelorg}

    \item renamed method \code{setColsBndsObjCoefs} to
          \code{changeColsBndsObjCoefs}

    \item updated documentation

    \item depends on \CRANpkg{glpkAPI} (version \eqn{\geq}{>=} 1.2.1)

    \item needs \CRANpkg{cplexAPI} (version \eqn{\geq}{>=} 1.2.0)

    \item switched from \CRANpkg{SparseM} to \CRANpkg{Matrix}
          (version \eqn{\geq}{>=} 1.0-4), a consequence is a dependency on R
          (version \eqn{\geq}{>=} 2.12.0)

    \item completely new implementation of \code{SYBIL_SETTINGS}

    \item reorganization of files: one class \eqn{\rightarrow}{>=} one file

    \item use Collate field in file \file{DESCRIPTION}

    \item LIFO/FIFO (stack/queue) implementation with new functions \cr
          \code{stinit()}, \code{stclear()}, \code{stpush()},
          \code{stpop()}, \cr
          \code{stunshift()}, \code{stshift()}, \code{stseek()},
          \code{stfirst()}, \cr
          \code{stlist()}, \code{stlength()} and \code{stexists()}

    \item a few compatibility changes in \code{readTSVmod()} for output of
          \href{http://bigg.ucsd.edu/}{BiGG database}: added arguments
          \code{quoteChar}, \code{commentChar} and \code{oneSubSystem}

    \item added argument \code{minimalSet} to \code{modelorg2tsv()}

    \item added function \code{geneDeletion()} for \eqn{n} gene deletions

    \item fixed a bug in \code{doubleGeneDel()} when \code{allComb = FALSE}

    \item added \code{geneDeletion} to \code{multiDel()}

    \item added argument \code{checkId} to \code{geneDel()}

    \item arguments \code{poCmd} and \code{prCmd} to \code{optimizer} and
          \code{simpleFBA} must be of type \code{list}

    \item some improvements in \code{sybilLogClass}

    \item \code{optimizer()} now makes use of class \code{sybilLog}

    \item removed arguments \code{resolve} and \code{resolveParm} from
          \code{optimizer()}

    \item removed \pkg{sybil} parameters \code{LP_ALT_METHOD} and \cr
          \code{SOLVER_ALT_CTRL_PARAM} from \code{SYBIL_SETTINGS()}

    \item renamed \pkg{sybil} parameter \code{LP_SOLVER to SOLVER}

    \item renamed \pkg{sybil} parameter \code{LP_METHOD to METHOD}

    \item moved methods \code{length} and \code{hist} from class
          \code{optsol_fluxdel} to class \code{optsol}

    \item added slot num_of_prob in class checksol

    \item fixed a bug in class \code{optObj} when setting parameters to
          \CRANpkg{lpSolveAPI}

    \item added arguments \code{connect} and \code{colconnect} to the
          \code{plot} mathod of class \code{optsol_fluxVar}

    \item updated package vignette
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.11 2011-12-12}{
  \itemize{
    \item fixed a bug in \code{readTSVmod()} when reading closed networks

    \item \code{.createReactionString()} now adds the compartment id to the
          metabolite id if it is not already there

    \item if the metabolite id's do not contain the compartment abbreviation in
          square brackets, the compartment names are used by
          \code{modelorg2tsv()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.10 2011-12-09}{
  \itemize{
    \item added arguments \code{exclM} and \code{exclR} to
          \code{.singletonMetabolite()}

    \item in \code{readTSVmod()}: upper and lower bounds which are larger
          than \code{abs(def_bnd)} are set to \code{abs(def_bnd)}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.9 2011-08-30}{
  \itemize{
    \item added slots \code{met_de} and \code{react_de} to \code{modelorg}

    \item added Boolean arguments \code{remMet}, \code{contrMet},
          \code{singletonMet} and \code{deadEndmet} to \code{readTSVmod()}, the
          two Boolean arguments \code{remSingleMet} and \code{constrSingleMet}
          are removed, see \code{help("readTSVmod")} for details
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.8 2011-08-22}{
  \itemize{
    \item fixed a bug in constructor of class \code{sybilError}

    \item depends on \CRANpkg{cplexAPI} (version \eqn{\geq}{>=} 1.1.7)

    \item \code{checkOptSol()} is compatible with \CRANpkg{cplexAPI}
          (version 1.1.7) in combination with IBM ILOG CPLEX (version 12.3)
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.7 2011-08-15}{
  \itemize{
    \item slots \code{met_single} and \code{react_single} of class
          \code{modelorg} are initialized with \code{NA}, not \code{FALSE}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.6 2011-08-15}{
  \itemize{
    \item some minor changes in \code{readTSVmod()} according to
          \CRANpkg{codetools}

    \item fixed a bug in class \code{logFileFH}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.5 2011-08-12}{
  \itemize{
    \item added slots \code{met_single} and \code{react_single} to class
          \code{modelorg}

    \item new arguments for \code{readTSVmod()}:
          \code{mergeMet}, \code{balanceReact}, \code{remUnusedMetReact},
          \code{remSingleMet}, \code{constrSingleMet} and \code{tol} see
          \code{help("readTSVmod")} for details
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.4 2011-08-08}{
  \itemize{
    \item method \code{logError} of class \code{sybilLog} returns object of
          class \code{sybilError}

    \item fixed a bug in \code{.printNamedList()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.3 2011-08-05}{
  \itemize{
    \item added class \code{sybilLog}

    \item added logging in \code{readTSVmod()}, see \code{help("readTSVmod")}
          for details and new arguments
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.2 2011-07-29}{
  \itemize{
    \item added \code{NAMESPACE}

    \item fixed a minor bug in \code{findExchReact()}
    \item fixed a bug in method \code{show} for class \code{SYBIL_SETTINGS}
    \item fixed a bug in \code{.validModelorg()}

    \item changes in \code{readTSVmod()}:
      \itemize{
        \item If a metabolite is used more than once as product or
        educt of a particular reaction, it is merged:
        \preformatted{a + (2) a}
        is converted to
        \preformatted{(3) a}
        and a warning will be given.

        \item If a metabolite is used first as educt and then as
        product of a particular reaction, the reaction is
        balanced:
        \preformatted{(2) b + a -> b + c}
        is converted to
        \preformatted{    b + a ->     c}

        \item The stoichiometric matrix S is scanned for metabolites, which are
              used only once in S. If there are some, a warning will be given.

        \item The stoichiometric matrix S is scanned for reactions, which are
              not used in S. If there are some, a warning will be given and the
              corresponding reactions will be removed from the model.

        \item Some improvements in parsing the reaction string: if more than one
              reaction arrow is found, a warning is given and the reaction will
              be removed from the model.
    }
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.1 2011-07-12}{
  \itemize{
    \item support for low level interface \CRANpkg{glpkAPI} (version 1.1.2)
    \item support for low level interface \CRANpkg{clpAPI} (version 1.1.1)
    \item support for low level interface \CRANpkg{cplexAPI} (version 1.1.1)

    \item added new slot in class \code{optObj}: \code{probType}.
          The constructor function \code{optObj()} has a new argument
          \code{pType}, with \code{lp} as default value (linear programming).
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.5.0 2011-05-20}{
  \itemize{
    \item updated documentation
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.4.1 2011-05-18}{
  \itemize{
    \item slot \code{subSys} in class \code{modelorg} is now of class
          \code{matrix}, so one reaction can belong to more than one subsystem,
          for example:
          \tabular{llll}{
                       \tab \code{cytosol} \tab  \code{mitochondria} \tab  \code{external} \cr
            \code{v_1} \tab \code{TRUE}    \tab  \code{FALSE}        \tab  \code{FALSE}    \cr
            \code{v_2} \tab \code{TRUE}    \tab  \code{TRUE}         \tab  \code{FALSE}    \cr
            \code{v_3} \tab \code{FALSE}   \tab  \code{TRUE}         \tab  \code{FALSE}    \cr
            \code{b_1} \tab \code{FALSE}   \tab  \code{FALSE}        \tab  \code{TRUE}     \cr
            \code{b_2} \tab \code{FALSE}   \tab  \code{FALSE}        \tab  \code{TRUE}     \cr
          }
          In slot \code{subSys}, only the column names are named according to
          the subsystem. The rows do not have names, the first row corresponds
          to the first reaction in slot \code{react_id}, the second row to the
          second reaction id, and so forth.

    \item added method \code{getRedCost} to class \code{optObj}.
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.4.0 2011-03-01}{
  \itemize{
    \item SBML compatibility is moved to the package \pkg{sybilSBML}, making
          \pkg{sybil} independend from rsbml. The new package \pkg{sybilSBML}
          requires a working \pkg{rsbml} installation (available from
          \href{http://www.bioconductor.org/}{Bioconductor}).

    \item added functions \code{modelorg2tsv()} and \code{readTSVmod()} in order
          to handle \file{tsv} files compatible to the
          \href{http://bigg.ucsd.edu/}{BiGG database}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.3.6 2010-12-16}{
  \itemize{
    \item fixed a bug in \code{optimizer()} when pre/post-processing
          should be performed, but no optimization was done.
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.3.5 2010-12-15}{
  \itemize{
    \item added argument \code{solverParm} in \code{fluxVar()}, argument
          \code{\dots} is passed to \code{simpleFBA()} now

    \item cleaning of class \code{optObj}:
    \itemize{
      \item new classes extending \code{optObj}:
            \code{optObj_glpk}, \code{optObj_clp}, \code{optObj_cplex}
            and \code{optObj_lpSolveAPI}

      \item methods \code{setRhsZero} and \code{setColsBndsObjCoefs} are not
            usable for initial model creation (this is a feature, not a bug).
            Use \code{addRows} and \code{addCols} here.
    }

    \item added stack like functions
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.3.4 2010-11-05}{
  \itemize{

    \item added function \code{multiDel()}: \code{multicore} support for
          \code{oneGeneDel()}, \code{doubleGeneDel()}, \code{oneFluxDel()}
          and \code{doubleFluxDel()}

    \item fixed a bug in \code{fluxVar()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.3.3 2010-09-28}{
  \itemize{
    \item removed file \file{NAMESPACE}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.3.2 2010-09-22}{
  \itemize{
    \item blockedReact() now works also for irreversible models.
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.3.1 2010-09-14}{
  \itemize{
    \item some improvements in \code{readSBMLmod()}

    \item depends on \pkg{rsbml} version \eqn{\geq}{>=} 2.4.0
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.3.0 2010-08-20}{
  \itemize{
    \item first public beta release
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.2.8 2010-08-07}{
  \itemize{
    \item some things were renamed:
      \itemize{
        \item class \code{postAnalysis} is now class \code{ppProc}
        \item function \code{postProcessing()} is now function
              \code{.ppProcessing()}
        \item slot \code{postAna} in class \code{optsol_simpleFBA} is now
              slot \code{postProc}
        \item argument \code{ppCmd} in function \code{simpleFBA()} is now
              argument \code{poCmd}
      }

    \item added slot \code{preProc} in class \code{optsol_simpleFBA()}

    \item added argument \code{prCmd} in function \code{simpleFBA()}:
          same purpose as argument \code{poCmd}, but the commands are
          executed immideately before solving the optimization problem.
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.2.7 2010-08-06}{
  \itemize{
    \item added slot \code{lethal} to class \code{optsol_doublefluxdel}

    \item added method \code{ind2id()} to class \code{optsol_fluxdel},
          \code{optsol_genedel} and \code{optsol_doublegenedel}

    \item some minor improvements in \code{doubleGeneDel()}

    \item added classes \code{sybilError} and \code{postAnalysis}

    \item added function \code{postProcessing()}

    \item added slot \code{postAna} in class \code{optsol_simpleFBA}

    \item added method \code{sensitivityAnalysis} in class \code{optObj}

    \item added argument \code{ppCmd} in \code{simpleFBA()}:

      The argument \code{ppCmd} should be a character vector. If it has a
      length \eqn{=} 1, it will be treated as a function name, if it has
      length \eqn{>} 1, it will be treated as function name (\code{ppCmd[1]})
      plus arguments (\code{ppCmd[-1]}).  Basically, \code{ppCmd} is treated as
      a command, executed on the problem object (object of class \code{optObj}),
      after the problem has been solved.  The first element of \code{ppCmd} will
      be used as function name, all other elements as arguments to that
      function. The command will be joint with \code{\sQuote{,}} and
      \code{\sQuote{()}}:
      \preformatted{
ppCmd[1] "(" ppCmd[-1] ")"
      }
      and \code{ppCmd[-1]} will be joind with \code{\sQuote{,}}.
    
      The string \code{\dQuote{LP_PROB}} will be used as a placeholder for the
      variable name of the problem object. If \code{\dQuote{LP_PROB}} is used
      in \code{ppCmd}, it will be replaced.  The result of the execution of
      \code{ppCmd} will be stored in an object of class \code{postAnalysis}
      in the slot \code{postAna} of class \code{optsol_simpleFBA}.
    
      Some examples:
      \itemize{
        \item perform sensitivity analysis with GLPK:
        \preformatted{
simpleFBA(<model>, ppCmd = "sensitivityAnalysis")
        }
        Here, \code{ppCmd} has length \eqn{=} 1,
        \code{\dQuote{sensitivityAnalysis}} will be treated as a function name
        and executed on the problem object.

        \item access reduced costs in conjunction with IBM ILOG CPLEX:
        \preformatted{
simpleFBA(<model>, solver = "cplex",
          ppCmd = c("getDjCPLEX",
                    "LP_PROB@oobj$env", "LP_PROB@oobj$lp",
                    "0", "react_num(<model>)-1"))
        }
        In that example, \code{ppCmd} has length \eqn{>} 1, the first element
        is the function name, the others are the arguments.  For IBM ILOG CPLEX,
        slot \code{oobj} of objects of class \code{optObj} is a list,
        containing a pointer to the cplex environment \code{env} and a second
        pointer to the problem object \code{lp}.  The third and fourth argument
        are \code{begin} and \code{end} for the function \code{getDjCPLEX()}.
        Remember: in IBM ILOG CPLEX, the first element has index 0!
      }
      
      Instances of class \code{postAnalysis} have two slots:
      \describe{
        \item{cmd}{a single character string containing the executed command}
        \item{pa}{the result of the post processing. If an error occured,
                  \code{pa} contains the error message and is of class
                  \code{sybilError}}
      }
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.2.6 2010-06-30}{
  \itemize{
    \item fixed a bug in \code{optimizer()}:
          flux distribution was not saved when argument \code{rebuildModel} is
          set to \code{TRUE} and the used algorithm is
          \code{\dQuote{linearMOMA}}

    \item option \code{resolve = TRUE} in \code{optimizer()} is working again.

    \item setting of parameters to lp solver with \code{SYBIL_SETTINGS} is
          working again.

    \item improved function \code{blockedReact()}.
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.2.5 2010-06-22}{
  \itemize{
    \item fixed a bug in \code{changeObjCoefs()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.2.4 2010-05-19}{
  \itemize{
    \item fixed a bug in \code{onlyChangeGPR()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.2.3 2010-05-17}{
  \itemize{
    \item fixed a bug in \code{changeGPR()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.2.2 2010-05-12}{
  \itemize{
    \item added function \code{changeGPR()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.2.1 2010-04-28}{
  \itemize{
    \item fixed a small bug in \code{readSBMLmod()}

    \item fixed a bug in \code{optimizer()} in conjunction with IBM ILOG CPLEX
          and parameter \code{copyModel = TRUE}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.2.0 2010-04-26}{
  \itemize{
    \item fixed a bug in \code{optObj_Class}:
          \code{loadProblemDataML()} produced an error when
          \code{solver = "lpSolveAPI"} and \code{alg = "linearMOMA_COBRA"}

    \item method \code{loadProblemDataMTF} in \code{optObj_Class} failed when
          IBM ILOG CPLEX is used

    \item fixed a bug in \code{mod2irrev()}: slot \code{rev2irrev} contained
          wrong entries.
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.1.9 2010-02-28}{
  \itemize{
    \item argument \code{resolve} in \code{optimizer()} is not working in this
          version!

    \item completely rewritten function \code{simpleFBA()}; new (changed)
          arguments:
      \describe{
        \item{react}{(default: \code{NA}) fluxes with changed lower and upper
          bounds}
        \item{lb}{(default: \code{NA}) new lower bounds for fluxes in argument
          \code{react}}
        \item{ub}{(default: \code{NA}) new upper bounds for fluxes in argument
          \code{react}}
        \item{minTotalFlux}{(default: \code{FALSE}) Boolean, if set to
          \code{TRUE}, after FBA, the sum of all flux rates will be minimized}
        \item{minDist}{(default: \code{FALSE}) Boolean, if set to \code{TRUE},
          a minimized distance to a wild type flux distribution will be
          generated}
        \item{wtFluxes}{(default: \code{NA}) a single flux value (if
          argument \code{minTotalFlux} is set to \code{TRUE}) for on optimized
          objective function, or a flux vector (if argument \code{minDist} is
          set to \code{TRUE}) for an optimized flux distribution.
          If \code{wtFluxes} is \code{NA}, FBA will be performed. If arguments
          \code{minDist} or \code{minTotalFlux} are TRUE and \code{wtFluxes} is
          \code{NA}, the needed values will be calculated by FBA.}
      }

    \item added method \code{backupProb()} in class \code{optObj}:
      \describe{
        \item{glpk/cplex}{the functions
          \code{copyProbGLPK()}/\code{cloneProbCPLEX()} will be used here, which
          return a pointer to the new problem object}
        \item{clp/lpSolveAPI}{the functions
          \code{saveModelCLP()}/\code{write.lp()} will be used here, which write
          the specified problem to file. That file will be deleted after
          building the new problem out of those. That may cause a lot IO.}
      }
    \item new arguments to \code{optimizer()} [can be passed to
          \code{optimizer()} by argument \code{\dots} in e.g.
          \code{oneGeneDel()} or \code{doubleGeneDel()}]:
      \describe{
        \item{rebuildModel}{(default: \code{FALSE}) Boolean. If set to
          \code{TRUE}, the problem object will be recreated for every
          optimization.  That will increase the running time significantly!}
        \item{copyModel}{(default: \code{FALSE}) Boolean. If set to \code{TRUE},
          the problem object will be taken from a backup copy. That backup copy
          will be created prior to the very first optimization from the modelorg
          object.}
      }
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.1.8 2010-01-17}{
  \itemize{
    \item argument \code{alg} in \code{optimizer()} works now for all solvers
    \item added the used algorithm in the logfile
    \item added dependency on \CRANpkg{SparseM}
    \item added options \code{exLethal} and \code{tol} to \code{doubleGeneDel()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.1.7 2009-12-07}{
  \itemize{
    \item argument \code{alg} in \code{optimizer()}: can be \code{\dQuote{FBA}}
          (default), \code{\dQuote{linearMOMA}} or
          \code{\dQuote{linearMOMA_COBRA}}; works only with GLPK
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.1.6 2009-12-03}{
  \itemize{
    \item added method \code{length} for objects of class \code{optsol_fluxdel}

    \item added method \code{\dQuote{[}} for objects of class
          \code{optsol_fluxdel}

    \item added argument \code{alg} in \code{optimizer()}: can be
          \code{\dQuote{FBA}} (default) or \code{\dQuote{linearMOMA}};
          works only with glpk

    \item added slot \code{algorithm} and methods \code{algorithm} and
          \code{algorithm<-} for objects of class \code{optsol_fluxdel}
          containing the name of the used algorithm.

    \item added slot \code{TOLERANCE} in \code{SYBIL_SETTINGS}

    \item added slot \code{MAXIMUM} in \code{SYBIL_SETTINGS}

    \item added slot \code{ALGORITHM} in \code{SYBIL_SETTINGS}

    \item added slot \code{OPT_DIRECTION} in \code{SYBIL_SETTINGS}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.1.5 2009-11-16}{
  \itemize{
    \item fixed a bug in \code{optObj_Class}, method \code{changeColsBnds} when
          using ILOG CPLEX as solver

    \item in \code{optObj_Class} the methods \code{getColsLowBnds} and
          \code{getColsUppBnds} now use \code{getLowBndsIdsCPLEX} and
          \code{getUppBndsIdsCPLEX} respectively when using ILOG CPLEX as solver
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.1.4 2009-10-01}{
  \itemize{
    \item fixed a bug in \code{nonZeroElements()}:
          empty rows and columns are now handled correctly when argument
          \code{maorder} is set to \code{\dQuote{column}} or \code{\dQuote{row}}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.1.3 2009-07-23}{
  \itemize{
    \item added support for low level interface \pkg{sybilCPLEX} (version 0.0.2)

    \item minor changes applied to \file{uglyHack.R} in order to support
          libsbml version 3.4.1
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.1.2 2009-07-21}{
  \itemize{
    \item added setting \code{LP_ALT_METHOD}, used for resolve in
          \code{optimizer()} and \code{refineSol()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.1.1 2009-07-14}{
  \itemize{
    \item added class \code{SYBIL_SETTINGS} which contains default settings,
          currently for the default lp solver and corresponding method.
          The class contains the relevant set/get methods to fit the
          content to ones needs.

    \item added method \code{addRowsCols} to class \code{optObj}

    \item added support for low level interface \CRANpkg{lpSolveAPI}
          (version 5.5.0.14)

    \item added support for low level interface \pkg{sybilCLP} (version 0.0.1)

    \item argument \code{resolve} in \code{oneFluxDel()}, \code{oneGeneDel()},
          \code{doubleFluxDel()} and \code{doubleGeneDel()}
      \itemize{
        \item default: \code{NA}
        \item if \code{resolve} is enabled, value should be a valid method
              depending on the desired solver (e.g. \code{\dQuote{exact}} in
              case of \code{solver = "glpk"})
      }
    \item renamed optObj-method \code{setColsBnds} to \code{changeColsBnds}

    \item renamed optObj-method \code{setRowsBnds} to \code{changeRowsBnds}

    \item renamed optObj-method \code{setObjCoefs} to \code{changeObjCoefs}

    \item argument \code{resolve} in \code{optimizer()} and \code{refineSol()}:
      \itemize{
        \item no longer Boolean
        \item default: \code{NA}
        \item if non optimal solutions should be resolved: set to method
              resolve should use
      }

    \item argument \code{logfile} in \code{optimizer()} and \code{refineSol()}:
      \itemize{
        \item no longer Boolean
        \item default: \code{NA}
        \item if a logfile should printed, \code{logfile} will be the filename
      }

    \item argument \code{scaling} in \code{optimizer()} and \code{refineSol()}:
      \itemize{
        \item no longer Boolean
        \item default: \code{NA}
        \item if the model should be scaled, scaling will be the scaling method
      }

    \item added \code{solver} and \code{method} in \file{logfile}

    \item added argument \code{checkOptSolObj} to function \code{optimizer()}:
          print a warning, in cases where solution status \eqn{\neq}{!=} 0
          in objects of class optsol (default: \code{FALSE})

    \item added slots \code{chlb} and \code{chub} to class \code{optsol}:
      \itemize{
        \item containing lower and upper bounds of changed flux rates.
        \item replace method: \code{ch?b<-}
        \item get method: \code{ch?b}
      }
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 0.1.0 2009-07-09    (initial release)}{
  \itemize{
    \item support for low level interface \pkg{sybilGLPK} (version 0.1.0)
  }
}
