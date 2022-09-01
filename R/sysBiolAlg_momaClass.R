setClass(Class = "sysBiolAlg_moma",
         contains = "sysBiolAlg"
)

#                            default constructor                               #
# contructor for class sysBiolAlg_moma
setMethod(f = "initialize",
          signature = "sysBiolAlg_moma",
          definition = function(.Object,
                                model,
                                wtflux = NULL,
                                Qmat = NULL,
                                scaleDist = NULL,
                                useNames = SYBIL_SETTINGS("USE_NAMES"),
                                cnames = NULL,
                                rnames = NULL,
                                pname = NULL,
                                scaling = NULL,
                                writeProbToFileName = NULL, ...) {

              if ( ! missing(model) ) {

                  # wild type or given flux distribution
                  if (is.null(wtflux)) {
                      tmp <- .generateWT(model, ...)
                      wtsol <- tmp$fluxes[tmp$fldind]
                  }
                  else {
                      wtsol <- wtflux
                  }

                  stopifnot(is(model, "modelorg"),
                            is(wtsol, "numeric"),
                            length(wtsol) == react_num(model),
                            is.null(scaleDist) ||
                            length(scaleDist) == react_num(model))
                  
                  #  the problem: minimize
                  #
                  #            |     
                  #         S  |  = 0
                  #            |     
                  #       -----------
                  #     lb_del
                  #     ub_del
                  #
                  #  obj sum(v_wt - v_del)^2


                  # problem dimensions
                  nCols <- react_num(model)
                  nRows <- met_num(model)

                  if (is.null(scaleDist)) {
                      sdf <- rep(1, nCols)
                  }
                  else {
                      stopifnot(is(scaleDist, "numeric"))
                      sdf <- scaleDist
                  }

                  if (is.null(Qmat)) {
                      Q <- rep(2, nCols)
                  }
                  else {
                      Q <- Qmat
                  }

                  # scaling of particular reactions in the objective function
                  Q <- Q * sdf

                  # row and column names for the problem object
                  if (isTRUE(useNames)) {
                      if (is.null(cnames)) {
                          colNames <- .makeLPcompatible(react_id(model),
                                                                prefix = "x")
                      }
                      else {
                          stopifnot(is(cnames, "character"),
                                    length(cnames) == nCols)
                          colNames <- cnames
                      }

                      if (is.null(rnames)) {
                          rowNames <- .makeLPcompatible(met_id(model),
                                                                prefix = "r")
                      }
                      else {
                          stopifnot(is(rnames, "character"),
                                    length(rnames) == nRows)
                          rowNames <- rnames
                      }

                      if (is.null(pname)) {
                          probName <- .makeLPcompatible(
                              paste("MOMA", mod_id(model), sep = "_"),
                              prefix = "")
                      }
                      else {
                          stopifnot(is(pname, "character"),
                                    length(pname) == 1)
                          probName <- pname
                      }
                  }
                  else {
                      colNames <- NULL
                      rowNames <- NULL
                      probName <- NULL
                  }

                  # build problem object
                  .Object <- callNextMethod(.Object,
                                            sbalg      = "moma",
                                            pType      = "qp",
                                            scaling    = scaling,
                                            fi         = 1:nCols,
                                            nCols      = nCols,
                                            nRows      = nRows,
                                            mat        = S(model),
                                            ub         = uppbnd(model),
                                            lb         = lowbnd(model),
                                            #obj        = -2 * wtsol,
                                            obj        = (-2 * wtsol) * sdf,
                                            rlb        = rep(0, nRows),
                                            rtype      = rep("E", nRows),
                                            lpdir      = "min",
                                            rub        = NULL,
                                            ctype      = NULL,
                                            cnames     = colNames,
                                            rnames     = rowNames,
                                            pname      = probName,
                                            algPar     = list("wtflux" = wtsol,
                                                              "Qmat" = Q,
                                                              "scaleDist" = sdf),
                                            ...)

                  # add quadratic part of objective function
                  loadQobj(.Object@problem, Q)
                  #loadQobj(.Object@problem, rep(2, nCols))
                  #loadQobj(.Object@problem, 2 * Diagonal(nCols))

                  if (!is.null(writeProbToFileName)) {
                      writeProb(problem(.Object),
                                fname = as.character(writeProbToFileName))
                  }

              }
              return(.Object)
          }
)
