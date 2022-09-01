setClass(Class = "sysBiolAlg_fbaEasyConstraint",
			representation(
				easyConstraint = "list"
			),
			contains = "sysBiolAlg"
)

#                            default constructor                               #
# contructor for class sysBiolAlg_fbaEasyConstraint
setMethod(f = "initialize",
          signature = "sysBiolAlg_fbaEasyConstraint",
          definition = function(.Object,
                                model,
                                lpdir = SYBIL_SETTINGS("OPT_DIRECTION"),
                                useNames = SYBIL_SETTINGS("USE_NAMES"),
                                cnames = NULL,
                                rnames = NULL,
                                pname = NULL,
                                scaling = NULL,
                                easyConstraint = NULL,
                                writeProbToFileName = NULL, ...) {

              if ( ! missing(model) ) {

                  stopifnot(is(model, "modelorg"),
                            is(lpdir, "character"))
                  
                  # problem dimensions
                  nCols <- react_num(model)
                  nRows <- met_num(model)

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
                              paste("FBA", mod_id(model), sep = "_"),
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
                  
                  mat <- S(model)
                  rtype <- rep("E", nRows)
                  rlb <- rep(0, nRows)
                  rub <- rep(0, nRows)
                  
                  #add easyConstraints:
                  if(!is.null(easyConstraint)){
                  	if(		length(easyConstraint$react) != length(easyConstraint$x)
                  		| 	length(easyConstraint$react) != length(easyConstraint$rtype)
                  		){
                  		stop("easyConstraint elements have to have equal lengths")
                  	}
                  	stopifnot(is.list(easyConstraint$react))
                  	stopifnot(is.list(easyConstraint$x))
                  	stopifnot(all(easyConstraint$rtype %in% c("F", "L", "U", "D", "E")))
                  	
                  	# setting and checking rlb
                  	if(is.null(easyConstraint$lb)){
                  		rlb <- c(rlb, rep(0, length(easyConstraint$react)))
                  	}else{
                  		if(length(easyConstraint$react) != length(easyConstraint$lb)){
                  			stop("easyConstraint$lb length has to match length of react argument")
                  		}else{
                  			stopifnot(is.numeric(easyConstraint$lb))
                  			rlb <- c(rlb, easyConstraint$lb)
                  		}
                  	}
                  	
                  	# setting and checking rub
                  	if(is.null(easyConstraint$ub)){
                  		rub <- c(rub, rep(0, length(easyConstraint$react)))
                  	}else{
                  		if(length(easyConstraint$react) != length(easyConstraint$ub)){
                  			stop("easyConstraint$ub length has to match length of react argument")
                  		}else{
                  			stopifnot(is.numeric(easyConstraint$ub))
                  			rub <- c(rub, easyConstraint$ub)
                  		}
                  	}
                  	
                  	m <- Matrix(0, ncol=nCols, nrow=length(easyConstraint$react))
                  	
                  	for(i in 1:length(easyConstraint$react)){
                  		m[i, easyConstraint$react[[i]]] <- easyConstraint$x[[i]]
                  	}
                  	
                  	
                  	mat <- rbind2(mat, m)
                  	rtype <- c(rtype, easyConstraint$rtype)
                  	nRows <- nRows + length(easyConstraint$react)
                  	if(!is.null(rowNames)){
                  		rowNames <- c(rowNames, paste0("easyConstraint", 1:length(easyConstraint$react)))
                  	}
                  	
                  }
                  
                  # generate problem object
                  .Object <- callNextMethod(.Object,
                                            sbalg      = "fba",
                                            pType      = "lp",
                                            scaling    = scaling,
                                            fi         = 1:nCols,
                                            nCols      = nCols,
                                            nRows      = nRows,
                                            mat        = mat,
                                            ub         = uppbnd(model),
                                            lb         = lowbnd(model),
                                            obj        = obj_coef(model),
                                            rlb        = rlb,
                                            rtype      = rtype,
                                            lpdir      = lpdir,
                                            rub        = rub,
                                            ctype      = NULL,
                                            cnames     = colNames,
                                            rnames     = rowNames,
                                            pname      = probName,
                                            ...)
					.Object@easyConstraint <- easyConstraint
                  if (!is.null(writeProbToFileName)) {
                      writeProb(problem(.Object),
                                fname = as.character(writeProbToFileName))
                  }
              }
              return(.Object)
          }
)
