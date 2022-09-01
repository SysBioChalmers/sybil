#                  definition of the class optsol_fluxVar                      
setClass("optsol_fluxVar",
         representation(
              react    = "reactId"    # reactions to be analyzed
         ),
         contains = "optsol_optimizeProb"
)

#                            setters and getters                               
# react
setMethod("react", signature(object = "optsol_fluxVar"),
          function(object) {
              return(object@react)
          }
)

setReplaceMethod("react", signature(object = "optsol_fluxVar"),
                 function(object, value) {
                     object@react <- value
                     return(object)
                 }
)

#                               other methods                                  
###
# use barplot? or better hist allow user to set plot to false
###
#setGeneric("plotRangeVar", function(object, ...) standardGeneric("plotRangeVar"))
setMethod("plotRangeVar", signature(object = "optsol_fluxVar"),
          function(object, ...) {
              range <- (
                  (abs(maxSol(object, lp_obj)) - abs(minSol(object, lp_obj)))
                  /abs(maxSol(object, lp_obj))
              )
              #range <- (abs(maxSol(object, lp_obj)) - abs(minSol(object, lp_obj)))
              #blubber <- hist(range, breaks = 500, ...)
              blubber <- hist(range, ...)
              #barplot(range,
              #        names.arg = react_id(react(object)),
              #        las = 2,
              #        ...)
              #return(blubber)
              return(range)
          }
)

setMethod("blReact", signature(object = "optsol_fluxVar"),
          function(object, tol = SYBIL_SETTINGS("TOLERANCE")) {
              
              bl <- abs(minSol(object, "lp_obj")) < tol &
                    abs(maxSol(object, "lp_obj")) < tol
              
              return(bl)
          }
)

setMethod("minSol", signature(object = "optsol_fluxVar"),
          function(object, slot) {

              if (missing(slot)) {
                  stop("argument 'slot' is missing")
              }

              np <- num_of_prob(object)
              
              if (np > 1) {
                  ms <- 1 : floor(np/2)
              }
              else {
                  stop("not enough optimization problems")
              }
              
              #ms <- seq(1, num_of_prob(object), 2)
              
              command  <- paste("is(",
                                deparse(substitute(slot)),
                                "(",
                                deparse(substitute(object)),
                                "))", sep = "")
              slottype <- eval(parse(text = command))

              if (is.na(match("Matrix", slottype))) {
                  command <- paste(deparse(substitute(slot)),
                                   "(",
                                   deparse(substitute(object)),
                                   ")[ms]", sep = "")
              }
              else {
                  command <- paste(deparse(substitute(slot)),
                                   "(",
                                   deparse(substitute(object)),
                                   ")[,ms]", sep = "")
              }
              minimalSolutions <- eval(parse(text = command))
              return(minimalSolutions)
          }
)

setMethod("maxSol", signature(object = "optsol_fluxVar"),
          function(object, slot) {

              if (missing(slot)) {
                  stop("argument 'slot' is missing")
              }

              np <- num_of_prob(object)
              
              if (np > 1) {
                  ms <- ceiling((np/2+1) : np) 
              }
              else {
                  stop("not enough optimization problems")
              }

              #ms <- seq(2, num_of_prob(object), 2)
              
              command  <- paste("is(",
                                deparse(substitute(slot)),
                                "(",
                                deparse(substitute(object)),
                                "))", sep = "")
              slottype <- eval(parse(text = command))

              if (is.na(match("Matrix", slottype))) {
                  command <- paste(deparse(substitute(slot)),
                                   "(",
                                   deparse(substitute(object)),
                                   ")[ms]", sep = "")
              }
              else {
                  command <- paste(deparse(substitute(slot)),
                                   "(",
                                   deparse(substitute(object)),
                                   ")[,ms]", sep = "")
              }
              maximalSolutions <- eval(parse(text = command))
              return(maximalSolutions)
          }
)

setMethod("plot", signature(x = "optsol_fluxVar", y = "missing"),
          function(x, y,
                   ylim,
                   xlab = "reaction no.",
                   ylab = "flux rate",
                   pch = 20,
                   col = "black",
                   collower, colupper, pchupper, pchlower,
                   dottedline = FALSE,
                   baseline = 0,
                   connect = TRUE,
                   colconnect = "black",
                   ...) {

              if (missing(ylim)) {
                  largest  <- ceiling(max(lp_obj(x)))
                  smallest <- floor(min(lp_obj(x)))
                  ylim <- c(smallest, largest)
              }
              else {
                  largest <- ylim[2]
                  smallest <- ylim[1]
              }
              
              if (missing(collower)) {
                  collower <- col
              }
              
              if (missing(colupper)) {
                  colupper <- col
              }

              if (missing(pchlower)) {
                  pchlower <- pch
              }

              if (missing(pchupper)) {
                  pchupper <- pch
              }

              np <- num_of_prob(x)
              num_dots <- np/2
              
              xdat <- rep(1:num_dots, 2)
              #minfl <- lp_obj(x)[c(seq(1, num_of_prob(x), 2))]
              #maxfl <- lp_obj(x)[c(seq(2, num_of_prob(x), 2))]
              minfl <- lp_obj(x)[1:(np/2)]
              maxfl <- lp_obj(x)[(np/2+1):np]

              plot(xdat, c(minfl, maxfl), xlab = xlab, ylab = ylab, ylim = ylim,
                   col = c(collower, colupper),
                   pch = c(pchlower, pchupper), ...)

              if (isTRUE(connect)) {
                  arrows(1:num_dots, minfl, 1:num_dots, maxfl,
                         length = 0, col = colconnect)
              }

              if (dottedline == TRUE) {
                  segments(c(1:num_dots), rep(smallest, num_dots),
                           c(1:num_dots), minfl,
                           lty = "dotted")
              }

              if (!is.na(baseline)) {
                  points(c(1, num_dots), c(baseline, baseline),
                         type = "s", lty = "dashed")
              }
              
          }
)
