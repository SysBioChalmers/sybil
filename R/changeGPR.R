#     model must be an object of modelorg
#     react stands for the reaction, which will be changed
#     (Instance of checkReactId, positions or names)
#     gprRules stands for the new logical Expressions (GPR Rules)
changeGPR <- function(model, react, gprRules = "logicalExpression", verboseMode = 1) {
  # is the chosen model type of modelorg?
  if (!is(model, "modelorg")) {
      stop("needs an object of class modelorg!")
  }
  
  # is the gprRules field empty?
  if (missing(gprRules)) {
      stop("please input an expression!")
  }

  if (any(!is.na(match(gprRules,"")))) {
      stop("\"\" is no valid logical expression!")
  }

  # check the reaction
  if (is(react, "reactId")) {
      reactNr <- react_pos(react)
  }
  else {
      if (!is(checkReactId(model, react), "reactId")) {
          stop("At least one reaction does not exist in the model!")
      }
      else {
          reactNr <- react_pos(checkReactId(model, react))
      }
  }

  if ( !identical(length(reactNr), length(gprRules)) ) {
      stop("not as many logical expressions as reactions!")
  }

  ###### is the logical Expression correct? ######

  checkV <- onlyCheckGPR(model, gprRules, reactNr, verboseMode=verboseMode)

  if (verboseMode > 1) { cat("Updating ... ") }

  model <- onlyChangeGPR(model, gprRules, reactNr, verboseMode=verboseMode)
  if (verboseMode > 1) { cat("OK.\n") }
  if (verboseMode > 0) { cat("Finished.\n") }
  return(model)
}
