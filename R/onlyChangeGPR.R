#     gprRules stands for the new logical Expressions (GPR Rules)
#     reactNr must be numeric
onlyChangeGPR <- function(model, gprRules, reactNr, verboseMode = 0) {
  # is the chosen model type of modelorg?
  if (!is(model, "modelorg")) {
      stop("needs an object of class modelorg!")
  }

  if (!missing(reactNr)) {
      if (!is(reactNr, "numeric")) {
          stop("argument reactNr must be numeric!")
      }
  }

  # allowed logical symbols are stored in logSymb
  logSymb <- c("and", "or", "not", "AND", "OR", "NOT", "&", "|", "!")

  #if (verboseMode > 1) { cat("Updating ... ") }

  for (anz in seq(1, length(gprRules), by = 1)) {

      #check <- tolower(gprRules[anz])
      #check <- gsub("[()]","", check)
      check <- gsub("[()]","", gprRules[anz])
      check <- strsplit(check, "\\s+")
      check <- unlist(check)

      if (check[1] == "") {	
          check <- check[-1]
      }

      gpr(model)[reactNr[anz]] <- gprRules[anz]


      genes(model)[[reactNr[anz]]] <- ""
      rxnGeneMat(model)[reactNr[anz],] <- 0
      gprRules(model)[reactNr[anz]] <- .parseBoolean(gprRules[anz])[[2]]
      gprRules_tmp <- as.numeric(unlist(strsplit(gprRules(model)[reactNr[anz]], "\\D+", perl = TRUE))[-1])

      j <- 1

      for (i in seq(1, length(check), by = 2)) {

          genes(model)[[reactNr[anz]]][j] <- check[i]
          rxnGeneMat(model)[reactNr[anz],match(check[i], allGenes(model))] <- 1
          gprRules(model)[reactNr[anz]] <- gsub( paste("\\(", gprRules_tmp[j], "\\)", sep = ""),
                                                 paste("[", match(check[i], allGenes(model)),"]", sep = ""),
                                                 gprRules(model)[reactNr[anz]]
                                               )
   	      j <- j + 1

      }
      
      genes(model)[[reactNr[anz]]] <- unique(genes(model)[[reactNr[anz]]])
            
      if (length(check) > 1) {

          gpr(model)[reactNr[anz]] <- gsub(" ","  ",gpr(model)[reactNr[anz]]) 
          gprRules(model)[reactNr[anz]] <- gsub(" ","  ",gprRules(model)[reactNr[anz]]) 
          gpr(model)[reactNr[anz]] <- paste("(",gpr(model)[reactNr[anz]],")")
          gprRules(model)[reactNr[anz]] <- paste("(",gprRules(model)[reactNr[anz]],")")

      }
  }
  return(model)
}
