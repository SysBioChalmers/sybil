# The function geneDel() is inspired by the function
# deleteModelGenes() contained in the COBRA Toolbox.
geneDel <- function(model, genes, checkId = FALSE) {
#geneDel <- function(model, genes, lpmodel, solver = SYBIL_SETTINGS("SOLVER")) {
  #if (missing(lpmodel)) {
  #    solver = "none"
  #}
  if (!is(model, "modelorg")) {
      stop("needs an object of class modelorg!")
  }
  
  stopifnot(checkVersion(model))
  
  if (isTRUE(checkId)) {
      if (is(genes, "character")) {
          # Check if all genes are there
          geneExist <- which(is.na(match(genes, allGenes(model))))
        
          # if thats not the case ...
          if (length(geneExist) != 0) {
              stop(sprintf(ngettext(length(geneExist),
                                    "gene %s does not exist",
                                    "genes %s do not exist"
                                    ), paste(sQuote(genes[geneExist]), collapse = ", ")
                           )
                   )
          }
        
          geneInd <- match(genes, allGenes(model))
      }
      else {
          if (max(genes) > length(allGenes(model))) {
              stop("indices in argument genes do not exist")
          }
          else {
              geneInd <- genes
          }
      }
  }
  else {
      if (is(genes, "numeric")) {
          geneInd <- genes
      }
      else {
          stop("argument genes must be numeric, if checkId is FALSE")
      }
  }

  # Get the reaction id's of reactions in the gene list. Returns a list
  # Perhaps we find something better here.
#  reactInd <- as.list(apply(
#                    as.matrix(rxnGeneMat(model)[,geneInd]), 2, function(x)
#                                                        which(x != 0)
#
#                    ))
  reactInd <- apply(rxnGeneMat(model)[,geneInd, drop = FALSE], 2, function(x) which(x != 0) )
  reactInd <- unlist(reactInd)

  xAll <- rep(TRUE, length(allGenes(model)))
  xAll[geneInd] <- FALSE
  names(xAll) <- allGenes(model)
  constReact <- logical(length(reactInd))

  # Constrain a reaction if the corresponding gpr rule is FALSE.
  # If that's the case, the reaction needs gene bla.
  ru <- gprRules(model)[reactInd]
  ge <- genes(model)[reactInd]
  for(i in 1:length(reactInd)) {
      #print(reactInd[i])
      #print(ru[i])
      #ev <- eval(parse(text = ru[i]))
      
      #define x for eval:
      x <- xAll[ge[[i]]]
      
      ev <- tryCatch(eval(parse(text = ru[i])), error = function(e) e)
      if (is(ev, "simpleError")) {
          stop("wrong gene association:",
               "\nreaction no. ", reactInd[i],
               "\nreaction id: ", react_id(model)[reactInd[i]],
               "\ngpr: ", gpr(model)[reactInd[i]])
      }
      if (any(is.na(ev))) {
          warning("reference to non existing gene id in gene association:",
                  "\nreaction no. ", reactInd[i],
                  "\nreaction id: ", react_id(model)[reactInd[i]],
                  "\nignoring gpr ", sQuote(gpr(model)[reactInd[i]]))
      }
      else {
          #if (eval(parse(text = gprRules(model)[reactInd[i]])) == FALSE) {
		  #if (eval(parse(text = ru[i])) == FALSE) {
		  if (ev == FALSE) {
			  #print(reactInd[i])
			  #print("cool")
			  constReact[i] <- TRUE
		  }
      }
  }

  if (any(constReact)) {
    return(unique(reactInd[constReact]))
  }
  return(NULL)
}
