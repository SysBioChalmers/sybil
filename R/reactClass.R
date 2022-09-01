#					   definition of the class react						#
setClass("react",
	representation(
		 react_rev	  = "logical",	   # vector reversibilities
		 react_id	  = "character",   # reaction id
		 react_name	  = "character",   # reaction name
		 react_single = "logical",	   # reaction using metabolites appearing only once in S
		 react_de	  = "logical",	   # reaction using dead end metabolites
		 react_attr	  = "data.frame",  # reaction attributes
		 met_id		  = "character",   # metabolites used in this reaction
		 met_comp	  = "character",   # compartments of metabolites
		 met_name	  = "character",   # metabolite names
		 met_attr	  = "data.frame",  # metabolite attributes
		 comp_attr	  = "data.frame",  # compartment attributes
		 s			  = "numeric",	   # matrix S
		 lowbnd		  = "numeric",	   # reaction lower bound
		 uppbnd		  = "numeric",	   # reaction upper bound
		 obj_coef	  = "numeric",	   # objective coefficient
		 gprRule	  = "character",
		 genes		  = "character",
		 gpr		  = "character",
		 subSys		  = "character"

	),
	validity = .validreact
)

#							 default constructor							   #
setMethod(f = "initialize",
			signature = "react",
			definition = function(.Object, 
								id,
								name="",
								rev=TRUE,
								single=NA,
								de=NA,
								met_id,
								met_name=NULL,
								met_comp=NULL,
								s,
								lowbnd=-1000,
								uppbnd=1000,
								obj_coef=0,
								gprRule="",
								genes="",
								gpr = "",
								subSys = "",
								met_attr = data.frame(),
								react_attr = data.frame(),
								comp_attr = data.frame()
								) {
			stopifnot(!missing(id))
			stopifnot(!missing(met_id))
			stopifnot(!missing(s))
			
			.Object@react_id <- id
			.Object@react_name <- name
			.Object@react_rev <- rev
			.Object@react_single <- single
			.Object@react_de <- de
			.Object@met_id <- met_id
			.Object@met_comp <- met_comp
			.Object@met_name <- met_name
			.Object@s <- s
			.Object@lowbnd <- lowbnd
			.Object@uppbnd <- uppbnd
			.Object@obj_coef <- obj_coef
			.Object@gprRule <- gprRule
			.Object@genes <- genes
			.Object@gpr <- gpr
			.Object@subSys <- subSys
			
			.Object@met_attr <- met_attr
			.Object@react_attr <- react_attr
			.Object@comp_attr <- comp_attr
			return(.Object)
		  }
)

#							 setters and getters							   #
# metabolite id's
setMethod("met_id", signature(object = "react"),
		  function(object) {
			  return(object@met_id)
		  }
)

setReplaceMethod("met_id", signature(object = "react"),
		  function(object, value) {
			  object@met_id <- value
			  return(object)
		  }
)

# metabolite names
setMethod("met_name", signature(object = "react"),
		  function(object) {
			  return(object@met_name)
		  }
)

setReplaceMethod("met_name", signature(object = "react"),
		  function(object, value) {
			  object@met_name <- value
			  return(object)
		  }
)

# metabolites compartments
setMethod("met_comp", signature(object = "react"),
		  function(object) {
			  return(object@met_comp)
		  }
)

setReplaceMethod("met_comp", signature(object = "react"),
		  function(object, value) {
			  object@met_comp <- value
			  return(object)
		  }
)

# reversibilities
setMethod("react_rev", signature(object = "react"),
		  function(object) {
			  return(object@react_rev)
		  }
)

setReplaceMethod("react_rev", signature(object = "react"),
		  function(object, value) {
			  object@react_rev <- value
			  return(object)
		  }
)

# reaction id's
setMethod("react_id", signature(object = "react"),
		  function(object) {
			  return(object@react_id)
		  }
)

setReplaceMethod("react_id", signature(object = "react"),
		  function(object, value) {
			  object@react_id <- value
			  return(object)
		  }
)

# reaction names
setMethod("react_name", signature(object = "react"),
		  function(object) {
			  return(object@react_name)
		  }
)

setReplaceMethod("react_name", signature(object = "react"),
		  function(object, value) {
			  object@react_name <- value
			  return(object)
		  }
)

# singletons
setMethod("react_single", signature(object = "react"),
		  function(object) {
			  return(object@react_single)
		  }
)

setReplaceMethod("react_single", signature(object = "react"),
		  function(object, value) {
			  object@react_single <- value
			  return(object)
		  }
)


# dead ends
setMethod("react_de", signature(object = "react"),
		  function(object) {
			  return(object@react_de)
		  }
)

setReplaceMethod("react_de", signature(object = "react"),
		  function(object, value) {
			  object@react_de <- value
			  return(object)
		  }
)

# stoichiometric matrix
setMethod("s", signature(object = "react"),
		  function(object) {
			  return(object@s)
		  }
)

setReplaceMethod("s", signature(object = "react"),
		  function(object, value) {
			  object@s <- value
			  return(object)
		  }
)

# lower bounds
setMethod("lowbnd", signature(object = "react"),
		  function(object) {
			  return(object@lowbnd)
		  }
)

setReplaceMethod("lowbnd", signature(object = "react"),
		  function(object, value) {
			  object@lowbnd <- value
			  return(object)
		  }
)

# upper bounds
setMethod("uppbnd", signature(object = "react"),
		  function(object) {
			  return(object@uppbnd)
		  }
)

setReplaceMethod("uppbnd", signature(object = "react"),
		  function(object, value) {
			  object@uppbnd <- value
			  return(object)
		  }
)


# objective coefficient
setMethod("obj_coef", signature(object = "react"),
		  function(object) {
			  return(object@obj_coef)
		  }
)

setReplaceMethod("obj_coef", signature(object = "react"),
		  function(object, value) {
			  object@obj_coef <- value
			  return(object)
		  }
)


# gprRules
setMethod("gprRule", signature(object = "react"),
		  function(object) {
			  return(object@gprRule)
		  }
)

setReplaceMethod("gprRule", signature(object = "react"),
		  function(object, value) {
			  object@gprRule <- value
			  return(object)
		  }
)

# genes
setMethod("genes", signature(object = "react"),
		  function(object) {
			  return(object@genes)
		  }
)

setReplaceMethod("genes", signature(object = "react"),
		  function(object, value) {
			  object@genes <- value
			  return(object)
		  }
)


# gpr associations
setMethod("gpr", signature(object = "react"),
		  function(object) {
			  return(object@gpr)
		  }
)

setReplaceMethod("gpr", signature(object = "react"),
		  function(object, value) {
			  object@gpr <- value
			  return(object)
		  }
)

# reaction sub systems
setMethod("subSys", signature(object = "react"),
		  function(object) {
			  return(object@subSys)
		  }
)

setReplaceMethod("subSys", signature(object = "react"),
		  function(object, value) {
			  object@subSys <- value
			  return(object)
		  }
)

# metabolites attributes
setMethod("met_attr", signature(object = "react"),
          function(object) {
              return(object@met_attr)
          }
)

setReplaceMethod("met_attr", signature(object = "react"),
          function(object, value) {
              object@met_attr <- value
              return(object)
          }
)

# reaction attributes
setMethod("react_attr", signature(object = "react"),
          function(object) {
              return(object@react_attr)
          }
)

setReplaceMethod("react_attr", signature(object = "react"),
          function(object, value) {
              object@react_attr <- value
              return(object)
          }
)

# compartment attributes
setMethod("comp_attr", signature(object = "react"),
          function(object) {
              return(object@comp_attr)
          }
)

setReplaceMethod("comp_attr", signature(object = "react"),
          function(object, value) {
              object@comp_attr <- value
              return(object)
          }
)

# model attributes
setMethod("mod_attr", signature(object = "react"),
          function(object) {
              return(object@mod_attr)
          }
)

setReplaceMethod("mod_attr", signature(object = "react"),
          function(object, value) {
              object@mod_attr <- value
              return(object)
          }
)

setMethod("show", signature(object = "react"),
	function(object) {
		cat("react id:              ", react_id(object), "\n")
		cat("react name:            ", react_name(object), "\n")
		cat("stoichiometry:\n")
		print(data.frame(met_id=met_id(object), s_coef=s(object)))
		cat("lower bound:           ", lowbnd(object), "\n")
		cat("upper bound:           ", uppbnd(object), "\n")
		cat("objective function:    ", obj_coef(object), "\n")
	}
)

setMethod("printReaction", signature(object = "react"),
	function(object, printOut = TRUE, ...) {
	
		mat <- s(object)
		reaction <- character(1)

		
		met <- met_id(object)
		nzv <- mat
		
		ed <- nzv < 0
		pd <- nzv > 0

		if (sum(ed) > 0) {
			educt	<- paste(paste("(", abs(nzv[ed]), ")", sep = ""),
							 met[ed], collapse = " + ")
		}
		else {
			educt = ""
		}

		if (sum(pd) > 0) {
			product <- paste(paste("(", nzv[pd], ")", sep = ""),
							 met[pd], collapse = " + ")
		}
		else {
			product = ""
		}
		
		arrow <- ifelse(react_rev(object), " <==> ", " --> ")
		
		reaction <- paste(react_id(object),
							 paste(educt, product, sep = arrow), sep = "\t")

		if (isTRUE(printOut)) {
		   cat("abbreviation\tequation", reaction, sep = "\n", ...)
		}
		return(invisible(reaction))
	}
)











