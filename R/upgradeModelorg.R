# Takes an instance of a old modelorg version and returns one updated to the
# current version.
upgradeModelorg <- function(object){
	stopifnot(is(object, "modelorg"))
	
	if(!.hasSlot(object, "version") || compareVersion(version(object), "2.0") == -1){
		# object is from a time before versions were introduced.
		# just add version slot
		object@version <- "2.0"
		
		# update gprRules to new format
		rules <- lapply(gpr(object), .parseBoolean)
		if(length(rules) == 0){
			genes(object) <- list()
			gprRules(object) <- character(0)
		}else{
			genes(object) <- lapply(rules, "[[", "gene")
			gprRules(object) <- sapply(rules, "[[", "rule")
		}
		# set attribute slots
		react_attr(object) <- data.frame()
		comp_attr(object) <- data.frame()
		met_attr(object) <- data.frame()
		mod_attr(object) <- data.frame()
		
		#recursively upgrade to latest version.
		return(upgradeModelorg(object))
	}
	if(compareVersion(version(object), SYBIL_SETTINGS("MODELORG_VERSION")) == 0){
		stopifnot(validObject(object))
		return(object)
	}
	stop("unsupported version of modelorg")
}
